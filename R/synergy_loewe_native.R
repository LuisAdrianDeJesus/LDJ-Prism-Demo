# Staging implementation. Fit one monotone 3PL curve per monotherapy, fixing
# the lower asymptote to the observed zero-dose control. Concentrations use M.
ldj_fit_loewe_monotherapy <- function(dose, response, baseline) {
  keep <- dose > 0 & is.finite(dose) & is.finite(response)
  x <- log(dose[keep])
  y <- response[keep]
  if (length(unique(x)) < 5L || !is.finite(baseline) || baseline < 0 || baseline >= 100 ||
      any(y < 0 | y > 100) || diff(range(y)) < 1e-6) return(NULL)
  predict <- function(p) baseline + (p[1] - baseline) *
    stats::plogis(exp(p[3]) * (x - p[2]))
  loss <- function(p) sum((y - predict(p))^2)
  upper <- c(100, max(x), log(10))
  lower <- c(baseline + 1e-6, min(x), log(0.1))
  starts <- stats::quantile(x, c(.25, .5, .75), names = FALSE)
  fits <- lapply(starts, function(start) stats::optim(
    c(max(baseline + 1e-6, max(y)), start, 0), loss,
    method = "L-BFGS-B", lower = lower, upper = upper,
    control = list(factr = 1e3, pgtol = 1e-9)))
  fits <- Filter(function(f) f$convergence == 0L && is.finite(f$value), fits)
  if (!length(fits)) return(NULL)
  fit <- fits[[which.min(vapply(fits, function(f) f$value, numeric(1)))]]
  p <- fit$par
  h <- exp(p[3])
  sigmoid <- stats::plogis(h * (x - p[2]))
  rate <- (p[1] - baseline) * sigmoid * (1 - sigmoid)
  jacobian <- cbind(sigmoid, -h * rate, h * (x - p[2]) * rate)
  if (qr(jacobian)$rank != 3L || !is.finite(kappa(jacobian)) ||
      kappa(jacobian) > 1e6 || p[2] - lower[2] < 1e-6 || upper[2] - p[2] < 1e-6 ||
      p[3] - lower[3] < 1e-6 || upper[3] - p[3] < 1e-6) return(NULL)
  bounds <- range(baseline + (p[1] - baseline) *
    stats::plogis(h * (range(x) - p[2])))
  list(bottom = baseline, top = p[1], ec50 = exp(p[2]), hill = h,
    bounds = bounds, rmse = sqrt(fit$value / length(y)))
}

ldj_loewe_continuous <- function(grid) {
  keys <- c("dose_A_M", "dose_B_M")
  stopifnot(all(c(keys, "response") %in% names(grid)),
    all(vapply(grid[c(keys, "response")], is.numeric, logical(1))),
    !anyDuplicated(grid[keys]), all(is.finite(grid$dose_A_M) & grid$dose_A_M >= 0),
    all(is.finite(grid$dose_B_M) & grid$dose_B_M >= 0))
  a <- grid$dose_A_M
  b <- grid$dose_B_M
  baseline <- grid$response[a == 0 & b == 0]
  if (length(baseline) != 1L) baseline <- NA_real_
  fit_a <- ldj_fit_loewe_monotherapy(a[b == 0], grid$response[b == 0], baseline)
  fit_b <- ldj_fit_loewe_monotherapy(b[a == 0], grid$response[a == 0], baseline)
  cells <- grid[a > 0 & b > 0, c(keys, "response"), drop = FALSE]
  cells$expected <- rep(NA_real_, nrow(cells))
  cells$status <- rep("unidentifiable_monotherapy", nrow(cells))
  if (!is.null(fit_a) && !is.null(fit_b)) {
    lower <- max(fit_a$bounds[1L], fit_b$bounds[1L])
    upper <- min(fit_a$bounds[2L], fit_b$bounds[2L])
    inverse <- function(y, fit) {
      fraction <- (y - fit$bottom) / (fit$top - fit$bottom)
      fit$ec50 * exp(stats::qlogis(fraction) / fit$hill)
    }
    for (i in seq_len(nrow(cells))) {
      y <- cells$response[i]
      if (!is.finite(y) || y < 0 || y > 100) {
        cells$status[i] <- "invalid_response"
        next
      }
      cells$status[i] <- "outside_common_observed_domain"
      if (lower >= upper) next
      residual <- function(y) cells$dose_A_M[i] / inverse(y, fit_a) +
        cells$dose_B_M[i] / inverse(y, fit_b) - 1
      if (residual(lower) < 0 || residual(upper) > 0) next
      cells$expected[i] <- stats::uniroot(residual, c(lower, upper), tol = 1e-10)$root
      cells$status[i] <- "candidate"
    }
  }
  cells$excess <- cells$response - cells$expected
  list(cells = cells, fit_A = fit_a, fit_B = fit_b)
}

# Map model results to the same physical cells of each biological unit.
# The mean is per unit and describes its evaluable cells only.
ldj_synergy_model_cells <- function(d, model = "Loewe") {
  stopifnot(model %in% c("Loewe", "ZIP"), nrow(d) > 0L,
    !anyDuplicated(d[c("experiment_id", "biological_replicate_id", "dose_A_M", "dose_B_M")]))
  units <- split(d, list(d$experiment_id, d$biological_replicate_id), drop = TRUE)
  do.call(rbind, lapply(units, function(unit) {
    a <- unit[!duplicated(unit$dose_A_M), ]
    b <- unit[!duplicated(unit$dose_B_M), ]
    grid <- rbind(data.frame(dose_A_M = unit$dose_A_M, dose_B_M = unit$dose_B_M, response = unit$observed),
      data.frame(dose_A_M = a$dose_A_M, dose_B_M = 0, response = a$mono_A),
      data.frame(dose_A_M = 0, dose_B_M = b$dose_B_M, response = b$mono_B),
      data.frame(dose_A_M = 0, dose_B_M = 0, response = unit$control[1L]))
    fit <- if (identical(model, "Loewe")) ldj_loewe_continuous(grid) else ldj_synergy_reference(grid, model)
    cells <- fit$cells[c("dose_A_M", "dose_B_M", "response", "expected", "excess")]
    cells$fitted <- if (is.null(fit$cells$fitted)) rep(NA_real_, nrow(cells)) else fit$cells$fitted
    cells$status <- if (is.null(fit$cells$status)) fit$status else fit$cells$status
    cells$experiment_id <- unit$experiment_id[1L]
    cells$biological_replicate_id <- unit$biological_replicate_id[1L]
    cells$model <- model
    cells$warnings <- paste(c(fit$warnings, fit$detail), collapse = " | ")
    cells$evaluable <- is.finite(cells$excess)
    cells$evaluated_n <- sum(cells$evaluable)
    cells$matrix_n <- nrow(cells)
    cells$coverage <- cells$evaluated_n / cells$matrix_n
    cells$global_pp <- if (any(cells$evaluable)) mean(cells$excess[cells$evaluable]) else NA_real_
    for (agent in c("A", "B")) {
      parameters <- fit[[paste0("fit_", agent)]]
      for (key in c("bottom", "top", "ec50", "hill", "rmse"))
        cells[[paste(agent, key, sep = "_")]] <- if (is.null(parameters[[key]])) NA_real_ else parameters[[key]]
      cells[[paste0(agent, "_fit_status")]] <- if (!identical(model, "Loewe")) "No expuesto por la referencia"
        else if (is.null(parameters)) "No identificable" else "Convergente"
    }
    physical_key <- function(x) paste(signif(x$dose_A_M, 14), signif(x$dose_B_M, 14))
    cell_index <- match(physical_key(cells), physical_key(unit))
    stopifnot(!anyNA(cell_index), !anyDuplicated(cell_index))
    bliss <- unit$Bliss_excess[cell_index]
    hsa <- unit$HSA_excess[cell_index]
    common <- cells$evaluable & is.finite(bliss) & is.finite(hsa)
    cells$common_domain <- common
    cells$Bliss_common_pp <- ifelse(common, bliss, NA_real_)
    cells$HSA_common_pp <- ifelse(common, hsa, NA_real_)
    cells$common_n <- sum(common)
    cells$zip_experimental <- identical(model, "ZIP")
    cells
  }))
}

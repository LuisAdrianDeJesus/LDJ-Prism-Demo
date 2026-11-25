# Optional reference backend. Input is a single complete synthetic or normalized
# inhibition matrix with canonical molar doses and exactly one score per cell.
# SynergyFinder 3.20.0 is an optional external dependency (MPL 2.0).
ldj_synergy_reference <- function(grid, model) {
  stopifnot(length(model) == 1L, model %in% c("Loewe", "ZIP"))
  keys <- c("dose_A_M", "dose_B_M")
  stopifnot(all(c(keys, "response") %in% names(grid)), nrow(grid) > 0L,
    all(vapply(grid[c(keys, "response")], is.numeric, logical(1))),
    !anyDuplicated(grid[keys]),
    all(is.finite(grid$dose_A_M) & grid$dose_A_M >= 0),
    all(is.finite(grid$dose_B_M) & grid$dose_B_M >= 0))
  a <- sort(unique(grid$dose_A_M))
  b <- sort(unique(grid$dose_B_M))
  combos <- grid$dose_A_M > 0 & grid$dose_B_M > 0
  result <- grid[combos, c(keys, "response"), drop = FALSE]
  result$expected <- result$fitted <- result$excess <- rep(NA_real_, nrow(result))
  nd <- function(reason) list(model = model, status = reason, cells = result,
                             global_pp = NA_real_)
  if (length(a) < 6L || length(b) < 6L || a[1L] != 0 || b[1L] != 0 ||
      nrow(grid) != length(a) * length(b)) return(nd("incomplete_grid"))
  if (any(!is.finite(grid$response) | grid$response < 0 | grid$response > 100))
    return(nd("invalid_response"))
  mono_a <- grid$response[grid$dose_B_M == 0 & grid$dose_A_M > 0]
  mono_b <- grid$response[grid$dose_A_M == 0 & grid$dose_B_M > 0]
  if (diff(range(mono_a)) <= 1e-8 || diff(range(mono_b)) <= 1e-8)
    return(nd("flat_monotherapy"))
  if (!requireNamespace("synergyfinder", quietly = TRUE) ||
      as.character(utils::packageVersion("synergyfinder")) != "3.20.0")
    return(nd("reference_unavailable"))

  # Fit in µM to avoid poorly scaled numerical optimization in molar units.
  # Match the application's 14-significant-digit canonical dose resolution.
  # Only the concentration representation changes; responses remain untouched.
  backend_dose <- function(molar) signif(molar * 1e6, 14)
  input <- data.frame(conc1 = backend_dose(grid$dose_A_M),
                      conc2 = backend_dose(grid$dose_B_M), response = grid$response)
  if (anyDuplicated(input[c("conc1", "conc2")])) return(nd("dose_key_collision"))
  input <- input[order(input$conc2, input$conc1), , drop = FALSE]
  rownames(input) <- NULL
  warnings <- character()
  fit <- tryCatch(withCallingHandlers(switch(model,
    Loewe = synergyfinder::Loewe(input, quiet = FALSE),
    ZIP = synergyfinder::ZIP(input, quiet = FALSE)), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
    }), error = identity)
  if (inherits(fit, "error")) {
    output <- nd("fit_failed")
    output$detail <- conditionMessage(fit)
    output$backend_input <- input
    output$warnings <- warnings
    return(output)
  }
  dose_key <- function(a, b) paste(signif(a, 14), signif(b, 14))
  reference_keys <- dose_key(fit$conc1, fit$conc2)
  result$backend_A_uM <- backend_dose(result$dose_A_M)
  result$backend_B_uM <- backend_dose(result$dose_B_M)
  matched <- match(dose_key(result$backend_A_uM, result$backend_B_uM), reference_keys)
  if (anyDuplicated(reference_keys) || anyNA(matched)) return(nd("unmatched_reference_grid"))
  result$expected <- fit[[paste0(model, "_ref")]][matched]
  result$fitted <- if (model == "ZIP") fit$ZIP_fit[matched] else result$response
  result$excess <- fit[[paste0(model, "_synergy")]][matched]
  if (any(!is.finite(result$expected) | !is.finite(result$fitted) |
          !is.finite(result$excess))) return(nd("non_finite_fit"))
  list(model = model, status = "reference_candidate", cells = result,
       global_pp = mean(result$excess), backend_input = input, warnings = warnings)
}

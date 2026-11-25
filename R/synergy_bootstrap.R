# Equal weight per independent experiment, on a prespecified common dose grid.
# cells comes from ldj_synergy_excess(), after within-unit dose matching.
ldj_synergy_bootstrap <- function(cells, metric = "Bliss_excess", independent = FALSE,
                                 iterations = 1000L, seed = 1L) {
  stopifnot(isTRUE(independent), metric %in% c("Bliss_excess", "HSA_excess"),
            length(iterations) == 1L, iterations %in% c(1000L, 5000L, 10000L),
            length(seed) == 1L, is.finite(seed), seed == as.integer(seed))
  keys <- c("experiment_id", "biological_replicate_id", "dose_A_M", "dose_B_M")
  stopifnot(all(c(keys, metric) %in% names(cells)), nrow(cells) > 0L,
            !anyNA(cells[keys]), !anyDuplicated(cells[keys]))
  grid_size <- nrow(unique(cells[c("dose_A_M", "dose_B_M")]))
  experiments <- split(cells, cells$experiment_id, drop = TRUE)
  summarize <- function(experiment) {
    matrices <- split(experiment, experiment$biological_replicate_id, drop = TRUE)
    scores <- vapply(matrices, function(matrix) {
      values <- matrix[[metric]]
      if (length(values) != grid_size || any(!is.finite(values))) return(NA_real_)
      mean(values)
    }, numeric(1))
    data.frame(n_units = length(scores), complete = all(is.finite(scores)),
               estimate = if (all(is.finite(scores))) mean(scores) else NA_real_)
  }
  scores <- do.call(rbind, lapply(experiments, summarize))
  scores$experiment_id <- rownames(scores)
  rownames(scores) <- NULL
  available <- scores$estimate[scores$complete]
  n <- length(available)
  result <- list(metric = metric, scores = scores, n_experiments = n,
    n_excluded = sum(!scores$complete), n_cells = grid_size,
    estimate = if (n) mean(available) else NA_real_, interval = c(NA_real_, NA_real_),
    iterations = iterations, seed = seed, failed = NA_integer_,
    leave_one_out = data.frame(experiment_id = scores$experiment_id[scores$complete],
      estimate = if (n > 1L) (sum(available) - available) / (n - 1L) else rep(NA_real_, n)))
  if (n < 3L) {
    result$status <- "insufficient_complete_experiments"
    return(result)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", previous, envir = .GlobalEnv)
    else rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  draws <- replicate(iterations, mean(available[sample.int(n, n, replace = TRUE)]))
  valid <- is.finite(draws)
  result$failed <- sum(!valid)
  result$status <- if (all(valid)) "ok" else "failed_resamples"
  if (all(valid)) result$interval <- unname(stats::quantile(draws, c(.025, .975)))
  result
}

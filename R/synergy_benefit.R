# Inputs are one summarized percentage per experimental context and dose pair.
# dose_A/unit_A and dose_B/unit_B describe the same ordered compounds in both assays.
ldj_synergy_benefit <- function(activity, viability = NULL,
                                activity_min = 70, viability_min = 80) {
  context <- c("compound_A", "compound_B", "cell_line", "exposure_hours", "experiment_id")
  keys <- c(context, "dose_A_M", "dose_B_M")
  normalize <- function(data, measure) {
    stopifnot(is.data.frame(data), all(c(context, "dose_A", "dose_B",
              "unit_A", "unit_B", measure) %in% names(data)),
              !anyNA(data[context]), is.numeric(data$exposure_hours),
              all(is.finite(data$exposure_hours) & data$exposure_hours > 0),
              all(vapply(data[setdiff(context, "exposure_hours")],
                function(x) all(nzchar(trimws(as.character(x)))), logical(1))),
              is.numeric(data[[measure]]))
    for (axis in c("A", "B")) {
      dose <- data[[paste0("dose_", axis)]]
      stopifnot(is.numeric(dose), all(is.finite(dose) & dose >= 0))
      converted <- ldj_convert_concentration(dose, data[[paste0("unit_", axis)]], "M")
      stopifnot(all(is.finite(converted) & converted >= 0))
      data[[paste0("dose_", axis, "_M")]] <- signif(converted, 14)
    }
    stopifnot(!anyDuplicated(data[keys]))
    data[c(keys, measure)]
  }
  thresholds <- c(activity_min, viability_min)
  stopifnot(length(activity_min) == 1L, length(viability_min) == 1L,
            is.numeric(thresholds), all(is.finite(thresholds) & thresholds >= 0 & thresholds <= 100))
  result <- normalize(activity, "activity_pct")
  if (is.null(viability)) {
    result$viability_pct <- rep(NA_real_, nrow(result))
  } else {
    viable <- normalize(viability, "viability_pct")
    result <- merge(result, viable, by = keys, all.x = TRUE, sort = FALSE)
  }
  valid_activity <- is.finite(result$activity_pct) & result$activity_pct >= 0 & result$activity_pct <= 100
  valid_viability <- is.finite(result$viability_pct) & result$viability_pct >= 0 & result$viability_pct <= 100
  result$cytotoxicity_pct <- ifelse(valid_viability, 100 - result$viability_pct, NA_real_)
  result$benefit <- ifelse(valid_activity & valid_viability,
    result$activity_pct >= activity_min & result$viability_pct >= viability_min, NA)
  result$status <- ifelse(!valid_activity, "activity_ND",
    ifelse(!valid_viability, "viability_ND", ifelse(result$benefit, "meets_thresholds", "below_thresholds")))
  result
}

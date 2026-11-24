app <- new.env()
sys.source("app.R",app)
file <- tempfile(fileext=".R")
stopifnot(system2("git",c("show","046ed98:R/analysis.R"),stdout=file)==0)
baseline <- new.env()
sys.source(file,baseline)
dir.create("test-artifacts",showWarnings=FALSE)
results <- list()
for(module in c("mtt","activity")) {
  d <- app$editorial_example(module)
  stopifnot(!anyDuplicated(d$observation_id),!anyDuplicated(d$independent_unit_id))
  for(key in unique(d$Compuesto)) {
    z <- d[d$Compuesto==key & d$control_type=="treatment",]
    stopifnot(all(table(z$Concentracion_uM)==4),length(unique(z$Concentracion_uM))==7)
    mapped <- data.frame(dose=z$Concentracion_uM,response=z$Viabilidad_pct)
    for(model in c("3PL","4PL")) {
      before <- baseline$fit_response(mapped,model)
      after <- app$fit_response(mapped,model)
      stopifnot(identical(before$table,after$table),identical(before$curve,after$curve))
      results[[length(results)+1L]] <- data.frame(module=module,series=key,model=model,
        estimate_base=before$table$estimate,estimate_current=after$table$estimate,
        lower_base=before$table$lower,lower_current=after$table$lower,
        upper_base=before$table$upper,upper_current=after$table$upper)
    }
  }
}
mtt <- app$editorial_example("mtt")
result <- app$ldj_mtt_analyze_table(mtt,"Concentracion_uM","Viabilidad_pct","Compuesto",
  unique(mtt$Compuesto),"uM",inference=TRUE,independent=TRUE)
p <- unlist(lapply(result$rows,function(row) row$inference$dunnett$p_adjusted))
stopifnot(length(p)==28,any(p>=.05),any(p<.05))
activity <- app$editorial_example("activity")
physical <- app$prepare_controls(activity,"Concentracion_uM","Viabilidad_pct",series_column="Compuesto")
visual <- app$expand_control_contexts(physical,"Compuesto")
activity_stats <- lapply(split(visual,visual$Compuesto),function(d)
  app$ldj_activity_inference(d,"Concentracion_uM","Viabilidad_pct","infected_untreated",
    "SYNTHETIC-ACTIVITY",TRUE))
stopifnot(all(vapply(activity_stats,function(row) length(row$audit$control_ids)==4L &&
  length(row$audit$independent_units)==32L && nrow(row$dunnett)==7L,logical(1))))
safe_stats <- function(stats) lapply(stats,function(z) {z$anova <- as.data.frame(z$anova); z})
jsonlite::write_json(list(mtt=safe_stats(app$ldj_mtt_inference_report(result)),
  activity=safe_stats(activity_stats)),"test-artifacts/demo-anova-dunnett.json",
  pretty=TRUE,auto_unbox=TRUE,digits=NA)
safety <- app$editorial_example("safety")
parameters <- safety[!is.na(safety$parameter),]
stopifnot(nrow(parameters)==12,all(is.finite(parameters$lower)),all(is.finite(parameters$upper)))
normalized <- app$ldj_safety_results(parameters,"Compuesto")
p <- reshape(normalized[c("series","parameter","estimate")],timevar="parameter",idvar="series",direction="wide")
stopifnot(isTRUE(all.equal(p$estimate.SI,p$estimate.CC50/p$estimate.IC50)),
  any(p$estimate.CC20<p$estimate.IC50),any(p$estimate.CC20/p$estimate.IC50>5))
write.csv(do.call(rbind,results),"test-artifacts/demo-ic50-cc50-vs-046ed98.csv",row.names=FALSE)
cat("Synthetic demos and exact IC50/CC50 estimates and CI regression against 046ed98: PASS\n")

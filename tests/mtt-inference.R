for (file in c("analysis", "mtt_data_entry", "mtt_visualization_v4_LDJ_Prism", "mtt_shiny", "pharmacology"))
  source(file.path("R",paste0(file,".R")))
series <- LETTERS[1:4]
d <- expand.grid(dose=c(0,.25,.5,1,2,4,8),replicate=1:3,series=series,stringsAsFactors=FALSE)
d$response <- with(d,5+95/(1+(dose/2)^1.4)+replicate-2)
d$control_type <- ifelse(d$dose==0,"untreated","treatment")
d$experiment_id <- "synthetic"
d$observation_id <- paste0("obs-",seq_len(nrow(d)))
d$independent_unit_id <- paste0("unit-",seq_len(nrow(d)))
d$control_source <- "Synthetic fixture"
d$inference_eligible <- d$dose==0
fit <- function(d, enabled, model="4PL",unit="uM") ldj_mtt_analyze_table(d,"dose","response","series",
  series,unit,model,inference=enabled,independent=TRUE)
for(model in c("3PL","4PL")) {
  old <- fit(d,FALSE,model)
  new <- fit(d,TRUE,model)
  for(i in seq_along(series)) stopifnot(identical(old$rows[[i]]$fit$table,new$rows[[i]]$fit$table),
    identical(old$rows[[i]]$fit$curve,new$rows[[i]]$fit$curve))
}
new <- fit(d,TRUE)
for(i in seq_along(series)) {
  row <- new$rows[[i]]
  reference <- subset(d,series==row$series)
  reference$level <- factor(reference$dose)
  af <- aov(response~level,reference)
  set.seed(20261116)
  p <- summary(multcomp::glht(af,linfct=multcomp::mcp(level="Dunnett")))$test
  z <- row$inference
  stopifnot(isTRUE(all.equal(z$anova,as.data.frame(summary(af)[[1]]))),
    isTRUE(all.equal(z$dunnett$difference,unname(p$coefficients))),
    isTRUE(all.equal(z$dunnett$p_adjusted,unname(p$pvalues),check.attributes=FALSE)),
    all(z$audit$n==3))
}
for(unit in c("nM","mM","M")) {
  d2 <- d
  d2$dose <- ldj_convert_concentration(d$dose,"uM",unit)
  z <- fit(d2,TRUE,unit=unit)
  stopifnot(isTRUE(all.equal(new$rows[[1]]$fit$table,z$rows[[1]]$fit$table,tolerance=1e-7)))
}
for(d2 in list(subset(d,dose>0),within(d,{control_source <- ""}),
  within(d,{independent_unit_id <- "technical-repeat"}),
  within(d,{experiment_id[dose==0] <- "other"}))) {
  z <- fit(d2,TRUE)
  stopifnot(all(vapply(z$rows,function(row) is.null(row$inference$dunnett) &&
    startsWith(row$inference$status,"Inferencia no disponible:"),logical(1))))
}
dir.create("test-artifacts",showWarnings=FALSE)
jsonlite::write_json(ldj_mtt_inference_report(new),"test-artifacts/mtt-anova-reference.json",pretty=TRUE,auto_unbox=TRUE)
cat("MTT ANOVA and Dunnett reference, physical units, blocked ineligible designs, and identical CC50/CI: PASS\n")

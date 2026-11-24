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
comparison <- list()
for(model in c("3PL","4PL")) {
  old <- fit(d,FALSE,model)
  new <- fit(d,TRUE,model)
  comparison[[model]] <- do.call(rbind,lapply(seq_along(series),function(i) {
    a <- old$rows[[i]]$fit$table; b <- new$rows[[i]]$fit$table
    data.frame(model=model,series=series[i],before=a$estimate,after=b$estimate,
      lower_before=a$lower,lower_after=b$lower,upper_before=a$upper,upper_after=b$upper)
  }))
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
    all(z$audit$n$n==3))
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
shared <- d
for(s in series) {
  ix <- shared$series==s & shared$dose==0
  shared$observation_id[ix] <- paste0("shared-",1:3)
  shared$independent_unit_id[ix] <- paste0("shared-unit-",1:3)
}
z <- fit(shared,TRUE)
stopifnot(z$audit$shared_physical_n==3,z$audit$shared_visual_n==12,
  all(vapply(z$rows,function(row) length(row$inference$audit$control_ids)==3L,logical(1))))
conflict <- shared
conflict$independent_unit_id[which(conflict$series=="D" & conflict$dose==0)[1]] <- "contradictory-unit"
z <- fit(conflict,TRUE)
stopifnot(all(vapply(z$rows,function(row) is.null(row$inference$dunnett) &&
  grepl("contradictorias",row$inference$status,fixed=TRUE),logical(1))))
for(d2 in list(d[!(d$dose==0 & d$replicate!=1),],within(d,{response[which(dose>0)[1]] <- NA_real_}))) {
  dd <- prepare_controls(d2,"dose","response",series_column="series")
  stopifnot(is.null(ldj_mtt_inference(dd[dd$series=="A",],"dose","response",confirmed=TRUE)$dunnett))
}
dir.create("test-artifacts",showWarnings=FALSE)
write.csv(do.call(rbind,comparison),"test-artifacts/mtt-cc50-invariance.csv",row.names=FALSE)
jsonlite::write_json(ldj_mtt_inference_report(new),"test-artifacts/mtt-anova-reference.json",pretty=TRUE,auto_unbox=TRUE,digits=NA)
cat("MTT ANOVA and Dunnett reference, physical units, blocked ineligible designs, and identical CC50/CI: PASS\n")


# Compact layouts keep one pair per selected compound.
viability <- data.frame(group=d$series, concentration=d$dose, response=d$response)
for (n in 1:4) {
  parameters <- expand.grid(group=series[seq_len(n)],parameter=c("IC50","CC20","CC50"),
    stringsAsFactors=FALSE)
  parameters$estimate <- c(IC50=.2,CC20=1,CC50=2)[parameters$parameter]
  parameters$unit <- "uM"
  estimates <- ldj_safety_results(parameters,"group")
  fits <- ldj_safety_editorial(estimates,viability,"concentration","response","group","uM","M","4PL",NULL)
  stopifnot(length(fits$rows)==n,isTRUE(all.equal(estimates$estimate[estimates$parameter=="SI"],rep(10,n))),
    inherits(ldj_safety_editorial_plot(fits,ldj_appearance(),"uM"),"patchwork"))
}
stopifnot(isTRUE(all.equal(unname(ldj_editorial_dimensions("mtt")*600),c(5130,7260))),
  isTRUE(all.equal(unname(ldj_editorial_dimensions("activity")*600),c(5070,7200))),
  isTRUE(all.equal(unname(ldj_editorial_dimensions("safety")*600),c(5010,6270))))
cat("Editorial reference dimensions and Safety layouts for one to four compounds: PASS\n")

root <- "apps/ldj-prism"
for(f in c("analysis","mtt_visualization_v4_LDJ_Prism","mtt_data_entry","mtt_shiny","pharmacology"))
  source(file.path(root,"R",paste0(f,".R")))
d <- data.frame(group=c("S1","S1","S2","S2"),parameter=c("IC50","CC50","IC50","CC50"),
 estimate=c(200,2,NA,0.003),unit=c("nM","µM","M","mM"),
 lower=c(150,1,NA,NA),upper=c(300,3,NA,NA))
z <- ldj_safety_results(d,"group")
stopifnot(isTRUE(all.equal(z$estimate[z$parameter=="SI"],c(10,NA_real_))),
 sum(is.finite(z$lower))==2,any(grepl("no proporcionado",z$status)))
b <- ldj_mtt_synthetic_bundle()
x <- b$orig;x$Replica <- x$replicate
v <- ldj_mtt_analyze_table(x,"conc_uM","viability","drug",ldj_mtt_defaults()$order,"uM")
v$metric <- "IC50"
state <- ldj_ensure_series(ldj_appearance(),c("S1","S2",ldj_mtt_defaults()$order))
before <- serialize(list(v,z),NULL)
figs <- list(activity=ldj_mtt_table_figure(v,list(appearance=state))$figure,
 safety=ldj_safety_plot(z,state,"uM"),mtt=ldj_mtt_editor_build(b,list(appearance=state))$figure)
dir.create("test-artifacts",showWarnings=FALSE)
for(n in names(figs)) for(ext in c("svg","pdf","png")) {
  file <- file.path("test-artifacts",paste0("modules-",n,".",ext))
  ldj_mtt_editor_export(list(figure=figs[[n]],config=list(appearance=state)),file,12,if(n=="safety") 9 else 18,600)
  stopifnot(file.info(file)$size>1000)
}
stopifnot(identical(before,serialize(list(v,z),NULL)))
cat("Module exports, mixed units, SI, ND and immutable results: PASS\n")

keys <- ldj_mtt_defaults()$order
raw <- data.frame(group=rep(keys,each=3),parameter=rep(c("IC50","CC20","CC50"),4),
 estimate=c(.05,.1,2,.2,.1,.4,1,2,6,2,4,12),unit="uM",
 lower=c(.04,rep(NA_real_,11)),upper=c(.06,rep(NA_real_,11)))
sf <- ldj_safety_editorial(ldj_safety_results(raw,"group"),x,"conc_uM","viability","drug","uM","uM","4PL",NULL)
for(i in seq_along(v$rows)) {
  q <- v$rows[[i]]$data
  q <- q[!q$.is_control,,drop=FALSE]
  q$.category <- as.character(q$.category)
  q$experiment_id <- "synthetic"
  q$observation_id <- q$independent_unit_id <- paste0("t",i,"-",seq_len(nrow(q)))
  control <- q[1:6,,drop=FALSE]
  control$.category <- "Infectado sin tratamiento"
  control$.is_control <- control$.eligible <- TRUE
  control$.control_type <- "infected_untreated"
  control$conc_uM <- 0
  control$viability <- c(97,101,98,102,100,99)
  control$observation_id <- control$independent_unit_id <- paste0("shared-",1:6)
  q <- rbind(q,control)
  inf <- ldj_activity_inference(q,"conc_uM","viability","infected_untreated","synthetic",TRUE)
  stopifnot(length(inf$audit$control_ids)==6,nrow(inf$labels)==7,
    is.null(ldj_activity_inference(rbind(q,q[1,]),"conc_uM","viability","infected_untreated","synthetic",TRUE)$labels),
    is.null(ldj_activity_inference(q,"conc_uM","viability","vehicle","synthetic",TRUE)$labels))
  v$rows[[i]]$data <- q
  v$rows[[i]]$inference <- inf
}
snapshot <- serialize(list(v,sf),NULL)
plots <- list(activity=ldj_mtt_table_figure(v,list(appearance=state))$figure,
 safety=ldj_safety_editorial_plot(sf,state,"uM"))
for(format in c("svg","pdf","png")) for(n in names(plots))
 ldj_mtt_editor_export(list(figure=plots[[n]],config=list(appearance=state)),
  file.path("test-artifacts",paste0("editorial-",n,".",format)),12,18,600)
stopifnot(identical(snapshot,serialize(list(v,sf),NULL)))
svg <- readLines("test-artifacts/editorial-activity.svg",warn=FALSE)
stopifnot(sum(grepl("ANOVA F",svg,fixed=TRUE))==4,any(grepl("fill-opacity",svg,fixed=TRUE)))
cat("Activity ANOVA/Dunnett, physical IDs and safety editorial exports: PASS\n")

dose <- c(1,1,1,2,2,2,20)
response <- c(0,2,4,1,6,11,9)
for (measure in c("SD","SEM","IC95")) {
  summary <- ldj_dose_dispersion(dose,response,measure)
  bands <- ldj_estimable_dispersion(summary)
  stopifnot(nrow(bands)==2,all(bands$xmin>0),bands$xmax[1]<bands$xmin[2],
    is.na(summary$ymin[3]),is.na(summary$ymax[3]))
}
bands <- ldj_estimable_dispersion(ldj_dose_dispersion(dose,response,"SD"))
stopifnot(identical(bands$ymin,c(0,1)),identical(bands$ymax,c(4,11)))
safety_svg <- readLines("test-artifacts/editorial-safety.svg",warn=FALSE)
# Four viability bands must render as translucent rectangles in the default palette.
viability_bands <- grepl("<rect", safety_svg, fixed = TRUE) &
  grepl("fill: #B9DCCB", safety_svg, fixed = TRUE) &
  grepl("fill-opacity: 0.25", safety_svg, fixed = TRUE)
stopifnot(sum(viability_bands) == 4L)
cat("Known observed dispersion and rendered safety reference bands: PASS\n")

audit_style <- ldj_color_edit(state,"global",element="band",hex="#A13C9B",alpha=.38,linewidth=.8)
audit_figures <- list(activity=ldj_mtt_table_figure(v,list(appearance=audit_style))$figure,
  mtt=ldj_mtt_editor_build(b,list(appearance=audit_style))$figure)
for (name in names(audit_figures)) {
  path <- file.path("test-artifacts",paste0("SD-audit-",name,".svg"))
  ldj_mtt_editor_export(list(figure=audit_figures[[name]],config=list(appearance=audit_style)),path,12,18,600)
  xml <- readLines(path,warn=FALSE)
  stopifnot(sum(grepl("<rect",xml,fixed=TRUE) & grepl("#A13C9B",xml,fixed=TRUE) &
    grepl("fill-opacity",xml,fixed=TRUE))>=4)
}
stopifnot(identical(snapshot,serialize(list(v,sf),NULL)))
cat("Rendered MTT and activity dispersion and style-only invariance: PASS\n")

cc20_snapshot <- serialize(sf,NULL)
key <- sf$rows[[1]]$series
fit <- sf$rows[[1]]$fit
table <- sf$table[sf$table$series==key & sf$table$parameter!="SI",]
different <- ldj_cc20_sources(table,fit,10)
stopifnot(different$exceeds, isTRUE(all.equal(different$gap,
  abs(table$estimate[table$parameter=="CC20"]-fit$table$estimate))))
same <- sf
is_cc20 <- same$table$parameter=="CC20"
same$table$estimate[is_cc20] <- vapply(same$table$series[is_cc20],function(key)
  same$rows[[match(key,vapply(same$rows,function(row) row$series,""))]]$fit$table$estimate,numeric(1))
stopifnot(!ldj_cc20_sources(same$table[same$table$series==key,],fit)$exceeds,
  is.na(ldj_cc20_sources(table,list(error="ND"))$gap),
  abs(ldj_convert_concentration(different$gap,"M","nM")-
    1000*ldj_convert_concentration(different$gap,"M","uM"))<1e-8)
for (mode in c("table","regression","both")) {
  f <- ldj_safety_editorial_plot(sf,state,"uM",cc20_source=mode)
  file <- file.path("test-artifacts",paste0("cc20-",mode,".svg"))
  ldj_mtt_editor_export(list(figure=f,config=list(appearance=state)),file,12,18,600)
  svg <- paste(readLines(file,warn=FALSE),collapse=" ")
  stopifnot(grepl("CC20 · Ajuste",svg,fixed=TRUE)==(mode!="table"),
    grepl("CC20 · Tabla",svg,fixed=TRUE)==(mode!="regression"),
    grepl("Discrepancia:",svg,fixed=TRUE),grepl("µM",svg,fixed=TRUE))
}
for (variant in c("concordant","discrepant")) for (ext in c("svg","pdf","png")) {
  f <- ldj_safety_editorial_plot(if(variant=="concordant") same else sf,state,"uM",cc20_source="both")
  ldj_mtt_editor_export(list(figure=f,config=list(appearance=state)),
    file.path("test-artifacts",paste0("cc20-",variant,".",ext)),12,18,600)
}
stopifnot(identical(cc20_snapshot,serialize(sf,NULL)))
cat("CC20 source modes, discrepancy, concordance, units and exports: PASS\n")

stopifnot(as.character(getRversion())=="4.6.0")
root <- if(file.exists("R/mtt_shiny.R")) "." else "apps/ldj-prism"
before <- list.files(tempdir(),recursive=TRUE)
e <- new.env(parent=globalenv())
sys.source(file.path(root,"R/mtt_visualization_v4_LDJ_Prism.R"),envir=e)
stopifnot(identical(before,list.files(tempdir(),recursive=TRUE)))
source(file.path(root,"R/mtt_visualization_v4_LDJ_Prism.R"))
source(file.path(root,"R/mtt_data_entry.R"))
source(file.path(root,"R/mtt_shiny.R"))
b <- ldj_mtt_synthetic_bundle(); snapshot <- serialize(b,NULL)
out <- ldj_mtt_editor_build(b,list(label_significance=FALSE,
  plot_caption="DEMO · synthetic data",short=setNames(paste("Example",1:4),ldj_mtt_defaults()$order)))
stopifnot(identical(names(out$panels),LETTERS[1:8]),length(out$panels)==8,
  all(vapply(out$panels,inherits,logical(1),what="ggplot")),
  out$audit$shared_physical_n==6,out$audit$shared_visual_n==12,
  identical(snapshot,serialize(b,NULL)))
for(drug in c("Letermovir","Lopinavir")) {
  d <- ldj_mtt_bar_data(b,drug)
  stopifnot(sum(d$is_assigned_control)==6,
    setequal(d$record_id[d$is_assigned_control],b$controls$record_id))
}
for(tag in c("A","C","E","G")) {
  p <- out$panels[[tag]]
  stopifnot(levels(p$data$dose)[1]=="0",p$data$n[which(p$data$conc_uM==0)]==6)
}
styled <- ldj_mtt_editor_build(b,list(label_significance=FALSE,
  panels=list(A=list(title="A. Edited title",color="#123456",y_label="Edited response",annotation="Editable note"))))
stopifnot(styled$panels$A$labels$title=="A. Edited title",
  styled$panels$A$labels$y=="Edited response",identical(snapshot,serialize(b,NULL)))
invalid <- try(ldj_mtt_editor_build(b,list(concentration_unit="nM")),silent=TRUE)
stopifnot(inherits(invalid,"try-error"))
missing_ci <- b; missing_ci$cc50$ci95_low_uM[1] <- NA_real_
nd <- suppressWarnings(ldj_mtt_editor_build(missing_ci,list(label_significance=FALSE)))
stopifnot(!any(vapply(nd$panels$B$layers,function(layer) inherits(layer$geom,"GeomLabelRepel"),logical(1))))
dir.create("test-artifacts",showWarnings=FALSE)
for(ext in c("svg","pdf","png")) {
  path <- file.path("test-artifacts",paste0("MTT-4x2.",ext))
  ldj_mtt_editor_export(out,path,width=12,height=18,dpi=600)
  stopifnot(file.info(path)$size>10000)
}
svg <- paste(readLines("test-artifacts/MTT-4x2.svg",warn=FALSE),collapse="\n")
stopifnot(all(vapply(LETTERS[1:8],function(tag) grepl(paste0(tag,". "),svg,fixed=TRUE),logical(1))))
# PNG IHDR stores width/height big endian. 12x18 inches at 600 dpi.
con <- file("test-artifacts/MTT-4x2.png","rb"); seek(con,16)
dim <- readBin(con,integer(),n=2,size=4,endian="big"); close(con)
stopifnot(identical(dim,c(7200L,10800L)))
cat("MTT v4: source without side effects; 8 panels; 6 unique shared controls; immutable input; panel styles; SVG/PDF/PNG 600dpi: PASS\n")


# Session-table integration uses different series names and measured units.
source(file.path(root,"R/analysis.R"))
d <- b$orig
d$Series <- paste0("New-",match(d$drug,ldj_mtt_defaults()$order))
d$Replica <- d$replicate
d$control_type <- ifelse(d$conc_uM==0,"untreated","treatment")
d$observation_id <- d$record_id
d$experiment_id <- "SYN-TABLE"
d$control_source <- "Synthetic fixture"
d$inference_eligible <- TRUE
z <- ldj_mtt_analyze_table(d,"conc_uM","viability","Series",paste0("New-",1:4),"uM")
snapshot2 <- serialize(z,NULL)
g <- ldj_mtt_table_figure(z,list(panels=list(A=list(title="Edited session title"))))
stopifnot(length(g$panels)==8,identical(names(g$panels),LETTERS[1:8]),
  identical(snapshot2,serialize(z,NULL)),g$panels$A$labels$title=="Edited session title")
cat("Selected table, arbitrary series names, eight panels and immutable analysis: PASS\\n")

for(selection in list(c("New-1","New-1","New-3","New-4"),c("absent","New-2","New-3","New-4"),c("","New-2","New-3","New-4"))) {
  stopifnot(inherits(try(ldj_mtt_analyze_table(d,"conc_uM","viability","Series",selection,"uM"),silent=TRUE),"try-error"))
}
small <- d[d$conc_uM<.1,]
stopifnot(inherits(try(ldj_mtt_analyze_table(small,"conc_uM","viability","Series",paste0("New-",1:4),"uM"),silent=TRUE),"try-error"))
for(unit in c("nM","uM","mM","M")) {
  converted <- ldj_mtt_display_unit(z,unit)
  original <- z$rows[[1]]$fit$table$estimate
  restored <- ldj_convert_concentration(converted$rows[[1]]$fit$table$estimate,unit,z$unit)
  stopifnot(isTRUE(all.equal(original,restored)),identical(snapshot2,serialize(z,NULL)))
}
cat("Negative series validation and physical concentration invariance: PASS\\n")

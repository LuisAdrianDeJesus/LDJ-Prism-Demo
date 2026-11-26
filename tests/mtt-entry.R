# Synthetic-only integration: table ingestion, physical units and control mappings.
source("apps/ldj-prism/R/analysis.R")
source("apps/ldj-prism/R/mtt_visualization_v4_LDJ_Prism.R")
source("apps/ldj-prism/R/mtt_data_entry.R")
source("apps/ldj-prism/R/mtt_shiny.R")

tsv <- paste(c("dose\tresponse\tseries\tReplica", "0,1\t80,5\tA\t1",
               "0,3\t60,2\tB\t2"),collapse="\n")
a <- ldj_mtt_paste_table(tsv, header=TRUE, decimal=",")
stopifnot(identical(names(a),c("dose","response","series","Replica")),
          is.numeric(a$dose), abs(a$dose[1]-.1)<1e-12,
          abs(a$response[2]-60.2)<1e-12)
plain <- ldj_mtt_paste_table("1\t2\n3\t4",header=FALSE)
stopifnot(ncol(plain)==2, nrow(plain)==2, is.numeric(plain[[1]]))
stopifnot(inherits(try(ldj_mtt_paste_table(""),silent=TRUE),"try-error"))

file <- tempfile(fileext=".csv")
writeLines(c("dose;response;series","0,1;80,5;A","0,3;60,2;B"),file)
csv <- ldj_mtt_import_table(file,"example.csv",separator=";",decimal=",")
stopifnot(abs(csv$dose[1]-.1)<1e-12,abs(csv$response[2]-60.2)<1e-12)
writeLines(c("0.1,80.5,A","0.3,60.2,B"),file)
csv <- ldj_mtt_import_table(file,"example.csv",header=FALSE,decimal=".")
stopifnot(nrow(csv)==2,ncol(csv)==3,is.numeric(csv[[1]]))
unlink(file)

xlsx <- tempfile(fileext=".xlsx")
writexl::write_xlsx(list(First=data.frame(dose=c(.1,.3),response=c(80,60)),
  Second=data.frame(dose=c(1,3),response=c(50,20))),xlsx)
first <- ldj_mtt_import_table(xlsx,"test.xlsx",sheet="First")
second <- ldj_mtt_import_table(xlsx,"test.xlsx",sheet="Second")
stopifnot(isTRUE(all.equal(first$dose,c(.1,.3))),
          isTRUE(all.equal(second$dose,c(1,3))))
stopifnot(inherits(try(ldj_mtt_import_table(xlsx,"test.xlsx",sheet="missing"),silent=TRUE),"try-error"))
unlink(xlsx)
xls <- readxl::readxl_example("datasets.xls")
xls_sheets <- readxl::excel_sheets(xls)
stopifnot(length(xls_sheets) >= 1,
          nrow(ldj_mtt_import_table(xls,"datasets.xls",sheet=xls_sheets[[1]])) > 10)
cat("CSV/TSV paste, XLSX mult-sheet, XLS, header/decimal variants: PASS\n")

b <- ldj_mtt_synthetic_bundle()
d <- b$orig
d$Series <- paste0("S",match(d$drug,ldj_mtt_defaults()$order))
d$Replica <- d$replicate
d$control_type <- ifelse(d$conc_uM==0,"untreated","treatment")
d$observation_id <- d$record_id
d$experiment_id <- "E1"; d$control_source <- "synthetic"
d$inference_eligible <- TRUE
selection <- paste0("S",1:4)
map <- ldj_mtt_default_control_map(d,"conc_uM","viability","Series")
stopifnot(nrow(map)==12)
base <- ldj_mtt_analyze_table(d,"conc_uM","viability","Series",selection,"uM",control_assignments=map)
fit_ref <- vapply(base$rows,function(r) r$fit$table$estimate,numeric(1))
for (u in c("nM","uM","mM","M")) {
  tmp <- d; tmp$conc_uM <- ldj_convert_concentration(d$conc_uM,"uM",u)
  fit <- ldj_mtt_analyze_table(tmp,"conc_uM","viability","Series",selection,u)
  vals <- vapply(fit$rows,function(r) r$fit$table$estimate,numeric(1))
  stopifnot(isTRUE(all.equal(vals,fit_ref,tolerance=1e-7)))
  shown <- ldj_mtt_display_unit(base,u)
  stopifnot(isTRUE(all.equal(ldj_convert_concentration(
    shown$rows[[1]]$fit$table$estimate,u,"uM"),fit_ref[1],tolerance=1e-10)),
    identical(base$unit,"uM"))
}
mixed <- d
mixed$concentration_unit <- rep(c("nM","uM","mM","M"),length.out=nrow(mixed))
mixed$conc_uM <- mapply(function(value, unit) {
  ldj_convert_concentration(value, "uM", unit)
}, d$conc_uM, mixed$concentration_unit, USE.NAMES=FALSE)
fit_mixed <- ldj_mtt_analyze_table(mixed,"conc_uM","viability","Series",selection,"uM")
stopifnot(isTRUE(all.equal(fit_ref,vapply(fit_mixed$rows,
  function(r) r$fit$table$estimate,numeric(1)),tolerance=1e-7)))
cat("Canonical uM fits invariant for nM/uM/mM/M and mixed-unit rows: PASS\n")

phys <- prepare_controls(d,"conc_uM","viability",series_column="Series")
extra <- rbind(map,transform(map[1,,drop=FALSE],series="S2"))
assigned <- ldj_mtt_apply_control_map(phys,"Series",selection,extra)
expanded <- expand_control_contexts(assigned,"Series")
stopifnot(sum(expanded$.is_control)==sum(phys$.is_control)+1,
  sum(assigned$.is_control)==sum(phys$.is_control))
stopifnot(inherits(try(ldj_mtt_apply_control_map(phys,"Series",selection,
  rbind(extra,extra[1,,drop=FALSE])),silent=TRUE),"try-error"))
wrong <- extra; wrong$experiment_id[1] <- "unknown"
stopifnot(inherits(try(ldj_mtt_apply_control_map(phys,"Series",selection,wrong),silent=TRUE),"try-error"))
wrong <- extra; wrong$series[1] <- "absent"
stopifnot(inherits(try(ldj_mtt_apply_control_map(phys,"Series",selection,wrong),silent=TRUE),"try-error"))
cat("Control provenance, experiment matching and shared physical IDs: PASS\n")
# A fifth unrelated series must not contaminate the four selected panels.
fifth <- d[d$Series=="S1" & d$control_type=="treatment",,drop=FALSE]
fifth$Series <- "S5"
fifth$observation_id <- paste0("EXTRA-",seq_len(nrow(fifth)))
plus <- rbind(d,fifth)
all_maps <- ldj_mtt_default_control_map(plus,"conc_uM","viability","Series")
four <- ldj_mtt_analyze_table(plus,"conc_uM","viability","Series",selection,
  "uM",control_assignments=all_maps)
stopifnot(length(four$rows)==4,
  all(vapply(four$rows,function(r) r$series,character(1))==selection),
  isTRUE(all.equal(fit_ref,vapply(four$rows,
    function(r) r$fit$table$estimate,numeric(1)),tolerance=1e-7)))
cat("Four selected series unaffected by additional source series: PASS\n")


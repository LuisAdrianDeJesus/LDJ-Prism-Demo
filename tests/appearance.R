# Synthetic-only editorial acceptance and immutable-analysis contract.
source("apps/ldj-prism/R/analysis.R")
source("apps/ldj-prism/R/mtt_visualization_v4_LDJ_Prism.R")
source("apps/ldj-prism/R/mtt_data_entry.R")
source("apps/ldj-prism/R/mtt_shiny.R")
bundle <- ldj_mtt_synthetic_bundle()
before <- serialize(bundle,NULL)
catalog <- ldj_palette_catalog()
stopifnot(all(c("Black and White","Floral","Prism Light","Prism Dark","Colorblind Safe",
  "Waves","Starry","Pearl","Viridis","Magma","Inferno","Plasma")%in%names(catalog)))
for(p in catalog) stopifnot(all(grepl("^#[[:xdigit:]]{6}$",p$colors)),p$alpha>0,p$alpha<=1)
state <- ldj_ensure_series(ldj_appearance(),c("S1","S2","S3","S4"))
stopifnot(identical(ldj_series_colors(state,c("S4","S1","S3","S2")),ldj_series_colors(state,c("S1","S2","S3","S4"))))
custom <- ldj_color_edit(state,"series","S1","series","#184A73",alpha=.65)
custom <- ldj_color_edit(custom,"panel","H","background","#F3F5FA")
roundtrip <- ldj_validate_colors(jsonlite::fromJSON(jsonlite::toJSON(custom,auto_unbox=TRUE),simplifyVector=FALSE))
stopifnot(identical(custom$series,roundtrip$series),isTRUE(all.equal(custom$panels,roundtrip$panels)))
bad <- custom;bad$global$text <- "red; script";stopifnot(inherits(try(ldj_validate_colors(bad),silent=TRUE),"try-error"))
bad <- custom;bad$global$alpha <- 2;stopifnot(inherits(try(ldj_validate_colors(bad),silent=TRUE),"try-error"))
bad <- custom;bad$global$text <- NULL;stopifnot(inherits(try(ldj_validate_colors(bad),silent=TRUE),"try-error"))
stopifnot(ldj_contrast("#000000","#FFFFFF")>20,length(ldj_color_checks(custom,c("S1","S2","S3","S4")))>0)
variants <- list(`LDJ Editorial — Original`=ldj_appearance(),
  `LDJ Minimalista`=ldj_appearance(preset="minimal"),
  `Black and White`=ldj_appearance("Black and White"),`Prism Light`=ldj_appearance("Prism Light"),
  `Prism Dark`=ldj_appearance("Prism Dark"),`Colorblind Safe`=ldj_appearance("Colorblind Safe"),
  Viridis=ldj_appearance("Viridis"),`HEX personalizado`=ldj_color_edit(ldj_appearance(),"series","DTG","series","#A43F70",.65))
dir.create("test-artifacts",showWarnings=FALSE)
comparisons <- list()
for(i in seq_along(variants)) {
  a <- ldj_ensure_series(variants[[i]],ldj_mtt_defaults()$order)
  cfg <- list(appearance=a,palette=c(ldj_mtt_defaults()$palette),label_significance=FALSE,
    plot_width=12,plot_height=18,dpi=600,figure_title=names(variants)[i],
    plot_caption="Datos sintéticos. Barras y bandas locales: media ± SD; no IC de regresión. Sin contrastes inferenciales.")
  cfg$palette[ldj_mtt_defaults()$order] <- ldj_series_colors(a,ldj_mtt_defaults()$order)[ldj_mtt_defaults()$order]
  r <- ldj_mtt_editor_build(bundle,cfg)
  stopifnot(length(r$panels)==8,identical(names(r$panels),LETTERS[1:8]),identical(serialize(bundle,NULL),before))
  for(tag in c("A","C","E","G")) stopifnot(any(vapply(r$panels[[tag]]$layers,
    function(layer) inherits(layer$geom,"GeomCol"),logical(1))))
  for(ext in c("svg","pdf","png")) {
    f <- file.path("test-artifacts",paste0("palette-",i,".",ext))
    ldj_mtt_editor_export(r,f)
    stopifnot(file.info(f)$size>10000)
  }
  svg <- paste(readLines(file.path("test-artifacts",paste0("palette-",i,".svg")),warn=FALSE),collapse="\n")
  stopifnot(grepl("CC50",svg,fixed=TRUE))
  comparisons[[i]] <- patchwork::wrap_elements(full=r$figure)
}
ggplot2::ggsave("test-artifacts/palette-comparison.png",patchwork::wrap_plots(comparisons,ncol=4),
  width=48,height=36,dpi=100,limitsize=FALSE,bg="white")
panel_state <- ldj_color_edit(ldj_appearance(),"panel","H","curve","#39BBA1",.4,1.3)
panel_state <- ldj_color_edit(panel_state,"global",element="bar_border",hex="#253244",linewidth=1.2)
panel_result <- ldj_mtt_editor_build(bundle,list(appearance=panel_state,label_significance=FALSE))
h_line <- Filter(function(layer) class(layer$geom)[1]=="GeomLine",panel_result$panels$H$layers)[[1]]
f_line <- Filter(function(layer) class(layer$geom)[1]=="GeomLine",panel_result$panels$F$layers)[[1]]
a_bar <- Filter(function(layer) class(layer$geom)[1]=="GeomCol",panel_result$panels$A$layers)[[1]]
stopifnot(h_line$aes_params$colour=="#39BBA1",h_line$aes_params$alpha==.4,
  h_line$aes_params$linewidth==1.3,f_line$aes_params$colour!="#39BBA1",
  substr(a_bar$aes_params$colour,1,7)=="#253244",a_bar$aes_params$linewidth==1.2,
  identical(serialize(bundle,NULL),before))
# Selected-table renderer: shared control remains gray and its physical record count is unchanged.
d <- bundle$orig;d$Series <- d$drug;d$Replica <- d$replicate
d$control_type <- ifelse(d$conc_uM==0,"untreated","treatment")
d$observation_id <- d$record_id;d$experiment_id <- "SYN-STYLE";d$control_source <- "synthetic";d$inference_eligible <- TRUE
r <- ldj_mtt_analyze_table(d,"conc_uM","viability","Series",ldj_mtt_defaults()$order,"uM")
snapshot <- serialize(r,NULL)
for(a in variants) {p <- ldj_mtt_table_figure(r,list(appearance=a));stopifnot(length(p$panels)==8,identical(snapshot,serialize(r,NULL)))}
env <- new.env(parent=globalenv());sys.source("apps/ldj-prism/app.R",envir=env)
env$style_fixture <- r;env$style_data <- d;env$fit_calls <- 0L
original_fit <- env$fit_response
env$fit_response <- function(...) {env$fit_calls <- env$fit_calls+1L;original_fit(...)}
shiny::testServer(env$server,{
  working(env$style_data)
  session$setInputs(x="conc_uM",y="viability",group="Series",unit="uM",model="4PL",
    module="mtt",mtt_view="editorial",mtt_user_data=TRUE,mtt_source="synthetic",mtt_display_unit="uM",
    mtt_series_1="DTG",mtt_series_2="Letermovir",mtt_series_3="CDQ",mtt_series_4="Lopinavir",
    font_size=12,font_family="sans",palette="LDJ Editorial",dispersion="SD",points=TRUE,
    point_shape="21",point_size=2,plot_title="",x_label="Dose",y_label="Response",
    x_scale="log10",width=12,height=18,dpi="600",control_label_default="Control")
  table_result(env$style_fixture);table_snapshot(analysis_signature())
  baseline <- serialize(table_result(),NULL)
  mtt_result();calls <- env$fit_calls
  for(a in variants) {
    appearance(a);mtt_result()
    stopifnot(env$fit_calls==calls,identical(baseline,serialize(table_result(),NULL)),
      identical(table_snapshot(),analysis_signature()))
  }
  session$setInputs(module="activity",kind="line",model_reason="Synthetic cache test",
    threshold_mode="absolute",zero_policy="exclude",control=0,y_scale="linear",legend_title="",legend_position="right",
    alpha=1,line_width=.9,line_type="solid",grid_lines=FALSE,caption="Synthetic")
  cached_curves <- fitted_curves();final_plot();calls <- env$fit_calls
  appearance(ldj_appearance("Prism Dark"));final_plot()
  stopifnot(env$fit_calls==calls,identical(cached_curves,fitted_curves()))
})
cat("Palette catalog, named assignments, JSON validation, contrast/CVD checks, 8 variants × SVG/PDF/PNG 600dpi, immutable models/data/CI/controls: PASS\n")

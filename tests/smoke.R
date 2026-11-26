# Run from repository root: Rscript apps/ldj-prism/tests/smoke.R
env <- new.env()
sys.source("apps/ldj-prism/app.R", envir = env)
stopifnot(nrow(env$demo_data) == 60L,
          identical(env$palette_ldj(2, "Okabe-Ito"), c("#0072B2", "#D55E00")))
original <- env$demo_data
shiny::testServer(env$server, {
  session$setInputs(x = "Concentracion_uM", y = "Viabilidad_pct",
    group = "Compuesto", kind = "scatter", x_scale = "log10",
    y_scale = "linear", palette = "Okabe-Ito", points = TRUE,
    point_size = 2, point_shape = "16", alpha = 0.8, line_type = "solid",
    line_width = 0.9, font_size = 12, font_family = "sans",
    legend_position = "right", legend_title = "", plot_title = "",
    x_label = "Concentración", y_label = "Respuesta", grid_lines = FALSE,
    caption = "Sintético", width = 7, height = 5, dpi = "600")
  for (kind in c("scatter", "line", "box")) {
    session$setInputs(kind = kind)
    p <- plot_builder()
    stopifnot(inherits(p, "ggplot"), length(ggplot2::ggplot_build(p)$data) > 0)
    for (fmt in c("png", "pdf", "svg")) {
      f <- tempfile(fileext = paste0(".", fmt))
      export_plot(f, fmt)
      stopifnot(file.info(f)$size > 0)
      unlink(f)
    }
  }
  session$setInputs(kind = "line", palette = "Personalizada",
    custom_color_1 = "#112233", custom_color_2 = "#AABBCC")
  p <- plot_builder()
  stopifnot(identical(unname(p$scales$get_scales("colour")$palette(2)),
                     c("#112233", "#AABBCC")))
  session$setInputs(x = "Compuesto", x_scale = "log10", kind = "scatter")
  rejected <- tryCatch({plot_builder(); FALSE}, error = function(e) TRUE)
  stopifnot(rejected)
  stopifnot(identical(dataset(), original))
})
cat("Synthetic rendering, export, color mapping, log validation and data integrity: PASS\n")

public_env <- new.env()
sys.source("apps/ldj-prism/app.R",envir=public_env)
csv <- tempfile(fileext=".csv")
write.csv(data.frame(Dose=c(0,1,2),Response=c(100,70,40),Group="Synthetic"),csv,row.names=FALSE)
shiny::testServer(public_env$server, {
  session$setInputs(separator=",",decimal=".",
    upload=list(name="synthetic.csv",datapath=csv,size=file.info(csv)$size,type="text/csv"))
  session$setInputs(x="Dose",y="Response",group="Group")
  stopifnot(nrow(dataset())==3,isTRUE(all.equal(mapped()$response,c(100,70,40))))
  session$setInputs(reset_grid=1)
  stopifnot(nrow(dataset())==3)
})
unlink(csv)
cat("Public synthetic CSV import, column mapping and restore: PASS\n")

xlsx <- tempfile(fileext=".xlsx")
writexl::write_xlsx(list(First=data.frame(Dose=1:3,Response=c(90,60,30)),
  Second=data.frame(Dose=c(0,2,4),Response=c(100,50,20))),xlsx)
shiny::testServer(public_env$server, {
  session$setInputs(upload=list(name="synthetic.xlsx",datapath=xlsx,
    size=file.info(xlsx)$size,type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    decimal=".")
  session$setInputs(sheet="Second")
  stopifnot(isTRUE(all.equal(dataset()$Response,c(100,50,20))))
  session$setInputs(sheet="First")
  stopifnot(isTRUE(all.equal(dataset()$Response,c(90,60,30))))
})
unlink(xlsx)
cat("Synthetic XLSX two-sheet import and selection: PASS\n")

shiny::testServer(env$server, {
  session$setInputs(x = "Concentracion_uM", y = "Viabilidad_pct",
    group = "Compuesto", kind = "scatter", x_scale = "log10",
    y_scale = "linear", palette = "Okabe-Ito", points = TRUE,
    point_size = 2, point_shape = "16", alpha = 0.8, line_type = "solid",
    line_width = 0.9, font_size = 12, font_family = "sans",
    legend_position = "right", legend_title = "", plot_title = "",
    x_label = "Concentración", y_label = "Respuesta", grid_lines = FALSE,
    caption = "Sintético", width = 7, height = 5, dpi = "600")
  working(data.frame(Concentracion_uM=c(0,0,1,2),
    Viabilidad_pct=c(100,98,70,40),Compuesto="Synthetic"))
  session$setInputs(module="universal",kind="bar",control_label_default="No tratado",
    control_type_default="untreated",dispersion="SD",x_scale="log10")
  p <- plot_builder()
  stopifnot(levels(p$data$.category)[1]=="No tratado",
    sum(p$data$.is_control)==2)
  built <- ggplot2::ggplot_build(p)
  stopifnot(nrow(built$data[[3]])==4)
  session$setInputs(kind="scatter")
  cp <- control_plot()
  stopifnot(nrow(cp$data)==2,all(cp$data$.control_label=="No tratado"),
            all(cp$data$Concentracion_uM==0))
  combined <- final_plot()
  stopifnot(inherits(combined,"patchwork"))
  f <- tempfile(fileext=".svg"); export_plot(f,"svg")
  stopifnot(file.info(f)$size>0); unlink(f)
})
cat("Control-first bars, individual observations and separate zero-dose panel: PASS\n")

shiny::testServer(env$server, {
  session$setInputs(x = "Concentracion_uM", y = "Viabilidad_pct",
    group = "Compuesto", kind = "scatter", x_scale = "log10",
    y_scale = "linear", palette = "Okabe-Ito", points = TRUE,
    point_size = 2, point_shape = "16", alpha = 0.8, line_type = "solid",
    line_width = 0.9, font_size = 12, font_family = "sans",
    legend_position = "right", legend_title = "", plot_title = "",
    x_label = "Concentración", y_label = "Respuesta", grid_lines = FALSE,
    caption = "Sintético", width = 7, height = 5, dpi = "600")
  session$setInputs(module="universal",multipanel=TRUE)
  stopifnot(sum(physical_data()$.is_control)==9,
    sum(visual_data()$.is_control)==18,
    all(is.na(physical_data()$Concentracion_uM[physical_data()$.is_control])),
    all(table(visual_data()$Compuesto[visual_data()$.is_control])==9))
  for(kind in c("bar","line")) {
    session$setInputs(kind=kind)
    p <- final_plot()
    stopifnot(inherits(p,"patchwork"))
    for(fmt in c("svg","pdf")) {
      f <- tempfile(fileext=paste0(".",fmt)); export_plot(f,fmt)
      stopifnot(file.info(f)$size>0); unlink(f)
    }
  }
  working(dataset()[,setdiff(names(dataset()),"control_source")])
  stopifnot(nrow(mapped())==42)
})
cat("Shared control physical n, per-series n, missing dose and multipanel SVG/PDF: PASS\n")

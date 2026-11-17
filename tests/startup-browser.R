Sys.unsetenv("LDJ_PRISM_MODE")
chrome <- Sys.which("google-chrome")
if(nzchar(chrome)) Sys.setenv(CHROMOTE_CHROME=chrome)
app <- shinytest2::AppDriver$new(app_dir=".",name="startup",load_timeout=90000)
tryCatch({
  app$wait_for_js("Boolean(document.querySelector('#plot img') && document.querySelector('#plot img').naturalWidth > 0)",timeout=60000)
  stopifnot(app$get_value(input="x")=="Concentracion_uM",
    app$get_value(input="y")=="Viabilidad_pct",app$get_value(input="group")=="Compuesto",
    app$get_js("document.querySelectorAll('.shiny-output-error').length") == 0)
  cat("Browser first-load render with no manual inputs: PASS\n")
  app$set_inputs(plot_title="Synthetic startup test",x_label="Dose",y_label="Response",palette="Viridis")
  app$wait_for_idle()
  stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
  for(kind in c("bar","line")) {
    app$set_inputs(kind=kind,multipanel=TRUE)
    app$wait_for_idle()
    stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
    app$set_inputs(inspector="Exportar")
    app$wait_for_idle()
    for(fmt in c("svg","pdf")) {
      f <- tempfile(fileext=paste0(".",fmt))
      app$get_download(fmt,filename=f)
      stopifnot(file.info(f)$size>0); unlink(f)
    }
  }
  cat("Browser synthetic controls, bars, log curves, multipanel and SVG/PDF: PASS\n")
  app$set_inputs(inspector="Gráfica",figure_template="mtt4x2")
  app$wait_for_idle(timeout=90000)
  app$wait_for_js("Boolean(document.querySelector('#plot img') && document.querySelector('#plot img').naturalWidth > 0)",timeout=90000)
  stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
  app$set_inputs(inspector="MTT 4×2",mtt_target="A",mtt_title="A. Browser panel title",mtt_hex="#123456",mtt_note="Synthetic annotation")
  app$click("mtt_apply"); app$wait_for_idle()
  stopifnot(app$get_js("document.querySelectorAll('.shiny-output-error').length")==0)
  app$set_inputs(inspector="Exportar")
  for(fmt in c("svg","pdf")) {
    f <- tempfile(fileext=paste0(".",fmt)); app$get_download(fmt,filename=f)
    stopifnot(file.info(f)$size>10000)
    if(fmt=="svg") stopifnot(any(grepl("Browser panel title",readLines(f,warn=FALSE),fixed=TRUE)))
    unlink(f)
  }
  cat("Browser MTT 8-panel editor, per-panel title/HEX/annotation and vector exports: PASS\n")
},finally=app$stop())


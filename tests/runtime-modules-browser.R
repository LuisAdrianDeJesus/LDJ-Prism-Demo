# Regression tests for switching analytical modules on the public Shiny demo.
# Synthetic-only. Exercise defaults as well as valid explicitly entered parameters.
Sys.unsetenv("LDJ_PRISM_MODE")
chrome <- Sys.which("google-chrome")
if (nzchar(chrome)) Sys.setenv(CHROMOTE_CHROME=chrome)
app <- shinytest2::AppDriver$new(app_dir=".",name="switch-modules",load_timeout=90000)
tryCatch({
  check_render <- function(label, expect_image=TRUE) {
    app$wait_for_idle(timeout=90000)
    errors <- app$get_js("document.querySelectorAll('.shiny-output-error').length")
    if(errors) {
      message("Unexpected Shiny output failure in ",label)
      message(app$get_js("Array.from(document.querySelectorAll('.shiny-output-error')).map(x=>x.textContent).join(' | ')"))
    }
    stopifnot(identical(errors,0))
    if(expect_image) {
      app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth>0)",timeout=90000)
    }
    cat(label,": PASS\n")
  }
  check_render("Initial editor")
  app$set_inputs(module="activity",inspector="Gráfica")
  check_render("Activity selected with default empty model justification",expect_image=FALSE)
  # Now supply an explicit model reason and check the full activity figure.
  app$set_inputs(inspector="Análisis",model_reason="Synthetic QA: concentration-response model")
  app$click("run_analysis")
  app$set_inputs(inspector="Gráfica")
  check_render("Activity after model justification")
  app$set_inputs(inspector="Exportar")
  f <- tempfile(fileext=".svg");app$get_download("svg",filename=f)
  stopifnot(file.info(f)$size>1000);unlink(f)
  app$set_inputs(module="safety",inspector="Gráfica",safety_display="summary")
  check_render("Safety with no parameters (ND should not be a server error)")
  app$set_inputs(inspector="Análisis",safety_table=FALSE,ic50_value=0.2,
    cc20_value=0.8,cc50_value=2.0,ic_unit="uM",cc_unit="uM")
  app$click("run_analysis")
  app$set_inputs(inspector="Gráfica")
  check_render("Safety after entry of valid IC50/CC20/CC50")
  app$set_inputs(inspector="Exportar")
  g <- tempfile(fileext=".svg");app$get_download("svg",filename=g)
  stopifnot(file.info(g)$size>1000,any(grepl("selectividad",readLines(g,warn=FALSE),ignore.case=TRUE)))
  unlink(g)
  app$set_inputs(module="mtt",inspector="Gráfica")
  check_render("Return to MTT 4x2")
  cat("Public module switch and parameter entry regression: PASS\n")
},finally=app$stop())

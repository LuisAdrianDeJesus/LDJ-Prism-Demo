# Regression tests for switching analytical modules on the public Shiny demo.
# Synthetic-only. Exercise defaults as well as valid explicitly entered parameters.
Sys.unsetenv("LDJ_PRISM_MODE")
chrome <- Sys.which("google-chrome")
if (nzchar(chrome)) Sys.setenv(CHROMOTE_CHROME=chrome)
app <- shinytest2::AppDriver$new(app_dir=".",name="switch-modules",load_timeout=90000)
tryCatch({
  check_render <- function(label, expect_image=TRUE) {
    app$wait_for_idle(timeout=90000)
    errors <- app$get_js("document.querySelectorAll('.shiny-output-error:not(.shiny-output-error-validation)').length")
    if(errors) {
      message("Unexpected Shiny output failure in ",label)
      message(app$get_js("Array.from(document.querySelectorAll('.shiny-output-error')).map(x=>x.textContent).join(' | ')"))
    }
    stopifnot(isTRUE(as.numeric(errors)==0))
    if(expect_image) {
      app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth>0)",timeout=90000)
    }
    cat(label,": PASS\n")
  }
  check_render("Initial editor")
  app$set_inputs(module="activity",inspector="Gráfica")
  check_render("Activity automatically displays the bundled synthetic example")
  stopifnot(grepl("Demostración sintética", app$get_value(input="model_reason"),fixed=TRUE))
  app$set_inputs(inspector="Datos",entry_type="observations")
  app$click("blank_table")
  app$wait_for_js("document.querySelector('#model_reason').value === ''",timeout=30000)
  app$click("load_example")
  app$wait_for_js("document.querySelector('#model_reason').value.includes('Demostración sintética')",timeout=30000)
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
  app$set_inputs(inspector="Análisis",ic50_value=200,ic_unit="nM",cc50_value=.002,cc_unit="mM")
  app$wait_for_js("Array.from(document.querySelectorAll('#results tr')).some(r=>Array.from(r.cells).some((c,i)=>c.textContent.trim()==='SI' && Number(r.cells[i+1]?.textContent.trim())===10))",timeout=30000)
  app$set_inputs(ic50_value=2e-7,ic_unit="M",cc50_value=2,cc_unit="uM")
  app$wait_for_js("Array.from(document.querySelectorAll('#results tr')).some(r=>Array.from(r.cells).some((c,i)=>c.textContent.trim()==='SI' && Number(r.cells[i+1]?.textContent.trim())===10))",timeout=30000)
  cat("Safety SI unit equivalence: PASS\n")
  app$set_inputs(module="mtt",inspector="Gráfica")
  check_render("Return to MTT 4x2")
  cat("Public module switch and parameter entry regression: PASS\n")
},finally=app$stop())

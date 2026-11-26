Sys.unsetenv("LDJ_PRISM_MODE")
source("R/synergy_studio.R")
app <- shinytest2::AppDriver$new(app_dir = ".", name = "editorial-stress", load_timeout = 90000)
dir.create("test-artifacts/stress", recursive = TRUE, showWarnings = FALSE)
app$set_inputs(module = "synergy", wait_ = FALSE)
app$wait_for_js("Boolean(document.querySelector('#synergy-plot img')?.naturalWidth)", timeout = 60000)
fixtures <- data.frame(size = c(6L, 7L, 12L), experiments = c(1L, 3L, 6L),
  unit = c("nM", "uM", "mM"), conversion = c(1000, 1, .001))
for (i in seq_len(nrow(fixtures))) {
  f <- fixtures[i, ]
  data <- ldj_synergy_demo(f$size)
  blocks <- split(data, data$experiment_id)
  data <- do.call(rbind, lapply(seq_len(f$experiments), function(i) {
    x <- blocks[[1L + ((i - 1L) %% length(blocks))]]
    x$experiment_id <- sprintf("Experimento sintético con encabezado largo %02d", i)
    x$observation_id <- paste(i, x$observation_id, sep = "-")
    x
  }))
  data$dose_A <- data$dose_A * f$conversion
  data$dose_B <- data$dose_B * f$conversion
  data$unit_A <- data$unit_B <- f$unit
  data$compound_A <- "Compuesto sintético con denominación editorial larga A"
  data$compound_B <- "Compuesto sintético con denominación editorial larga B"
  prefix <- sprintf("test-artifacts/stress/%d-%d-%s", f$size, f$experiments, f$unit)
  utils::write.csv(data, paste0(prefix, "-input.csv"), row.names = FALSE)
  app$run_js("document.querySelector('#synergy-views a[data-value=Datos]').click()")
  app$upload_file(`synergy-file` = paste0(prefix, "-input.csv"))
  app$wait_for_js("document.querySelector('#synergy-provenance')?.textContent.includes('Calcule')", timeout = 30000)
  app$click("synergy-calculate", timeout_ = 90000)
  app$wait_for_js("document.querySelector('#synergy-provenance')?.textContent === 'Datos editados o importados.'", timeout = 60000)
  app$run_js("document.querySelector(\"#synergy-views a[data-value='Clasificación']\").click()")
  app$set_inputs(`synergy-unit` = f$unit, `synergy-template` = "custom",
    `synergy-gallery_sequence` = "bubble,contour,conditional_a,surface,dumbbell,marginal",
    `synergy-figure_columns` = 2, timeout_ = 90000)
  for (panel in LETTERS[1:6]) {
    app$set_inputs(`synergy-panel_export` = panel, timeout_ = 90000)
    output <- paste0(prefix, "-", panel, ".svg")
    app$get_download("synergy-svg", filename = output)
    stopifnot(any(grepl("<svg", readLines(output, n = 5), fixed = TRUE)))
  }
  app$get_download("synergy-figure_json_out", filename = paste0(prefix, "-config.json"))
}
app$stop()

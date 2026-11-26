source("R/synergy_gallery.R")
one <- ldj_synergy_gallery_sequence("bubble@a,bubble@b,observed@base", character())
two <- ldj_synergy_gallery_sequence("observed@base,bubble@b,bubble@a", character())
stopifnot(identical(one, two[names(one)]),
  inherits(try(ldj_synergy_gallery_sequence("bubble@x,bubble@x", character()), silent = TRUE), "try-error"))
a <- list(bottom = 0, top = 100, ec50 = 1e-6, hill = 1, bounds = c(5, 95))
b <- list(bottom = 0, top = 100, ec50 = 3e-6, hill = 2, bounds = c(10, 90))
iso <- ldj_synergy_isobole(a, b, 50)
stopifnot(all.equal(iso$dose_A_M, c(0, 1e-6)), all.equal(iso$dose_B_M, c(3e-6, 0)),
  is.null(ldj_synergy_isobole(a, b, 95)),
  all.equal(ldj_synergy_isobole(modifyList(a, list(ec50 = 1)), modifyList(b, list(ec50 = 3)), 50), iso * 1e6),
  is.null(ldj_synergy_surface(1:2, 1:2, matrix(c(1, 2, 3, NA_real_), 2))$faces),
  nrow(ldj_synergy_surface(1:2, 1:2, matrix(1:4, 2))$faces) == 4L)
monos <- expand.grid(a = seq(0, 1, .01), b = seq(0, 1, .01))
hsa <- with(monos, pmax(a, b))
bliss <- with(monos, a + b - a*b)
stopifnot(all(bliss >= hsa - 1e-12),
  all((.9 - bliss <= .1) | (.9 - hsa > .1)),
  is.null(ldj_synergy_density(1:4, rep("sparse", 4))$curves),
  nrow(ldj_synergy_density(1:5, rep("supported", 5))$curves) == 128)
pareto_fixture <- data.frame(compound_A = "A", compound_B = "B", cell_line = "test",
  exposure_hours = 24, experiment_id = c("E1", "E1", "E1", "E2", "E2"),
  dose_A_M = 1:5 * 1e-6, dose_B_M = 1e-6,
  activity_pct = c(90, 80, 92, 20, 50), viability_pct = c(90, 70, 75, 30, NA))
stopifnot(identical(ldj_synergy_pareto(pareto_fixture), c(TRUE, FALSE, TRUE, TRUE, NA)),
  identical(ldj_synergy_gallery_design("AB/CC", 3), "AB\nCC"),
  inherits(try(ldj_synergy_gallery_design("AB/BA", 2), silent = TRUE), "try-error"))
chrome <- Sys.which("google-chrome")
if(nzchar(chrome)) Sys.setenv(CHROMOTE_CHROME=chrome)
app <- shinytest2::AppDriver$new(app_dir=".",name="synergy",load_timeout=90000)
svg_text <- function(path) {
  parser <- paste(
    "import sys, xml.etree.ElementTree as ET",
    "root = ET.parse(sys.argv[1]).getroot()",
    "print(' '.join(' '.join(''.join(el.itertext()) for el in",
    "root.iter('{http://www.w3.org/2000/svg}text')).split()))", sep = "\n")
  result <- system2("python3", c("-c", shQuote(parser), shQuote(path)), stdout = TRUE)
  stopifnot(is.null(attr(result, "status")), length(result) == 1L)
  result
}

tryCatch({
  app$set_inputs(module="synergy",wait_=FALSE)
  app$wait_for_js("Boolean(document.querySelector('#synergy-plot img')?.naturalWidth)",timeout=60000)
  dir.create("test-artifacts",showWarnings=FALSE)
  path <- "test-artifacts/synergy-classification.svg"
  app$get_download("synergy-svg",filename=path)
  stopifnot(any(grepl("Clasificación por unidad",readLines(path,warn=FALSE),fixed=TRUE)))
  app$get_screenshot("test-artifacts/synergy-classification.png")
  for (design in c("class","integral")) {
    app$set_inputs(`synergy-template`=design,timeout_=30000)
    path <- paste0("test-artifacts/synergy-",design,".svg")
    app$get_download("synergy-svg",filename=path)
    svg <- svg_text(path)
    if (design == "class") {
      stopifnot(grepl("Umbral operativo", svg, fixed = TRUE))
    } else {
      for (id in c("F", "G")) {
        label <- if (id == "F") "Loewe" else "ZIP"
        app$set_inputs(`synergy-panel_export` = id, timeout_ = 30000)
        nd_path <- file.path("test-artifacts", paste0("synergy-ND-", label, ".svg"))
        app$get_download("synergy-svg", filename = nd_path)
        panel <- svg_text(nd_path)
        stopifnot(grepl(label, panel, fixed = TRUE),
                  if (id == "F") !grepl("Validación pendiente", panel, fixed = TRUE) else grepl("análisis experimental", panel, fixed = TRUE))
      }
      app$set_inputs(`synergy-panel_export` = "", timeout_ = 30000)
    }
  }
  app$set_inputs(`synergy-template` = "integral", `synergy-panel_order` = "E,D",
    `synergy-edit_panel` = "E", `synergy-panel_title` = "Bliss editorial", timeout_=30000)
  app$click("synergy-save_panel")
  config <- "test-artifacts/synergy-figure-config.json"
  app$get_download("synergy-figure_json_out", filename = config)
  config_json <- jsonlite::read_json(config)
  stopifnot(config_json$visual$size_preset == "auto", config_json$visual$dose_digits == 3)
  stopifnot(config_json$version == 3, config_json$order == "E,D",
            config_json$panels[["integral/E"]]$title == "Bliss editorial")
  app$set_inputs(`synergy-panel_order` = "", timeout_=30000)
  app$upload_file(`synergy-figure_json` = config)
  app$wait_for_js("document.querySelector('#synergy-panel_order')?.value === 'E,D'", timeout=30000)
  app$get_download("synergy-svg", filename="test-artifacts/synergy-composition.svg")
  app$set_inputs(`synergy-panel_export` = "E", timeout_=30000)
  individual <- "test-artifacts/synergy-panel-E.svg"
  app$get_download("synergy-svg", filename=individual)
  stopifnot(any(grepl("Bliss editorial", readLines(individual, warn=FALSE), fixed=TRUE)))
  pdf <- "test-artifacts/synergy-panel-E.pdf"
  app$get_download("synergy-pdf", filename=pdf)
  stopifnot(identical(readChar(pdf, 4L, useBytes=TRUE), "%PDF"))
  png_dpi <- function(path) {
    bytes <- readBin(path, "raw", n = 1000000)
    pos <- which(bytes == charToRaw("p"))
    pos <- pos[pos + 12L <= length(bytes)]
    pos <- pos[vapply(pos, function(i) identical(bytes[i:(i + 3L)], charToRaw("pHYs")), logical(1))]
    stopifnot(length(pos) == 1L, as.integer(bytes[pos + 12L]) == 1L)
    sum(as.integer(bytes[(pos + 4L):(pos + 7L)]) * 256^(3:0)) * .0254
  }
  for (dpi in c(300L, 600L)) {
    path <- paste0("test-artifacts/synergy-panel-E-", dpi, ".png")
    app$get_download(if(dpi == 300L) "synergy-png300" else "synergy-png", filename=path)
    stopifnot(abs(png_dpi(path) - dpi) <= 0.05)
  }
  app$get_screenshot("test-artifacts/synergy-figure-studio.png")
  app$run_js("document.querySelector('#synergy-views a[data-value=Incertidumbre]').click();")
  app$wait_for_js("document.querySelector('#synergy-bootstrap_summary')?.textContent.includes('Confirme')",
                  timeout=30000)
  app$set_inputs(`synergy-bootstrap_independent`=TRUE, `synergy-bootstrap_model`="Bliss",
    `synergy-bootstrap_seed`=57, timeout_=30000)
  app$wait_for_js("document.querySelector('#synergy-bootstrap_summary')?.textContent.includes('IC95% percentil')",
                  timeout=30000)
  app$wait_for_js("Boolean(document.querySelector('#synergy-bootstrap_plot img')?.naturalWidth)",
                  timeout=30000)
  bootstrap <- "test-artifacts/synergy-bootstrap.csv"
  app$get_download("synergy-bootstrap_csv", filename=bootstrap)
  tab <- read.csv(bootstrap)
  stopifnot(nrow(tab) == 3L, all(tab$complete), all(tab$seed == 57L),
            all(is.finite(tab$leave_one_out)))
  app$get_screenshot("test-artifacts/synergy-bootstrap.png")
  app$set_inputs(`synergy-bootstrap_model`="HSA", `synergy-bootstrap_iterations`="5000",
    `synergy-bootstrap_seed`=92, timeout_=30000)
  second <- "test-artifacts/synergy-bootstrap-HSA.csv"
  app$get_download("synergy-bootstrap_csv", filename=second)
  second_tab <- read.csv(second)
  stopifnot(all(second_tab$metric == "HSA_excess"), all(second_tab$iterations == 5000L),
            all(second_tab$seed == 92L))
  app$run_js("document.querySelector(\"#synergy-views a[data-value='Beneficio farmacológico']\").click();")
  app$click("synergy-benefit_demo")
  app$wait_for_js("Boolean(document.querySelector('#synergy-benefit_plot img')?.naturalWidth)",
                  timeout=60000)
  app$wait_for_js("document.querySelector('#synergy-benefit_status')?.textContent.includes('3 pares')",
                  timeout=30000)
  app$get_screenshot("test-artifacts/synergy-benefit.png")
  app$get_download("synergy-benefit_csv", filename="test-artifacts/synergy-benefit.csv")
  app$set_inputs(`synergy-viability_min`=100, timeout_=30000)
  app$wait_for_js("document.querySelector('#synergy-benefit_status')?.textContent.includes('0 pares')",
                  timeout=30000)
  app$run_js("document.querySelector(\"#synergy-views a[data-value='Clasificación']\").click();")
  app$set_inputs(`synergy-panel_export` = "", `synergy-panel_order` = "",
    `synergy-viability_min` = 80, timeout_=30000)
  app$set_inputs(`synergy-template` = "integral", `synergy-preview_panel` = "F", timeout_ = 30000)
  app$wait_for_js("Boolean(document.querySelector('#synergy-plot img')?.naturalWidth)", timeout = 60000)
  app$get_download("synergy-svg", filename = "test-artifacts/preview-export-before.svg")
  app$get_download("synergy-csv", filename = "test-artifacts/preview-science-before.csv")
  session <- app$get_chromote_session()
  for (viewport in list(c(390, 844), c(768, 1024), c(1366, 768), c(1920, 1080))) {
    session$Emulation$setDeviceMetricsOverride(width = viewport[1], height = viewport[2], deviceScaleFactor = 1, mobile = FALSE)
    app$run_js("document.querySelector('.ldj-preview').dataset.zoom = '200'")
    app$wait_for_js("(() => {const v=document.querySelector('.ldj-preview');const i=v.querySelector('img');return i && v.scrollWidth > v.clientWidth && Math.abs(i.width-i.naturalWidth*2)<2})()", timeout=30000)
    app$run_js("document.querySelector('.ldj-preview').dataset.zoom = 'fit'")
    app$wait_for_js("(() => {const v=document.querySelector('.ldj-preview');const i=v.querySelector('img');return i && i.width <= v.clientWidth})()", timeout=30000)
    app$get_screenshot(sprintf("test-artifacts/preview-%dx%d.png", viewport[1], viewport[2]))
  }
  session$Emulation$clearDeviceMetricsOverride()
  app$get_download("synergy-svg", filename = "test-artifacts/preview-export-after.svg")
  app$get_download("synergy-csv", filename = "test-artifacts/preview-science-after.csv")
  for (kind in c("export", "science")) {
    extension <- if (kind == "export") "svg" else "csv"
    hashes <- tools::md5sum(sprintf("test-artifacts/preview-%s-%s.%s", kind, c("before","after"), extension))
    stopifnot(!anyNA(hashes), length(unique(hashes)) == 1L)
  }
  app$set_inputs(`synergy-preview_panel` = "", timeout_ = 30000)
  scaled <- data.frame(template = character(), format = character(),
    width_inches = numeric(), height_inches = numeric(), dpi = integer())
  for (design in c("class", "integral", "benefit")) {
    app$set_inputs(`synergy-template` = design, timeout_=30000)
    minimum <- if (design == "integral") c(12, 17) else c(10, 8)
    stem <- paste0("test-artifacts/editorial-", design)
    app$get_download("synergy-svg", filename = paste0(stem, ".svg"))
    svg <- readLines(paste0(stem, ".svg"), n = 4, warn = FALSE)
    dimensions <- regmatches(svg, gregexpr("[0-9]+[.][0-9]+pt", svg))
    dims <- as.numeric(sub("pt", "", unlist(dimensions))) / 72
    stopifnot(length(dims) == 2L, all(dims >= minimum - .01))
    app$get_download("synergy-pdf", filename = paste0(stem, ".pdf"))
    stopifnot(identical(readChar(paste0(stem, ".pdf"), 4L, useBytes = TRUE), "%PDF"))
    scaled <- rbind(scaled, data.frame(template = design, format = c("SVG", "PDF"),
      width_inches = dims[1], height_inches = dims[2], dpi = NA_integer_))
    for (dpi in c(300L, 600L)) {
      out <- paste0(stem, "-", dpi, ".png")
      app$get_download(if (dpi == 300L) "synergy-png300" else "synergy-png", filename = out)
      header <- readBin(out, "raw", n = 24L)
      integer32 <- function(x) sum(as.integer(x) * 256^(3:0))
      stopifnot(abs(png_dpi(out) - dpi) <= 0.05,
        abs(integer32(header[17:20]) - dims[1] * dpi) <= 2,
        abs(integer32(header[21:24]) - dims[2] * dpi) <= 2)
      scaled <- rbind(scaled, data.frame(template = design, format = "PNG",
        width_inches = dims[1], height_inches = dims[2], dpi = dpi))
    }
  }
  write.csv(scaled, "test-artifacts/editorial-dimensions.csv", row.names = FALSE)
  app$get_download("synergy-csv", filename = "test-artifacts/synergy-style-before.csv")
  app$set_inputs(`synergy-template` = "class", `synergy-panel_export` = "F",
    `synergy-class_scope` = "experiment", `synergy-heat_ticks_y` = 4,
    `synergy-benefit_legend` = "Ensayo y condición", timeout_=30000)
  exported_class <- "test-artifacts/classification-by-experiment.svg"
  app$get_download("synergy-svg", filename = exported_class)
  stopifnot(grepl("Clasificación por experimento", svg_text(exported_class), fixed = TRUE))
  app$get_download("synergy-csv", filename = "test-artifacts/synergy-style-after.csv")
  stopifnot(identical(tools::md5sum("test-artifacts/synergy-style-before.csv")[[1L]],
    tools::md5sum("test-artifacts/synergy-style-after.csv")[[1L]]))
  app$run_js("document.querySelector('#synergy-experimental_model').closest('details').open = true;")
  app$set_inputs(`synergy-experimental_model` = "Loewe", timeout_=30000)
  app$wait_for_js("Boolean(document.querySelector('#synergy-experimental_plot img')?.naturalWidth)", timeout=30000)
  experimental <- "test-artifacts/experimental-Loewe.csv"
  app$get_download("synergy-experimental_csv", filename = experimental)
  diagnostics <- read.csv(experimental)
  stopifnot(nrow(diagnostics) > 0L, any(diagnostics$evaluable),
    all(is.na(diagnostics$excess[!diagnostics$evaluable])),
    all(diagnostics$common_domain <= diagnostics$evaluable))
  app$get_screenshot("test-artifacts/experimental-Loewe.png")

  zip_mode <- Sys.getenv("LDJ_TEST_ZIP")
  if (nzchar(zip_mode)) {
    app$set_inputs(`synergy-experimental_model` = "ZIP", timeout_=60000)
    zip_out <- "test-artifacts/experimental-ZIP.csv"
    app$get_download("synergy-experimental_csv", filename = zip_out)
    zip <- read.csv(zip_out)
    stopifnot(nrow(zip) > 0, all(zip$model == "ZIP"))
    if (zip_mode == "absent") {
      stopifnot(all(zip$status == "reference_unavailable"), all(!zip$evaluable),
        all(is.na(zip$global_pp)))
    } else {
      stopifnot(utils::packageVersion("synergyfinder") == "3.20.0",
        any(zip$evaluable), all(is.finite(zip$expected[zip$evaluable])),
        all(is.finite(zip$global_pp[zip$evaluable])), all(is.finite(zip$excess[zip$evaluable])))
      app$wait_for_js("Boolean(document.querySelector('#synergy-experimental_plot img')?.naturalWidth)", timeout=60000)
      app$get_screenshot("test-artifacts/experimental-ZIP.png")
    }
  }
  for (preset in c("landscape", "concordance", "reproducibility", "optimization")) {
    app$set_inputs(`synergy-template` = preset, `synergy-panel_export` = "", timeout_ = 30000)
    file <- file.path("test-artifacts", paste0("gallery-", preset, ".svg"))
    app$get_download("synergy-svg", filename = file)
    stopifnot(nchar(svg_text(file)) > 80)
  }
  app$set_inputs(`synergy-template` = "custom", `synergy-gallery_layout` = "AB/CC/DD",
    `synergy-gallery_sequence` = "observed,bubble,bubble,dumbbell",
    `synergy-gallery_widths` = "1,2", `synergy-gallery_heights` = "1,1,2",
    `synergy-gallery_values` = TRUE, timeout_ = 30000)
  for (suffix in c("svg", "pdf", "png300", "png")) {
    extension <- if (suffix %in% c("svg", "pdf")) suffix else paste0(suffix, ".png")
    app$get_download(paste0("synergy-", suffix),
      filename = file.path("test-artifacts", paste0("gallery-custom.", extension)))
  }
  app$get_screenshot("test-artifacts/figure-studio-gallery.png")
  config <- "test-artifacts/gallery-config.json"
  app$get_download("synergy-figure_json_out", filename = config)
  restored <- jsonlite::read_json(config)
  stopifnot(identical(restored$gallery$layout, "AB/CC/DD"), isTRUE(restored$gallery$values))
  app$set_inputs(`synergy-template` = "unit", timeout_ = 30000)
  app$upload_file(`synergy-figure_json` = config)
  app$wait_for_js("document.querySelector('#synergy-template').value === 'custom'", timeout = 30000)
  app$get_download("synergy-csv", filename = "test-artifacts/gallery-science.csv")
  stopifnot(identical(tools::md5sum("test-artifacts/synergy-style-before.csv")[[1L]],
    tools::md5sum("test-artifacts/gallery-science.csv")[[1L]]))
  app$set_inputs(`synergy-gallery_sequence` = "bland_altman,upset,raincloud,ridgeline,contour",
    `synergy-gallery_layout` = "AB/CD/EE", `synergy-gallery_heights` = "1,1,1", timeout_ = 30000)
  labels <- c(A = "Bland–Altman descriptivo", B = "Intersecciones por modelo", C = "Distribución con observaciones", D = "Densidades por experimento", E = "Contornos de interacción")
  for (id in names(labels)) {
    app$set_inputs(`synergy-panel_export` = id, timeout_ = 30000)
    file <- file.path("test-artifacts", paste0("second-generation-", id, ".svg"))
    app$get_download("synergy-svg", filename = file)
    stopifnot(grepl(labels[[id]], svg_text(file), fixed = TRUE),
      any(grepl("<(circle|polygon|rect) ", readLines(file, warn = FALSE))))
  }
  app$set_inputs(`synergy-panel_export` = "", timeout_ = 30000)
  app$get_download("synergy-svg", filename = "test-artifacts/second-generation.svg")
  app$get_screenshot("test-artifacts/second-generation.png")
  app$set_inputs(`synergy-gallery_sequence` = "marginal,surface,isobole", `synergy-gallery_layout` = "AB/CC",
    `synergy-gallery_heights` = "1,1", `synergy-gallery_isobole_effect` = 50, timeout_ = 30000)
  app$get_download("synergy-svg", filename = "test-artifacts/gallery-advanced.svg")
  app$get_screenshot("test-artifacts/gallery-advanced.png")
  catalog_dir <- "test-artifacts/gallery-catalog"
  dir.create(catalog_dir, recursive = TRUE, showWarnings = FALSE)
  app$set_inputs(`synergy-gallery_layout` = "", `synergy-gallery_widths` = "",
    `synergy-gallery_heights` = "", `synergy-panel_export` = "A", timeout_ = 30000)
  gallery <- ldj_synergy_gallery()
  stopifnot(length(gallery) == 21L)
  for (id in names(gallery)) {
    app$set_inputs(`synergy-gallery_sequence` = id, timeout_ = 30000)
    stem <- file.path(catalog_dir, id)
    app$get_download("synergy-svg", filename = paste0(stem, ".svg"))
    labels <- svg_text(paste0(stem, ".svg"))
    stopifnot(nzchar(labels))
    app$get_download("synergy-pdf", filename = paste0(stem, ".pdf"))
    stopifnot(identical(readChar(paste0(stem, ".pdf"), 4L, useBytes = TRUE), "%PDF"))
    for (dpi in c(300L, 600L)) {
      png <- paste0(stem, "-", dpi, ".png")
      app$get_download(if (dpi == 300L) "synergy-png300" else "synergy-png", filename = png)
      stopifnot(abs(png_dpi(png) - dpi) <= .05)
    }
    config <- paste0(stem, ".json")
    app$get_download("synergy-figure_json_out", filename = config)
    stopifnot(identical(jsonlite::read_json(config)$gallery$sequence, id))
    alternate <- if (id == "mono") "observed" else "mono"
    app$set_inputs(`synergy-gallery_sequence` = alternate, timeout_ = 30000)
    app$upload_file(`synergy-figure_json` = config)
    app$wait_for_js(sprintf("document.querySelector('#synergy-gallery_sequence').value === '%s'", id),
      timeout = 30000)
    restored_svg <- paste0(stem, "-restored.svg")
    app$get_download("synergy-svg", filename = restored_svg)
    stopifnot(identical(labels, svg_text(restored_svg)),
      identical(unname(tools::md5sum(paste0(stem, ".svg"))), unname(tools::md5sum(restored_svg))))
  }
  app$set_inputs(`synergy-gallery_sequence` = "isobole", `synergy-gallery_isobole_effect` = 99, timeout_ = 30000)
  outside <- file.path(catalog_dir, "isobole-ND.svg")
  app$get_download("synergy-svg", filename = outside)
  stopifnot(grepl("ND", svg_text(outside), fixed = TRUE))
  fixture <- jsonlite::read_json(file.path(catalog_dir, "bubble.json"))
  fixture$gallery$sequence <- "bubble@first,bubble@second"
  fixture$panels <- list("instance/bubble/first" = list(title = "Instancia primera", subtitle = "", tag = "P"),
    "instance/bubble/second" = list(title = "Instancia segunda", subtitle = "", tag = "Q",
      family = "serif", ticks_x = "2", gap = "3", min_width = "1", min_height = "1"))
  fixture_file <- file.path(catalog_dir, "keyed-instances.json")
  jsonlite::write_json(fixture, fixture_file, auto_unbox = TRUE)
  app$upload_file(`synergy-figure_json` = fixture_file)
  app$wait_for_js("document.querySelector('#synergy-gallery_sequence').value === 'bubble@first,bubble@second'",
    timeout = 30000)
  app$set_inputs(`synergy-gallery_sequence` = "bubble@second,bubble@first",
    `synergy-panel_export` = "A", timeout_ = 30000)
  keyed <- file.path(catalog_dir, "keyed-reordered.svg")
  app$get_download("synergy-svg", filename = keyed)
  stopifnot(grepl("Instancia segunda", svg_text(keyed), fixed = TRUE),
    !grepl("Instancia primera", svg_text(keyed), fixed = TRUE))
  app$set_inputs(`synergy-model` = "Loewe", `synergy-template` = "landscape",
    `synergy-panel_export` = "", timeout_ = 30000)
  file <- "test-artifacts/Loewe-main.csv"
  app$get_download("synergy-csv", filename = file)
  main <- read.csv(file)
  stopifnot(all(c("Loewe_expected", "Loewe_excess", "Loewe_inference_status") %in% names(main)),
    nrow(main) == 108L, sum(is.finite(main$score)) == 75L,
    sum(main$Loewe_status == "outside_common_observed_domain") == 33L,
    all(startsWith(main$Loewe_inference_status, "ND")), identical(main$score, main$Loewe_excess),
    identical(main$score, diagnostics$excess),
    all(is.na(main$score[main$Loewe_status != "candidate"])))
  app$get_download("synergy-svg", filename = "test-artifacts/Loewe-main.svg")
  stopifnot(grepl("Loewe", svg_text("test-artifacts/Loewe-main.svg"), fixed = TRUE))

  for (choice in c("HSA/Bliss", "HSA/Loewe", "Bliss/Loewe", "HSA/Bliss/Loewe")) {
    app$set_inputs(`synergy-agreement_models` = choice, timeout_ = 30000)
    path <- file.path("test-artifacts", paste0("agreement-", gsub("/", "-", choice), ".csv"))
    app$get_download("synergy-csv", filename = path)
    comparison <- read.csv(path)
    expected_n <- if (choice == "HSA/Bliss") 108L else 75L
    stopifnot(sum(comparison$agreement_common) == expected_n,
      all(comparison$agreement_common_n == expected_n),
      all(comparison$agreement_excluded_n == 108L - expected_n),
      all(comparison$agreement_status[!comparison$agreement_common] == "ND"))
  }
  app$set_inputs(`synergy-template` = "concordance", timeout_ = 30000)
  path <- "test-artifacts/agreement-three-models.svg"
  app$get_download("synergy-svg", filename = path)
  stopifnot(grepl("75/108", svg_text(path), fixed = TRUE))

  source("R/synergy_studio.R")
  revised <- ldj_synergy_demo(7)
  revised <- revised[revised$dose_A != max(revised$dose_A) &
    !(revised$dose_A > 1 & revised$dose_B > 1 & revised$experiment_id == "SIM-u2"), ]
  revised$dose_A <- revised$dose_A^1.07 * 1000
  revised$unit_A <- "nM"
  revised$compound_A <- "Compuesto nuevo"
  revised$cell_line <- "Contexto alternativo"
  revised$exposure_hours <- 48
  revised$experiment_id <- sub("SIM", "EDIT", revised$experiment_id)
  input_file <- "test-artifacts/Loewe-irregular-input.csv"
  write.csv(revised, input_file, row.names = FALSE)
  app$run_js("document.querySelector('#synergy-views a[data-value=Datos]').click();")
  app$upload_file(`synergy-file` = input_file)
  app$wait_for_js("document.querySelector('#synergy-provenance')?.textContent.includes('Calcule')",
    timeout = 30000)
  stale_export <- suppressWarnings(try(app$get_download("synergy-csv",
    filename = tempfile(fileext = ".csv")), silent = TRUE))
  stopifnot(inherits(stale_export, "try-error"))
  app$click("synergy-calculate", timeout_ = 60000)
  app$wait_for_js("document.querySelector('#synergy-provenance')?.textContent === 'Datos editados o importados.'",
    timeout = 30000)
  file <- "test-artifacts/Loewe-recalculated.csv"
  app$get_download("synergy-csv", filename = file)
  changed <- read.csv(file)
  stopifnot(all(startsWith(changed$experiment_id, "EDIT")), nrow(changed) < nrow(main),
    nrow(changed) == 90L, sum(changed$Loewe_status == "invalid_response") == 2L,
    sum(changed$Loewe_status == "outside_common_observed_domain") == 40L,
    sum(is.finite(changed$Loewe_excess)) == 48L, sum(changed$agreement_common) == 48L)
  app$set_inputs(module="mtt",wait_=FALSE)
  app$wait_for_js("Boolean(document.querySelector('#plot img')?.naturalWidth)",timeout=60000)
  cat("Integrated Synergy classification, editable source and MTT navigation: PASS\n")
},finally=app$stop())

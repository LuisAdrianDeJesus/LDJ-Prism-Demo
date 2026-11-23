#!/usr/bin/env Rscript
# LDJ Prism — MTT Figure 4x2, refactor of mtt_visualization_v4.R
# This module IS NOT a new statistical analysis. It visualizes audited inputs.
# Importing/source() has no side effects: no global library paths, no file reads,
# no ggplot exports, no model refits. Suitable for Shiny reactive() calls.
# Source reference: mtt_visualization_v4.R (user-supplied).
# IMPORTANT: LET input column is conc_uM in the source; do NOT relabel as nM
# until its original units and any necessary conversion have been verified.

ldj_mtt_required_packages <- function() {
  packages <- c("ggplot2", "ggrepel", "patchwork", "svglite", "drc")
  absent <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Instale los paquetes requeridos: ", paste(absent, collapse = ", "))
  invisible(TRUE)
}

ldj_mtt_defaults <- function() {
  list(
    order = c("DTG", "Letermovir", "CDQ", "Lopinavir"),
    short = c(DTG = "DTG", Letermovir = "LET", CDQ = "CDQ", Lopinavir = "Lopinavir"),
    palette = c(Untreated = "#9DA4AD", DTG = "#307CA6",
                Letermovir = "#CA8939", CDQ = "#9266AD",
                Lopinavir = "#17857E", Text = "#27262C", Accent = "#501C46"),
    control_label = "No tratado",
    shared_control_label = "No tratado\n(control compartido)",
    concentration_unit = "µM",     # Unit used by original v4 input `conc_uM`.
    font_family = "sans",           # Change to "Arial" if installed on the server.
    font_size = 13,
    plot_width = 15,
    plot_height = 22,
    dpi = 600,
    y_limits = c(0, 120),
    y_breaks = seq(0, 120, by = 30),
    show_points = TRUE,
    point_shape = 21,
    point_size = 2.15,
    bar_width = 0.72,
    summary_measure = "SD",         # Explicit; SD is the v4 source standard.
    label_significance = TRUE,
    annotate_cc50 = TRUE,
    curve_band = TRUE,              # Local observed mean +/- SD, NOT a CI.
    plot_caption = paste(
      "Bars: mean ± SD and individual observations; stars: Dunnett-adjusted comparisons.",
      "LET and Lopinavir share six physical PI-assigned controls; correspondence by run remains pending.",
      "Dose-response ribbons represent local observed mean ± SD, not confidence intervals.",
      "Lopinavir CC50 uses the documented constrained 3PL model (lower asymptote 0)."
    ),
    panels = list()                  # A..H overrides (e.g. title, x_label, y_label,
                                    # y_limits, y_breaks, x_limits, color, point_size).
  )
}

ldj_mtt_style <- function(config = list()) {
  if (!is.list(config)) stop("config debe ser una lista.")
  utils::modifyList(ldj_mtt_defaults(), config)
}

ldj_mtt_panel <- function(config, panel_tag) {
  p <- config$panels[[panel_tag]]
  if (is.null(p)) p <- list()
  utils::modifyList(config, p)
}

ldj_mtt_read_csv <- function(path) {
  if (!file.exists(path)) stop("Falta archivo de entrada: ", path)
  utils::read.csv(path, na.strings = c("", "NA"), check.names = FALSE,
                  stringsAsFactors = FALSE)
}

ldj_mtt_read_bundle <- function(root = ".") {
  # Explicit, immutable upstream inputs. Work should preserve their hashes.
  # There is intentionally no fallback to fabricated curves or p values.
  get <- function(path) ldj_mtt_read_csv(file.path(root, path))
  model_file <- file.path(root, "results", "mtt", "pi_assigned_shared_controls_v4",
                          "nonlinear_models_v4.rds")
  if (!file.exists(model_file)) stop("Falta modelo RDS auditado: ", model_file)
  out <- list(
    orig = get("data/mtt_viability_tidy.csv"),
    controls = get("data/mtt_pi_assigned_shared_controls_registry.csv"),
    cc50 = get("results/mtt/pi_assigned_shared_controls_v4/cc50_consolidated_v4.csv"),
    dunnett_shared = get("results/mtt/pi_assigned_shared_controls_v4/let_lopinavir_dunnett.csv"),
    dunnett_original = get("results/mtt/dunnett_cdq_dtg.csv"),
    cdq_dtg_4pl = get("results/mtt/mtt_4pl_cc50.csv"),
    models = readRDS(model_file)
  )
  ldj_mtt_validate_bundle(out)
  out
}

ldj_mtt_columns <- function(df, names_expected, table_name) {
  missing <- setdiff(names_expected, names(df))
  if (length(missing)) stop(table_name, ": faltan columnas ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

ldj_mtt_validate_bundle <- function(bundle) {
  if (!is.list(bundle)) stop("bundle debe ser una lista de entradas auditadas.")
  necessary <- c("orig", "controls", "cc50", "dunnett_shared", "dunnett_original",
                 "cdq_dtg_4pl", "models")
  if (!all(necessary %in% names(bundle)))
    stop("Faltan componentes del bundle: ", paste(setdiff(necessary, names(bundle)), collapse = ", "))
  ldj_mtt_columns(bundle$orig, c("drug", "conc_uM", "replicate", "viability"), "orig")
  ldj_mtt_columns(bundle$controls,
    c("record_id", "replicate", "viability", "is_synthetic_reference"), "controls")
  ldj_mtt_columns(bundle$cc50,
    c("drug", "cc50_uM", "ci95_low_uM", "ci95_high_uM"), "cc50")
  ldj_mtt_columns(bundle$dunnett_shared,
    c("drug", "contrast", "p_adjusted_dunnett"), "dunnett_shared")
  ldj_mtt_columns(bundle$dunnett_original,
    c("drug", "comparison", "p_value_dunnett_adjusted"), "dunnett_original")
  ldj_mtt_columns(bundle$cdq_dtg_4pl,
    c("drug", "upper_asymptote", "lower_asymptote", "slope", "raw_absolute_cc50_uM"),
    "cdq_dtg_4pl")
  if (nrow(bundle$controls) != 6L || anyDuplicated(bundle$controls$record_id))
    stop("La referencia original requiere exactamente seis controles físicos únicos.")
  synthetic <- tolower(as.character(bundle$controls$is_synthetic_reference))
  if (any(is.na(synthetic) | synthetic %in% c("true", "t", "1")))
    stop("Los seis controles originales no pueden ser sintéticos o de estatus desconocido.")
  if (sum(is.na(bundle$orig$viability)) != 1L)
    warning("La fuente actual no tiene exactamente un valor de viabilidad NA; revise la versión de datos.", call. = FALSE)
  drugs <- ldj_mtt_defaults()$order
  if (!all(drugs %in% bundle$orig$drug) || !all(drugs %in% bundle$cc50$drug))
    stop("Los datos o parámetros no contienen los cuatro compuestos originales.")
  if (!all(c("let_4pl", "lop_3pl") %in% names(bundle$models)))
    stop("Faltan los modelos ajustados originales let_4pl / lop_3pl.")
  invisible(TRUE)
}

ldj_mtt_theme <- function(cfg) {
  pal <- cfg$palette
  ggplot2::theme_classic(base_size = cfg$font_size, base_family = cfg$font_family) +
    ggplot2::theme(
      text = ggplot2::element_text(color = unname(pal[["Text"]])),
      plot.title = ggplot2::element_text(size = cfg$font_size + 5,
        face = "bold", hjust = 0, margin = ggplot2::margin(b = 8)),
      axis.title = ggplot2::element_text(size = cfg$font_size + 1),
      axis.text = ggplot2::element_text(size = cfg$font_size - 1),
      axis.line = ggplot2::element_line(color = unname(pal[["Text"]]), linewidth = .45),
      axis.ticks = ggplot2::element_line(color = unname(pal[["Text"]]), linewidth = .45),
      plot.margin = ggplot2::margin(8, 11, 8, 8),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
}

ldj_mtt_summary <- function(d, measure = "SD") {
  if (!measure %in% c("SD", "SEM", "IC95")) stop("Medida de dispersión no admitida.")
  by_dose <- split(d$viability, as.character(d$conc_uM))
  out <- lapply(names(by_dose), function(k) {
    y <- by_dose[[k]]; y <- y[is.finite(y)]
    n <- length(y)
    sdv <- if (n > 1L) stats::sd(y) else NA_real_
    spread <- switch(measure,
      SD = sdv,
      SEM = sdv / sqrt(n),
      IC95 = if (n > 1L) stats::qt(.975, df = n - 1) * sdv / sqrt(n) else NA_real_)
    data.frame(conc_uM = as.numeric(k), mean = if(n) mean(y) else NA_real_,
               sd = sdv, n = n, spread = spread)
  })
  do.call(rbind, out)
}

ldj_mtt_bar_data <- function(bundle, drug) {
  d <- bundle$orig
  x <- d[!is.na(d$drug) & d$drug == drug & is.finite(d$viability), , drop = FALSE]
  if (drug %in% c("Letermovir", "Lopinavir")) {
    c0 <- bundle$controls
    controls <- data.frame(drug = drug, conc_uM = 0,
       replicate = c0$replicate, viability = c0$viability,
       is_assigned_control = TRUE, record_id = c0$record_id,
       stringsAsFactors = FALSE)
    # Source entries displayed by analytical context, not new observations.
    x <- data.frame(drug = x$drug, conc_uM = x$conc_uM,
       replicate = x$replicate, viability = x$viability,
       is_assigned_control = FALSE, record_id = NA_character_,
       stringsAsFactors = FALSE)
    x <- rbind(x, controls)
  } else {
    x <- data.frame(drug = x$drug, conc_uM = x$conc_uM,
       replicate = x$replicate, viability = x$viability,
       is_assigned_control = x$conc_uM == 0, record_id = NA_character_,
       stringsAsFactors = FALSE)
  }
  x
}

ldj_mtt_stars <- function(p) {
  ifelse(is.na(p), "ND", ifelse(p < 1e-4, "****",
    ifelse(p < .001, "***", ifelse(p < .01, "**",
      ifelse(p < .05, "*", "ns")))))
}

ldj_mtt_significance <- function(bundle, drug) {
  x <- if (drug %in% c("Letermovir", "Lopinavir")) {
    d <- bundle$dunnett_shared
    d <- d[d$drug == drug, , drop = FALSE]
    data.frame(dose = sub(" - 0$", "", d$contrast), p = d$p_adjusted_dunnett)
  } else {
    d <- bundle$dunnett_original
    d <- d[d$drug == drug, , drop = FALSE]
    data.frame(dose = sub(" - 0$", "", d$comparison), p = d$p_value_dunnett_adjusted)
  }
  if (!nrow(x)) warning("No hay comparaciones Dunnett para ", drug, call. = FALSE)
  x
}

ldj_mtt_make_bar <- function(bundle, drug, tag, config = ldj_mtt_defaults()) {
  ldj_mtt_required_packages()
  cfg <- ldj_mtt_panel(config, tag)
  pal <- cfg$palette; clr <- if (!is.null(cfg$color)) cfg$color else pal[[drug]]
  if (is.null(clr) || length(clr) != 1L) stop("Falta color para ", drug)
  x <- ldj_mtt_bar_data(bundle, drug)
  doses <- sort(unique(x$conc_uM))
  x$dose <- factor(as.character(x$conc_uM), levels = as.character(doses))
  sums <- ldj_mtt_summary(x, cfg$summary_measure)
  sums$dose <- factor(as.character(sums$conc_uM), levels = as.character(doses))
  sums$fill <- ifelse(sums$conc_uM == 0, unname(pal[["Untreated"]]), clr)
  labels <- stats::setNames(format(doses, scientific = FALSE, trim = TRUE), as.character(doses))
  if ("0" %in% names(labels)) {
    labels["0"] <- if (drug %in% c("Letermovir", "Lopinavir"))
      cfg$shared_control_label else cfg$control_label
  }
  p <- ggplot2::ggplot(sums, ggplot2::aes(x = dose, y = mean)) +
    ggplot2::geom_col(ggplot2::aes(fill = fill), width = cfg$bar_width,
      color = unname(pal[["Text"]]), linewidth = .3) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(0, mean - spread),
      ymax = mean + spread), width = .16, linewidth = .45) +
    ggplot2::scale_x_discrete(labels = labels) +
    ggplot2::scale_y_continuous(breaks = cfg$y_breaks,
      expand = ggplot2::expansion(mult = c(0,.025))) +
    # The shared-control label is intentionally multiline.  Extra bottom
    # margin and unclipped drawing prevent it from being silently truncated
    # in the 4x2 editorial layout while preserving the category order.
    ggplot2::coord_cartesian(ylim = cfg$y_limits, clip = "off") +
    ggplot2::labs(
      title = if (!is.null(cfg$title)) cfg$title else paste0(tag, ". ",
        cfg$short[[drug]], " cell viability"),
      x = if (!is.null(cfg$x_label)) cfg$x_label else paste0("Concentration (", cfg$concentration_unit, ")"),
      y = if (!is.null(cfg$y_label)) cfg$y_label else "Cell viability (%)") +
    ldj_mtt_theme(cfg) +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 11, 24, 32))
  if (isTRUE(cfg$show_points)) {
    p <- p + ggplot2::geom_point(data = x,
      ggplot2::aes(x = dose, y = viability), inherit.aes = FALSE,
      shape = cfg$point_shape, size = cfg$point_size,
      stroke = .35, color = unname(pal[["Text"]]), fill = "white",
      position = ggplot2::position_jitter(width = .085, height = 0, seed = 20261015))
  }
  if (isTRUE(cfg$label_significance)) {
    sig <- ldj_mtt_significance(bundle, drug)
    if (nrow(sig)) {
      sig$dose <- factor(sig$dose, levels = as.character(doses))
      sig <- sig[!is.na(sig$dose), , drop = FALSE]
      if (nrow(sig)) {
        sig$y <- cfg$y_limits[2] - 5
        sig$label <- ldj_mtt_stars(sig$p)
        p <- p + ggplot2::geom_text(data = sig,
          ggplot2::aes(x = dose, y = y, label = label),
          inherit.aes = FALSE, color = unname(pal[["Accent"]]),
          size = 4.2, fontface = "bold")
      }
    }
  }
  p
}

ldj_mtt_curve_grid <- function(bundle, drug, n = 300L) {
  x <- bundle$orig
  x <- x[x$drug == drug & is.finite(x$viability) & is.finite(x$conc_uM) & x$conc_uM > 0,
         , drop = FALSE]
  if (nrow(x) < 2) stop("No hay suficientes dosis positivas para ", drug)
  g <- exp(seq(log(min(x$conc_uM)), log(max(x$conc_uM)), length.out = n))
  model <- if (drug == "Letermovir") bundle$models$let_4pl else
    if (drug == "Lopinavir") bundle$models$lop_3pl else NULL
  if (!is.null(model)) {
    y <- as.numeric(stats::predict(model, newdata = data.frame(conc_uM = g)))
  } else {
    r <- bundle$cdq_dtg_4pl
    r <- r[r$drug == drug, , drop = FALSE]
    if (nrow(r) != 1L) stop("Parámetros ajustados ambiguos para ", drug)
    ratio <- ((r$upper_asymptote - r$lower_asymptote) /
       (50 - r$lower_asymptote) - 1)
    if (!is.finite(ratio) || ratio <= 0 || !is.finite(r$slope) || r$slope == 0)
      stop("Parámetros 4PL inválidos para ", drug)
    e <- r$raw_absolute_cc50_uM / ratio^(1 / r$slope)
    y <- r$lower_asymptote +
       (r$upper_asymptote - r$lower_asymptote) / (1 + (g/e)^r$slope)
  }
  if (any(!is.finite(y))) stop("Curva ajustada con predicciones inválidas: ", drug)
  data.frame(conc_uM = g, fitted = y)
}

# Size dose-local rectangles in the coordinates of the displayed axis.
ldj_dose_band_bounds <- function(dose, width = .65, log = TRUE) {
  ok <- is.finite(dose) & (!log | dose > 0)
  coord <- if (log) log10(dose[ok]) else dose[ok]
  centers <- sort(unique(coord))
  gaps <- diff(centers)
  nearest <- if (length(gaps)) pmin(c(Inf, gaps), c(gaps, Inf)) else
    if (log) .25 else pmax(abs(centers) * .25, .Machine$double.eps)
  half <- nearest[match(coord, centers)] * min(.95, max(.05, width)) / 2
  bounds <- data.frame(xmin = rep(NA_real_, length(dose)), xmax = rep(NA_real_, length(dose)))
  invert <- if (log) function(x) 10^x else identity
  bounds$xmin[ok] <- invert(coord - half)
  bounds$xmax[ok] <- invert(coord + half)
  bounds
}

ldj_dose_dispersion <- function(dose, response, measure, width = .65, log = TRUE) {
  valid <- is.finite(dose) & is.finite(response) & (!log | dose > 0)
  groups <- split(which(valid), dose[valid])
  sums <- data.frame(dose = numeric(), mean = numeric(), ymin = numeric(), ymax = numeric())
  if (length(groups)) sums <- do.call(rbind, lapply(groups, function(ix) {
    v <- dispersion_summary(response[ix], measure)
    data.frame(dose = dose[ix[1]], mean = v$y, ymin = v$ymin, ymax = v$ymax)
  }))
  cbind(sums, ldj_dose_band_bounds(sums$dose, width, log))
}

ldj_estimable_dispersion <- function(x) {
  x[is.finite(x$ymin) & is.finite(x$ymax) & x$ymax > x$ymin, , drop = FALSE]
}

ldj_mtt_make_curve <- function(bundle, drug, tag, config = ldj_mtt_defaults()) {
  ldj_mtt_required_packages()
  cfg <- ldj_mtt_panel(config, tag)
  pal <- cfg$palette; clr <- if (!is.null(cfg$color)) cfg$color else pal[[drug]]
  x <- bundle$orig
  x <- x[x$drug == drug & is.finite(x$conc_uM) & x$conc_uM > 0 &
         is.finite(x$viability), , drop = FALSE]
  sums <- ldj_mtt_summary(x, cfg$summary_measure)
  sums <- cbind(sums, ldj_dose_band_bounds(sums$conc_uM, if (is.null(cfg$band_width)) .65 else cfg$band_width, !identical(cfg$x_scale, "linear")))
  sums$ymin <- sums$mean - sums$spread
  sums$ymax <- sums$mean + sums$spread
  cv <- ldj_mtt_curve_grid(bundle, drug)
  z <- bundle$cc50
  z <- z[z$drug == drug, , drop = FALSE]
  if (nrow(z) != 1L) stop("CC50 ambiguo o ausente para ", drug)
  # Draw dose-local SD shading before lines/points, as in the original v4.
  p <- ggplot2::ggplot()
  if (isTRUE(cfg$curve_band)) {
    p <- p + ggplot2::geom_rect(data = ldj_estimable_dispersion(sums),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = clr, alpha = .24, color = NA, inherit.aes = FALSE)
  }
  p <- p +
    ggplot2::geom_line(data = cv, ggplot2::aes(x = conc_uM, y = fitted),
      color = clr, linewidth = 1.05) +
    ggplot2::geom_errorbar(data = sums,
      ggplot2::aes(x = conc_uM, ymin = ymin, ymax = ymax),
      color = clr, width = .045, linewidth = .5) +
    ggplot2::geom_point(data = sums, ggplot2::aes(x = conc_uM, y = mean),
      color = clr, size = 2.8) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
      color = unname(pal[["Text"]]), linewidth = .45) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_continuous(breaks = cfg$y_breaks,
      expand = ggplot2::expansion(mult = c(0,.025))) +
    ggplot2::coord_cartesian(ylim = cfg$y_limits, xlim = cfg$x_limits) +
    ggplot2::labs(
      title = if (!is.null(cfg$title)) cfg$title else paste0(tag, ". ",
        cfg$short[[drug]], " dose-response"),
      x = if (!is.null(cfg$x_label)) cfg$x_label else paste0("Concentration (", cfg$concentration_unit, ", log scale)"),
      y = if (!is.null(cfg$y_label)) cfg$y_label else "Cell viability (%)") +
    ldj_mtt_theme(cfg)
  if (isTRUE(cfg$annotate_cc50)) {
    v <- as.numeric(z$cc50_uM)
    low <- as.numeric(z$ci95_low_uM)
    high <- as.numeric(z$ci95_high_uM)
    if (length(v) == 1L && all(is.finite(c(v, low, high))) &&
        v > 0 && low > 0 && high > low) {
      desc <- if (drug == "Lopinavir") " (constrained 3PL)" else ""
      lab <- paste0("CC50 = ", formatC(v, format = "f", digits = 2), " ",
        cfg$concentration_unit, desc, "\n95% CI: ",
        formatC(low, format = "f", digits = 2), "–",
        formatC(high, format = "f", digits = 2), " ", cfg$concentration_unit)
      label <- data.frame(x = v, y = 50, label = lab)
      p <- p + ggplot2::geom_segment(data = label,
        ggplot2::aes(x = x, xend = x, y = 0, yend = 50),
        inherit.aes = FALSE, color = clr, linetype = "dashed", linewidth = .65) +
        ggplot2::geom_point(data = label, ggplot2::aes(x = x, y = y),
          inherit.aes = FALSE, color = clr, size = 3) +
        ggrepel::geom_label_repel(data = label,
          ggplot2::aes(x = x, y = y, label = label),
          inherit.aes = FALSE, seed = 20261015, color = clr,
          fill = "white", size = 3.4, label.size = .18,
          min.segment.length = 0, nudge_y = 55)
    } else {
      warning("CC50 o IC95 no representable para ", drug, "; se omite la anotación.", call. = FALSE)
    }
  }
  p
}

ldj_mtt_build <- function(bundle, config = list()) {
  ldj_mtt_validate_bundle(bundle)
  ldj_mtt_required_packages()
  cfg <- ldj_mtt_style(config)
  if (!identical(cfg$concentration_unit, "µM")) {
    stop("La entrada original conc_uM está en µM: convierta las concentraciones " ,
         "y todos los parámetros e IC antes de cambiar la unidad del eje.")
  }
  if (!identical(cfg$summary_measure, "SD")) {
    warning("La referencia v4 usó SD. Se mostrará ", cfg$summary_measure,
            " únicamente como modificación visual explícita.", call. = FALSE)
  }
  if (!setequal(cfg$order, ldj_mtt_defaults()$order) || length(cfg$order) != 4L)
    stop("La figura de referencia requiere cuatro compuestos distintos.")
  plots <- list(); layout <- list()
  for (i in seq_along(cfg$order)) {
    drug <- cfg$order[[i]]
    b_tag <- LETTERS[2L*i-1L]; c_tag <- LETTERS[2L*i]
    plots[[b_tag]] <- ldj_mtt_make_bar(bundle, drug, b_tag, cfg)
    plots[[c_tag]] <- ldj_mtt_make_curve(bundle, drug, c_tag, cfg)
    layout[[i]] <- plots[[b_tag]] + plots[[c_tag]]
  }
  figure <- patchwork::wrap_plots(layout, ncol = 1) +
    patchwork::plot_annotation(caption = cfg$plot_caption) &
    ggplot2::theme(plot.caption = ggplot2::element_text(
      size = 10, hjust = 0, margin = ggplot2::margin(t = 12)))
  # visual_context_count != physical_controls_n; do NOT treat display duplication
  # as additional independent control observations.
  list(figure = figure, panels = plots, config = cfg,
       audit = list(physical_shared_controls_n = nrow(bundle$controls),
                    shared_contexts = c("Letermovir", "Lopinavir"),
                    inferential_correspondence = "pending run-level confirmation"))
}

ldj_mtt_export <- function(result, filename,
                           width = result$config$plot_width,
                           height = result$config$plot_height,
                           dpi = result$config$dpi) {
  if (!is.list(result) || is.null(result$figure)) stop("Resultado LDJ Prism inválido.")
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("png", "svg", "pdf")) stop("Formato admitido: png, svg o pdf.")
  dev <- switch(ext, png = "png", svg = svglite::svglite, pdf = "pdf")
  ggplot2::ggsave(filename = filename, plot = result$figure, device = dev,
    width = width, height = height, units = "in", dpi = dpi,
    bg = "white", limitsize = FALSE)
  invisible(normalizePath(filename, mustWork = TRUE))
}

# Standalone CLI wrapper, opt-in only; Shiny should call ldj_mtt_build() directly:
#   source("R/ldj_mtt_figures.R", local=TRUE)
#   bundle <- ldj_mtt_read_bundle("/private/project/Alan")
#   result <- ldj_mtt_build(bundle, list(font_family="sans",
#     panels=list(A=list(title="A. Viabilidad DTG",y_limits=c(0,130)),
#                 B=list(title="B. Curva DTG"))))
#   output$figure <- shiny::renderPlot(result$figure)
#   ldj_mtt_export(result, "MTT_4x2.svg")
# No data files or RDS models should be uploaded to the public demo.


# Pareto dominance is evaluated only among assay-matched dose pairs in one
# experimental context. Ties remain on the frontier.
ldj_synergy_pareto <- function(data) {
  keys <- c("compound_A", "compound_B", "cell_line", "exposure_hours", "experiment_id")
  stopifnot(all(c(keys, "activity_pct", "viability_pct", "dose_A_M", "dose_B_M") %in% names(data)))
  frontier <- rep(NA, nrow(data))
  eligible <- which(with(data, is.finite(activity_pct) & activity_pct >= 0 & activity_pct <= 100 &
    is.finite(viability_pct) & viability_pct >= 0 & viability_pct <= 100 & dose_A_M > 0 & dose_B_M > 0))
  for (group in split(eligible, interaction(data[eligible, keys], drop = TRUE))) {
    a <- data$activity_pct[group]
    v <- data$viability_pct[group]
    frontier[group] <- !vapply(seq_along(group), function(i)
      any(a >= a[i] & v >= v[i] & (a > a[i] | v > v[i])), logical(1))
  }
  frontier
}

ldj_synergy_gallery <- function() {
  list(
    mono = c("Monoterapias", "Monotherapies", "Respuesta observada por dosis", "Controles monofármaco"),
    conditional_a = c("Curvas A a B fija", "A curves at fixed B", "Perfiles observados estratificados por dosis de B", "Dosis de A y B"),
    conditional_b = c("Curvas B a A fija", "B curves at fixed A", "Perfiles observados estratificados por dosis de A", "Dosis de A y B"),
    observed = c("Inhibición observada", "Observed inhibition", "Valores observados por celda", "Inhibición"),
    interaction = c("Mapa de interacción", "Interaction heatmap", "Exceso descriptivo centrado en cero", "Exceso HSA o Bliss"),
    bubble = c("Burbujas de interacción", "Interaction bubbles", "Área proporcional a la magnitud del exceso", "Exceso HSA o Bliss"),
    dumbbell = c("Combinación frente a monoterapia", "Combination vs monotherapy", "Comparación pareada descriptiva frente a A, B o la mejor monoterapia", "Pares evaluables"),
    forest = c("Incertidumbre experimental", "Experimental uncertainty", "Contrastes e incertidumbre entre experimentos", "Declaración de independencia"),
    concordance = c("Concordancia HSA–Bliss", "HSA–Bliss agreement", "Excesos sobre las mismas celdas evaluables", "Dominio común"),
    benefit = c("Eficacia y viabilidad", "Efficacy and viability", "Pares compatibles de ambos ensayos", "Correspondencia experimental"),
    pareto = c("Frontera de Pareto", "Pareto frontier", "Pares no dominados en inhibición y viabilidad", "Correspondencia experimental"),
    observed_expected = c("Observado frente a esperado", "Observed vs expected", "Diagnóstico de referencia descriptiva", "Respuesta esperada HSA o Bliss"),
    bland_altman = c("Bland–Altman descriptivo", "Descriptive Bland–Altman", "Diferencia Bliss menos HSA frente a su media", "Dominio común"),
    upset = c("Intersecciones por modelo", "Model intersections", "Pares que exceden el umbral en HSA y Bliss", "Dominio común"),
    raincloud = c("Distribución con observaciones", "Raincloud", "Densidad, observaciones y mediana por experimento", "Exceso HSA o Bliss"),
    marginal = c("Mapa con monoterapias", "Marginal heatmap", "Inhibición observada y perfiles monofármaco", "Dosis físicas y monoterapias"),
    surface = c("Superficie de interacción", "Interaction surface", "Malla 3D estática sobre celdas observadas", "Rectángulos completos"),
    isobole = c("Isobola aditiva experimental", "Experimental additive isobole", "Equivalencia de dosis para un efecto fijo", "Curvas identificables y dominio común"),
    contour = c("Contornos de interacción", "Interaction contours", "Interpolación local entre vértices evaluables", "Matriz de exceso"),
    ridgeline = c("Densidades por experimento", "Experiment ridgelines", "Distribuciones descriptivas escaladas por grupo", "Exceso HSA o Bliss"),
    distribution = c("Distribución de excesos", "Distribution of excesses", "Distribución descriptiva por experimento", "Exceso HSA o Bliss")
  )
}

# Each ID occupies a nonempty rectangle. Dots represent empty slots.
ldj_synergy_gallery_design <- function(text, size) {
  rows <- strsplit(trimws(text), "\\s*[/\\n]\\s*", perl = TRUE)[[1L]]
  stopifnot(length(rows) %in% 1:12)
  cells <- strsplit(toupper(gsub("[[:blank:]]", "", rows)), "", fixed = TRUE)
  stopifnot(length(unique(lengths(cells))) == 1L, lengths(cells)[1L] %in% 1:12)
  grid <- do.call(rbind, cells)
  ids <- LETTERS[seq_len(size)]
  stopifnot(all(grid %in% c(ids, ".")), all(ids %in% grid))
  for (id in ids) {
    positions <- which(grid == id, arr.ind = TRUE)
    stopifnot(all(grid[seq(min(positions[, 1]), max(positions[, 1])),
      seq(min(positions[, 2]), max(positions[, 2])), drop = FALSE] == id))
  }
  paste(apply(grid, 1L, function(row) paste(ifelse(row == ".", "#", row), collapse = "")), collapse = "\n")
}

ldj_synergy_gallery_sequence <- function(text, selection) {
  ids <- if (nzchar(trimws(text))) trimws(strsplit(text, ",", fixed = TRUE)[[1L]]) else selection
  stopifnot(length(ids) <= 12L)
  parts <- strsplit(ids, "@", fixed = TRUE)
  types <- vapply(parts, function(x) x[1L], "")
  stopifnot(all(types %in% names(ldj_synergy_gallery())), all(lengths(parts) <= 2L))
  occurrences <- ave(seq_along(types), types, FUN = seq_along)
  keys <- vapply(seq_along(parts), function(i) {
    if (length(parts[[i]]) == 2L) parts[[i]][2L] else paste0(types[i], "-", occurrences[i])
  }, "")
  stopifnot(all(grepl("^[A-Za-z0-9_-]{1,40}$", keys)), !anyDuplicated(keys))
  stats::setNames(types, keys)
}

ldj_synergy_gallery_weights <- function(text, size) {
  if (!nzchar(trimws(text))) return(NULL)
  values <- suppressWarnings(as.numeric(strsplit(text, ",", fixed = TRUE)[[1L]]))
  stopifnot(length(values) == size, all(is.finite(values) & values > 0))
  values
}

ldj_synergy_density <- function(score, group, adjust = 1, height = .72) {
  groups <- split(score[is.finite(score)], group[is.finite(score)])
  levels <- names(groups)
  curves <- lapply(seq_along(groups), function(i) {
    values <- groups[[i]]
    if (length(values) < 5L || length(unique(values)) < 3L) return(NULL)
    estimate <- stats::density(values, adjust = adjust, from = min(values), to = max(values), n = 128)
    data.frame(group = levels[i], index = i, x = estimate$x,
      offset = height * estimate$y / max(estimate$y))
  })
  list(levels = levels, curves = do.call(rbind, curves))
}

# An effect-specific Loewe equivalence line. Different effects generally
# have different intercept ratios when Hill slopes or maxima differ.
ldj_synergy_isobole <- function(fit_a, fit_b, effect) {
  if (is.null(fit_a) || is.null(fit_b)) return(NULL)
  stopifnot(is.numeric(effect), length(effect) == 1L, is.finite(effect))
  inverse <- function(fit) {
    fraction <- (effect - fit$bottom) / (fit$top - fit$bottom)
    if (effect < fit$bounds[1L] || effect > fit$bounds[2L] ||
        !is.finite(fraction) || fraction <= 0 || fraction >= 1) return(NA_real_)
    fit$ec50 * exp(stats::qlogis(fraction) / fit$hill)
  }
  a <- inverse(fit_a)
  b <- inverse(fit_b)
  if (!all(is.finite(c(a, b))) || min(a, b) <= 0) return(NULL)
  data.frame(dose_A_M = c(0, a), dose_B_M = c(b, 0))
}

# Orthographic projection of complete grid faces. Missing vertices produce
# holes. The plotted facets connect measured vertices without extrapolation.
ldj_synergy_surface <- function(x, y, z, theta = 38, phi = 28, perspective = 0, vertical = .6) {
  stopifnot(length(x) == nrow(z), length(y) == ncol(z), all(diff(x) > 0), all(diff(y) > 0))
  if (length(x) < 2L || length(y) < 2L || !any(is.finite(z))) return(NULL)
  theta <- theta * pi / 180
  phi <- phi * pi / 180
  values <- expand.grid(x = x, y = y)
  values$z <- as.vector(z)
  normalize <- function(v) (v - min(v)) / diff(range(v))
  xx <- normalize(values$x) - .5
  yy <- normalize(values$y) - .5
  z_scale <- max(1, abs(values$z[is.finite(values$z)]))
  project <- function(x, y, z) {
    zz <- z / z_scale * vertical
    depth <- sin(theta) * x + cos(theta) * y
    camera <- cos(phi) * depth + sin(phi) * zz
    denom <- 1 + perspective * camera
    data.frame(u = (cos(theta) * x - sin(theta) * y) / denom,
      v = (cos(phi) * zz - sin(phi) * depth) / denom, depth = camera)
  }
  projected <- project(xx, yy, values$z)
  values$u <- projected$u
  values$v <- projected$v
  depth <- projected$depth
  index <- matrix(seq_len(nrow(values)), nrow = length(x))
  faces <- lapply(seq_len(length(x) - 1L), function(i) lapply(seq_len(length(y) - 1L), function(j) {
    k <- index[cbind(c(i, i + 1L, i + 1L, i), c(j, j, j + 1L, j + 1L))]
    if (!all(is.finite(values$z[k]))) return(NULL)
    data.frame(values[k, c("u", "v")], face = paste(i, j), fill = mean(values$z[k]), depth = mean(depth[k]))
  }))
  faces <- do.call(rbind, unlist(faces, recursive = FALSE))
  corners <- index[cbind(c(1L, length(x), length(x), 1L, 1L), c(1L, 1L, length(y), length(y), 1L))]
  z_range <- range(c(0, values$z), na.rm = TRUE)
  axis <- function(name, ticks, x, y, z) {
    cbind(project(x, y, z), axis = name, tick = ticks)
  }
  ticks <- rbind(
    axis("A", seq(min(x), max(x), length.out = 3), c(-.5, 0, .5), -.5, z_range[1L]),
    axis("B", seq(min(y), max(y), length.out = 3), -.5, c(-.5, 0, .5), z_range[1L]),
    axis("Z", seq(z_range[1L], z_range[2L], length.out = 3), -.5, -.5,
      seq(z_range[1L], z_range[2L], length.out = 3)))
  list(points = values[is.finite(values$z), , drop = FALSE], ticks = ticks,
    faces = if (is.null(faces)) NULL else faces[order(faces$depth, decreasing = TRUE), ],
    zero = project(xx[corners], yy[corners], 0))
}

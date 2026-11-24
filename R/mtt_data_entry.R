# LDJ Prism: ingestion of general statistical tables and explicit control mapping.
# Sourcing the module has no I/O or side effects.

ldj_mtt_normalize_numeric <- function(d, decimal = ".") {
  if (!decimal %in% c(".", ",")) stop("Separador decimal no admitido.")
  if (!is.data.frame(d) || !ncol(d)) stop("La tabla debe tener columnas.")
  identifiers <- c("observation_id", "independent_unit_id", "record_id", "experiment_id", "control_source",
    "control_type", "control_label", "control_set_id", "inference_eligible", "control_series",
    "concentration_unit", "Compuesto", "Series", "drug", "series")
  for (name in setdiff(names(d), identifiers)) {
    v <- d[[name]]
    if (is.factor(v)) v <- as.character(v)
    if (!is.character(v)) next
    s <- trimws(v)
    present <- !is.na(s) & nzchar(s)
    if (!any(present)) next
    s_num <- if (decimal == ",") gsub(",", ".", s, fixed = TRUE) else s
    converted <- suppressWarnings(as.numeric(s_num))
    if (all(is.finite(converted[present]))) d[[name]] <- converted
  }
  d
}

ldj_mtt_import_table <- function(path, filename, header = TRUE, decimal = ".",
                                 separator = ",", sheet = NULL) {
  if (!file.exists(path)) stop("No se encontró el archivo seleccionado.")
  if (!decimal %in% c(".", ",") || !separator %in% c(",", ";", "\t"))
    stop("Formato CSV no admitido.")
  extension <- tolower(tools::file_ext(filename))
  if (extension %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Se necesita readxl.")
    sheets <- readxl::excel_sheets(path)
    if (is.null(sheet) || !sheet %in% sheets) stop("Seleccione una hoja existente.")
    d <- as.data.frame(readxl::read_excel(path, sheet = sheet,
      col_names = isTRUE(header), col_types = "text", .name_repair = "minimal"),
      stringsAsFactors = FALSE)
  } else if (extension %in% c("csv", "tsv")) {
    sep <- if (extension == "tsv") "\t" else separator
    d <- utils::read.table(path, sep = sep, header = isTRUE(header),
      colClasses = "character", quote = "\"", comment.char = "",
      na.strings = c("", "NA"), strip.white = TRUE, check.names = FALSE,
      fill = TRUE, stringsAsFactors = FALSE)
  } else stop("Formato admitido: CSV, TSV, XLS o XLSX.")
  if (!isTRUE(header)) names(d) <- paste0("V",seq_len(ncol(d)))
  if (!ncol(d) || !nrow(d)) stop("La tabla importada no contiene observaciones.")
  if (anyNA(names(d)) || any(!nzchar(names(d))) || anyDuplicated(names(d)))
    stop("Los encabezados deben ser únicos y no estar vacíos.")
  ldj_mtt_normalize_numeric(d, decimal)
}

ldj_mtt_paste_table <- function(text, header = TRUE, decimal = ".") {
  if (length(text) != 1L || is.na(text) || !nzchar(trimws(text)))
    stop("Pegue un bloque tabulado no vacío.")
  connection <- textConnection(text)
  on.exit(close(connection), add = TRUE)
  d <- utils::read.table(connection, sep = "\t", header = isTRUE(header),
    colClasses = "character", quote = "\"", comment.char = "",
    na.strings = c("", "NA"), strip.white = TRUE, check.names = FALSE,
    fill = TRUE, stringsAsFactors = FALSE)
  if (!nrow(d) || !ncol(d) || anyDuplicated(names(d)))
    stop("No se pudo interpretar el bloque tabulado.")
  ldj_mtt_normalize_numeric(d, decimal)
}

ldj_mtt_default_control_map <- function(d, x, y, group) {
  empty <- data.frame(experiment_id = character(), observation_id = character(),
    series = character(), stringsAsFactors = FALSE)
  if (!is.data.frame(d) || !all(c(x, y, group, "experiment_id", "observation_id") %in% names(d)))
    return(empty)
  physical <- prepare_controls(d, x, y, series_column = group)
  contexts <- attr(physical, "control_contexts")
  if (is.null(contexts) || !nrow(contexts)) return(empty)
  controls <- physical[physical$.is_control, c("experiment_id", "observation_id"), drop = FALSE]
  controls$key <- paste(controls$experiment_id, controls$observation_id, sep = "::")
  idx <- match(contexts$key, controls$key)
  if (anyNA(idx)) stop("Correspondencia de control sin observación física.")
  out <- controls[idx, c("experiment_id", "observation_id"), drop = FALSE]
  out$series <- as.character(contexts$series)
  rownames(out) <- NULL
  unique(out)
}

ldj_mtt_apply_control_map <- function(physical, group, series, assignments) {
  if (is.null(assignments)) return(physical)
  needed <- c("experiment_id", "observation_id", "series")
  if (!is.data.frame(assignments) || !all(needed %in% names(assignments)))
    stop("El mapa de controles requiere experiment_id, observation_id y series.")
  if (!nrow(assignments)) return(physical) # no eligible mapping; descriptive only
  if (!all(c(group, "experiment_id", "observation_id") %in% names(physical)))
    stop("Los controles requieren identificadores físicos y experimentales.")
  a <- assignments[, needed, drop = FALSE]
  a[] <- lapply(a, as.character)
  a[] <- lapply(a, trimws)
  if (anyNA(a) || any(!nzchar(as.matrix(a))))
    stop("El mapa de controles contiene asignaciones incompletas.")
  if (any(!a$series %in% series)) stop("Asignación a una serie no seleccionada.")
  keys <- paste(a$experiment_id, a$observation_id, sep = "::")
  control <- physical[physical$.is_control, , drop = FALSE]
  qualified <- !is.na(control$experiment_id) & nzchar(as.character(control$experiment_id)) &
    !is.na(control$observation_id) & nzchar(as.character(control$observation_id))
  valid_keys <- paste(control$experiment_id[qualified], control$observation_id[qualified], sep = "::")
  if (any(!keys %in% valid_keys)) stop("El mapa hace referencia a controles físicos inexistentes.")
  if (anyDuplicated(paste(keys, a$series, sep = "::")))
    stop("Un control físico no puede asignarse dos veces al mismo contexto.")
  if (length(setdiff(valid_keys, keys)))
    stop("Cada control físico debe conservar al menos un contexto visual.")
  treated <- physical[!physical$.is_control, , drop = FALSE]
  for (i in seq_len(nrow(a))) {
    if (!any(as.character(treated[[group]]) == a$series[i] &
             as.character(treated$experiment_id) == a$experiment_id[i], na.rm = TRUE))
      stop("El control y los tratamientos asignados deben pertenecer al mismo experimento: ",
           a$experiment_id[i], " / ", a$series[i])
  }
  attr(physical, "control_contexts") <- unique(data.frame(key = keys,
    series = a$series, stringsAsFactors = FALSE))
  physical
}

# An explicit control_set_id groups physical units. A display label never
# creates a group, and a set cannot cross experiment, type or provenance.
ldj_control_reference_keys <- function(d) {
  keys <- rep(NA_character_,nrow(d))
  required <- c("experiment_id","observation_id","control_type","control_source")
  if(!all(required %in% names(d))) return(keys)
  ok <- Reduce("&",lapply(d[required],function(x) !is.na(x)&nzchar(trimws(as.character(x))))) &
    d$control_type!="treatment"
  if(!any(ok)) return(keys)
  encode <- function(x) paste0(nchar(x,type="bytes"),":",x)
  physical <- encode(as.character(d$observation_id[ok]))
  if("control_set_id" %in% names(d)) {
    set <- trimws(as.character(d$control_set_id[ok]))
    use <- !is.na(set)&nzchar(set)
    physical[use] <- paste0("set:",encode(set[use]))
  }
  keys[ok] <- paste0("physical:",encode(as.character(d$experiment_id[ok])),
    encode(as.character(d$control_type[ok])),encode(as.character(d$control_source[ok])),physical)
  keys
}

ldj_control_reference_choices <- function(d) {
  keys <- ldj_control_reference_keys(d)
  groups <- split(which(!is.na(keys)),keys[!is.na(keys)])
  labels <- vapply(groups,function(i) {
    z <- d[i,,drop=FALSE]
    units <- as.character(z$independent_unit_id)
    reported <- unique(units[!is.na(units)&nzchar(units)])
    flag <- !is.na(z$inference_eligible) & tolower(as.character(z$inference_eligible)) %in% c("true","1")
    label <- if("control_label" %in% names(z)) unique(na.omit(z$control_label)) else character()
    label <- label[nzchar(label)]
    id <- if("control_set_id" %in% names(z) && !is.na(z$control_set_id[1]) &&
      nzchar(z$control_set_id[1])) z$control_set_id[1] else z$observation_id[1]
    paste(paste(label,collapse=" / "),z$control_type[1],z$experiment_id[1],z$control_source[1],id,
      paste(length(reported),"unidades registradas"),
      if(length(flag) && all(flag) && length(reported)>=2L) "elegibilidad declarada" else "revisar elegibilidad",
      sep=" · ")
  },character(1))
  setNames(as.character(names(groups)),unname(labels))
}

ldj_select_control_reference <- function(d,reference) {
  if(!startsWith(reference,"physical:")) return(list(data=d,type=reference))
  keys <- ldj_control_reference_keys(d)
  matching <- d$.is_control & !is.na(keys) & keys==reference
  if(!any(matching)) stop("El control físico seleccionado no está asignado a esta serie.")
  type <- unique(d$.control_type[matching])
  list(data=d[!d$.is_control | matching,,drop=FALSE],type=type)
}

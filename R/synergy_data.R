# Input values are explicitly normalized percent inhibition. Raw signals and
# viability require separate preprocessing and cannot enter this estimator.
ldj_synergy_prepare <- function(data) {
  required <- c("compound_A","compound_B","dose_A","dose_B","unit_A","unit_B",
    "readout_value","readout_type","experiment_id","biological_replicate_id","observation_id")
  if(!is.data.frame(data) || !nrow(data) || !all(required %in% names(data)))
    stop("La tabla larga no cumple el contrato de combinaciones.")
  for(name in setdiff(required,c("dose_A","dose_B","readout_value"))) {
    z <- trimws(as.character(data[[name]]))
    if(anyNA(z) || any(!nzchar(z))) stop("Campo de trazabilidad vacío: ",name)
  }
  if(anyDuplicated(data[c("experiment_id","observation_id")]))
    stop("Hay observaciones físicas repetidas. Resuelva la procedencia antes de agregar.")
  if(any(data$readout_type!="percent_inhibition"))
    stop("Esta versión requiere inhibición porcentual documentada.")
  if(nrow(unique(data[c("compound_A","compound_B")]))!=1L)
    stop("Seleccione un solo par de compuestos.")
  # Older tables lack these optional columns. When supplied, do not pool
  # incompatible contexts in a single comparison or plot.
  for (field in intersect(c("cell_line", "exposure_hours"), names(data))) {
    values <- trimws(as.character(data[[field]]))
    if (anyNA(values) || any(!nzchar(values)) || length(unique(values)) != 1L)
      stop("Separe los contextos experimentales antes de calcular: ", field)
  }
  for(axis in c("A","B")) {
    n <- paste0("dose_",axis)
    x <- data[[n]]
    if(!is.numeric(x) || any(!is.finite(x) | x<0)) stop("Dosis inválida: ",n)
    data[[paste0(n,"_M")]] <- signif(ldj_convert_concentration(x,data[[paste0("unit_",axis)]],"M"),14)
  }
  value <- data$readout_value
  if(!is.numeric(value) || any(is.infinite(value))) stop("Respuestas no numéricas o infinitas.")
  data
}

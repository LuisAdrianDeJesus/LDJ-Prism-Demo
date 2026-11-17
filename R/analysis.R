summary_groups <- function(d) {
  do.call(rbind, lapply(split(d, interaction(d$series, d$dose, drop=TRUE)), function(g) {
    y <- g$response[is.finite(g$response)]; n <- length(y)
    s <- if(n>1) sd(y) else NA_real_
    data.frame(series=g$series[1], dose=g$dose[1], n=n, mean=mean(y),
      sd=s, sem=s/sqrt(n))
  }))
}
map_data <- function(d, x, y, group, decimal=".") {
  number <- function(v) {
    if(is.numeric(v)) return(v)
    z <- trimws(as.character(v))
    if(decimal==",") z <- sub(",", ".", z, fixed=TRUE)
    out <- suppressWarnings(as.numeric(z))
    if(any(!is.na(v) & nzchar(z) & is.na(out))) stop("Hay valores no numéricos; revise columnas y separador decimal.")
    out
  }
  if(!all(c(x,y) %in% names(d))) stop("Seleccione columnas válidas.")
  out <- data.frame(dose=number(d[[x]]),response=number(d[[y]]),
    series=if(group==".none") "Datos" else as.character(d[[group]]))
  if(any(!is.finite(out$dose)) || any(out$dose<0)) stop("Las concentraciones deben ser finitas y no negativas.")
  if(any(is.na(out$series))) stop("Faltan identificadores de serie.")
  out
}
fit_response <- function(d, model="4PL", target=50, threshold_mode="absolute", control_dose=0, zero_policy="exclude") {
  if(!model %in% c("3PL","4PL")) stop("Modelo no admitido.")
  if(!threshold_mode %in% c("absolute","control")) stop("Modo de umbral no admitido.")
  if(!zero_policy %in% c("exclude","include")) stop("Política de cero no admitida.")
  if(any(!is.finite(d$dose)) || any(d$dose<0)) stop("Dosis no válidas.")
  controls <- d$response[d$dose==control_dose & is.finite(d$response)]
  control_mean <- if(length(controls)) mean(controls) else NA_real_
  requested_target <- target
  if(threshold_mode=="control") {
    if(length(controls)<2 || control_mean<=0) stop("Se necesitan dos controles con media positiva.")
    if(target<=0 || target>=100) stop("Porcentaje relativo fuera de rango.")
    target <- control_mean*target/100
  }
  n_zero <- sum(d$dose==0 & is.finite(d$response))
  d <- d[is.finite(d$response) & (d$dose>0 | zero_policy=="include"),]
  if(length(unique(d$dose))<5 || nrow(d)<8) stop("Se requieren al menos cinco dosis positivas y ocho observaciones.")
  form <- if(model=="3PL") drc::LL.3() else drc::LL.4()
  fit <- drc::drm(response ~ dose, data=d, fct=form)
  cf <- coef(fit); cv <- vcov(fit)
  if(any(!is.finite(cf)) || any(!is.finite(cv)) || qr(cv)$rank<ncol(cv) ||
     kappa(cv,exact=TRUE)>1e12) stop("Modelo no identificable o covarianza mal condicionada.")
  if(cf[1]<=0) stop("El modelo no respalda una respuesta decreciente.")
  lo <- if(model=="3PL") 0 else cf[2]
  hi <- if(model=="3PL") cf[2] else cf[3]
  if(lo>=hi || target<=lo || target>=hi) stop("Umbral fuera de las asíntotas.")
  ed <- drc::ED(fit, respLev=target, type="absolute", interval="delta", display=FALSE)
  if(any(!is.finite(ed))) stop("Umbral o incertidumbre no estimables.")
  estimate <- as.numeric(ed[1,1])
  if(estimate<=0 || ed[1,3]<=0 || ed[1,4]<=ed[1,3]) stop("Estimación no positiva.")
  grid <- data.frame(dose=exp(seq(log(min(d$dose[d$dose>0])),log(max(d$dose)),length.out=150)))
  grid$response <- as.numeric(predict(fit,newdata=grid))
  list(table=data.frame(threshold_mode=threshold_mode,requested_target=requested_target,control_mean=control_mean,zero_policy=zero_policy,n_zero=n_zero,ci_method="delta; conditional on control mean",target=target,estimate=estimate,se=ed[1,2],
    lower=ed[1,3],upper=ed[1,4],model=model,
    extrapolated=estimate<min(d$dose[d$dose>0]) || estimate>max(d$dose)),
    curve=grid,fit=fit)
}
compare_control <- function(d, control=0) {
  if(!control %in% d$dose) stop("No existe el control seleccionado.")
  d <- d[is.finite(d$response),]
  d$level <- relevel(factor(d$dose), ref=as.character(control))
  if(any(table(d$level)<2)) stop("Cada grupo necesita al menos dos observaciones independientes.")
  fit <- aov(response ~ level, data=d)
  set.seed(20261116)
  post <- summary(multcomp::glht(fit,linfct=multcomp::mcp(level="Dunnett")))$test
  list(anova=as.data.frame(summary(fit)[[1]]),
    diagnostics=data.frame(shapiro_p=if(nrow(d)>=3 && nrow(d)<=5000) shapiro.test(residuals(fit))$p.value else NA_real_,
      fligner_p=fligner.test(response~level,data=d)$p.value,
      residual_df=df.residual(fit)),
    dunnett=data.frame(contrast=names(post$coefficients),difference=unname(post$coefficients),
      p_adjusted=unname(post$pvalues)))
}
selectivity <- function(ic50, cc20, cc50, ic_unit, cc_unit) {
  scale <- c(M=1,mM=1e-3,uM=1e-6,nM=1e-9)
  if(!all(c(ic_unit,cc_unit)%in%names(scale))) stop("Unidad no admitida.")
  if(any(!is.finite(c(ic50,cc20,cc50))) || any(c(ic50,cc20,cc50)<=0)) stop("Se requieren valores positivos finitos.")
  data.frame(IC50_M=ic50*scale[[ic_unit]],CC20_M=cc20*scale[[cc_unit]],
    CC50_M=cc50*scale[[cc_unit]],SI=cc50*scale[[cc_unit]]/(ic50*scale[[ic_unit]]))
}

validate_provenance <- function(experiment, control_experiment, source, independent) {
  if(!isTRUE(independent) || !nzchar(trimws(experiment)) ||
     !identical(experiment,control_experiment) || !nzchar(trimws(source)))
    stop("Documente experimentos coincidentes, procedencia del control e independencia.")
  invisible(TRUE)
}

# Optional control registry columns are preserved in the session table.
prepare_controls <- function(d, x, y, default_label="No tratado", default_type="untreated", series_column=NULL) {
  types <- c("untreated","vehicle","negative","positive","infected_untreated","treatment")
  if(!all(c(x,y)%in%names(d))) stop("Seleccione ejes válidos.")
  numeric_x <- is.numeric(d[[x]])
  inferred <- if(numeric_x) !is.na(d[[x]]) & d[[x]]==0 else rep(FALSE,nrow(d))
  kind <- if("control_type"%in%names(d)) as.character(d$control_type) else ifelse(inferred,default_type,"treatment")
  if(any(is.na(kind) | !kind%in%types)) stop("control_type no válido.")
  d$.is_control <- kind!="treatment"
  labels <- c(untreated=default_label,vehicle="Vehículo",negative="Control negativo",
    positive="Control positivo",infected_untreated="Infectado sin tratamiento")
  d$.control_label <- ifelse(d$.is_control,unname(labels[kind]),"")
  if("control_label"%in%names(d)) {
    supplied <- !is.na(d$control_label) & nzchar(trimws(d$control_label))
    d$.control_label[supplied & d$.is_control] <- as.character(d$control_label[supplied & d$.is_control])
  }
  d$.control_type <- kind
  d$.eligible <- FALSE
  registry <- NULL
  metadata <- c("observation_id","experiment_id","control_source","inference_eligible")
  if(all(metadata%in%names(d))) {
    valid <- !is.na(d$observation_id) & nzchar(as.character(d$observation_id)) &
      !is.na(d$experiment_id) & nzchar(as.character(d$experiment_id)) &
      !is.na(d$control_source) & nzchar(as.character(d$control_source))
    d$.eligible <- valid & !is.na(d$inference_eligible) &
      tolower(as.character(d$inference_eligible))%in%c("true","1")
    key <- paste(d$experiment_id,d$observation_id,sep="::")
    if(!is.null(series_column) && series_column%in%names(d)) {
      registry <- unique(data.frame(key=key[d$.is_control & valid],
        series=as.character(d[[series_column]][d$.is_control & valid]),stringsAsFactors=FALSE))
    }
    repeated <- d$.is_control & valid & duplicated(key)
    for(i in which(repeated)) {
      j <- which(key==key[i])[1]
      fields <- c(x,y,"control_type","control_source","inference_eligible")
      fields <- intersect(fields,names(d))
      if(!identical(d[j,fields,drop=FALSE] |> unname() |> as.list(),
                    d[i,fields,drop=FALSE] |> unname() |> as.list()))
        stop("Un control reutilizado tiene valores o metadatos contradictorios.")
    }
    d <- d[!repeated,,drop=FALSE]
  }
  if(any(d$.is_control & !is.finite(d[[y]])))
    stop("Hay controles sin coordenadas válidas; corrija los datos antes de graficar.")
  # Stable control-first categories, without changing concentration values.
  category <- ifelse(d$.is_control,d$.control_label,as.character(d[[x]]))
  d$.category <- factor(category,levels=unique(c(category[d$.is_control],category[!d$.is_control])))
  attr(d,"control_contexts") <- registry
  d
}
expand_control_contexts <- function(physical, series_column) {
  registry <- attr(physical,"control_contexts")
  if(is.null(registry) || is.null(series_column) || !series_column%in%names(physical)) return(physical)
  key <- paste(physical$experiment_id,physical$observation_id,sep="::")
  rows <- lapply(seq_len(nrow(physical)),function(i) {
    contexts <- if(physical$.is_control[i]) registry$series[registry$key==key[i]] else character()
    if(!length(contexts)) return(physical[i,,drop=FALSE])
    z <- physical[rep(i,length(contexts)),,drop=FALSE]
    z[[series_column]] <- contexts
    z
  })
  do.call(rbind,rows)
}
dispersion_summary <- function(y, measure="SD") {
  y <- y[is.finite(y)]; n <- length(y)
  spread <- if(n>1) sd(y) else NA_real_
  if(measure=="SEM") spread <- spread/sqrt(n)
  if(measure=="IC95") spread <- if(n>1) qt(.975,n-1)*spread/sqrt(n) else NA_real_
  data.frame(y=mean(y),ymin=mean(y)-spread,ymax=mean(y)+spread)
}

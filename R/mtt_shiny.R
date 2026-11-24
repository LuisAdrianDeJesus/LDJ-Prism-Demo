# Adapter: original v4 source remains byte-for-byte unchanged.
source(if(file.exists("R/appearance.R")) "R/appearance.R" else "apps/ldj-prism/R/appearance.R",local=TRUE)
ldj_convert_concentration <- function(value, from, to) {
  normalize <- function(u) chartr("µμ", "uu", as.character(u))
  scales <- c(M=1, mM=1e-3, uM=1e-6, nM=1e-9)
  from <- normalize(from); to <- normalize(to)
  if (length(to) != 1L || !to %in% names(scales)) stop("Unidad de destino inválida.")
  if (length(from) == 1L) from <- rep(from, length(value))
  if (length(from) != length(value) || anyNA(from) || any(!from %in% names(scales)))
    stop("Una o más unidades de origen son inválidas.")
  as.numeric(value) * unname(scales[from]) / unname(scales[to])
}

ldj_mtt_display_unit <- function(result, unit) {
  # Convert a copy of the analyzed result, NEVER refit and NEVER mutate the source.
  unit <- chartr("µμ", "uu", as.character(unit))
  ldj_convert_concentration(1, "uM", unit)
  out <- result
  for (i in seq_along(out$rows)) {
    d <- out$rows[[i]]$data
    d[[out$x]] <- ldj_convert_concentration(d[[out$x]], result$unit, unit)
    if (".category" %in% names(d) && ".is_control" %in% names(d)) {
      category <- as.character(d$.category)
      original_category <- category
      doses <- !d$.is_control
      category[doses] <- format(d[[out$x]][doses], scientific=FALSE, trim=TRUE, digits=8)
      marks <- out$rows[[i]]$inference$labels
      if(!is.null(marks)) out$rows[[i]]$inference$labels$category <- category[match(marks$category,original_category)]
      d$.category <- factor(category, levels=unique(c(category[!doses], category[doses])))
    }
    out$rows[[i]]$data <- d
    out$rows[[i]]$fit$curve$dose <- ldj_convert_concentration(
      out$rows[[i]]$fit$curve$dose, result$unit, unit)
    for (k in c("estimate", "lower", "upper", "se")) {
      if (k %in% names(out$rows[[i]]$fit$table))
        out$rows[[i]]$fit$table[[k]] <- ldj_convert_concentration(
          out$rows[[i]]$fit$table[[k]], result$unit, unit)
    }
  }
  if (!is.null(out$physical) && out$x %in% names(out$physical))
    out$physical[[out$x]] <- ldj_convert_concentration(out$physical[[out$x]], result$unit, unit)
  out$unit <- unit
  out
}

ldj_mtt_inference <- function(d,x,y,type="untreated",confirmed=FALSE) {
  unavailable <- function(reason) list(status=paste("Inferencia no disponible:",reason))
  if(!any(d$.is_control & d$.control_type==type))
    return(unavailable(paste("No hay control",type,"asignado a la serie.")))
  missing <- setdiff(c("control_type","observation_id","experiment_id","control_source",
    "inference_eligible","independent_unit_id"),names(d))
  if(length(missing)) return(unavailable(paste("Faltan columnas:",paste(missing,collapse=", "))))
  experiments <- d$experiment_id[!d$.is_control]
  if(anyNA(experiments) || any(!nzchar(trimws(experiments))) || length(unique(experiments))!=1L)
    return(unavailable("Se requiere un experimento por serie. Separe experimentos o especifique un diseño por bloques."))
  out <- ldj_activity_inference(d,x,y,type,unique(experiments),confirmed)
  if(is.null(out$labels)) return(unavailable(sub("^Inferencia ND: ","",out$status)))
  included <- d[!d$.is_control | (d$.control_type==type & d$experiment_id==unique(experiments)),,drop=FALSE]
  included <- included[!is.na(included$experiment_id),,drop=FALSE]
  out$audit$controls <- included[included$.is_control,c("observation_id","experiment_id","control_source","independent_unit_id"),drop=FALSE]
  out$diagnostics$warnings <- paste(c(
    if(isTRUE(out$diagnostics$shapiro_p < .05)) "Shapiro: revise la normalidad de residuos.",
    if(isTRUE(out$diagnostics$fligner_p < .05)) "Fligner: revise la homogeneidad de varianzas."),collapse=" ")
  counts <- table(as.character(included$.category))
  out$audit$n <- data.frame(condition=names(counts), n=as.integer(counts))
  out$dunnett$category <- out$labels$category
  out$dunnett$significance <- out$labels$label
  out
}

ldj_mtt_inference_report <- function(result) {
  setNames(lapply(result$rows,function(row) {
    row$inference %||% list(status="Solo análisis descriptivo.")
  }),vapply(result$rows,function(row) row$series,character(1)))
}

ldj_mtt_analyze_table <- function(data, x, y, group, series, unit,
                                  model="4PL", control_assignments=NULL,
                                  inference=FALSE, reference="untreated", independent=FALSE) {
  if (!all(c(x, y, group) %in% names(data))) stop("Seleccione columnas X, Y y serie existentes.")
  if (!is.numeric(data[[x]]) || !is.numeric(data[[y]]))
    stop("Las columnas de dosis y respuesta deben ser numéricas.")
  if (length(series) != 4L || anyNA(series) || any(!nzchar(trimws(series))) ||
      anyDuplicated(series))
    stop("Asigne cuatro series distintas a A/B, C/D, E/F y G/H.")
  if (!all(series %in% as.character(data[[group]])))
    stop("Las cuatro series deben existir en la tabla seleccionada.")
  if (!model %in% c("3PL", "4PL")) stop("Modelo no admitido.")
  # Normalize the measured physical concentration once. Input unit affects
  # interpretation and requires a new analysis; display unit never does.
  sources <- if ("concentration_unit" %in% names(data)) as.character(data$concentration_unit) else unit
  if (length(sources) > 1L) sources[is.na(sources) | !nzchar(sources)] <- unit
  replica_columns <- intersect(c("replicate", "Replica"), names(data))
  if (!length(replica_columns)) stop("Incluya replicate o Replica con identificadores de réplica.")
  if (length(replica_columns) == 2L) {
    a <- trimws(as.character(data$replicate))
    b <- trimws(as.character(data$Replica))
    has_a <- !is.na(a) & nzchar(a)
    has_b <- !is.na(b) & nzchar(b)
    if (any(has_a & has_b & a != b)) stop("Las columnas replicate y Replica contienen identificadores distintos.")
    data$replicate <- ifelse(has_a, a, b)
  } else {
    data$replicate <- data[[replica_columns]]
  }
  canonical <- data
  canonical[[x]] <- ldj_convert_concentration(canonical[[x]], sources, "uM")
  # Limit the analysis to the four chosen series. A separately recorded control
  # may also be retained if its physical ID is explicitly assigned to them.
  keep <- as.character(canonical[[group]]) %in% series
  if (!is.null(control_assignments)) {
    mapped <- control_assignments[as.character(control_assignments$series) %in% series, , drop=FALSE]
    if (nrow(mapped) && all(c("experiment_id","observation_id") %in% names(canonical))) {
      physical_key <- paste(canonical$experiment_id,canonical$observation_id,sep="::")
      mapping_key <- paste(mapped$experiment_id,mapped$observation_id,sep="::")
      is_control <- if ("control_type" %in% names(canonical))
        as.character(canonical$control_type) != "treatment" else
        !is.na(canonical[[x]]) & canonical[[x]] == 0
      keep <- keep | (is_control & physical_key %in% mapping_key)
    }
    control_assignments <- mapped
  }
  canonical <- canonical[keep, , drop=FALSE]
  # Check the registered units before physical de-duplication can hide a conflict.
  ambiguous_ids <- character()
  ids_needed <- c("control_type","experiment_id","observation_id","independent_unit_id")
  if(isTRUE(inference) && all(ids_needed %in% names(canonical))) {
    control_rows <- canonical$control_type == reference &
      !is.na(canonical$experiment_id) & nzchar(trimws(canonical$experiment_id)) &
      !is.na(canonical$observation_id) & nzchar(trimws(canonical$observation_id))
    controls <- canonical[which(control_rows),,drop=FALSE]
    keys <- paste(controls$experiment_id,controls$observation_id,sep="::")
    units <- split(trimws(as.character(controls$independent_unit_id)),keys)
    ambiguous_ids <- names(units)[vapply(units,function(x)
      length(x)>1L && (anyNA(x) || any(!nzchar(x)) || length(unique(x))!=1L),logical(1))]
  }
  physical <- prepare_controls(canonical, x, y, series_column=group)
  physical <- ldj_mtt_apply_control_map(physical, group, series, control_assignments)
  visual <- expand_control_contexts(physical, group)
  rows <- lapply(series, function(s) {
    d <- visual[as.character(visual[[group]]) == s, , drop=FALSE]
    if (!nrow(d)) stop("Serie ausente: ", s)
    if (any(!is.finite(d[[y]]))) stop("Resuelva respuestas faltantes o no finitas en ", s)
    if (any(!d$.is_control & (!is.finite(d[[x]]) | d[[x]] <= 0)))
      stop("Los tratamientos requieren concentraciones positivas: ", s)
    if (anyNA(d$replicate) || any(!nzchar(trimws(as.character(d$replicate)))))
      stop("Faltan identificadores de réplica: ", s)
    treatment <- d[!d$.is_control, , drop=FALSE]
    if (nrow(treatment) < 8L || length(unique(treatment[[x]])) < 5L)
      stop("Datos insuficientes en ", s, ": mínimo ocho observaciones y cinco dosis positivas distintas.")
    mapped <- data.frame(dose=treatment[[x]], response=treatment[[y]], series=s)
    fit <- fit_response(mapped, model, 50, "absolute", 0, "exclude")
    stats <- NULL
    if(isTRUE(inference)) {
      physical_ids <- paste(d$experiment_id,d$observation_id,sep="::")
      stats <- if(any(physical_ids[d$.is_control & d$.control_type==reference] %in% ambiguous_ids))
        list(status="Inferencia no disponible: un control físico tiene unidades independientes contradictorias o faltantes.")
      else ldj_mtt_inference(d,x,y,reference,independent)
    }
    list(data=d, fit=fit, series=s, inference=stats)
  })
  list(rows=rows, unit="uM", input_unit=unit, x=x, y=y, physical=physical,
       control_assignments=control_assignments,
       audit=list(synthetic=FALSE, shared_physical_n=sum(physical$.is_control),
         shared_visual_n=sum(visual$.is_control),
         eligible_controls=sum(physical$.is_control & physical$.eligible),
         inference=if(isTRUE(inference)) vapply(rows,function(row) row$inference$status,character(1)) else
           "Ajustes absolutos; controles descriptivos."))
}
ldj_mtt_table_figure <- function(result,config=list()) {
  cfg <- ldj_mtt_style(config); plots <- list()
  appearance <- config$appearance %||% ldj_appearance()
  for(i in seq_along(result$rows)) for(j in 1:2) {
    tag <- LETTERS[2*i-2+j]; pc <- ldj_mtt_panel(cfg,tag)
    row <- result$rows[[i]]; d <- row$data
    if(!is.null(result$metric)) {
      d <- d[d$.is_control | (is.finite(d[[result$x]]) & d[[result$x]]>0),,drop=FALSE]
      reference <- config$reference_control %||% "infected_untreated"
      if(identical(config$show_other_controls,FALSE))
        d <- d[!d$.is_control | d$.control_type==reference,,drop=FALSE]
      display_order <- order(!d$.is_control, d$.is_control & d$.control_type!=reference,
        d[[result$x]], na.last=TRUE)
      d$.category <- factor(as.character(d$.category),levels=unique(as.character(d$.category[display_order])))
    }
    color <- if(!is.null(pc$color)) pc$color else unname(cfg$palette[c("DTG","Letermovir","CDQ","Lopinavir")][i])
    if(!is.null(config$appearance) && is.null(pc$color)) color <- unname(ldj_series_colors(appearance,row$series)[1])
    s <- ldj_element_style(appearance,row$series,tag,color)
    if(j==1) {
      if(!is.null(result$metric) && appearance$palette %in% c("LDJ Editorial","LDJ Editorial · semitransparente") &&
         identical(s$control,"#9EA7B3") && identical(config$reference_control,"infected_untreated"))
        s$control <- "#8C6786"
      p <- ggplot2::ggplot(d,ggplot2::aes(.category,.data[[result$y]])) +
        ggplot2::stat_summary(ggplot2::aes(fill=.is_control),fun=mean,geom="col",
          color=grDevices::adjustcolor(s$bar_border,alpha.f=s$bar_border_alpha %||% 1),
          alpha=1,linewidth=s$bar_border_linewidth %||% s$linewidth,
          linetype=s$bar_border_linetype %||% "solid",width=.72) +
        ggplot2::scale_fill_manual(values=c(`FALSE`=grDevices::adjustcolor(s$bar_fill,alpha.f=s$bar_fill_alpha %||% s$alpha),
          `TRUE`=grDevices::adjustcolor(s$control,alpha.f=s$control_alpha %||% s$alpha)),guide="none")+
        ggplot2::stat_summary(fun.data=function(v) dispersion_summary(v,pc$summary_measure),geom="errorbar",width=.2,
          color=s$error,alpha=s$error_alpha %||% 1,linewidth=s$error_linewidth %||% .5,
          linetype=s$error_linetype %||% "solid")
      if(isTRUE(pc$show_points)) p <- p+ggplot2::geom_point(position=ggplot2::position_jitter(width=.08,seed=42),
        size=s$point_size %||% pc$point_size,shape=s$point_shape %||% pc$point_shape,
        color=s$point,fill=s$panel_background,alpha=s$point_alpha %||% 1,stroke=s$point_linewidth %||% .35)
      if(!is.null(result$metric)) p <- p+ggplot2::scale_x_discrete(labels=function(labels)
        vapply(labels,function(label) paste(strwrap(label,width=20),collapse="\n"),character(1)))
      if(!is.null(row$inference)) {
        if(!identical(config$show_anova,FALSE) || is.null(row$inference$labels))
          p <- p+ggplot2::labs(subtitle=paste(strwrap(row$inference$status,width=56),collapse="\n"))
        if(!is.null(row$inference$labels) && !identical(config$show_significance,FALSE)) {
          marks <- row$inference$labels
          if(identical(config$show_ns,FALSE)) marks <- marks[marks$label!="ns",,drop=FALSE]
          span <- max(diff(range(d[[result$y]],finite=TRUE)),1)
          marks$height <- vapply(marks$category,function(k) {
            v <- d[[result$y]][as.character(d$.category)==k]
            max(c(v,dispersion_summary(v,pc$summary_measure)$ymax),na.rm=TRUE)+(config$significance_offset %||% .07)*span
          },numeric(1))
          p <- p+ggplot2::geom_text(data=marks,ggplot2::aes(category,height,label=label),inherit.aes=FALSE,
            colour=s$significance,alpha=s$significance_alpha %||% 1,fontface="bold",size=config$significance_size %||% 3)
        }
      }
    } else {
      treatment <- d[!d$.is_control,,drop=FALSE]
      p <- ggplot2::ggplot(treatment,ggplot2::aes(.data[[result$x]],.data[[result$y]]))+
        ggplot2::geom_point(color=s$point,fill=s$curve,size=s$point_size %||% pc$point_size,
          shape=s$point_shape %||% pc$point_shape,alpha=s$point_alpha %||% .8,stroke=s$point_linewidth %||% .35)+
        ggplot2::geom_line(data=row$fit$curve,ggplot2::aes(dose,response),inherit.aes=FALSE,color=s$curve,
          linewidth=s$curve_linewidth %||% 1,alpha=s$curve_alpha %||% s$alpha,linetype=s$curve_linetype %||% "solid")+
        ggplot2::geom_hline(yintercept=row$fit$table$target %||% 50,linetype=s$reference_linetype %||% "dashed",color=s$reference,
          linewidth=s$reference_linewidth %||% .45,alpha=s$reference_alpha %||% 1)
      if(!identical(pc$x_scale,"linear")) p <- p+ggplot2::scale_x_log10()
      z <- row$fit$table
      sums <- ldj_dose_dispersion(treatment[[result$x]], treatment[[result$y]],
        pc$summary_measure, s$band_linewidth %||% .65, !identical(pc$x_scale,"linear"))
      p$layers <- p$layers[-1L]
      if(isTRUE(pc$curve_band) && appearance$preset=="editorial") {
        band <- ggplot2::geom_rect(data=ldj_estimable_dispersion(sums),
          ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),inherit.aes=FALSE,
          fill=s$band,alpha=s$band_alpha %||% .24,color=NA)
        p$layers <- c(list(band),p$layers)
      }
      p <- p+ggplot2::geom_errorbar(data=sums,ggplot2::aes(x=dose,ymin=ymin,ymax=ymax),
        inherit.aes=FALSE,color=s$error,linewidth=s$error_linewidth %||% .5,width=.03,na.rm=TRUE)+
        ggplot2::geom_point(data=sums,ggplot2::aes(dose,mean),inherit.aes=FALSE,
          colour=s$point,fill=s$curve,shape=s$point_shape %||% pc$point_shape,
          size=s$point_size %||% pc$point_size,alpha=s$point_alpha %||% 1,
          stroke=s$point_linewidth %||% .35)
      if(appearance$preset=="editorial") {
        if(is.finite(z$estimate)&&z$estimate>0) p <- p+ggplot2::geom_vline(xintercept=z$estimate,
          color=s$reference,linetype=s$reference_linetype %||% "dashed",linewidth=s$reference_linewidth %||% .5,
          alpha=s$reference_alpha %||% 1)
      }
      estimate <- if(is.finite(z$estimate)) paste(signif(z$estimate,4),result$unit) else "ND"
      interval <- if(all(is.finite(c(z$lower,z$upper)))) {
        paste0(signif(z$lower,4),"–",signif(z$upper,4)," ",result$unit)
      } else "ND"
      note <- paste0(result$metric %||% "CC50"," = ",estimate,"\nIC95: ",interval,
        if(isTRUE(z$extrapolated)) " (extrapolado)" else "")
      if(!is.null(result$metric) && appearance$preset=="editorial") {
        p <- p+ggplot2::annotate("label",x=Inf,y=Inf,label=note,hjust=1.06,vjust=1.12,
          colour=s$text,fill=s$panel_background,size=max(2.5,pc$font_size*.24))
      } else p <- p+ggplot2::labs(subtitle=note)
    }
    p <- p+ldj_plot_theme(s,pc$font_size,pc$font_family,isTRUE(cfg$grid))+ggplot2::labs(title=if(is.null(pc$title)) paste(tag,row$series) else pc$title,
      x=if(is.null(pc$x_label)) paste("Concentración",result$unit) else pc$x_label,
      y=if(is.null(pc$y_label)) result$y else pc$y_label,caption=pc$annotation)+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1),plot.margin=ggplot2::margin(12,16,28,20))
    p <- p+ggplot2::coord_cartesian(ylim=if(j==1 && !is.null(row$inference$labels) && !identical(config$show_significance,FALSE) && nrow(marks) && !is.null(pc$y_limits))
      c(pc$y_limits[1],max(pc$y_limits[2],marks$height+.07*span)) else if(is.null(result$metric)) pc$y_limits else config$panels[[tag]]$y_limits,xlim=if(j==2) pc$x_limits else NULL)
    plots[[tag]] <- p
  }
  cfg$appearance <- appearance
  list(figure=patchwork::wrap_plots(plots,ncol=2,nrow=length(result$rows))+patchwork::plot_annotation(title=config$figure_title,
    caption=paste(strwrap(if(!is.null(result$metric)) paste("Medias ±",cfg$summary_measure,"· Las sombras indican dispersión observada de la media, no incertidumbre del ajuste.") else paste0("Medias ± ",cfg$summary_measure," · Sombras de dispersión observada. Ajustes absolutos; controles descriptivos. ",
      if(any(vapply(result$rows,function(row) !is.null(row$inference$labels),logical(1))))
        "Contrastes Dunnett solo en series elegibles; ajuste por serie. Unidad: " else "Sin comparaciones relativas al control. Unidad: ",result$unit),width=88),collapse="\n"),
    theme=ldj_plot_theme(ldj_element_style(appearance),cfg$font_size,cfg$font_family)),
    panels=plots,config=cfg,audit=result$audit)
}

ldj_mtt_synthetic_bundle <- function() {
  drugs <- ldj_mtt_defaults()$order
  controls <- data.frame(record_id=paste0("SYN-C",1:6),replicate=1:6,
    viability=c(97,101,99,103,98,102),is_synthetic_reference=TRUE,
    experiment_id="SYN-MTT-1",control_source="Deterministic synthetic fixture",
    inference_eligible=TRUE)
  orig <- params <- estimates <- list(); models <- list()
  for(i in seq_along(drugs)) {
    # Purely illustrative scales, unrelated to any experimental estimates.
    midpoint <- c(2,0.4,6,12)[i]
    d <- expand.grid(conc_uM=midpoint*c(.03,.1,.3,1,3,10,30),replicate=1:6)
    d$viability <- 100/(1+(d$conc_uM/midpoint)^1.25) +
      rep(c(-2,1,3,-1,2,-3),each=7)*(.6+.2*cos(d$conc_uM))
    d$drug <- drugs[i]
    d$record_id <- paste0("SYN-",i,"-",seq_len(nrow(d)))
    if(i%in%c(1,3)) d <- rbind(d,data.frame(conc_uM=0,replicate=1:6,
      viability=controls$viability,drug=drugs[i],record_id=paste0("SYN-",i,"-C",1:6)))
    orig[[i]] <- d
    fit <- drc::drm(viability~conc_uM,data=d[d$conc_uM>0,],
      fct=if(i==4) drc::LL.3() else drc::LL.4())
    ed <- drc::ED(fit,50,type="absolute",interval="delta",display=FALSE)
    estimates[[i]] <- data.frame(drug=drugs[i],cc50_uM=ed[1,1],
      ci95_low_uM=ed[1,3],ci95_high_uM=ed[1,4])
    if(i==2) models$let_4pl <- fit
    if(i==4) models$lop_3pl <- fit
    if(i%in%c(1,3)) {
      cf <- unname(stats::coef(fit))
      params[[length(params)+1L]] <- data.frame(drug=drugs[i],
        upper_asymptote=cf[3],lower_asymptote=cf[2],slope=cf[1],
        raw_absolute_cc50_uM=ed[1,1])
    }
  }
  list(orig=do.call(rbind,orig),controls=controls,cc50=do.call(rbind,estimates),
    cdq_dtg_4pl=do.call(rbind,params),models=models,
    dunnett_shared=data.frame(drug=character(),contrast=character(),p_adjusted_dunnett=numeric()),
    dunnett_original=data.frame(drug=character(),comparison=character(),p_value_dunnett_adjusted=numeric()),
    synthetic=TRUE)
}

ldj_mtt_demo_inference <- function(bundle) {
  stopifnot(isTRUE(bundle$synthetic))
  keys <- ldj_mtt_defaults()$order
  stats::setNames(lapply(keys,function(key) {
    d <- ldj_mtt_bar_data(bundle,key)
    control <- d$conc_uM==0
    d$control_type <- ifelse(control,"untreated","treatment")
    d$experiment_id <- "SYN-MTT-1"
    d$observation_id <- ifelse(is.na(d$record_id),
      paste("SYN",key,d$conc_uM,d$replicate,sep=":"),d$record_id)
    d$independent_unit_id <- d$observation_id
    d$control_source <- "Demostración sintética LDJ"
    d$inference_eligible <- TRUE
    d$.eligible <- TRUE
    d$.control_type <- d$control_type
    d$.is_control <- control
    d$.category <- as.character(d$conc_uM)
    ldj_mtt_inference(d,"conc_uM","viability","untreated",TRUE)
  }),keys)
}

ldj_mtt_demo_annotations <- function(p,result,config,style) {
  if(!identical(config$show_anova,FALSE)) p <- p+ggplot2::labs(subtitle=result$status)
  marks <- result$labels
  if(!is.null(marks) && !identical(config$show_significance,FALSE)) {
    if(identical(config$show_ns,FALSE)) marks <- marks[marks$label!="ns",,drop=FALSE]
    ymax <- stats::setNames(p$data$mean+p$data$spread,as.character(p$data$dose))
    marks$y <- unname(ymax[marks$category])+(config$significance_offset %||% .07)*diff(range(c(0,ymax),finite=TRUE))
    p <- p+ggplot2::geom_text(data=marks,ggplot2::aes(x=category,y=y,label=label),
      inherit.aes=FALSE,colour=style$significance,fontface="bold",size=config$significance_size %||% 3)
    if(!is.null(config$y_limits) && any(is.finite(marks$y))) {
      ylim <- config$y_limits
      ylim[2] <- max(ylim[2],max(marks$y,na.rm=TRUE)+.04*diff(ylim))
      p <- suppressMessages(p+ggplot2::coord_cartesian(ylim=ylim,clip="off"))
    }
  }
  p
}

ldj_mtt_editor_build <- function(bundle,config=list()) {
  if(!isTRUE(bundle$synthetic)) {
    # Private data must pass the original strict scientific contract.
    ldj_mtt_validate_bundle(bundle)
  } else {
    stopifnot(all(bundle$controls$is_synthetic_reference),
      !anyDuplicated(bundle$controls$record_id),nrow(bundle$controls)==6,
      all(ldj_mtt_defaults()$order%in%bundle$orig$drug))
  }
  cfg <- ldj_mtt_style(config)
  if(isTRUE(bundle$synthetic) && is.null(config$plot_caption)) cfg$plot_caption <-
    "Datos sintéticos. Dispersión observada en los paneles de barras y curvas. Sin contrastes inferenciales."
  cfg$control_label <- paste(strwrap(cfg$control_label,width=12),collapse="\n")
  if(is.null(config$shared_control_label))
    cfg$shared_control_label <- paste0(cfg$control_label,"\n(compartido)")
  for(values in list(bundle$dunnett_shared$p_adjusted_dunnett,bundle$dunnett_original$p_value_dunnett_adjusted))
    if(any(is.finite(values) & (values<0 | values>1))) stop("Valores p fuera de [0,1].")
  if(!identical(cfg$concentration_unit,"µM")) stop("Convierta datos, parámetros e IC antes de cambiar unidades.")
  if(length(cfg$order)!=4 || !setequal(cfg$order,ldj_mtt_defaults()$order)) stop("Se requieren cuatro series.")
  plots <- list()
  for(i in seq_along(cfg$order)) {
    drug <- cfg$order[i]
    for(j in 1:2) {
      tag <- LETTERS[2*i-2+j]; pc <- ldj_mtt_panel(cfg,tag)
      if(!is.null(pc$color) && !grepl("^#[[:xdigit:]]{6}$",pc$color)) stop("Color HEX inválido.")
      if(!is.null(pc$y_limits) && (length(pc$y_limits)!=2 || any(!is.finite(pc$y_limits)) || diff(pc$y_limits)<=0)) stop("Límites Y inválidos.")
      if(!is.null(pc$x_limits) && (length(pc$x_limits)!=2 || any(!is.finite(pc$x_limits)) || diff(pc$x_limits)<=0 || (j==2 && min(pc$x_limits)<=0))) stop("Límites X inválidos; curvas requieren dosis positivas.")
      draw_cfg <- cfg
      if(j==2 && !is.null(config$appearance)) {
        style <- ldj_element_style(config$appearance,drug,tag,pc$color %||% unname(cfg$palette[drug]))
        draw_cfg$panels[[tag]]$band_width <- style$band_linewidth %||% .65
      }
      p <- if(j==1) ldj_mtt_make_bar(bundle,drug,tag,cfg) else ldj_mtt_make_curve(bundle,drug,tag,draw_cfg)
      if(j==1) p <- p + ggplot2::theme(
        axis.text.x=ggplot2::element_text(angle=35,hjust=1,vjust=1),
        plot.margin=ggplot2::margin(8,11,8,32))
      if(j==2) {
        # Explicit alternative for zero-dose controls; no artificial log dose.
        p <- p + ggplot2::labs(subtitle="Control y réplicas: panel de barras contiguo")
        if(identical(pc$x_scale,"linear")) p <- suppressMessages(p+ggplot2::scale_x_continuous())
        for(k in seq_along(p$layers)) if(inherits(p$layers[[k]]$geom,"GeomPoint")) {
          p$layers[[k]]$aes_params$shape <- pc$point_shape
          p$layers[[k]]$aes_params$size <- pc$point_size
        }
      }
      if(!is.null(pc$annotation) && nzchar(pc$annotation))
        p <- p + ggplot2::labs(caption=pc$annotation)
      if(!is.null(config$appearance)) {
        a <- config$appearance; s <- ldj_element_style(a,drug,tag,pc$color %||% unname(cfg$palette[drug]))
        p <- ldj_style_existing_plot(p,s,j==1,a$preset)
        if(j==2) for(layer in p$layers) if(inherits(layer$geom,"GeomRect"))
          layer$aes_params$alpha <- s$band_alpha %||% .24
        if(j==2) {
          # Place parameters above the plotting area, avoiding right-edge clipping.
          p$layers <- Filter(function(layer) !inherits(layer$geom,"GeomLabelRepel"),p$layers)
          z <- bundle$cc50[bundle$cc50$drug==drug,,drop=FALSE]
          p <- p+ggplot2::labs(subtitle=paste0("CC50 = ",signif(z$cc50_uM,4)," µM\nIC95: ",
            signif(z$ci95_low_uM,4),"–",signif(z$ci95_high_uM,4)," µM"))

        }
        p <- p+ldj_plot_theme(s,pc$font_size,pc$font_family)
        if(j==1) {
          dose_labels <- local({
            zero_label <- if(drug%in%c("Letermovir","Lopinavir")) pc$shared_control_label else pc$control_label
            function(values) vapply(values,function(v) if(v=="0") zero_label else
              format(signif(as.numeric(v),4),trim=TRUE,scientific=FALSE),character(1))
          })
          p <- suppressMessages(p+ggplot2::scale_x_discrete(labels=dose_labels))+
            ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1,vjust=1),
              plot.margin=ggplot2::margin(10,18,28,24))
        }
      }
      if(j==1 && !is.null(config$demo_inference) && !is.null(config$appearance))
        p <- ldj_mtt_demo_annotations(p,config$demo_inference[[drug]],utils::modifyList(config,pc),s)
      plots[[tag]] <- p
    }
  }
  figure <- patchwork::wrap_plots(plots,ncol=2,nrow=4,byrow=TRUE) +
    patchwork::plot_annotation(title=cfg$figure_title,
      caption=paste(strwrap(cfg$plot_caption,width=130),collapse="\n"))
  if(!is.null(config$appearance)) figure <- figure+patchwork::plot_annotation(
    theme=ldj_plot_theme(ldj_element_style(config$appearance),cfg$font_size,cfg$font_family))
  list(figure=figure,panels=plots,config=cfg,audit=list(
    synthetic=isTRUE(bundle$synthetic),physical_controls=bundle$controls,
    shared_physical_n=nrow(bundle$controls),shared_visual_n=2*nrow(bundle$controls),
    inference="No inference is recomputed by this renderer"))
}

# Cairo embeds Unicode-capable fonts; the upstream pdf device substitutes dashes.
# Keep upstream export unchanged and delegate PNG/SVG to it.
ldj_mtt_editor_export <- function(result,filename,width=result$config$plot_width,
                                  height=result$config$plot_height,dpi=result$config$dpi) {
  if(!is.null(result$config$appearance)) {
    ext <- tolower(tools::file_ext(filename))
    device <- switch(ext,pdf=grDevices::cairo_pdf,svg=svglite::svglite,png="png",stop("Formato inválido."))
    ggplot2::ggsave(filename,plot=result$figure,device=device,width=width,height=height,
      units="in",dpi=dpi,bg=result$config$appearance$global$background,limitsize=FALSE)
    return(invisible(normalizePath(filename,mustWork=TRUE)))
  }
  if(tolower(tools::file_ext(filename))!="pdf")
    return(ldj_mtt_export(result,filename,width,height,dpi))
  if(!capabilities("cairo")) stop("La exportación PDF editorial requiere Cairo.")
  ggplot2::ggsave(filename,plot=result$figure,device=grDevices::cairo_pdf,
    width=width,height=height,units="in",bg="white",limitsize=FALSE)
  invisible(normalizePath(filename,mustWork=TRUE))
}

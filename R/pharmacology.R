# Rendering and normalization for generic parameter tables.
ldj_safety_results <- function(data, group, decimal=".") {
  required <- c("parameter","estimate","unit")
  if(!all(required %in% names(data))) stop("Se requieren columnas parameter, estimate y unit.")
  number <- function(x) suppressWarnings(as.numeric(if(identical(decimal,",")) sub(",",".",as.character(x),fixed=TRUE) else as.character(x)))
  p <- toupper(trimws(as.character(data$parameter)))
  g <- if(length(group)==1L && group %in% names(data)) trimws(as.character(data[[group]])) else rep("Datos",nrow(data))
  if(!length(p)||anyNA(g)||any(!nzchar(g))||anyNA(p)||any(!p %in% c("IC50","CC20","CC50"))) stop("Indique un grupo válido y un parámetro IC50, CC20 o CC50 por fila.")
  if(anyDuplicated(data.frame(g,p))) stop("Debe haber una sola estimación por grupo y parámetro.")
  convert <- function(v) {
    u <- chartr("µμ","uu",trimws(as.character(data$unit)))
    value <- rep(NA_real_,length(p))
    ok <- is.finite(v)&v>0&u%in%c("M","mM","uM","nM")
    if(any(ok)) value[ok] <- ldj_convert_concentration(v[ok],u[ok],"M")
    value
  }
  e <- convert(number(data$estimate))
  lo <- if("lower"%in%names(data)) convert(number(data$lower)) else rep(NA_real_,length(p))
  hi <- if("upper"%in%names(data)) convert(number(data$upper)) else rep(NA_real_,length(p))
  valid <- is.finite(e)&is.finite(lo)&is.finite(hi)&lo<hi&lo<=e&e<=hi
  out <- data.frame(series=g,parameter=p,estimate=e,lower=ifelse(valid,lo,NA_real_),
    upper=ifelse(valid,hi,NA_real_),status=ifelse(!is.finite(e),
      "ND: estimación ausente/no positiva o unidad inválida",ifelse(valid,"IC95 proporcionado","IC95 ND: límites ausentes o incompatibles")))
  missing <- expand.grid(series=unique(g),parameter=c("IC50","CC20","CC50"),stringsAsFactors=FALSE)
  missing <- missing[!paste(missing$series,missing$parameter,sep="\r")%in%paste(g,p,sep="\r"),,drop=FALSE]
  if(nrow(missing)) out <- rbind(out,transform(missing,estimate=NA_real_,lower=NA_real_,upper=NA_real_,status="ND: parámetro no proporcionado"))
  si <- lapply(unique(g),function(key) {
    z <- out[out$series==key,]
    ratio <- z$estimate[z$parameter=="CC50"]/z$estimate[z$parameter=="IC50"]
    data.frame(series=key,parameter="SI",estimate=ratio,lower=NA_real_,upper=NA_real_,
      status=if(is.finite(ratio)) "IC95 ND: se requiere incertidumbre conjunta" else "ND: requiere IC50 y CC50 válidos")
  })
  rbind(out,do.call(rbind,si))
}

ldj_safety_plot <- function(table, state, unit, title="", size=12, family="sans", grid=FALSE) {
  make <- function(si,tag) {
    z <- table[(table$parameter=="SI")==si,]
    z$label <- if(si) z$series else paste(z$series,z$parameter,sep=" · ")
    if(!si) for(k in c("estimate","lower","upper")) z[[k]] <- ldj_convert_concentration(z[[k]],"M",unit)
    s <- ldj_element_style(state,panel=tag)
    p <- ggplot2::ggplot(z,ggplot2::aes(y=label)) + ggplot2::geom_blank(x=1)
    for(key in unique(z$series)) {
      st <- ldj_element_style(state,key,tag)
      d <- z[z$series==key & is.finite(z$estimate),]
      if(!nrow(d)) next
      intervals <- d[is.finite(d$lower)&is.finite(d$upper),]
      if(nrow(intervals)) p <- p+ggplot2::geom_segment(data=intervals,ggplot2::aes(x=lower,xend=upper,yend=label),
        colour=st$error,linewidth=st$error_linewidth %||% .65,alpha=st$error_alpha %||% 1)
      p <- p+ggplot2::geom_point(data=d,ggplot2::aes(x=estimate,shape=parameter),fill=st$curve,
        colour=st$point,size=st$point_size %||% 3,alpha=st$point_alpha %||% st$alpha)
    }
    nd <- z[!is.finite(z$estimate),]
    if(nrow(nd)) p <- p+ggplot2::geom_text(data=nd,ggplot2::aes(x=Inf,label="ND"),hjust=1.15,colour=s$text)
    p+ggplot2::scale_shape_manual(values=c(IC50=21,CC20=23,CC50=22,SI=24))+
      ggplot2::scale_x_log10()+ggplot2::labs(title=if(si) "B. Índice de selectividad" else "A. Potencia y citotoxicidad",
        x=if(si) "SI (CC50 / IC50)" else paste0("Concentración (",unit,")"),y=NULL)+
      ldj_plot_theme(s,size,family,grid)
  }
  patchwork::wrap_plots(make(FALSE,"A"),make(TRUE,"B"),ncol=1)+patchwork::plot_annotation(title=title,
    caption="ND indica que faltan datos válidos. Consulte Resultados para conocer la causa y el estado de los intervalos.",
    theme=ldj_plot_theme(ldj_element_style(state),size,family))
}

# Inferential comparisons operate on independent units within one series and experiment.
ldj_activity_inference <- function(d,x,y,type,experiment,confirmed) {
  tryCatch({
    needed <- c("experiment_id","observation_id","independent_unit_id",".eligible",".control_type",".is_control",".category")
    if(!isTRUE(confirmed)) stop("Confirme la independencia de las réplicas.")
    if(!all(needed %in% names(d))) stop("Falta independent_unit_id o la trazabilidad del control físico.")
    if(length(experiment)!=1L || is.na(experiment) || !nzchar(experiment)) stop("Seleccione un experimento.")
    keep <- !is.na(d$experiment_id)&d$experiment_id==experiment & (!d$.is_control | d$.control_type==type)
    z <- d[keep,,drop=FALSE]
    if(any(!d$.is_control & (is.na(d$experiment_id)|d$experiment_id!=experiment)))
      stop("Separe los tratamientos por experimento antes de anotar barras.")
    if(length(unique(as.character(z$.category[z$.is_control])))!=1L) stop("Seleccione una categoría física de control inequívoca.")
    if(!any(z$.is_control) || !all(z$.eligible[z$.is_control])) stop("El control seleccionado falta o no es elegible.")
    ids <- as.character(z$observation_id); units <- as.character(z$independent_unit_id)
    if(anyNA(ids)||anyNA(units)||any(!nzchar(trimws(ids)))||any(!nzchar(trimws(units)))||
      anyDuplicated(ids)||anyDuplicated(units)) stop("Identificadores ausentes o repetidos. Agregue las réplicas técnicas por unidad independiente antes del análisis.")
    if(any(!is.finite(z[[y]]))||any(!is.finite(z[[x]][!z$.is_control])|z[[x]][!z$.is_control]<=0))
      stop("Hay observaciones incompletas o dosis de tratamiento no positivas.")
    dose <- ifelse(z$.is_control,0,z[[x]])
    if(length(unique(dose))<2) stop("Faltan grupos de tratamiento.")
    cmp <- compare_control(data.frame(dose=dose,response=z[[y]]),0)
    a <- cmp$anova
    p <- cmp$dunnett$p_adjusted
    if(any(!is.finite(p))||!is.finite(a[1,"F value"])) stop("ANOVA o Dunnett no estimables.")
    levels <- levels(relevel(factor(dose),ref="0"))[-1]
    category <- vapply(levels,function(k) {
      labs <- unique(as.character(z$.category[as.character(dose)==k]))
      if(length(labs)!=1L) stop("La categoría gráfica de la dosis es ambigua.")
      labs
    },character(1))
    cmp$labels <- data.frame(category=category,label=ifelse(p<.0001,"****",
      ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","ns")))))
    probability <- a[1,"Pr(>F)"]
    p_label <- if(probability < .0001) "p < 0.0001" else paste("p =",signif(probability,3))
    cmp$status <- sprintf("ANOVA F(%s, %s) = %.3g · %s",a[1,"Df"],a[2,"Df"],a[1,"F value"],p_label)
    cmp$audit <- list(experiment=experiment,control_type=type,control_ids=ids[z$.is_control],
      independent_units=units,adjustment="Dunnett dentro de cada serie")
    cmp
  },error=function(e) list(status=paste("Inferencia ND:",conditionMessage(e))))
}

ldj_safety_editorial <- function(table,d,x,y,group,input_unit,unit,model,state,measure="SD",title="") {
  keys <- unique(table$series)
  if(!length(keys) || length(keys)>4L) stop("El modo editorial requiere entre uno y cuatro grupos de parámetros.")
  treatment <- if("control_type"%in%names(d)) !is.na(d$control_type)&d$control_type=="treatment" else rep(TRUE,nrow(d))
  d <- d[treatment & is.finite(d[[x]]) & d[[x]]>0 & is.finite(d[[y]]),,drop=FALSE]
  fits <- lapply(keys,function(key) {
    w <- d[!is.na(d[[group]]) & d[[group]]==key,,drop=FALSE]
    z <- data.frame(dose=ldj_convert_concentration(w[[x]],input_unit,"M"),response=w[[y]])
    fit <- tryCatch({
      canonical <- z
      canonical$dose <- ldj_convert_concentration(z$dose,"M","uM")
      fit <- fit_response(canonical,model,80,"absolute",0,"exclude")
      fit$curve$dose <- ldj_convert_concentration(fit$curve$dose,"uM","M")
      for(k in c("estimate","lower","upper","se"))
        fit$table[[k]] <- ldj_convert_concentration(fit$table[[k]],"uM","M")
      fit
    },error=function(e) list(error=conditionMessage(e)))
    list(data=z,fit=fit,series=key)
  })
  list(table=table,rows=fits)
}

ldj_cc20_sources <- function(table, fit, tolerance = 10) {
  sources <- rbind(
    data.frame(source="regression",estimate=fit$table$estimate %||% NA_real_,
      lower=fit$table$lower %||% NA_real_,upper=fit$table$upper %||% NA_real_),
    transform(table[table$parameter=="CC20",c("estimate","lower","upper"),drop=FALSE],source="table"))
  sources$estimate[!is.finite(sources$estimate) | sources$estimate<=0] <- NA_real_
  valid_ci <- with(sources,is.finite(lower)&is.finite(upper)&lower>0&
    !is.na(estimate)&lower<upper&lower<=estimate&estimate<=upper)
  sources[!valid_ci,c("lower","upper")] <- NA_real_
  gap <- if(nrow(sources)==2L && all(is.finite(sources$estimate))) abs(diff(sources$estimate)) else NA_real_
  percent <- 100*(gap/sources$estimate[1])
  overlap <- if(nrow(sources)==2L && all(valid_ci)) max(sources$lower)<=min(sources$upper) else NA
  list(sources=sources,gap=gap,percent=percent,exceeds=isTRUE(percent>tolerance),overlap=overlap)
}

ldj_safety_editorial_plot <- function(result,state,unit,measure="SD",title="",x_label=NULL,y_label=NULL,cc20_source="table",cc20_tolerance=10,show_fit=FALSE,
                                      viability_label="80–100% viable",panel_styles=list(),
                                      font_size=12,font_family="sans",show_si=FALSE) {
  stopifnot(cc20_source %in% c("table","regression","both"),is.finite(cc20_tolerance),cc20_tolerance>=0)
  label_unit <- if(unit %in% c("uM","μM")) "µM" else unit
  plots <- list()
  for(i in seq_along(result$rows)) {
    row <- result$rows[[i]]; key <- row$series; tags <- LETTERS[c(2*i-1,2*i)]
    lc <- panel_styles[[tags[1]]] %||% list()
    rc <- panel_styles[[tags[2]]] %||% list()
    sf <- ldj_element_style(state,key,tags[1],lc$color)
    sp <- ldj_element_style(state,key,tags[2],rc$color)
    panel_measure <- lc$summary_measure %||% measure
    cv <- function(x) ldj_convert_concentration(x,"M",unit)
    d <- row$data; d$dose <- cv(d$dose)
    z <- result$table[result$table$series==key & result$table$parameter!="SI",,drop=FALSE]
    comparison <- ldj_cc20_sources(z,row$fit,cc20_tolerance)
    selected <- comparison$sources
    if(cc20_source!="both") selected <- selected[selected$source==cc20_source,,drop=FALSE]
    selected$parameter <- ifelse(selected$source=="regression","CC20 · Ajuste","CC20 · Tabla")
    parameters <- c("CC50",rev(selected$parameter),"IC50")
    z <- rbind(z[z$parameter!="CC20",c("parameter","estimate","lower","upper")],
      selected[,c("parameter","estimate","lower","upper")])
    z$parameter <- factor(z$parameter,levels=parameters)
    for(n in c("estimate","lower","upper")) z[[n]] <- cv(z[[n]])
    selected$display <- cv(selected$estimate)
    ic <- z$estimate[z$parameter=="IC50"]
    label_value <- function(x) ifelse(is.finite(x),paste(signif(x,4),label_unit),"ND")
    source_label <- paste(paste0(selected$parameter,": ",label_value(selected$display)),collapse="\n")
    notes <- character()
    if(is.finite(comparison$gap)) {
      notes <- c(paste0(if(comparison$exceeds) "Discrepancia: " else "Diferencia: ",
        label_value(cv(comparison$gap))," (",signif(comparison$percent,3),"% del ajuste)"),
        if(!is.na(comparison$overlap)) paste("IC95%:",if(comparison$overlap) "se solapan" else "sin solapamiento"))
    }
    sums <- ldj_dose_dispersion(d$dose,d$response,panel_measure,sf$band_linewidth %||% .65)
    limits <- if(nrow(sums)) range(c(sums$xmin,sums$xmax),finite=TRUE) else c(NA_real_,NA_real_)
    viability <- if(all(is.finite(limits)) && all(limits>0)) data.frame(xmin=limits[1],xmax=limits[2]) else
      data.frame(xmin=numeric(),xmax=numeric())
    left <- ggplot2::ggplot(d,ggplot2::aes(dose,response))+
      ggplot2::geom_rect(data=viability,ggplot2::aes(xmin=xmin,xmax=xmax,ymin=80,ymax=100),
        inherit.aes=FALSE,fill=sf$viability_band %||% "#B9DCCB",alpha=sf$viability_band_alpha %||% .42)+
      ggplot2::geom_rect(data=if(state$preset=="editorial") ldj_estimable_dispersion(sums) else sums[FALSE,],
        ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),inherit.aes=FALSE,fill=sf$band,alpha=sf$band_alpha %||% .24)+
      ggplot2::geom_errorbar(data=sums,ggplot2::aes(dose,ymin=ymin,ymax=ymax),inherit.aes=FALSE,
        width=.04,colour=sf$error,linewidth=sf$error_linewidth %||% .5,na.rm=TRUE)+
      ggplot2::geom_point(data=sums,ggplot2::aes(dose,mean),inherit.aes=FALSE,
        shape=lc$point_shape %||% sf$point_shape %||% 21,
        size=lc$point_size %||% sf$point_size %||% 2,fill=sf$curve,colour=sf$point)+
      ggplot2::geom_hline(yintercept=80,linetype="dashed",colour=sf$reference,alpha=sf$reference_alpha %||% 1)+
      (if(identical(lc$x_scale,"linear")) ggplot2::scale_x_continuous() else ggplot2::scale_x_log10())+
      ldj_plot_theme(sf,lc$font_size %||% font_size,font_family)+ggplot2::labs(title=lc$title %||% paste(tags[1],key,"· Viabilidad"),
        x=lc$x_label %||% x_label %||% paste0("Concentración (",label_unit,")"),
        y=lc$y_label %||% y_label %||% "Viabilidad (%)",caption=lc$annotation,
        subtitle=paste(c(source_label,if(!is.null(row$fit$error)) "Curva de ajuste: ND"),collapse="\n"))
    if(nrow(sums)>1L && identical(state$preset,"editorial")) left <- left+
      ggplot2::geom_line(data=sums,ggplot2::aes(dose,mean),inherit.aes=FALSE,
        colour=sf$curve,linewidth=sf$curve_linewidth %||% sf$linewidth)
    if(nrow(viability) && nzchar(viability_label %||% "")) left <- left+
      ggplot2::annotate("text",x=viability$xmax,y=99,label=viability_label,hjust=1,vjust=1,
        colour=sf$text,size=3)
    if(isTRUE(show_fit) && is.null(row$fit$error)) {
      curve <- row$fit$curve; curve$dose <- cv(curve$dose)
      left <- left+ggplot2::geom_line(data=curve,colour=sf$curve,linetype="dashed",alpha=sf$curve_alpha %||% sf$alpha)
    }
    model_dose <- selected$display[selected$source=="regression"]
    table_dose <- selected$display[selected$source=="table"]
    if(any(is.finite(model_dose))) left <- left+
      ggplot2::geom_vline(xintercept=model_dose,linetype="dashed",colour=sf$reference,alpha=sf$reference_alpha %||% 1)+
      ggplot2::annotate("point",x=model_dose,y=80,shape=23,size=3,fill=sf$curve,colour=sf$point)
    axis_marker <- min(c(80,sums$ymin,sums$mean),na.rm=TRUE)
    if(any(is.finite(table_dose))) left <- left+
      ggplot2::geom_vline(xintercept=table_dose,linetype="dotted",colour=sf$reference,alpha=sf$reference_alpha %||% 1)+
      ggplot2::annotate("point",x=table_dose,y=axis_marker,shape=24,size=3,fill=sf$curve,colour=sf$point)
    left <- left+ggplot2::theme(plot.subtitle=ggplot2::element_text(colour=sf$curve))+
      ggplot2::coord_cartesian(xlim=lc$x_limits,ylim=lc$y_limits)
    right <- ggplot2::ggplot(z,ggplot2::aes(y=parameter))+ggplot2::scale_y_discrete(drop=FALSE)
    for(j in which(is.finite(selected$display) & is.finite(ic) & ic>0)) {
      cc <- selected$display[j]
      right <- right+ggplot2::annotate("rect",xmin=min(cc,ic),xmax=max(cc,ic),ymin=-Inf,ymax=Inf,
        fill=sp$parameter_span %||% if(cc<ic) "#DA817A" else sp$curve,
        alpha=(sp$parameter_span_alpha %||% .16)/nrow(selected))
    }
    below <- selected$parameter[is.finite(selected$display)&is.finite(ic)&selected$display<ic]
    if(length(below)) notes <- c(paste0("Advertencia: CC20 < IC50 (",paste(sub("CC20 · ","",below),collapse=" / "),")"),notes)
    if(isTRUE(show_si)) {
      si <- result$table$estimate[result$table$series==key & result$table$parameter=="SI"]
      if(length(si)==1L && is.finite(si)) notes <- c(notes,paste("SI =",signif(si,4)))
    }
    intervals <- z[is.finite(z$lower)&is.finite(z$upper)&z$lower>0&z$upper>=z$lower,]
    shown <- z[is.finite(z$estimate),]; missing <- z[!is.finite(z$estimate),]
    if(nrow(intervals)) right <- right+ggplot2::geom_errorbar(data=intervals,ggplot2::aes(xmin=lower,xmax=upper,colour=parameter),
      orientation="y",width=.16,alpha=sp$error_alpha %||% 1)
    if(nrow(shown)) right <- right+ggplot2::geom_point(data=shown,ggplot2::aes(x=estimate,shape=parameter,fill=parameter),colour=sp$point,size=rc$point_size %||% sp$point_size %||% 3)+
      ggplot2::geom_text(data=shown,ggplot2::aes(x=estimate,label=label_value(estimate)),vjust=-.9,colour=sp$text,size=3)
    if(nrow(missing)) right <- right+ggplot2::geom_text(data=missing,ggplot2::aes(x=Inf,label="ND"),hjust=1.1,colour=sp$text)
    parameter_colors <- c(IC50="#0072B2",`CC20 · Ajuste`=sp$curve,`CC20 · Tabla`=sp$curve,CC50="#31805E")
    right <- right+ggplot2::scale_fill_manual(values=parameter_colors,guide="none")+
      ggplot2::scale_colour_manual(values=parameter_colors,guide="none")+
      ggplot2::scale_shape_manual(values=c(IC50=21,`CC20 · Ajuste`=23,`CC20 · Tabla`=24,CC50=22),guide="none")+
      (if(identical(rc$x_scale,"linear")) ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=.25)) else
        ggplot2::scale_x_log10(expand=ggplot2::expansion(mult=.25)))+
      ggplot2::coord_cartesian(xlim=rc$x_limits)+ldj_plot_theme(sp,rc$font_size %||% font_size,font_family)+
      ggplot2::labs(title=rc$title %||% paste(tags[2],key,"· Parámetros"),
        x=rc$x_label %||% paste0("Concentración (",label_unit,")"),y=rc$y_label,caption=rc$annotation,
        subtitle=if(length(notes)) paste(notes,collapse="\n") else "Franja: separación descriptiva IC50–CC20")
    plots <- c(plots,list(left,right))
  }
  patchwork::wrap_plots(plots,ncol=2)+patchwork::plot_annotation(title=title,
    caption=paste("Medias ±",measure,"· La franja entre parámetros es descriptiva. ND: parámetro no disponible. Triángulo al pie: concentración de tabla. IC95%: comparación descriptiva."))
}

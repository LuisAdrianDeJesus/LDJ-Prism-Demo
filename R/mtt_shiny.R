# Adapter: original v4 source remains byte-for-byte unchanged.
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
      p <- if(j==1) ldj_mtt_make_bar(bundle,drug,tag,cfg) else ldj_mtt_make_curve(bundle,drug,tag,cfg)
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
      plots[[tag]] <- p
    }
  }
  figure <- patchwork::wrap_plots(plots,ncol=2,nrow=4,byrow=TRUE) +
    patchwork::plot_annotation(title=cfg$figure_title,caption=cfg$plot_caption)
  list(figure=figure,panels=plots,config=cfg,audit=list(
    synthetic=isTRUE(bundle$synthetic),physical_controls=bundle$controls,
    shared_physical_n=nrow(bundle$controls),shared_visual_n=2*nrow(bundle$controls),
    inference="No inference is recomputed by this renderer"))
}

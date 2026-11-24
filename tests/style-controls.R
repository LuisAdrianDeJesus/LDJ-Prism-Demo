source("R/appearance.R")
source("R/mtt_data_entry.R")
d <- data.frame(experiment_id=c("A","A","B"),observation_id=c("1","2","1"),
  control_type="untreated",control_source="registro",control_label="Etiqueta repetida",
  control_set_id="grupo",independent_unit_id=c("u1","u2","u3"),inference_eligible=TRUE,
  .is_control=TRUE,.control_type="untreated")
k <- ldj_control_reference_keys(d)
stopifnot(k[1]==k[2],k[2]!=k[3],length(ldj_control_reference_choices(d))==2L)
d$control_label <- "Nuevo nombre"
stopifnot(identical(k,ldj_control_reference_keys(d)),nrow(ldj_select_control_reference(d,k[1])$data)==2L)
style <- list(schema="ldj-style",version=2,appearance=ldj_appearance(),
  settings=list(axis_y_mode="manual",axis_y_min=0,axis_y_max=90,width=8.35,height=10.45))
stopifnot(identical(ldj_validate_style(style)$settings,style$settings),
  identical(ldj_validate_style(style$appearance)$appearance,ldj_validate_colors(style$appearance)))
style$settings$model <- "3PL"
stopifnot(inherits(try(ldj_validate_style(style),silent=TRUE),"try-error"))

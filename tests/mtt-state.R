# Shiny reactive contract: analytical changes invalidate fits; purely visual changes do not.
env <- new.env(parent=globalenv())
sys.source("apps/ldj-prism/app.R",envir=env)
shiny::testServer(env$server,{
  b <- env$ldj_mtt_synthetic_bundle()
  d <- b$orig
  d$Series <- paste0("S",match(d$drug,env$ldj_mtt_defaults()$order))
  d$Replica <- d$replicate
  d$control_type <- ifelse(d$conc_uM==0,"untreated","treatment")
  d$observation_id <- d$record_id
  d$experiment_id <- "E1"; d$control_source <- "synthetic"
  d$inference_eligible <- TRUE
  working(d)
  session$setInputs(x="conc_uM",y="viability",group="Series",
    mtt_series_1="S1",mtt_series_2="S2",mtt_series_3="S3",mtt_series_4="S4",
    unit="uM",model="4PL",threshold_mode="absolute",zero_policy="exclude",
    control=0,control_type_default="untreated",experiment_id="E1",
    control_experiment="E1",control_source="synthetic",independent=TRUE,
    model_reason="synthetic model test",figure_template="mtt4x2",mtt_user_data=TRUE)
  mtt_control_map(env$ldj_mtt_default_control_map(d,"conc_uM","viability","Series"))
  table_result(list(dummy=TRUE))
  baseline <- analysis_signature()
  table_snapshot(baseline)
  stopifnot(identical(table_snapshot(),analysis_signature()))
  set_one <- function(k,v) do.call(session$setInputs,stats::setNames(list(v),k))
  # Test each inferential control independently; return to the original value.
  inferential <- list(unit="nM",model="3PL",threshold_mode="control",
    zero_policy="include",control=2,control_type_default="vehicle",
    experiment_id="E2",control_experiment="E2",control_source="different",
    independent=FALSE,model_reason="different reasoning",
    mtt_series_1="S4",x="viability",y="conc_uM",group="drug")
  for(k in names(inferential)) {
    old <- input[[k]]
    set_one(k,inferential[[k]])
    stopifnot(!identical(baseline,analysis_signature()))
    set_one(k,old)
  }
  modified <- d;modified$viability[1] <- modified$viability[1]+1
  working(modified)
  stopifnot(!identical(baseline,analysis_signature()))
  working(d)
  saved_map <- mtt_control_map()
  other <- rbind(saved_map,transform(saved_map[1,,drop=FALSE],series="S2"))
  mtt_control_map(other)
  stopifnot(!identical(baseline,analysis_signature()))
  mtt_control_map(saved_map)
  # Presentation controls must not change the analytical signature.
  cosmetic <- list(plot_title="New figure title",palette="Viridis",
    mtt_display_unit="nM",point_size=3,mtt_title="A changed title",
    x_label="New x label",y_label="New y label")
  before_visual <- analysis_signature()
  for(k in names(cosmetic)) {
    set_one(k,cosmetic[[k]])
    stopifnot(identical(before_visual,analysis_signature()))
  }
})
cat("Shiny analysis invalidation across data, fields, controls and model; style-only preservation: PASS\n")

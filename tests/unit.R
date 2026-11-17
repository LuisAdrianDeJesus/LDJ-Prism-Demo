source("R/analysis.R")
stopifnot(abs(selectivity(10,1,3,"nM","uM")$SI-300)<1e-10)
d <- data.frame(dose=rep(c(0,.1,.3,1,3,10,30),each=4))
d$response <- 100/(1+d$dose/2)+rep(c(-1.5,-.5,.5,1.5),7)
d$series <- "Synthetic"
s <- summary_groups(d)
stopifnot(all(s$n==4),nrow(s)==7)
f <- fit_response(d,"4PL",50)
stopifnot(abs(f$table$estimate-2)<.15,f$table$lower<f$table$estimate,
          f$table$upper>f$table$estimate)
a <- compare_control(d)
stopifnot(nrow(a$dunnett)==6,all(a$dunnett$p_adjusted>=0))
bad <- try(map_data(data.frame(x="bad",y=2),"x","y",".none"),silent=TRUE)
stopifnot(inherits(bad,"try-error"))
cat("General statistical functions: PASS\n")

relative <- d
relative$response <- relative$response*2
f_relative <- fit_response(relative,"4PL",50,"control")
stopifnot(abs(f_relative$table$estimate-f$table$estimate)<.15,
          abs(f_relative$table$target-100)<1e-8,
          f_relative$table$threshold_mode=="control")
f_zero <- fit_response(d,"4PL",50,zero_policy="include")
stopifnot(abs(f_zero$table$estimate-2)<.15,f_zero$table$n_zero==4)
stopifnot(inherits(try(fit_response(d,"4PL",150),silent=TRUE),"try-error"))
stopifnot(inherits(try(validate_provenance("A","B","plate",TRUE),silent=TRUE),"try-error"))
stopifnot(nrow(a$diagnostics)==1)
cat("Threshold definition, zero-dose policy, provenance and diagnostics: PASS\n")

controls <- data.frame(dose=c(0,0,0,1,2),response=c(100,98,100,70,40),
 series=c("A","A","B","A","A"),control_type=c("untreated","untreated","untreated","treatment","treatment"),
 observation_id=c("c1","c2","c1","t1","t2"),experiment_id="E1",
 control_source="plate-1",inference_eligible=TRUE)
z <- prepare_controls(controls,"dose","response")
stopifnot(sum(z$.is_control)==2,nrow(z)==4,
 all(z$.control_label[z$.is_control]=="No tratado"),all(z$.eligible),
 levels(z$.category)[1]=="No tratado",all(z$dose[z$.is_control]==0))
renamed <- prepare_controls(controls,"dose","response","Sin fármaco")
stopifnot(all(renamed$.control_label[renamed$.is_control]=="Sin fármaco"))
display_only <- controls
display_only$control_source <- NULL
stopifnot(!any(prepare_controls(display_only,"dose","response")$.eligible))
bad <- controls; bad$response[3] <- 50
stopifnot(inherits(try(prepare_controls(bad,"dose","response"),silent=TRUE),"try-error"))
for(type in c("untreated","vehicle","negative","positive","infected_untreated")) {
 sample <- controls[1:2,]; sample$control_type <- type
 stopifnot(sum(prepare_controls(sample,"dose","response")$.is_control)==2)
}
stopifnot(abs(dispersion_summary(c(98,100),"SD")$y-99)<1e-10)
cat("Control presence, labels, n, zero-dose preservation, eligibility and deduplication: PASS\n")

shared <- controls
shared$dose[1:3] <- NA_real_
physical <- prepare_controls(shared,"dose","response",series_column="series")
display <- expand_control_contexts(physical,"series")
stopifnot(sum(physical$.is_control)==2,sum(display$.is_control)==3,
  sum(display$.is_control & display$series=="A")==2,
  sum(display$.is_control & display$series=="B")==1,
  all(is.na(physical$dose[physical$.is_control])))
cat("Physical registry and display context separation: PASS\n")

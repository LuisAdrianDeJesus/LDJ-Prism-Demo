source("R/synergy_studio.R")
cfg <- ldj_map_config(list(visible="yes", precision="0", sign="yes", decimal=","))
labels <- ldj_map_number(c(0,.001,-.001,20,NA), cfg)
stopifnot(labels[1] == "0", grepl("^\\+.*e-",labels[2]), grepl("^-.*e-",labels[3]),
  labels[4] == "+20", labels[5] == "ND")
stopifnot(identical(ldj_synergy_dose_labels(c(0,4,.758,.75801)), c("0","4","0.758","0.75801")))
d <- ldj_didactic_matrix()
stopifnot(nrow(d)==147L, length(unique(d$experiment_id))==3L, !anyDuplicated(d$observation_id),
  all(d$readout_value %in% 0:100), identical(sort(unique(d$dose_A)),c(0,100,250,500,1000,2000,4000)),
  all(d$readout_value[d$dose_A==0 & d$dose_B==0]==0))
z <- data.frame(a=1,b=1,experiment_id=c("A","A","B"),context=1:3,value=c(1,3,10))
stopifnot(ldj_map_aggregate(z,"value","summary")$value==6)

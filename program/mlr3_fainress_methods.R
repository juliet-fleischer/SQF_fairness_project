# load learners
# create a complete cases dataset just to test the fairness methods
complete_cases <- sqf[complete.cases(sqf), ]
unprivileged <- c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC")
complete_cases$PA_GROUP <- ifelse(complete_cases$SUSPECT_RACE_DESCRIPTION %in% unprivileged, "POC", "White")

lrn_rpart = lrn("classif.ranger", predict_type = "prob")
lrn_rpart$id = "ranger"
l1 = as_learner(po("reweighing_wts") %>>% lrn("classif.ranger"))
l1$id = "reweight"

l2 = as_learner(po("learner_cv", lrn("classif.ranger")) %>>%
                  po("EOd"))
l2$id = "EOd"

# preprocess by collapsing factors
l3 = as_learner(po("collapsefactors") %>>% lrn("classif.fairzlrm"))
l3$id = "fairzlrm"

# load task and subset by rows and columns
task = as_task_classif(complete_cases, target = "SUSPECT_ARRESTED_FLAG", positive = "Y", response_type = "prob")
task$set_col_roles("PA_GROUP", "pta")

# run experiment
lrns = list(lrn_rpart, l1, l2, l3)
bmr = benchmark(benchmark_grid(task, lrns, rsmp("cv", folds = 2)))
meas = msrs(c("classif.acc", "fairness.eod"))
bmr$aggregate(meas)[,
                    .(learner_id, classif.acc, fairness.equalized_odds)]

fairness_accuracy_tradeoff(bmr, fairness_measure = msr("fairness.eod"),
                           accuracy_measure = msr("classif.ce")) +
  ggplot2::scale_color_viridis_d("Learner") +
  ggplot2::theme_minimal()

# in the graph low and right is good

# interestingly (on the complete dataset) decision tree performs with high accuracy and the highest fairness
# random forrest performs with even higher accuracy (as expected) but with lower fairness
# EOD and fairzml performs completetly different which is weird because it is an own learner and shouldt be influenced from chaning the learner should it?
# reweight is "the best" method
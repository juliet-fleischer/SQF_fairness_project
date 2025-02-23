theme_set(
  theme_minimal()
)
set.seed(024)
# 1. Fairness metrics ----
## punitive base measures ----
base_mrs_punitive <- list(
  fpr = msr("classif.fpr"),
  tnr = msr("classif.tnr"),
  ppv = msr("classif.ppv"),
  fdr = msr("classif.fdr")
)

# assistive base measures ----
base_mrs_assistive <- list(
  fnr = msr("classif.fnr"),
  tpr = msr("classif.tpr"),
  npv = msr("classif.npv"),
  fomr = msr("classif.fomr")
)

# mixed base measures ----
base_mrs_other <- list(
  acc = msr("classif.acc"),
  auc = msr("classif.auc"),
  bbrier = msr("classif.bbrier")
)

# punitive mlr3 measures ----
fairness_msr_punitive <- msrs(c("fairness.fpr","fairness.tnr","fairness.ppv"))
# assistive mlr3 measures ----
fairness_msr_assistive <- msrs(c("fairness.fnr","fairness.tpr", "fairness.npv",
                                 "fairness.fomr"))
# mixed mlr3 measures ---- 
fairness_msr_other <- msrs(c("fairness.acc", "fairness.cv", "fairness.eod"))

# 2. Data, learner and task ----
data2023_ex_ante <- copy(data2023)
cols_to_remove <- c(
  "OFFICER_EXPLAINED_STOP_FLAG", "OBSERVED_DURATION_MINUTES", "OTHER_PERSON_STOPPED_FLAG",
  "SUMMONS_ISSUED_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG",
  "ASK_FOR_CONSENT_FLG", "CONSENT_GIVEN_FLG", "OTHER_CONTRABAND_FLAG", "WEAPON_FOUND_FLAG"
)
data2023_ex_ante[, (cols_to_remove) := NULL] 

lrn_rf_2023 = lrn("classif.ranger", predict_type = "prob")
lrn_rf_2023$id = "ranger_rf"

# Preprocessing: reweighing
l1 = as_learner(mlr3pipelines::`%>>%`(po("reweighing_wts"), lrn("classif.rpart")))
l1$id = "reweight"

# Postprocessing: Equalized Odds
l2 = as_learner(po("learner_cv", lrn("classif.ranger")) %>>%
                  po("EOd"))
l2$id = "EOd"

# Inprocessing: fair logistic regression
l3 = as_learner(po("collapsefactors") %>>% lrn("classif.fairzlrm"))
l3$id = "fairzlrm"

# define a task
task_arrest_ex_ante <- as_task_classif(data2023_ex_ante, target = "SUSPECT_ARRESTED_FLAG",
                                       positive = "Y", response_type = "prob")
# set the PA
task_arrest_ex_ante$set_col_roles("PA_GROUP", "pta")
# split the data
data2023_split <- partition(task_arrest_ex_ante)


# 2. Fairness Auditing ----
## regular RF ----
lrn_rf_2023$train(task_arrest_ex_ante, data2023_split$train)
preds_ex_ante_2023 <- lrn_rf_2023$predict(task_arrest_ex_ante, data2023_split$test)

p1_rf <- fairness_prediction_density(preds_ex_ante_2023, task = task_arrest_ex_ante) +
  xlim(0, 1) +
  theme(legend.position = "bottom")
p2_rf <- compare_metrics(preds_ex_ante_2023,
                         msrs(c("fairness.tpr", "fairness.eod", "fairness.fpr", "fairness.ppv", "fairness.acc")),
                      task = task_arrest_ex_ante) 

calcGroupwiseMetrics(base_mrs_assistive, task_arrest_ex_ante, preds_ex_ante_2023)
calcGroupwiseMetrics(base_mrs_punitive, task_arrest_ex_ante, preds_ex_ante_2023)
calcGroupwiseMetrics(base_mrs_other, task_arrest_ex_ante, preds_ex_ante_2023)




# 3. Experiment ----
lrns = list(lrn_rf_2023, l1, l2, l3)
bmr = benchmark(benchmark_grid(task_arrest_ex_ante, lrns, rsmp("cv", folds = 5)))
meas = msrs(c("classif.acc", "fairness.eod", "fairness.tpr"))
bmr$aggregate(meas)[, .(learner_id, classif.acc, fairness.equalized_odds)]

p3 <- fairness_accuracy_tradeoff(bmr, fairness_measure = msr("fairness.fpr"),
                           accuracy_measure = msr("classif.ce")) +
  ggplot2::scale_color_viridis_d("Learner")

# # distribution of Y | A
# ggplot(data2023, aes(x = PA_GROUP, fill = SUSPECT_ARRESTED_FLAG)) +
#   geom_bar(position = "fill")
# data2023[, sum(SUSPECT_ARRESTED_FLAG == "Y") / .N, by = PA_GROUP]

# 4. Limitations ----
# estimate the tpr on the target population with the method from Kallus and Zhou
# and compare it to this tpr

# in the graph low and right is good

# interestingly (on the complete dataset) decision tree performs with high accuracy and the highest fairness
# random forrest performs with even higher accuracy (as expected) but with lower fairness
# EOD and fairzml performs completetly different which is weird because it is an own learner and shouldt be influenced from chaning the learner should it?
# reweight is "the best" method
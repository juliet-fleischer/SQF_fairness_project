set.seed(024)
theme_set(
  theme_minimal()
)

# 1. Training + Prediction  ----
# initialize the learner
# lrn_rf_2011 = lrn("classif.ranger", predict_type = "prob")
lrn_rf_2011$id = "ranger_rf_2011"

# create a Task
task_arrest_2011 <-  as_task_classif(data2011,
                                     target = "arstmade", response_type = "prob",
                                     positive = "Y", id = "arrested")
task_arrest_2011$col_roles$pta <- "pa_group"
splits.2011 <- partition(task_arrest_2011)
# lrn_rf_2011$train(task_arrest_2011, row_ids = splits.2011$train)

preds_2011 <- lrn_rf_2011$predict(task_arrest_2011, row_ids = splits.2011$test)
predictions_dt <- as.data.table(preds_2011)
predictions_dt$pa_group <- data2011[splits.2011$test, pa_group]


# 3. Fairness Audit ----
calcGroupwiseMetrics(base_mrs_punitive, task_arrest_2011, preds_2011)
calcGroupwiseMetrics(base_mrs_assistive, task_arrest_2011, preds_2011)
calcGroupwiseMetrics(base_mrs_other, task_arrest_2011, preds_2011)

preds_2011$score(fairness_msr_assistive, task = task_arrest_2011)
preds_2011$score(fairness_msr_punitive, task = task_arrest_2011)
preds_2011$score(fairness_msr_other, task = task_arrest_2011)

p9_rf_2011 <- fairness_prediction_density(preds_2011, task = task_arrest_2011) +
  xlim(0, 1)
p10_rf_2011 <- compare_metrics(preds_2011,
                               msrs(c("fairness.acc", "fairness.eod", "fairness.fpr", "fairness.ppv", "fairness.tpr")),
                task = task_arrest_2011)

## 3.1 Combines 2011 and 2021 analysis ----
p1_combined <- p1_rf / p9_rf_2011
p2_combined <- p2_rf + p10_rf_2011

ggplot(predictions_dt, aes(x = prob.Y, fill = pa_group)) +
  geom_density(alpha = 0.5)
ggplot(predictions_dt, aes(x = prob.N, fill = pa_group)) +
  geom_density(alpha = 0.5)


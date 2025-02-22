set.seed(024)
theme_set(
  theme_minimal()
)

# 1. Training + Prediction  ----
# initialize the learner
# lrn_rf = lrn("classif.ranger", predict_type = "prob")
# lrn_rf$id = "ranger_rf"
lrn_rf_2011 <- readRDS("program/trained_rf_2011.rds")
# Create a Task
task.2011 <-  as_task_classif(data2011, target = "arstmade", response_type = "prob",
                              positive = "Y", id = "arrested")
task.2011$col_roles$pta <- "pa_group"
splits.2011 <- partition(task.2011)
# lrn_rf$train(task.2011, row_ids = splits.2011$train)
predictions.2011 <- lrn_rf_2011$predict(task.2011, row_ids = splits.2011$test)
predictions_dt <- as.data.table(predictions.2011)
predictions_dt$pa_group <- data2011[predictions_dt$row_ids, pa_group]

# 3. Fairness Audit ----
calcGroupwiseMetrics(base_mrs_punitive, task.2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_assistive, task.2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_other, task.2011, predictions.2011)

predictions.2011$score(fairness_msr_assistive, task = task.2011)
predictions.2011$score(fairness_msr_punitive, task = task.2011)
predictions.2011$score(fairness_msr_other, task = task.2011)

p9_rf_2011 <- fairness_prediction_density(predictions.2011, task = task.2011) +
  xlim(0, 0.1) +
  theme_minimal()
p10_rf_2011 <- compare_metrics(predictions.2011,
                msrs(c("fairness.ppv", "fairness.acc", "fairness.eod", "fairness.fpr")),
                task = task.2011) +
  ylim(0, 0.04)

## 3.1 Combines 2011 and 2021 analysis ----
p1_combined <- p1_rf / p9_rf_2011
p2_combined <- p2_rf + p10_rf_2011


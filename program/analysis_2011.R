set.seed(024)
theme_set(
  theme_minimal()
)

# 1. Training + Prediction  ----
data2011_ex_ante <- copy(data2011)
cols_to_keep <- c("timestop", "weekday","month","inout", "crimsusp", "arstmade",
                  "offunif", "radio", "sex", "race", "age", "ht_feet", "ht_inch",
                  "weight", "haircolr", "eyecolor", "build", "city", "sector", "pa_group")
data2011_ex_ante <- data2011_ex_ante[, ..cols_to_keep]
# initialize the learner
# lrn_rf_2011 = lrn("classif.ranger", predict_type = "prob")
lrn_rf_2011$id = "ranger_rf_2011"
# lrn_rf_2011_2011 <- readRDS("program/trained_rf_2011.rds")
# Create a Task
task_arrest_2011 <-  as_task_classif(data2011_ex_ante,
                                     target = "arstmade", response_type = "prob",
                                     positive = "Y", id = "arrested")
task_arrest_2011$col_roles$pta <- "pa_group"
splits.2011 <- partition(task_arrest_2011)
# lrn_rf_2011$train(task_arrest_2011, row_ids = splits.2011$train)

data2011_ex_ante |> 
  group_by(pa_group) |>
  reframe(prop_arrested = mean(arstmade == "Y"))

predictions.2011 <- lrn_rf_2011$predict(task_arrest_2011, row_ids = splits.2011$test)
predictions_dt <- as.data.table(predictions.2011)
predictions_dt$pa_group <- data2011_ex_ante[splits.2011$test, pa_group]

predictions_dt |> 
  group_by(pa_group) |> 
  reframe(avg_score = mean(prob.Y), response_arrested = mean(response == "Y"),
          truth_arrested = mean(truth == "Y"))

# 3. Fairness Audit ----
calcGroupwiseMetrics(base_mrs_punitive, task_arrest_2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_assistive, task_arrest_2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_other, task_arrest_2011, predictions.2011)

predictions.2011$score(fairness_msr_assistive, task = task_arrest_2011)
predictions.2011$score(fairness_msr_punitive, task = task_arrest_2011)
predictions.2011$score(fairness_msr_other, task = task_arrest_2011)

p9_rf_2011 <- fairness_prediction_density(predictions.2011, task = task_arrest_2011) +
  xlim(0, 1)
p10_rf_2011 <- compare_metrics(predictions.2011,
                               msrs(c("fairness.acc", "fairness.eod", "fairness.fpr", "fairness.ppv", "fairness.tpr")),
                task = task_arrest_2011)

## 3.1 Combines 2011 and 2021 analysis ----
p1_combined <- p1_rf / p9_rf_2011
p2_combined <- p2_rf + p10_rf_2011

ggplot(predictions_dt, aes(x = prob.Y, fill = pa_group)) +
  geom_density(alpha = 0.5)
ggplot(predictions_dt, aes(x = prob.N, fill = pa_group)) +
  geom_density(alpha = 0.5)


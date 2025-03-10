set.seed(024)
# 1. Arrest as target ----
data2023_ex_ante <- copy(data2023)
cols_to_remove <- c(
  "OFFICER_EXPLAINED_STOP_FLAG", "OBSERVED_DURATION_MINUTES", "OTHER_PERSON_STOPPED_FLAG",
  "SUMMONS_ISSUED_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG",
  "ASK_FOR_CONSENT_FLG", "CONSENT_GIVEN_FLG", "OTHER_CONTRABAND_FLAG", "WEAPON_FOUND_FLAG"
)
data2023_ex_ante[, (cols_to_remove) := NULL] 

# initilaise random forrest learner
lrn_rf <- lrn("classif.ranger", predict_type = "prob")
# define a task
task_arrest_ex_ante <- as_task_classif(data2023_ex_ante, target = "SUSPECT_ARRESTED_FLAG",
                               positive = "Y", response_type = "prob")
# set the PA
task_arrest_ex_ante$set_col_roles("PA_GROUP", "pta")
# split the data
data2023_split <- partition(task_arrest_ex_ante)
# train the model
lrn_rf$train(task_arrest_ex_ante, data2023_split$train)
preds_arrest <- lrn_rf$predict(task_arrest_ex_ante, data2023_split$test) 
fairness_prediction_density(preds_arrest, task = task_arrest_ex_ante) + theme_minimal()
compare_metrics(preds_arrest,
                msrs(c("fairness.ppv", "fairness.fpr", "fairness.eod", "fairness.acc")),
                task = task_arrest_ex_ante) 
calcGroupwiseMetrics(base_mrs_punitive, task_arrest_ex_ante, preds_arrest)
calcGroupwiseMetrics(base_mrs_assistive, task_arrest_ex_ante, preds_arrest)
calcGroupwiseMetrics(base_mrs_other, task_arrest_ex_ante, preds_arrest)

# combine preds_cpw with the test data
data2023_test <- data2023[data2023_split$test, ]
preds_arrest_df <- as.data.table(preds_arrest)
preds_arrest_df$PA_GROUP <- data2023_test$PA_GROUP

preds_arrest_df |> 
  group_by(PA_GROUP) |>
  reframe(avg_score = mean(prob.Y))

data2023 |> 
  group_by(PA_GROUP) |>
  reframe(prop_arrested = mean(SUSPECT_ARRESTED_FLAG == "Y"))

# 2. CPW as target ----

data2023_ex_ante <- copy(data2023)
cols_to_remove <- c(
  "OFFICER_EXPLAINED_STOP_FLAG", "OBSERVED_DURATION_MINUTES", "OTHER_PERSON_STOPPED_FLAG",
  "SUSPECT_ARRESTED_FLAG", "SUMMONS_ISSUED_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG",
  "ASK_FOR_CONSENT_FLG", "CONSENT_GIVEN_FLG", "OTHER_CONTRABAND_FLAG"
)
data2023_ex_ante[, (cols_to_remove) := NULL] 
lrn_rf <- lrn("classif.ranger", predict_type = "prob")
# define a task
task_cpw <- as_task_classif(data2023_ex_ante, target = "WEAPON_FOUND_FLAG", positive = "Y",
                            response_type = "prob")
# set the PA
task_cpw$set_col_roles("PA_GROUP", "pta")
# split the data
data2023_split <- partition(task_cpw)
# train the model
lrn_rf$train(task_cpw, data2023_split$train)
preds_cpw <- lrn_rf$predict(task_cpw, data2023_split$test)
fairness_prediction_density(preds_cpw, task = task_cpw) + theme_minimal()

# combine preds_cpw with the test data
data2023_test <- data2023[data2023_split$test, ]
preds_arrest <- as.data.table(preds_arrest)
preds_arrest$PA_GROUP <- data2023_test$PA_GROUP

preds_arrest |> 
  group_by(PA_GROUP) |>
  reframe(median_score = median(prob.Y))

data2023 |> 
  group_by(PA_GROUP) |>
  reframe(prop_weapon_found = sum(WEAPON_FOUND_FLAG == "Y"))


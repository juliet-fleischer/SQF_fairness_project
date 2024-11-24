


### --- Missing data imputation --- ###
# create subset of the data
sqf.2023.subset <- subset(sqf.2023, select = c("SUSPECT_ARRESTED_FLAG", protected.a, features))
# impute missing data
imp <- mice(sqf.2023.subset, m = 1)
imputed_data <- complete(imp)


# possible dichotomizations of race
imputed_data <- imputed_data |> 
  mutate(race_group_a = ifelse(SUSPECT_RACE_DESCRIPTION %in% "BLACK", "u", "p"),
         race_group_b = ifelse(SUSPECT_RACE_DESCRIPTION %in% c("WHITE", "ASIAN / PACIFIC ISLANDER",
                                                               "MIDDLE EASTERN/SOUTHWEST ASIAN", "WHITE HISPANIC"), "p", "u"),
         race_group_c = ifelse(SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC"), "u", "p"),
         race_group_d = ifelse(SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "BLACK HISPANIC"), "u", "p")
         )

race.grouping <- c("SUSPECT_RACE_DESCRIPTION", "race_group_a", "race_group_b", "race_group_c", "race_group_d")

### --- Random Forest --- ###
results <- list()
for (g in race.grouping) {
  # remove the other features
  f.to.remove <- setdiff(race.grouping, g)
  imputed_data_g <- imputed_data[, !names(imputed_data) %in% f.to.remove]
  set.seed(513)
  # initialize a classification task
  tsk_sqf <- as_task_classif(imputed_data_g, target = "SUSPECT_ARRESTED_FLAG",
                             positive = "1", id = "STOP_ID")
  # create train train split
  splits <- partition(tsk_sqf)
  # initialize a learner
  p <- ncol(imputed_data_g) - 1
  lrn_rf <- lrn("classif.ranger", mtry = ceiling(p / 2), predict_type = "prob")
  # load accuracy measure
  measures <- msrs(c("classif.acc", "classif.bbrier", "classif.auc"))
  # train learner
  lrn_rf$train(tsk_sqf, row_ids = splits$train)
  # make predictions on test data
  predictions <- lrn_rf$predict(tsk_sqf, row_ids = splits$test)
  scores <- predictions$score(measures)
  results[[g]] <- scores
}

# work with race_group_a model
# remove the other features
g <- "race_group_a"
f.to.remove <- setdiff(race.grouping, g)
imputed_data_g <- imputed_data[, !names(imputed_data) %in% f.to.remove]

set.seed(513)
# initialize a classification task
tsk_sqf <- as_task_classif(imputed_data_g, target = "SUSPECT_ARRESTED_FLAG",
                           positive = "1", id = "STOP_ID")
# specify the PA
tsk_sqf$col_roles$pta <- "race_group_a"
# create train train split
splits <- partition(tsk_sqf)
# initialize a learner
p <- ncol(imputed_data_g) - 1
lrn_rf <- lrn("classif.ranger", mtry = ceiling(p / 2), predict_type = "prob")
# initialize fairness measure
fairness_measures <- msrs("fairness.acc")
# train learner
lrn_rf$train(tsk_sqf, row_ids = splits$train)
# make predictions on test data
predictions <- lrn_rf$predict(tsk_sqf, row_ids = splits$test)



# , "fairness.fnr",
# "fairness.fpr", "fairness.tnr", "fairness.tpr",
# "fairness.npv", "fairness.ppv", "fairness.fomr",
# "fairness.eod")
                          

predictions$confusion

predictions$score(fairness_measures, task = tsk_sqf)
predictions$score(m1, task = tsk_sqf)
predictions$score(m2, task = tsk_sqf)
predictions$score(m3, task = tsk_sqf)
# this could be because there is more training data for black people
# while white people are extremely underrepresented which in itself is
# the representation of unfairness




# Apply groupwise_metrics to each measure
m1 <- groupwise_metrics(msr("classif.acc"), tsk_sqf)
m2 <- groupwise_metrics(msr("classif.auc"), tsk_sqf)
m3 <- groupwise_metrics(msr("classif.bbrier"), tsk_sqf)

# Inspect the result
print(groupwise_measures)

# pre, in, post processing illustrieren in Verbindung mit den Maßen


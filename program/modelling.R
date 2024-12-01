set.seed(513)
### --- Random Forest --- ###

# possible dichotomizations of race
imputed_data <- imputed_data |> 
  mutate(race_group_a = ifelse(SUSPECT_RACE_DESCRIPTION %in% "BLACK", "u", "p"),
         race_group_b = ifelse(SUSPECT_RACE_DESCRIPTION %in% c("WHITE", "ASIAN / PACIFIC ISLANDER",
                                                               "MIDDLE EASTERN/SOUTHWEST ASIAN", "WHITE HISPANIC"), "p", "u"),
         race_group_c = ifelse(SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC"), "u", "p"),
         race_group_d = ifelse(SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "BLACK HISPANIC"), "u", "p")
         )

race.grouping <- c("SUSPECT_RACE_DESCRIPTION", "race_group_a", "race_group_b", "race_group_c", "race_group_d")


### --- univariate feature selection --- ###
imputed_data_all <- imputed_data[, names(imputed_data) != "SUSPECT_RACE_DESCRIPTION"]

# initialize a classification task
tsk_sqf <- as_task_classif(imputed_data_all, target = "SUSPECT_ARRESTED_FLAG",
                           positive = "1", id = "STOP_ID")
# specify the PA
tsk_sqf$col_roles$pta <- "race_group_d"

# create train train split
splits <- partition(tsk_sqf)

# initialize a learner
p <- ncol(imputed_data_all) - 1
lrn_rf <- lrn("classif.ranger", mtry = ceiling(p / 2), predict_type = "prob", importance = "impurity")

# goal is to see which of the race groupings gets the highest feature importance
flt_importane <- flt("importance", learner = lrn_rf)
flt_importane$calculate(tsk_sqf)
as.data.table(flt_importane)


# goal is to see which of the race groupings lead to the best performance
classic.mrs <- list()
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
  classic.mrs[[g]] <- scores
}
classic.mrs <- as.data.frame(do.call(rbind, results))
which.max(classic.mrs$classif.acc)
which.min(classic.mrs$classif.bbrier)
which.max(classic.mrs$classif.auc)



### --- model training for [PA] as PA --- ###
# based on the previous analysis decide on a race grouping and train the final model
# initialize a classification task


if( PA == "SUSPECT_SEX") {
  imputed_data_g <- imputed_data
} else {
  g <- "race_group_d"
  f.to.remove <- setdiff(race.grouping, g)
  imputed_data_g <- imputed_data[, !names(imputed_data) %in% f.to.remove]
  imputed_data_g[[g]]<- factor(imputed_data_g[[g]])
}
tsk_sqf <- as_task_classif(imputed_data_g, target = target,
                           positive = "1", id = "STOP_ID")
if( PA == "SUSPECT_SEX") {
  tsk_sqf$col_roles$pta <- PA
} else {
  tsk_sqf$col_roles$pta <- g
}

# create train train split
splits <- partition(tsk_sqf)

# initialize a learner
p <- ncol(imputed_data_g) - 1
lrn_rf <- lrn("classif.ranger", mtry = ceiling(p / 2), predict_type = "prob", importance = "impurity")

# train learner
lrn_rf$train(tsk_sqf, row_ids = splits$train)
# make predictions on test data
predictions <- lrn_rf$predict(tsk_sqf, row_ids = splits$test)


measures <- msrs(c("classif.acc", "classif.bbrier", "classif.auc"))
predictions$score(measures, task = tsk_sqf)

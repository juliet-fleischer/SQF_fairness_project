### --- Random Forest --- ###
# initialize a learner
p <- ncol(sqf) - 1
lrn_rf_missing <- lrn("classif.random_forest_weka", predict_type = "prob") # can handle missings
# construct a resampling strategy
cv5 <- rsmp("cv", folds = 5)
# set performance measures
measures <- msrs(c("classif.acc", "classif.bbrier", "classif.auc"))

### ARRESTED full dataset ###
tsk_arrested_full <- as_task_classif(sqf, target = "SUSPECT_ARRESTED_FLAG", response_type = "prob",
                                     positive = "Y", id = "arrested w/ missing")
# set PA
tsk_arrested_full$col_roles$pta <- "SUSPECT_RACE_DESCRIPTION"
# split
splits_arrested_full <- partition(tsk_arrested_full)
# train
lrn_rf_missing$train(tsk_arrested_full, row_ids = splits_arrested_full$train)
# predict
predictions_arrested_full <- lrn_rf_missing$predict(tsk_arrested_full, row_ids = splits_arrested_full$test)
predictions_dt <- as.data.table(predictions_arrested_full)
predictions_dt$SUSPECT_RACE_DESCRIPTION <- sqf[predictions_dt$row_ids, SUSPECT_RACE_DESCRIPTION]
# create a dichotomouse race column for showcasing the definitions in a
# classical case in the presentation only
unprivileged <- c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC")
predictions_dt[, PA_GROUP := ifelse(SUSPECT_RACE_DESCRIPTION %in% unprivileged, "unprivileged", "privileged")]
predictions_dt$PA_GROUP <- factor(predictions_dt$PA_GROUP, levels = c("privileged", "unprivileged"), labels = c(0,1))


### --- ARRESTED Complete Case Analysis --- ###
# initialize a classification task
tsk_arrested <- as_task_classif(sqf.filtered[, -"STOP_ID"], target = "SUSPECT_ARRESTED_FLAG",
                                positive = "1", id = "arrest")
# specify the PA
tsk_arrested$col_roles$pta <- "SUSPECT_SEX"
# create train train split
splits_arrested <- partition(tsk_arrested)
# train
lrn_rf$train(tsk_arrested, row_ids = splits_arrested$train)
# make predictions on test data
predictions_arrested <- lrn_rf$predict(tsk_arrested, row_ids = splits_arrested$test)


predictions_dt <- as.data.table(predictions_arrested)
pa.data <- imputed_data_arrested[splits_arrested$test, .(SUSPECT_SEX)]
predictions_dt <- cbind(pa.data, predictions_dt)

### --- frisked specific --- ###
# remove ID column for training
imputed_data_frisked <- imputed_data_frisked[, -1]
# initialize a classification task
tsk_frisk <- as_task_classif(imputed_data_frisked, target = "FRISKED_FLAG",
                           positive = "1", id = "frisk")
# specify the PA
tsk_frisk$col_roles$pta <- "SUSPECT_SEX"
# create train train split
splits_frisk <- partition(tsk_frisk)

# train
lrn_rf$train(tsk_frisk, row_ids = splits_frisk$train)
# make predictions on test data
predictions_frisk <- lrn_rf$predict(tsk_frisk, row_ids = splits_frisk$test)



# get the PAs for the test rows

# # resample for GE estimation
# rr <- resample(tsk_arrest, lrn_rf, cv5)
# rr$aggregate(measures)

### --- ARRESTED with RACE as PA --- ###
task_arrested <- as_task_classif(sqf.filtered, target = "SUSPECT_ARRESTED_FLAG",
                           positive = "1", id = "arrest")
task_arrested$col_roles$pta <- "race_group"
splits_arrested <- partition(task_arrested)
lrn_rf$train(task_arrested_2, row_ids = splits_arrested_2$train)
predictions_arrested_2 <- lrn_rf$predict(task_arrested_2, row_ids = splits_arrested_2$test)

### --- searched specific --- ###
imputed_data_searched <- imputed_data_searched[, -1]
# initialize a classification task
tsk_searched <- as_task_classif(imputed_data_searched, target = "SEARCHED_FLAG",
                           positive = "1", id = "search")
# specify the PA
tsk_searched$col_roles$pta <- "SUSPECT_SEX"
# create train train split
splits_searched <- partition(tsk_searched)
# train
lrn_rf$train(tsk_searched, row_ids = splits_searched$train)
# make predictions on test data
predictions_searched <- lrn_rf$predict(tsk_searched, row_ids = splits_searched$test)


    
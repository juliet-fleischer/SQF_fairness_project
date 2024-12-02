### --- Random Forest --- ###

# initialize a learner
p <- ncol(imputed_data_frisked) - 1
lrn_rf <- lrn("classif.ranger", mtry = ceiling(p / 2), predict_type = "prob", importance = "impurity")
# construct a resampling strategy
cv5 <- rsmp("cv", folds = 5)
# set performance measures
measures <- msrs(c("classif.acc", "classif.bbrier", "classif.auc"))


### --- frisked specific --- ###
# remove ID column for training
imputed_data_frisked <- imputed_data_frisked[, -1]
# initialize a classification task
tsk_frisk <- as_task_classif(imputed_data_frisked, target = "FRISKED_FLAG",
                           positive = "1", id = "frisk")
# specify the PA
tsk_frisk$col_roles$pta <- "SUSPECT_SEX"
# create train train split
splits <- partition(tsk_frisk)



# train
lrn_rf$train(tsk_frisk, row_ids = splits$train)
# make predictions on test data
predictions <- lrn_rf$predict(tsk_frisk, row_ids = splits$test)

### --- arrested specific --- ###
# remove ID column for training
imputed_data_arrested <- imputed_data_full[, -1]
# initialize a classification task
tsk_arrest <- as_task_classif(imputed_data_arrested, target = "SUSPECT_ARRESTED_FLAG",
                           positive = "1", id = "arrest")
# specify the PA
tsk_arrest$col_roles$pta <- "SUSPECT_SEX"
# create train train split
splits <- partition(tsk_arrest)

# resample for GE estimation
rr <- resample(tsk_arrest, lrn_rf, cv5)
rr$aggregate(measures)

# train
lrn_rf$train(tsk_arrest, row_ids = splits$train)
# make predictions on test data
predictions <- lrn_rf$predict(tsk_arrest, row_ids = splits$test)



# specifyTask <- function(data, target, PA, id) {
#   data <- data[, -1]
#   tsk <- as_task_classif(data, target = target, positive = "1", id = id)
#   tsk$col_roles$pta <- PA
#   splits <- partition(tsk)
# }



# 
# # goal is to see which of the race groupings gets the highest feature importance
# flt_importane <- flt("importance", learner = lrn_rf)
# flt_importane$calculate(tsk_sqf)
# as.data.table(flt_importane)
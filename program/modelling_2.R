

### --- alternative classifier --- ###


# train model with mlr3 and tree learner
set.seed(103)
tsk_classif <- as_task_classif(sqf.complete.2023, target = "SUSPECT_ARRESTED_FLAG", positive = "Y")
lrn_rpart <- lrn("classif.rpart", predict_type = "prob")
measure <- msr("classif.auc")
splits <- partition(tsk_classif, ratio = 0.9)

# train
lrn_rpart$train(tsk_classif, splits$train)

# predict
prediction <- lrn_rpart$predict(tsk_classif, splits$test)

# evaluate
prediction$confusion
prediction$score(measure)

autoplot(prediction, type = "roc")


# with cv
# define CV
cv5 <- rsmp("cv", folds = 5)
# perform CV
rr <- resample(tsk_classif, lrn_rpart, cv5)
rr
# individual acc of each fold
acc <- rr$score(measure)
acc[, .(iteration, classif.auc)]
# aggregated accuracy
rr$aggregate(measure)

# get the predictions of each fold
rrp <- rr$predictions()
rrp1 <- rrp[1]

# this doesn't make sense I don't do hyperparameter tuning here...




# fit a LASSO logistic regression for SUSPECT_ARRESTED_FLAG for variable selecti


### --- logistic regression --- ### 
# leave 10 % out for testing the fairness definitions
set.seed(103)
idx.test <- sample(1:nrow(sqf.complete.2023), ceiling(0.1 * nrow(sqf.complete.2023)))
test.set <- sqf.complete.2023[idx.test, ]
train.set <- sqf.complete.2023[-idx.test, ]
# create row_idx column to double check
# test.set$row_idx <- idx.test

other.targets <- setdiff(targets, "SUSPECT_ARRESTED_FLAG")
arrest.model <- glm(SUSPECT_ARRESTED_FLAG ~ .,
                    data = train.set[, !..other.targets, with = FALSE], 
                    family = "binomial")
summary(arrest.model)
predicted <- predict(arrest.model, test.set, type = "response")


rocobj <- roc(test.set$SUSPECT_ARRESTED_FLAG, predicted)
auc <- round(auc(test.set$SUSPECT_ARRESTED_FLAG, predicted), 2)
ggroc(rocobj, colour = 'steelblue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  theme_minimal()

coords(rocobj, "best", ret = c("threshold", "accuracy", "sensitivity", "specificity"))

# hard label classifier based on optimal threshold or 0.5?
predicted.dt <- data.table(
  STOP_ID = test.set$STOP_ID,
  # row_ID = test.set$row_idx,
  truth = test.set$SUSPECT_ARRESTED_FLAG,
  response_opt = as.factor(ifelse(predicted > coords(rocobj, "best")$threshold, "Y", "N")),
  response_05 = as.factor(ifelse(predicted > 0.5, "Y", "N")),
  prob = predicted,
  age = test.set$SUSPECT_REPORTED_AGE,
  location = test.set$STOP_LOCATION_BOR,
  sex = test.set$SUSPECT_SEX,
  race = test.set$SUSPECT_RACE_DESCRIPTION
)



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


### --- logistic regression --- ### 
# leave 10 % out for testing the fairness definitions
set.seed(103)
idx.test <- sample(1:nrow(cpw.data), ceiling(0.1 * nrow(cpw.data)))
test.set <- cpw.data[idx.test, ]
train.set <- cpw.data[-idx.test, ]
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
test.set[, `:=`(
  prob = predicted,
  response = ifelse(predicted > coords(rocobj, "best")$threshold, 1, 0)
)]
test.set <- test.set |> 
  select(STOP_ID, SUSPECT_ARRESTED_FLAG, prob, response, everything())


### --- logistic regression with group lasso --- ###


# logistig regression with group lasso penalties

form.basic <-  WEAPON_FOUND_FLAG ~ 
  p(SUSPECT_REPORTED_AGE, pen = "lasso") +
  p(STOP_LOCATION_BORO_NAME, pen = "grouplasso") +
  SUSPECT_SEX +
  p(SUSPECT_RACE_DESCRIPTION, pen = "grouplasso") +
  LOCATION_IN_OUT_CODE +
  STOP_WAS_INITIATED +
  p(MONTH2, pen = "grouplasso") +
  p(DAY2, pen = "grouplasso") +
  STOP_DURATION_MINUTES + OFFICER_EXPLAINED_STOP_FLAG +
  OFFICER_IN_UNIFORM_FLAG + ASK_FOR_CONSENT_FLG + CONSENT_GIVEN_FLG +
  SUSPECT_HEIGHT + SUSPECT_WEIGHT +
  p(SUSPECT_BODY_BUILD_TYPE, pen = "grouplasso") +
  p(SUSPECT_EYE_COLOR, pen = "grouplasso") +
  p(SUSPECT_HAIR_COLOR, pen = "grouplasso")
# p(STOP_LOCATION_PRECINCT, pen = "grouplasso")

model_grouplasso_cv <- glmsmurf(formula = form.basic,
                                family = "binomial", data = train.set,
                                lambda = "cv.mse")
lasso <- model_grouplasso_cv$lambda
model_grouplasso <- glmsmurf(formula = form.basic,
                             family = "binomial", data = train.set,
                             lambda = lasso)
summary(model_grouplasso)

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


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


### --- Missing data imputation --- ###
# create subset of the data
sqf.2023.subset <- subset(sqf.2023, select = c("SUSPECT_ARRESTED_FLAG", protected.a, features))
# impute missing data
imp <- mice(sqf.2023.subset, m = 1)
imputed_data <- complete(imp)

# do an insensible and weird dichotomization of race
imputed_data <- imputed_data |> 
  mutate(race_group = case_when(
    SUSPECT_RACE_DESCRIPTION %in% c("WHITE", "ASIAN / PACIFIC ISLANDER") ~ "p",
    .default = "u"
  ))


### --- Random Forest --- ###
set.seed(513)

# initialize a classification task
tsk_sqf <- as_task_classif(imputed_data, target = "SUSPECT_ARRESTED_FLAG",
                           positive = "1", id = "STOP_ID")
# specify the PA
tsk_sqf$col_roles$pta <- "race_group"
# create train train split
splits <- partition(tsk_sqf)
# initialize a learner
p <- ncol(imputed_data) - 1
lrn_rf <- lrn("classif.ranger", mtry = ceiling(p / 2), predict_type = "prob")
# load accuracy measure
measures <- msrs(c("classif.acc", "classif.bbrier"))
# initialize fairness measure
fairness_measures <- msrs(c("fairness.acc", "fairness.fnr",
                            "fairness.fpr", "fairness.tnr", "fairness.tpr",
                            "fairness.npv", "fairness.ppv", "fairness.fomr",
                            "fairness.eod"))
# train learner
lrn_rf$train(tsk_sqf, row_ids = splits$train)
# make predictions on test data
predictions <- lrn_rf$predict(tsk_sqf, row_ids = splits$test)
predictions$score(measures)
predictions$confusion

predictions$score(fairness_measures, task = tsk_sqf)




# pre, in, post processing illustrieren in Verbindung mit den Maßen



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
test.set[, `:=`(
  prob = predicted,
  response = ifelse(predicted > coords(rocobj, "best")$threshold, 1, 0)
)]
test.set <- test.set |> 
  select(STOP_ID, SUSPECT_ARRESTED_FLAG, prob, response, everything())

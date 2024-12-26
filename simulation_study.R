# recreate the simulation study
simulation.data <- copy(sqf)
# 1 create a column that indicated whether a person was stopped or not
simulation.data$SUSPECT_RACE_DESCRIPTION <- NULL
simulation.data <- simulation.data |> 
  filter(race_group %in% c("BLACK", "WHITE"))
simulation.data$race_group <- droplevels(simulation.data$race_group)

# split data into two random halves
idx <- sample(1:nrow(simulation.data), size = nrow(simulation.data)/2)
sample1 <- simulation.data[idx,]
sample2 <- simulation.data[-idx,]
sample2$SUSPECT_ARRESTED_FLAG <- NULL

# train a random forrest on sample1 predicting arrest
# initialize RF learner that handles missing values
lrn_rf_missing <- lrn("classif.random_forest_weka", predict_type = "prob")
# define the task
tsk_arrested_sample1 <- as_task_classif(sample1, target = "SUSPECT_ARRESTED_FLAG",
                                      positive = "Y", id = "arrested w/ missing")
# train on the full sample1
lrn_rf_missing$train(tsk_arrested_sample1)


# predict arrest prob on sample2
pred_arrested_sample2 <- lrn_rf_missing$predict_newdata(sample2)
# convert to data.table
pred_arrested_sample2 <- as.data.table(pred_arrested_sample2)
# combine with sample2
sample2 <- cbind(sample2, pred_arrested_sample2)

sample2 |> 
  group_by(race_group) |> 
  summarise(mean_arrest_prob = mean(prob.Y))

# set race specific thresholds to predict SUSPECT_STOPPED
# keep the threshold for white and middle eastern constant at c4 = 0.25

cw <- rep(0.25, 10)
cb <- seq(from = 0.35, to = 0, length.out = 10)
tau <- cw-cb

test.dat <- copy(sample2)
test.dat$race_group <- ifelse(test.dat$race_group == "BLACK", 1, 0)
# set baseline level to be stopped for white people
test.dat$c0 <- median(test.dat$prob.Y)
# calculate the thresholds
threshold.list <- lapply(tau, function(t) test.dat$c0 - test.dat$race_group * t)
#
threshold.cols <- paste0("tau", seq_along(threshold.list))
test.dat[, (threshold.cols) := lapply(threshold.list, function(l) { ifelse(prob.Y > l, 1, 0) } )]

sapply(threshold.cols, function(col) {
  list(
    overall = mean(test.dat[[col]]),
    black = mean(test.dat[[col]][test.dat$race_group == 1]),
    white = mean(test.dat[[col]][test.dat$race_group == 0])
  )
})


results <- lapply(threshold.cols, function(tau_level) {
  rm.tau <- setdiff(threshold.cols, tau_level)
  test.dat.tau <- test.dat[, !..rm.tau, with = FALSE]
  
  # Clean up columns
  test.dat.tau[, `:=`(row_ids = NULL, prob.Y = NULL, prob.N = NULL, response = NULL, c0 = NULL)]
  
  # Define task
  tsk_tau <- as_task_classif(test.dat.tau, target = "truth", positive = "Y", id = tau_level)
  
  # Train and predict
  lrn_rf_missing$train(tsk_tau)
  pred_tau <- lrn_rf_missing$predict(tsk_tau)
  pred_tau <- as.data.table(pred_tau)
  test.dat.tau[, `:=`(response = pred_tau$response,
                      prob.Y = pred_tau$prob.Y)]

  # calculate the top 50% threshhold (the overall median of being predicted arrested)
  threshold_50 <- median(test.dat.tau$prob.Y)
  
  # Subset individuals in the top 50%
  top_50 <- test.dat.tau[prob.Y > threshold_50]
  
  # Calculate fraction of African Americans
  mean(top_50$race_group == 1, na.rm = TRUE)
})
names(results) <- threshold.cols
results



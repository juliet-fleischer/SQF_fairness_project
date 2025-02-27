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
# sample2$SUSPECT_ARRESTED_FLAG <- NULL

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
sample2$truth <- NULL


results <- lapply(seq_along(stopped_cols), function(i) {
  tau_level <- stopped_cols[i]

  # Fix total number of stops at 50%
  num_stopped <- floor(0.5 * nrow(test.dat))
  
  # Calculate the number of Black and White stops
  num_black <- floor(0.8 * num_stopped + (i - 1) * 0.015 * num_stopped)  # From 80% to 95%
  num_white <- num_stopped - num_black

  # Adjust thresholds dynamically
  c1 <- quantile(test.dat$prob.Y[test.dat$race_group == 1], probs = 1 - num_black / sum(test.dat$race_group == 1))
  c0 <- quantile(test.dat$prob.Y[test.dat$race_group == 0], probs = 1 - num_white / sum(test.dat$race_group == 0))
  
  # Apply thresholds to determine who is stopped
  test.dat[, (tau_level) := ifelse((race_group == 1 & prob.Y > c1) | 
                                     (race_group == 0 & prob.Y > c0), 1, 0)]
  
  # Subset to stopped individuals for training
  train_data <- test.dat[get(tau_level) == 1]
  
  # Clean up columns
  train_data[, `:=`(row_ids = NULL, prob.Y = NULL, prob.N = NULL, response = NULL)]
  
  # Define task and train the model
  tsk_tau <- as_task_classif(train_data, target = "SUSPECT_ARRESTED_FLAG", positive = "Y", id = tau_level)
  lrn_rf_missing$train(tsk_tau)
  
  # Predict on the entire dataset
  pred_tau <- lrn_rf_missing$predict_newdata(test.dat)
  pred_tau <- as.data.table(pred_tau)
  
  # Combine predictions with the dataset
  test.dat[, `:=`(response = pred_tau$response,
                  prob.Y = pred_tau$prob.Y)]
  
  # Calculate the top 50% threshold (median of predicted probabilities across entire population)
  threshold_50 <- median(test.dat$prob.Y)
  
  # Subset individuals in the top 50% and calculate fraction of Black individuals
  top_50 <- test.dat[prob.Y > threshold_50]
  mean(top_50$race_group == 1, na.rm = TRUE)
})
names(results) <- stopped_cols
results
results.df <- do.call(rbind, results) |> data.table()
results.df$tau <- target.tau
ggplot(results.df, aes(x = tau, y = V1)) +
  geom_point() +
  labs(title = "Fraction of Black Individuals in Top 50% of Predicted Probabilities",
       x = "Tau Level", y = "Fraction of Black Individuals") +
  theme_minimal()


# recreate the simulation study

# 1 create a column that indicated whether a person was stopped or not
simulation.data <- copy(sqf[, SUSPECT_STOPPED := 1])
simulation.data$SUSPECT_RACE_DESCRIPTION <- NULL

# split data into two random halves
idx <- sample(1:nrow(sqf), size = nrow(sqf)/2)
sample1 <- simulation.data[idx,]
sample2 <- simulation.data[-idx,]

# train a random forrest on sample1 predicting arrest
# Allocate more memory to JVM
options(java.parameters = "-Xmx10g") # Allocate 8 GB of memory (adjust as needed)

# Load necessary libraries
library(rJava)
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
sample2$SUSPECT_ARRESTED_FLAG <- NULL

sample2 |> 
  group_by(race_group) |> 
  summarise(mean_arrest_prob = mean(prob.Y))

# set race specific thresholds to predict SUSPECT_STOPPED
# 
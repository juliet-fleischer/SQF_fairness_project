# input
## base_mrs: list of evaluation measures, implemented in mlr3 (see mlr3::msr() for a list)
## task: a classification or regression task implemented in mlr3/ defined as mlr3 task
# output
# given metrics for each subgroup of the PA
# This function calculated the common perfromance metrics defined in mlr3
# for each subgroup of the PA. This is useful since the implemented fairness
# metrics in mlr3.fairness package only return the absolute difference
# in performance between the subgroups. But you cannot see towards which group
# the algorithm is biased.

# example use

calcGroupwiseMetrics <- function(base_mrs, task, predictions) {
  # assert whether base_mrs is a list containing measure mlr3 objects, or a single mlr3 measure
  assert_task(task)
  # Create groupwise metrics
  groupwise_measures <- lapply(base_mrs, function(b) groupwise_metrics(b, task = task))
  # Compute scores for each groupwise metric
  groupwise_results <- lapply(groupwise_measures, function(m) predictions$score(m, task))
  # Combine results into a data frame
  groupwise_results_df <- do.call(cbind, groupwise_results)
  # Inspect results
  print(groupwise_results_df)
}

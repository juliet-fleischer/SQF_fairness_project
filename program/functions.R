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


# 
# getFairnessAudit <- function(prediction, task) {
#   # initialize results list
#   res <- list()
#   pred_test <- prediction
#   res1 <- pred_test$score(fairness_msr_other, task = task)
#   res2 <- pred_test$score(fairness_msr_punitive, task = task)
#   res3 <- pred_test$score(fairness_msr_assistive, task = task)
#   list(
#     predictions = pred_test,
#     fairness_metrics = data.frame(
#       Metric = c(names(fairness_msr_other), names(fairness_msr_punitive), names(fairness_msr_assistive)),
#       Value = c(res1, res2, res3)
#     )
#   )
# }

# For the residual unfairness analysis
## function to estimate the error rates in the target population
estimateRate <- function(data, groupVal, truthVal, responseVal) {
  # Numerator: sum of weights for (group, truth, response)
  num <- data |>
    dplyr::filter(
      PA_GROUP == groupVal,
      truth == truthVal,
      response == responseVal
    ) |>
    dplyr::summarise(num = sum(weight)) |>
    dplyr::pull(num)
  
  # Denominator: sum of weights for (group, truth)
  den <- data |>
    dplyr::filter(
      PA_GROUP == groupVal,
      truth == truthVal
    ) |>
    dplyr::summarise(den = sum(weight)) |>
    dplyr::pull(den)
  
  # Safeguard against dividing by zero
  if (den == 0) {
    return(NA_real_)
  }
  
  rate <- num / den
  return(rate)
}

getErrorRates <- function(data, groupA, groupB) {
  # TPR for each group
  tprA <- estimateRate(data, groupA, truthVal = "Y", responseVal = "Y")
  tprB <- estimateRate(data, groupB, truthVal = "Y", responseVal = "Y")
  
  # FPR for each group
  fprA <- estimateRate(data, groupA, truthVal = "N", responseVal = "Y")
  fprB <- estimateRate(data, groupB, truthVal = "N", responseVal = "Y")
  
  # FNR for each group
  fnrA <- 1 - tprA
  fnrB <- 1 - tprB
  
  # TNR for each group
  tnrA <- 1 - fprA
  tnrB <- 1 - fprB
  
  # Return a tidy summary
  dplyr::tibble(
    group = c(groupA, groupB),
    TPR   = c(tprA, tprB),
    FPR   = c(fprA, fprB),
    FNR   = c(fnrA, fnrB),
    TNR   = c(tnrA, tnrB)
  )
}

# specifically for results tables from the fairness audit
formatResultTable <- function(data) {
  data_t <- as.data.frame(t(data))
  data_t$metric<- rownames(data_t)
  names(data_t) <- c("PoC", "White", "Metric")
  data_t <- data_t |> 
    select(Metric, PoC, White)
  return(data_t)
}
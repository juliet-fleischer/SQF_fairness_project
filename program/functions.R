#' Calculate Groupwise Fairness Metrics
#'
#' This function calculates multiple groupwise fairness metrics for a given task and its predictions.
#'
#' @param base_mrs A list of evaluation metrics, implemented in mlr3 (see [mlr3::msr()] for available metrics).
#' @param task An `mlr3` classification or regression task.
#' @param predictions A `mlr3` prediction object.
#' @return A data frame with the calculated metrics for each subgroup.
#' @export
calcGroupwiseMetrics <- function(base_mrs, task, predictions) {
  # Ensure task is valid
  assert_task(task)
  
  # Create groupwise metrics
  groupwise_measures <- lapply(base_mrs, function(b) groupwise_metrics(b, task = task))
  
  # Compute scores for each groupwise metric
  groupwise_results <- lapply(groupwise_measures, function(m) predictions$score(m, task))
  
  # Combine results into a data frame
  groupwise_results_df <- do.call(cbind, groupwise_results)
  
  # Inspect results
  print(groupwise_results_df)
  
  # Ensure function returns the data frame
  return(groupwise_results_df)
}


#' Estimate Error Rate for a Given Group
#'
#' A helper function that estimates the error rate in a target population 
#' based on group membership, ground truth, and response.
#'
#' @param data A data frame containing `PA_GROUP`, `truth`, `response`, and `weight` columns.
#' @param groupVal The group identifier.
#' @param truthVal The ground truth value (`"Y"` or `"N"`).
#' @param responseVal The predicted response value (`"Y"` or `"N"`).
#' @return A numeric value representing the estimated rate, or `NA_real_` if undefined.
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

#' Compute Error Rates for Two Groups
#'
#' Computes True Positive Rate (TPR), False Positive Rate (FPR),
#' False Negative Rate (FNR), and True Negative Rate (TNR) 
#' for two specified groups.
#'
#' @param data A data frame containing `PA_GROUP`, `truth`, `response`, and `weight` columns.
#' @param groupA The first group identifier.
#' @param groupB The second group identifier.
#' @return A tibble summarizing TPR, FPR, FNR, and TNR for both groups.
#' @export
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

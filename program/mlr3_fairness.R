# test mlr3 fairness metrics

# initialize fairness measure
# punitive
fairness_msr_punitive <- msrs(c("fairness.fpr","fairness.tnr","fairness.ppv"))
# "fairness.fdr" (not implemented but should be as equivalent for fairness.fomr)
predictions$score(fairness_msr_punitive, task = tsk_frisk)

# assistive
fairness_msr_assistive <- msrs(c("fairness.fnr","fairness.tpr", "fairness.npv",
                                "fairness.fomr"))
predictions$score(fairness_msr_assistive, task = tsk_frisk)

# in between
fairness_mrs_other <- msrs(c("fairness.acc", "fairness.cv", "fairness.eod"))
predictions$score(fairness_mrs_other, task = tsk_frisk)


# Defines punitive base measures
base_mrs_punitive <- list(
  fpr = msr("classif.fpr"),
  tnr = msr("classif.tnr"),
  ppv = msr("classif.ppv"),
  fdr = msr("classif.fdr")
)

# Define multiple base measures
base_mrs_assistive <- list(
  fnr = msr("classif.fnr"),
  tpr = msr("classif.tpr"),
  npv = msr("classif.npv"),
  fomr = msr("classif.fomr")
)

# define mixed base measures
base_mrs_other <- list(
  acc = msr("classif.acc"),
  auc = msr("classif.auc"),
  bbrier = msr("classif.bbrier")
)

calcGroupwiseMetrics(base_mrs_punitive, tsk_frisk, predictions_frisk)
calcGroupwiseMetrics(base_mrs_assistive, tsk_frisk, predictions_frisk)
calcGroupwiseMetrics(base_mrs_other, tsk_frisk, predictions_frisk)

calcGroupwiseMetrics(base_mrs_punitive, tsk_arrest, predictions_arrested)
calcGroupwiseMetrics(base_mrs_assistive, tsk_arrest, predictions_arrested)
calcGroupwiseMetrics(base_mrs_other, tsk_arrest, predictions_arrested)
# 
# fairness_prediction_density(predictions, task = tsk_sqf)
# compare_metrics(predictions, fairness_msr_punitive, task = tsk_sqf)
# compare_metrics(predictions, fairness_msr_assistive, task = tsk_sqf)
# compare_metrics(predictions, fairness_mrs_other, task = tsk_sqf)



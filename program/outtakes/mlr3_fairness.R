# test mlr3 fairness metrics

# initialize fairness measure
# punitive
fairness_msr_punitive <- msrs(c("fairness.fpr","fairness.tnr","fairness.ppv"))
# "fairness.fdr" (not implemented but should be as equivalent for fairness.fomr)
predictions_arrested$score(fairness_msr_punitive, task = tsk_frisk)

# assistive
fairness_msr_assistive <- msrs(c("fairness.fnr","fairness.tpr", "fairness.npv",
                                "fairness.fomr"))
predictions$score(fairness_msr_assistive, task = tsk_frisk)

# in between
fairness_msr_other <- msrs(c("fairness.acc", "fairness.cv", "fairness.eod"))
predictions$score(fairness_mrs_other, task = task)



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

# punitive measures
metrics_list_1 <- list(
  fpr = mlr3measures::acc,
  tnr = mlr3measures::tnr,
  ppv = mlr3measures::ppv,
  fdr = mlr3measures::fdr,
  fnr = mlr3measures::fnr,
  tpr = mlr3measures::tpr,
  npv = mlr3measures::npv,
  fomr = mlr3measures::fomr
)
sapply(metrics_list_1, \(f) f(predictions_dt$truth, predictions_dt$response, positive = "Y"))

# apply the whole list of metrics at once to the predictions_dt data table grouped by PA_GROUP
predictions_dt[, lapply(metrics_list_1, \(f) f(truth, response, positive = "Y")), by = PA_GROUP]

# mixed measures
acc(truth, response)

calcGroupwiseMetrics(base_mrs_punitive, tsk_arrested_full, predictions_arrested_full)
calcGroupwiseMetrics(base_mrs_assistive, tsk_arrested_full, predictions_arrested_full)
calcGroupwiseMetrics(base_mrs_other, tsk_arrested_full, predictions_arrested_full)

# Independence: 
stat.parity <- predictions_dt[, .(pred_arrest = sum(response == "Y") / .N), by = "SUSPECT_RACE_DESCRIPTION"][order(pred_arrest)]
stat.parity.binary <- predictions_dt[, .(pred_arrest = sum(response == "Y") / .N), by = "PA_GROUP"][order(pred_arrest)]
calcStatParity <- function(data, group, pos.label) {
  data[, .(pred_arrest = sum(response == pos.label) / .N), by = group][order(pred_arrest)]
}


# Score-based metrics - Separation:
# balance for positive/ negative class class
blnc.f.pos.clss <- predictions_dt[, .(avg_score_P = mean(prob.Y)), by = c("truth", "SUSPECT_RACE_DESCRIPTION")][truth == "Y"][order(avg_score_P)]
blnc.f.neg.clss <- predictions_dt[, .(avg_score_N = mean(prob.N)), by = c("truth", "SUSPECT_RACE_DESCRIPTION")][truth == "N"][order(avg_score_N)]

# Score-based metrics - Sufficiency:
# bin the prediction scores into categories from 0 to Y by 0.1
custom.breaks <- seq(0, 1, by = 0.1)
predictions_dt[, bin := cut(prob.Y, breaks = custom.breaks, include.lowest = TRUE)]
# Compute observed positive rates per bin and group
calibration <- predictions_dt[, .(
  observed_rate = mean(truth == "Y"),
  predicted_rate = mean(prob.Y)  # Avg predicted score in the bin
), by = .(bin, SUSPECT_RACE_DESCRIPTION)]
# Check well-calibration:
# Observed rates (Y=Y) should match predicted rates (S) across all bins and groups
well_calibrated <- all.equal(calibration$observed_rate, calibration$predicted_rate)
print(well_calibrated)
# check calibration visually
ggplot(calibration, aes(x = predicted_rate, y = observed_rate, color = SUSPECT_RACE_DESCRIPTION)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Well-Calibration Check", x = "Predicted Rate", y = "Observed Rate")


# what is the descriptive hit rate for sqf sample?





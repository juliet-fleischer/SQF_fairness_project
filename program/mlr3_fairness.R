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

calcGroupwiseMetrics(base_mrs_punitive, tsk_arrested, predictions_arrested)
calcGroupwiseMetrics(base_mrs_assistive, tsk_arrested, predictions_arrested)
calcGroupwiseMetrics(base_mrs_other, tsk_arrested, predictions_arrested)

# Independence: 
stat.parity <- predictions_dt[, .(sum(response == "1") / .N), by = "SUSPECT_SEX"]

# Score-based metrics - Separation:
# balance for positive/ negative class class
blnc.f.pos.clss <- predictions_dt[, .(mean(prob.1)), by = c("truth", "SUSPECT_SEX")][truth == "1"]
blnc.f.neg.clss <- predictions_dt[, .(mean(prob.0)), by = c("truth", "SUSPECT_SEX")][truth == "0"]

# Score-based metrics - Sufficiency:
# bin the prediction scores into categories from 0 to 1 by 0.1
custom.breaks <- seq(0, 1, by = 0.1)
predictions_dt[, bin := cut(prob.1, breaks = custom.breaks, include.lowest = TRUE)]
# Compute observed positive rates per bin and group
calibration <- predictions_dt[, .(
  observed_rate = mean(truth == "1"),
  predicted_rate = mean(prob.1)  # Avg predicted score in the bin
), by = .(bin, SUSPECT_SEX)]
# Check well-calibration:
# Observed rates (Y=1) should match predicted rates (S) across all bins and groups
well_calibrated <- all.equal(calibration$observed_rate, calibration$predicted_rate)
print(well_calibrated)
# check calibration visually
ggplot(calibration, aes(x = predicted_rate, y = observed_rate, color = SUSPECT_SEX)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Well-Calibration Check", x = "Predicted Rate", y = "Observed Rate")




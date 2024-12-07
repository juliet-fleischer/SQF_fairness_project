# load learners
lrn_rf$id <- "ranger"
l1 <- as_learner(po("reweighing_wts") %>>% lrn("classif.ranger"))
l1$id <- "reweight"
l2 = as_learner(po("learner_cv", lrn("classif.ranger")) %>>%
                  po("EOd"))
l2$id = "EOd"
# preprocess by collapsing factors
l3 = as_learner(po("collapsefactors") %>>% lrn("classif.fairzlrm"))
l3$id = "fairzlrm"

# run experiment
lrns = list(lrn_rf, l1, l2, l3)
bmr = benchmark(benchmark_grid(tsk_arrest, lrns, rsmp("cv", folds = 2)))
meas = msrs(c("classif.acc", "fairness.eod"))
bmr$aggregate(meas)[,
                    .(learner_id, classif.acc, fairness.equalized_odds)]

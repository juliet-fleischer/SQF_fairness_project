# post training analysis
### --- FEATURE IMPORTANCE --- ###

# features in the test data
sqf_x <- tsk_sqf$data(rows = splits$test, cols = tsk_sqf$feature_names)
# target in test data
sqf_y <- tsk_sqf$data(rows = splits$test, cols = tsk_sqf$target_names)
predictor <- Predictor$new(lrn_rf, data = sqf_x, y = sqf_y)

importance <-  FeatureImp$new(predictor, loss = "ce")
importance$plot()
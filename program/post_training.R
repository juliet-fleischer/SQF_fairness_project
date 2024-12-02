# post training analysis
### --- FEATURE IMPORTANCE --- ###

# features in the test data
sqf_x <- tsk_frisk$data(rows = splits$test, cols = tsk_frisk$feature_names)
# target in test data
sqf_y <- tsk_frisk$data(rows = splits$test, cols = tsk_frisk$target_names)
predictor <- Predictor$new(lrn_rf, data = sqf_x, y = sqf_y)

importance <-  FeatureImp$new(predictor, loss = "ce")
importance$plot()


# analyse feature importance for arrest task
arrest_x <- tsk_arrest$data(rows = splits$test, cols = tsk_arrest$feature_names)
arrest_y <- tsk_arrest$data(rows = splits$test, cols = tsk_arrest$target_names)
predictor_arrest <- Predictor$new(lrn_rf, data = arrest_x, y = arrest_y)

importance_arrest <-  FeatureImp$new(predictor_arrest, loss = "ce")
importance_arrest$plot()


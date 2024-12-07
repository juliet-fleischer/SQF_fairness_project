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


# analyse feature importance for search task
searched_x <- tsk_searched$data(rows = splits_searched$test, cols = tsk_searched$feature_names)
searched_y <- tsk_searched$data(rows = splits_searched$test, cols = tsk_searched$target_names)
predictor_searched <- Predictor$new(lrn_rf, data = searched_x, y = searched_y)

importance_searched <- FeatureImp$new(predictor_searched, loss = "ce")
importance_searched$plot()

# analyse feature importance for arrested_2
arrested_2_x <- task_arrested_2$data(rows = splits_arrested_2$test, cols = task_arrested_2$feature_names)
arrested_2_y <- task_arrested_2$data(rows = splits_arrested_2$test, cols = task_arrested_2$target_names)
predictor_arrested_2 <- Predictor$new(lrn_rf, data = arrested_2_x, y = arrested_2_y)
importance_arrested_2 <- FeatureImp$new(predictor_arrested_2, loss = "ce")
importance_arrested_2$plot()

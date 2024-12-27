# FTU
# Clone the task
task_train <- tsk_arrest$clone()

# Remove the PA column for training
task_train$select(setdiff(task_train$feature_names, "SUSPECT_RACE_DESCRIPTION"))

# Train the model on the modified task
lrn_rf$train(task_train, row_ids = splits_arrested$train)

# make a copy of the test data but with reversed sex column
test.data.original <- copy(imputed_data_arrested[splits_arrested$test])
# reverse the sex column
test.data.reversed <- copy(test.data.original)
test.data.reversed[, SUSPECT_SEX := ifelse(SUSPECT_SEX == "1", "0", "1")]
# add ID column
test.data.original[, ID := .I]
test.data.reversed[, ID := .I]

# predict on original data
predictions.original <- lrn_rf$predict_newdata(test.data.original)
# predict on reversed data
predictions.reversed <- lrn_rf$predict_newdata(test.data.reversed)

# Convert predictions to data.tables
predictions_original_dt <- as.data.table(predictions.original)
predictions_reversed_dt <- as.data.table(predictions.reversed)

# Merge predictions by ID
comparison <- merge(predictions_original_dt, predictions_reversed_dt, by = "row_ids", suffixes = c("_original", "_reversed"))

# Compare the predictions for each pair
comparison[, prediction_same := response_original == response_reversed]

# View discrepancies
print(comparison[prediction_same == FALSE])

# Satisfies FTU

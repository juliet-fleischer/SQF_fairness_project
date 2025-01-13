library(mlr3)
library(mlr3pipelines)
library(data.table)

# Load the data
data2011 <- fread("data/2004.csv")

# convert all empty entries "" into NA
data2011[data2011 == ""] <- NA

na.count <- colSums(is.na(data2011))
na.cols <- which(na.count/ nrow(data2011) > 0.2)
data2011 <- data2011[, -..na.cols]

# convert all columns of type "character" to factor with data.table
char.cols <- sapply(data2011, is.character)
data2011[, (names(char.cols)[char.cols]) := lapply(.SD, as.factor), .SDcols = names(char.cols)[char.cols]]

# Define a PipeOp to convert character columns to factors
# po_char_to_factor <- po("colapply", applicator = as.factor, affect_columns = selector_type("character"))

# Define the learner
lrn_rf_missing <- lrn("classif.random_forest_weka", predict_type = "prob")
# Create a Task
task.2011 <-  as_task_classif(data2011, target = "arstmade", response_type = "prob",
                              positive = "Y", id = "arrested w/ missing")
task.2011$col_roles$pta <- "race"
splits.2011 <- partition(task.2011)
lrn_rf_missing$train(task.2011, row_ids = splits.2011$train)
predictions.2011 <- lrn_rf_missing$predict(task.2011, row_ids = splits.2011$test)
predictions_dt <- as.data.table(predictions_arrested_full)
predictions_dt$race <- sqf[predictions_dt$row_ids, race]

calcGroupwiseMetrics(base_mrs_punitive, task.2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_assistive, task.2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_other, task.2011, predictions.2011)

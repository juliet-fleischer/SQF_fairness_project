

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

# remove a few unnecessary columns
data2011$stinter <- NULL
data2011$addrtyp <- NULL
data2011$premname <- NULL
data2011$recstat <- NULL
data2011$year <- NULL

# filter out the complete cases
data.cc <- data2011[complete.cases(data2011), ]

# dichotomise the race attribute
data.cc$pa_group <- ifelse(data.cc$race %in% c("B", "Q", "P"), "PoC", "White")

# Define a PipeOp to convert character columns to factors
# po_char_to_factor <- po("colapply", applicator = as.factor, affect_columns = selector_type("character"))

# Create a Task
task.2011 <-  as_task_classif(data.cc, target = "arstmade", response_type = "prob",
                              positive = "Y", id = "arrested")
task.2011$col_roles$pta <- "race"
splits.2011 <- partition(task.2011)
lrn_rf$train(task.2011, row_ids = splits.2011$train)
predictions.2011 <- lrn_rf$predict(task.2011, row_ids = splits.2011$test)
predictions_dt <- as.data.table(predictions.2011)
predictions_dt$race <- data.cc[predictions_dt$row_ids, race]

calcGroupwiseMetrics(base_mrs_punitive, task.2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_assistive, task.2011, predictions.2011)
calcGroupwiseMetrics(base_mrs_other, task.2011, predictions.2011)

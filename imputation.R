### --- Missing data imputation --- ###
na.count <- apply(sqf.2023, 2, function(x) sum(is.na(x))) / nrow(sqf.2023)
na.count[which.max(na.count)]
# create subset of the data for frisked as target
# sqf.2023.subset <- subset(sqf.2023, select = c(targets, protected.a, features))
# imputed missing data
imp <- mice(sqf.2023, m = 1)
imputed_data_full <- complete(imp)



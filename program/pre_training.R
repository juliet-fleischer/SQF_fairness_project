set.seed(513)

### --- Missing data imputation --- ###
# create subset of the data
sqf.2023.subset <- subset(sqf.2023, select = c("SUSPECT_ARRESTED_FLAG", protected.a, features))
# impute missing data
imp <- mice(sqf.2023.subset, m = 1)
imputed_data <- complete(imp)


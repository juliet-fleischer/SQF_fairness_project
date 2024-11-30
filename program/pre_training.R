set.seed(513)

### --- Missing data imputation --- ###
# create subset of the data for arrested as target
sqf.2023.subset1 <- subset(sqf.2023, select = c("SUSPECT_ARRESTED_FLAG", protected.a, features))
# impute missing data
imp1 <- mice(sqf.2023.subset1, m = 1)
imputed_data <- complete(imp1)

# create subset of the data for frisked as target
sqf.2023.subset2 <- subset(sqf.2023, select = c("FRISKED_FLAG", protected.a, features))
# imputed missing data
imp2 <- mice(sqf.2023.subset2, m = 1)
imputed_data <- complete(imp2)
# analysis plan for the missing values:
## first check how many missing each variable has
# remove the ones that have suuper many missing --> really not important
# the other ones look at them individually and find out their meaning
# if their meaning is important analyse the missing value patter on a sensible subset
# then impute based on this


### --- Missing data imputation --- ###
na.count <- apply(sqf.2023, 2, function(x) sum(is.na(x))) / nrow(sqf.2023)
na.count[which.max(na.count)]
# create subset of the data for frisked as target
# sqf.2023.subset <- subset(sqf.2023, select = c(targets, protected.a, features))
# imputed missing data
imp <- mice(sqf.2023, m = 1)
imputed_data_full <- complete(imp)




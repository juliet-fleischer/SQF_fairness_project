# create one complete imputed datasets including all targets and all possible
# covariables (filter appropriately for training)

targets <- c("SUSPECT_ARRESTED_FLAG", "SEARCHED_FLAG", "FRISKED_FLAG")
protected.a <- c("SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")

### --- Missing data imputation --- ###
na.count <- apply(sqf.2023, 2, function(x) sum(is.na(x))) / nrow(sqf.2023)
na.count[which.max(na.count)]
# create subset of the data for frisked as target
# sqf.2023.subset <- subset(sqf.2023, select = c(targets, protected.a, features))
# imputed missing data
imp <- mice(sqf.2023, m = 1)
imputed_data_full <- complete(imp)
# imputed_data_full <- read_excel("data/imputed_data_full.xlsx")
setDT(imputed_data_full)
# LOCATION_IN_OUT_CODE
# JURISDICTION_DESCRIPTION
# ASK_FOR_CONSENT_FLG
# CONSENT_GIVEN_FLG
# SUSPECT_RACE_DESCRIPTION
# SUSPECT_HEIGHT
# SUSPECT_WEIGHT
# SUSPECT_BODY_BUILD_TYPE
# SUSPECT_EYE_COLOR
# SUSPECT_HAIR_COLOR

# create subset for frisked as target
imputed_data_frisked <- copy(imputed_data_full)
other.targets <- setdiff(targets, "FRISKED_FLAG")
imputed_data_frisked[, (other.targets) := NULL]
imputed_data_frisked[, (16:20) := NULL] # related to weapon found (only possible after frisk or search)
imputed_data_frisked$ASK_FOR_CONSENT_FLG <- NULL
imputed_data_frisked$CONSENT_GIVEN_FLG <- NULL
imputed_data_frisked$BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG <- NULL
imputed_data_frisked$SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG <- NULL
# remove all columns related to search
pattern.search <- "SEARCH_[:alpha:]*"
search.cols <- grep(pattern.search, names(imputed_data_frisked))
imputed_data_frisked[, (search.cols) := NULL]

# create subset for searched as target
imputed_data_searched <- copy(imputed_data_full)
other.targets <- setdiff(targets, "SEARCHED_FLAG")
imputed_data_searched[, (other.targets) := NULL]
imputed_data_searched[, (16:20) := NULL] # related to weapon found (only possible after frisk or search)

# create subset for arrested as target
imputed_data_arrested <- copy(imputed_data_full)
other.targets <- setdiff(targets, "SUSPECT_ARRESTED_FLAG")
imputed_data_arrested[, (other.targets) := NULL]
imputed_data_arrested[, (16:20) := NULL] 
imputed_data_arrested$ASK_FOR_CONSENT_FLG <- NULL
imputed_data_arrested$CONSENT_GIVEN_FLG <- NULL
# remove all columns related to search
pattern.search <- "SEARCH_[:alpha:]*"
search.cols <- grep(pattern.search, names(imputed_data_arrested))
imputed_data_arrested[, (search.cols) := NULL]

# imputed_data_frisked$BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG <- NULL
# imputed_data_frisked$SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG <- NULL

# put FRISKED_FLAG AND SEARCHED_FLAG as features in the arrested model?
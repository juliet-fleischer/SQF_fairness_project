# create one complete imputed datasets including all targets and all possible
# covariables (filter appropriately for training)

targets <- c("SUSPECT_ARRESTED_FLAG", "SEARCHED_FLAG", "FRISKED_FLAG")
protected.a <- c("SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")
features <- c("MONTH2", "DAY2", "STOP_FRISK_TIME", "STOP_DURATION_MINUTES", "STOP_DURATION_MINUTES",
              "STOP_WAS_INITIATED", "OFFICER_EXPLAINED_STOP_FLAG", "OFFICER_IN_UNIFORM_FLAG",
              "ASK_FOR_CONSENT_FLG", "CONSENT_GIVEN_FLG", "OTHER_PERSON_STOPPED_FLAG",
              "SUSPECT_REPORTED_AGE", "SUSPECT_HEIGHT", "SUSPECT_WEIGHT", "SUSPECT_BODY_BUILD_TYPE",
              "SUSPECT_EYE_COLOR", "SUSPECT_HAIR_COLOR",
              "SUSPECTED_CRIME_DESCRIPTION", "WEAPON_FOUND_FLAG", "OTHER_CONTRABAND_FLAG",
              "STOP_LOCATION_BORO_NAME", "LOCATION_IN_OUT_CODE", "JURISDICTION_DESCRIPTION",
              # "STOP_LOCATION_PRECINCT",   take it out bc maybe afterwards found?
              )

# PA <- "SUSPECT_SEX"


### --- Missing data imputation --- ###


# create subset of the data for frisked as target
sqf.2023.subset <- subset(sqf.2023, select = c(targets, protected.a, features))
# imputed missing data
imp <- mice(sqf.2023.subset, m = 1)
imputed_data_full <- complete(imp)

# create subsets of the data for each target

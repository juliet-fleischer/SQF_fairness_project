set.seed(513)

target <- "SUSPECT_ARRESTED_FLAG"
# PAs
protected.a <- c("SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")

features <- c("MONTH2", "DAY2", "STOP_FRISK_TIME",
              "STOP_LOCATION_BORO_NAME", "LOCATION_IN_OUT_CODE", "STOP_DURATION_MINUTES",
              "SUSPECT_HEIGHT", "SUSPECT_WEIGHT", "SUSPECT_BODY_BUILD_TYPE",
              "SUSPECT_EYE_COLOR", "SUSPECT_HAIR_COLOR", "STOP_WAS_INITIATED",
              "OFFICER_EXPLAINED_STOP_FLAG", "OFFICER_IN_UNIFORM_FLAG",
              "ASK_FOR_CONSENT_FLG", "CONSENT_GIVEN_FLG")

if (target == "SUSPECT_ARRESTED_FLAG") {
  features <- c(features, "WEAPON_FOUND_FLAG")
  }

PA <- "SUSPECT_SEX"


### --- Missing data imputation --- ###


# create subset of the data for frisked as target
sqf.2023.subset2 <- subset(sqf.2023, select = c("FRISKED_FLAG", protected.a, features))
# imputed missing data
imp2 <- mice(sqf.2023.subset2, m = 1)
imputed_data <- complete(imp2)

# create subset of the data for searched as target



# create subset of the data for arrested as target
sqf.2023.subset1 <- subset(sqf.2023, select = c("SUSPECT_ARRESTED_FLAG", protected.a, features))
# impute missing data
imp1 <- mice(sqf.2023.subset1, m = 1)
imputed_data <- complete(imp1)
imputed_data_full <- read_excel("data/imputed_data_full.xlsx")
setDT(imputed_data_full)

targets <- c("SUSPECT_ARRESTED_FLAG", "SEARCHED_FLAG", "FRISKED_FLAG")
protected.a <- c("SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")

# some data preparation for the imputed data set
# convert all the targets to numeric
imputed_data_full[, (targets) := lapply(.SD, function(x) {
  ifelse(x == "1", 1, 0)
}), .SDcols = targets]

char.cols <- sapply(imputed_data_full, is.character)
imputed_data_full[, names(char.cols)[char.cols] := lapply(.SD, as.factor), .SDcols = names(char.cols)[char.cols]]

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

# create subset for FRISKED as target
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

# create subset for SEARCHED as target
imputed_data_searched <- copy(imputed_data_full)
other.targets <- setdiff(targets, "SEARCHED_FLAG")
imputed_data_searched[, (other.targets) := NULL]
imputed_data_searched[, (16:20) := NULL] # related to weapon found (only possible after frisk or search)

# create subset for ARRESTED as target
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

# possible dichotomizations of race
imputed_data_arrested_2 <- imputed_data_arrested |> 
  mutate(race_group = ifelse(SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "BLACK HISPANIC"), "c", "w"))
imputed_data_arrested_2$SUSPECT_RACE_DESCRIPTION <- NULL
# imputed_data_frisked$BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG <- NULL
# imputed_data_frisked$SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG <- NULL

# put FRISKED_FLAG AND SEARCHED_FLAG as features in the arrested model?
setDT(sqf.2023)
n <- nrow(sqf.2023)
sqf.2023[sqf.2023 == "(null)"] <- NA

# go through each column an check whether it matches alpha or digit
# if it matches digit, convert it to numeric
col.names <- names(sqf.2023)[-c(2,3)]

# Apply the transformation to all columns that match the pattern
sqf.2023[, (col.names) := lapply(.SD, function(x) {
  if (all(is.na(x) | grepl("[[:digit:]]+", x))) as.numeric(x) else x
}), .SDcols = col.names]

# remove levels that are very rare
hair.color <- c("GRN", "PLE", "PNK", "ORG", "SDY")
eye.color <- c("MAR", "MUL", "PNK", "OTH")

sqf.2023 <- sqf.2023 |> 
  filter(!SUSPECT_HAIR_COLOR %in% hair.color) |>
  filter(!SUSPECT_EYE_COLOR %in% eye.color)

# convert to date object
sqf.2023$STOP_FRISK_DATE <- as.POSIXct(sqf.2023$STOP_FRISK_DATE, tz = "EST")

# convert alls Yes-No columns 
convertFactorNumeric <- function(data, col, levels, labels) {
  data[, (col) := factor(get(col), levels = levels, labels = labels)]
  data[, (col) := as.numeric(as.character(get(col)))]
}

convertFactorNumeric(sqf.2023, "FRISKED_FLAG", c("Y", "N"), c(1, 0))
convertFactorNumeric(sqf.2023, "SEARCHED_FLAG", c("Y", "N"), c(1, 0))
convertFactorNumeric(sqf.2023, "SUSPECT_ARRESTED_FLAG", c("Y", "N"), c(1, 0))

sqf.2023$SUSPECT_SEX <- factor(sqf.2023$SUSPECT_SEX, levels = c("MALE", "FEMALE"), labels = c(1, 0))

sqf.2023$SUSPECT_RACE_DESCRIPTION <- factor(sqf.2023$SUSPECT_RACE_DESCRIPTION,
                                            levels = c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC",
                                                       "WHITE","ASIAN / PACIFIC ISLANDER",
                                                       "MIDDLE EASTERN/SOUTHWEST ASIAN",
                                                       "AMERICAN INDIAN/ALASKAN NATIVE"))

sqf.2023$STOP_LOCATION_BORO_NAME <- factor(sqf.2023$STOP_LOCATION_BORO_NAME)
sqf.2023$LOCATION_IN_OUT_CODE <- factor(sqf.2023$LOCATION_IN_OUT_CODE)
sqf.2023$STOP_WAS_INITIATED <- factor(sqf.2023$STOP_WAS_INITIATED)
sqf.2023$MONTH2 <- factor(sqf.2023$MONTH2)
sqf.2023$DAY2 <- factor(sqf.2023$DAY2)
sqf.2023$OFFICER_EXPLAINED_STOP_FLAG <- factor(sqf.2023$OFFICER_EXPLAINED_STOP_FLAG)
sqf.2023$OFFICER_IN_UNIFORM_FLAG <- factor(sqf.2023$OFFICER_IN_UNIFORM_FLAG)
sqf.2023$ASK_FOR_CONSENT_FLG <- factor(sqf.2023$ASK_FOR_CONSENT_FLG)
sqf.2023$CONSENT_GIVEN_FLG <- factor(sqf.2023$CONSENT_GIVEN_FLG)
sqf.2023$SUSPECT_BODY_BUILD_TYPE <- factor(sqf.2023$SUSPECT_BODY_BUILD_TYPE)
sqf.2023$SUSPECT_EYE_COLOR <- factor(sqf.2023$SUSPECT_EYE_COLOR)
sqf.2023$SUSPECT_HAIR_COLOR <- factor(sqf.2023$SUSPECT_HAIR_COLOR)
sqf.2023$STOP_LOCATION_PRECINCT <- factor(sqf.2023$STOP_LOCATION_PRECINCT)
sqf.2023$WEAPON_FOUND_FLAG <- factor(sqf.2023$WEAPON_FOUND_FLAG)

# binning of stop time of the day

# 6 - 12: AM
# 12 - 18: PM
# 18 - 22: evening
# 22 - 6 : night
# convert times to numeric values
sqf.2023 <- sqf.2023 |> 
  mutate(STOP_FRISK_TIME = sub(":", ".", substr(STOP_FRISK_TIME, 1, 5))) |> 
  mutate(STOP_FRISK_TIME = round(as.numeric(STOP_FRISK_TIME))) |> 
  mutate(STOP_FRISK_TIME = ifelse(STOP_FRISK_TIME == 24, 0, STOP_FRISK_TIME))

# bin time
sqf.2023$STOP_FRISK_TIME <- cut(
  sqf.2023$STOP_FRISK_TIME,
  breaks = c(0, 6, 12, 18, 24),
  labels = c("night", "AM", "PM", "evening"),
  right = FALSE
)

# Identify columns with only one level
which(sapply(lapply(sqf.2023, unique), length) == 1)
# remove year column (as we only have 2023)
sqf.2023$YEAR2 <- NULL
sqf.2023$RECORD_STATUS_CODE <- NULL



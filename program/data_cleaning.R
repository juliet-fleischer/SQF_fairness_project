

sqf.2023 <- read_excel("Data/sqf-2023.xlsx")
str(sqf.2023)
setDT(sqf.2023)

n <- nrow(sqf.2023)
sqf.2023[sqf.2023 == "(null)"] <- NA

## TARGETS
targets <- c("SUSPECT_ARRESTED_FLAG", "SUMMONS_ISSUED_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG")

# PAs
protected.a <- c("SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")

features <- c("MONTH2", "DAY2",
              "STOP_LOCATION_BORO_NAME", "LOCATION_IN_OUT_CODE", "STOP_DURATION_MINUTES",
              "SUSPECT_HEIGHT", "SUSPECT_WEIGHT", "SUSPECT_BODY_BUILD_TYPE",
              "SUSPECT_EYE_COLOR", "SUSPECT_HAIR_COLOR", "STOP_WAS_INITIATED",
              "OFFICER_EXPLAINED_STOP_FLAG", "OFFICER_IN_UNIFORM_FLAG",
              "ASK_FOR_CONSENT_FLG", "CONSENT_GIVEN_FLG", "WEAPON_FOUND_FLAG")

# "STOP_LOCATION_PRECINCT", "STOP_FRISK_DATE", "STOP_FRISK_TIME"


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
attr.to.convert <- c(targets,"WEAPON_FOUND_FLAG")
convertFactorNumeric <- function(data, col, levels, labels) {
  data[, (col) := factor(get(col), levels = levels, labels = labels)]
  data[, (col) := as.numeric(as.character(get(col)))]
}
for (a in attr.to.convert) {
  convertFactorNumeric(sqf.2023, a, c("Y", "N"), c(1, 0))
}

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




# 
# # Missing data handling
# na.count <- apply(sqf.2023, 2, function(x) sum(is.na(x)))
# # omit the columns that have more than 30% missing values
# # (because no reasonable imputation possible, these are columns that are basically not important)
# discard.cols <- names(sqf.2023)[(na.count / n) > 0.3]
# sqf.2023[, (discard.cols):= NULL]
# discard.cols.2 <- names(sqf.2023)[apply(sqf.2023, 2, function(x) sum(is.na(x))) > 0][1:4]
# sqf.2023[, (discard.cols.2) := NULL]

# Identify columns with only one level
which(sapply(lapply(sqf.2023, unique), length) == 1)
# remove year column (as we only have 2023)
sqf.2023$YEAR2 <- NULL
sqf.2023$RECORD_STATUS_CODE <- NULL


# 
# # complete case analysis based on already reduced data set
# # sqf.complete.2023 <- sqf.2023[complete.cases(sqf.2023), ]
# 
# # check for outliers
# summary(sqf.complete.2023)
# # Height is measured in feet
# 
# # check for duplicates
# duplicates <- sqf.complete.2023[duplicated(sqf.complete.2023), ] # no duplicates present
# 
# # reduce the complete data to a set of reasonable covariables
# selected.attr <- c("STOP_ID", targets, protected.a, "STOP_LOCATION_PRECINCT",
#                    "SUSPECT_WEIGHT", "WEAPON_FOUND_FLAG")
# sqf.complete.2023 <- sqf.complete.2023[, ..selected.attr]


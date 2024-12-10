sqf.2023 <- read_excel("data/sqf-2023.xlsx")
setDT(sqf.2023)
n <- nrow(sqf.2023)
sqf.2023[sqf.2023 == "(null)"] <- NA

### --- Missing data analysis --- ###
# count the missing values in each column
na.count <- apply(sqf.2023, 2, function(x) sum(is.na(x))) / nrow(sqf.2023)
sort(na.count)
enough.data.cols <- names(which(na.count < 0.5))
cols.to.keep <- sqf.2023[, names(sqf.2023) %in% enough.data.cols]
sqf.2023 <- sqf.2023[, ..cols.to.keep]

targets <- c("SUSPECT_ARRESTED_FLAG", "SEARCHED_FLAG", "FRISKED_FLAG")
protected.a <- c("SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")

# remove all officer columns
# pattern.officer <- "[:alpha:]*_OFFICER_[:alpha:]*"
# officer.cols <- grep(pattern.officer, names(sqf.2023))
# sqf.2023[, (officer.cols) := NULL]
# # remove all location columns except STOP_LOCATION_BORO_NAME
# pattern.location <- "[:alpha:]*_LOCATION_[:alpha:]*"
# location.cols <- grep(pattern.location, names(sqf.2023))
# sqf.2023[, (location.cols[-length(location.cols)]) := NULL]
# remove all columns that start with PHYSICAL_FORCE
# pattern.force <- "PHYSICAL_FORCE[:alpha:]*"
# force.cols <- grep(pattern.force, names(sqf.2023))
# sqf.2023[, (force.cols) := NULL]
# remove all columns relted to summons
# pattern.summons <- "SUMMONS[:alpha:]*"
# summons.cols <- grep(pattern.summons, names(sqf.2023))
# sqf.2023[, (summons.cols) := NULL]
# remove all columns without specifc pattern
sqf.2023$YEAR2 <- NULL
sqf.2023$STOP_FRISK_DATE <- NULL
sqf.2023$RECORD_STATUS_CODE <- NULL
sqf.2023$DEMEANOR_OF_PERSON_STOPPED <- NULL
sqf.2023$SUPERVISING_ACTION_CORRESPONDING_ACTIVITY_LOG_ENTRY_REVIEWED <- NULL
sqf.2023$JURISDICTION_CODE <- NULL
# sqf.2023$SUSPECTED_CRIME_DESCRIPTION <- NULL




# from column 23 to 42 set all the NAs to "N"
sqf.2023[, (19:38) := lapply(.SD, function(x) {
  ifelse(is.na(x), "N", x)
}), .SDcols = 19:38]

# convert all the columns that end on FLAG or FLG to factor
flag.cols <- grep("FLAG$|FLG$", names(sqf.2023))
sqf.2023[, (flag.cols) := lapply(.SD, as.factor), .SDcols = flag.cols]

# go through each column an check whether it matches alpha or digit
# if it matches digit, convert it to numeric
col.names <- names(sqf.2023)[-c(1,2)]
# Apply the transformation to all columns that match the pattern
sqf.2023[, (col.names) := lapply(.SD, function(x) {
  if (all(is.na(x) | grepl("[[:digit:]]+", x))) as.numeric(x) else x
}), .SDcols = col.names]

# convert all potential target columns to numeric
sqf.2023[ , (targets) := lapply(.SD,function(x) { ifelse(x == "Y", 1, 0)}), .SDcols = targets]
# convert sex to numeric 0 = female, 1 = male
sqf.2023[, SUSPECT_SEX := ifelse(SUSPECT_SEX == "FEMALE", 0, 1)]

# create factor columns
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
sqf.2023$SUSPECT_BODY_BUILD_TYPE <- factor(sqf.2023$SUSPECT_BODY_BUILD_TYPE)
sqf.2023$SUSPECT_EYE_COLOR <- factor(sqf.2023$SUSPECT_EYE_COLOR)
sqf.2023$SUSPECT_HAIR_COLOR <- factor(sqf.2023$SUSPECT_HAIR_COLOR)
sqf.2023$JURISDICTION_DESCRIPTION <- factor(sqf.2023$JURISDICTION_DESCRIPTION)


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

# remove levels that are very rare
hair.color <- c("GRN", "PLE", "PNK", "ORG", "SDY")
eye.color <- c("MAR", "MUL", "PNK", "OTH")

sqf.2023 <- sqf.2023 |> 
  filter(!SUSPECT_HAIR_COLOR %in% hair.color) |>
  filter(!SUSPECT_EYE_COLOR %in% eye.color)



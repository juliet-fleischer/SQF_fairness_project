# 2023 data ----
sqf <- read_excel("data/sqf-2023.xlsx")
setDT(sqf)
n <- nrow(sqf)
sqf[sqf == "(null)"] <- NA

## Missing data ----
# count the missing values in each column
na.count <- apply(sqf, 2, function(x) sum(is.na(x))) / nrow(sqf)
na.count.df <- as.data.frame(na.count)
# For plotting: sort the na.count into equidistant bins with 20% steps by using the cat function
na.count.df$na.count.binned <- cut(na.count.df$na.count, breaks = seq(0, 1, 0.2),
                                   labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                                   include.lowest = TRUE)
na.count.df$color_code <- ifelse(na.count.df$na.count.binned == "0-20%", "in", "out")

# Count missing values per row
row_missing <- rowSums(is.na(sqf))
# Summarize rows with different numbers of missing values
table(row_missing)

enough.data.cols <- names(which(na.count <= 0.2))
cols.to.keep <- sqf[, names(sqf) %in% enough.data.cols]
sqf <- sqf[, ..cols.to.keep]

## Variables ----
targets <- c("SUSPECT_ARRESTED_FLAG", "SEARCHED_FLAG", "FRISKED_FLAG", "SUMMONS_ISSUED_FLAG")
protected.a <- c("SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")

sqf$YEAR2 <- NULL
sqf$STOP_FRISK_DATE <- NULL
sqf$STOP_ID <- NULL
sqf$RECORD_STATUS_CODE <- NULL
sqf$DEMEANOR_OF_PERSON_STOPPED <- NULL
sqf$SUPERVISING_ACTION_CORRESPONDING_ACTIVITY_LOG_ENTRY_REVIEWED <- NULL
sqf$STOP_LOCATION_X <- NULL
sqf$STOP_LOCATION_Y <- NULL
sqf$STOP_LOCATION_FULL_ADDRESS <- NULL
sqf$STOP_LOCATION_PATROL_BORO_NAME <- NULL
sqf$STOP_LOCATION_STREET_NAME <- NULL
sqf$PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG <- NULL

# convert times to numeric values
sqf <- sqf |> 
  mutate(STOP_FRISK_TIME = sub(":", ".", substr(STOP_FRISK_TIME, 1, 5))) |> 
  mutate(STOP_FRISK_TIME = round(as.numeric(STOP_FRISK_TIME))) |> 
  mutate(STOP_FRISK_TIME = ifelse(STOP_FRISK_TIME == 24, 0, STOP_FRISK_TIME))

# binning of stop time of the day
# 6 - 12: AM
# 12 - 18: PM
# 18 - 22: evening
# 22 - 6 : night
sqf$STOP_FRISK_TIME <- cut(
  sqf$STOP_FRISK_TIME,
  breaks = c(0, 6, 12, 18, 24),
  labels = c("night", "AM", "PM", "evening"),
  right = FALSE
)

# convert all the columns that end on FLAG or FLG to factor
flag.cols <- grep("FLAG$|FLG$", names(sqf))
sqf[, (flag.cols) := lapply(.SD, as.factor), .SDcols = flag.cols]

# go through each column an check whether it matches alpha or digit
# if it matches digit, convert it to numeric
col.names <- names(sqf)[-c(1,2)]
# convert all numeric columns to numeric type
sqf[, (col.names) := lapply(.SD, function(x) {
  if (all(is.na(x) | grepl("[[:digit:]]+", x))) as.numeric(x) else x
}), .SDcols = col.names]

# convert all remaining character columns to factor
char.cols <- which(sapply(sqf, is.character))
sqf[, (char.cols) := lapply(.SD, as.factor), .SDcols = char.cols]

# create factor columns
sqf$STOP_LOCATION_BORO_NAME <- factor(sqf$STOP_LOCATION_BORO_NAME)
sqf$STOP_WAS_INITIATED <- factor(sqf$STOP_WAS_INITIATED)
sqf$MONTH2 <- factor(sqf$MONTH2)
sqf$DAY2 <- factor(sqf$DAY2)
sqf$SUSPECT_BODY_BUILD_TYPE <- factor(sqf$SUSPECT_BODY_BUILD_TYPE)
sqf$SUSPECT_EYE_COLOR <- factor(sqf$SUSPECT_EYE_COLOR)
sqf$SUSPECT_HAIR_COLOR <- factor(sqf$SUSPECT_HAIR_COLOR)

# creating groups for the race variable
sqf$SUSPECT_RACE_DESCRIPTION <- as.character(sqf$SUSPECT_RACE_DESCRIPTION)
other_racegroup <- c("AMERICAN INDIAN/ALASKAN NATIVE", "MIDDLE EASTERN/SOUTHWEST ASIAN")
sqf[, SUSPECT_RACE_DESCRIPTION := ifelse(SUSPECT_RACE_DESCRIPTION %in% other_racegroup, "OTHER", SUSPECT_RACE_DESCRIPTION)]
sqf$SUSPECT_RACE_DESCRIPTION <- factor(sqf$SUSPECT_RACE_DESCRIPTION,
                                       levels = c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC",
                                                  "WHITE", "ASIAN / PACIFIC ISLANDER", "OTHER"),
                         labels = c("Black", "Hispanic", "Black", "White", "Asian", "Other"))


# remove levels that are very rare
hair.color <- c("GRN", "PLE", "PNK", "ORG", "SDY")
eye.color <- c("MAR", "MUL", "PNK", "OTH")

sqf <- sqf |> 
  filter(!SUSPECT_HAIR_COLOR %in% hair.color) |>
  filter(!SUSPECT_EYE_COLOR %in% eye.color)

complete_cases <- sqf[complete.cases(sqf), ]
unprivileged <- c("Black", "Hispanic", "Black")
complete_cases$PA_GROUP <- ifelse(complete_cases$SUSPECT_RACE_DESCRIPTION %in% unprivileged, "POC", "White")
complete_cases$PA_GROUP <- factor(complete_cases$PA_GROUP)


# 2011 data ----
data2011 <- fread("data/2011.csv")

## Missing data ----
# convert all empty entries "" into NA
data2011[data2011 == ""] <- NA

na.count <- colSums(is.na(data2011))
na.cols <- which(na.count/ nrow(data2011) > 0.2)
data2011 <- data2011[, -..na.cols]

## Variables ----
# convert all columns of type "character" to factor with data.table
char.cols <- sapply(data2011, is.character)
data2011[, (names(char.cols)[char.cols]) := lapply(.SD, as.factor), .SDcols = names(char.cols)[char.cols]]

# remove a few unnecessary columns
data2011$stinter <- NULL
data2011$addrtyp <- NULL
data2011$premname <- NULL
data2011$recstat <- NULL
data2011$year <- NULL

# identify NA entries
data2011[race == "U", race := NA]
# Remove unused levels from the 'race' factor
data2011[, race := droplevels(race)]

# filter out the complete cases
complete_cases_2011 <- data2011[complete.cases(data2011), ]

# create proper month and weekday columns
complete_cases_2011[, datestop_char := sprintf("%08d", datestop)]
complete_cases_2011[, datestop := mdy(datestop_char)]
complete_cases_2011[, weekday := factor(weekdays(datestop))]
complete_cases_2011[, month := factor(month(datestop), levels = 1:12, labels = month.abb)]
complete_cases_2011$datestop <- NULL
complete_cases_2011$datestop_char <- NULL

# create proper time column
complete_cases_2011[, timestop_char := sprintf("%04d", timestop)]

# Extract hours and minutes as numeric values
complete_cases_2011[, timestop_hour := as.numeric(substr(timestop_char, 1, 2))]

# bin the time as in the 2023 data
complete_cases_2011[, timestop := cut(
  timestop_hour,
  breaks = c(0, 6, 12, 18, 24),
  labels = c("night", "AM", "PM", "evening"),
  right = FALSE)]
# remove all the hleper columns
complete_cases_2011[, c("timestop_char", "timestop_hour") := NULL]


# adjust the level of race to align with 2021 data and target population
complete_cases_2011$race <- factor(complete_cases_2011$race, levels = c("B", "Q", "P", "W", "A", "I", "Z"),
                        labels = c("Black", "Hispanic", "Black", "White", "Asian", "Other", "Other"))

# dichotomise the race attribute
complete_cases_2011$pa_group <- ifelse(complete_cases_2011$race %in% c("Black", "Hispanic"), "POC", "White")
complete_cases_2011$pa_group <- factor(complete_cases_2011$pa_group)

# Target population ----
# The data is from NYC Population FactFinder: https://popfactfinder.planning.nyc.gov/#11.67/40.7198/-73.9515
target_pop <- lapply(1:5, \(i) assign(paste0("data_", i), read_xlsx("data/target_pop_data.xlsx", sheet = i)))
names(target_pop) <- c("Brooklyn", "Manhattan", "Bronx", "Queens", "Staten Island")
# for each tibble in target_pop select only the first three columns
target_pop <- lapply(target_pop, \(x) select(x, 1:2))
# rename the columns for each tibble
target_pop <- lapply(target_pop, function(x) {
  colnames(x) <- c("group", "count")
  x$count <- as.numeric(gsub(",", "", x$count)) 
  return(x)
})
# bind everything together to one data frame
target_pop <- do.call(rbind, lapply(names(target_pop), \(x) cbind(target_pop[[x]], borough = x)))
target_pop$count <- as.integer(gsub("\\.", "", target_pop$count))


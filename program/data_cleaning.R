sqf <- read_excel("data/sqf-2023.xlsx")
setDT(sqf)
n <- nrow(sqf)
sqf[sqf == "(null)"] <- NA

### --- Missing data analysis --- ###
# count the missing values in each column
na.count <- apply(sqf, 2, function(x) sum(is.na(x))) / nrow(sqf)
na.count.df <- as.data.frame(na.count)
# sort the na.count into equidistant bins with 20% steps by using the cat function
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

targets <- c("SUSPECT_ARRESTED_FLAG", "SEARCHED_FLAG", "FRISKED_FLAG", "SUMMONS_ISSUED_FLAG")
protected.a <- c("SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION")

# remove all officer columns
# pattern.officer <- "[:alpha:]*_OFFICER_[:alpha:]*"
# officer.cols <- grep(pattern.officer, names(sqf))
# sqf[, (officer.cols) := NULL]
# # remove all location columns except STOP_LOCATION_BORO_NAME
# pattern.location <- "[:alpha:]*_LOCATION_[:alpha:]*"
# location.cols <- grep(pattern.location, names(sqf))
# sqf[, (location.cols[-length(location.cols)]) := NULL]
# remove all columns that start with PHYSICAL_FORCE
# pattern.force <- "PHYSICAL_FORCE[:alpha:]*"
# force.cols <- grep(pattern.force, names(sqf))
# sqf[, (force.cols) := NULL]
# remove all columns relted to summons
# pattern.summons <- "SUMMONS[:alpha:]*"
# summons.cols <- grep(pattern.summons, names(sqf))
# sqf[, (summons.cols) := NULL]
# remove all columns without specifc pattern
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
# sqf$STOP_LOCATION_STREET_NAME <- NULL


# 
# # from column 23 to 42 set all the NAs to "N"
# sqf[, (19:38) := lapply(.SD, function(x) {
#   ifelse(is.na(x), "N", x)
# }), .SDcols = 19:38]

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


# convert all potential target columns to numeric
# sqf[ , (targets) := lapply(.SD,function(x) { ifelse(x == "Y", 1, 0)}), .SDcols = targets]
# convert sex to numeric 0 = female, 1 = male
# sqf[, SUSPECT_SEX := ifelse(SUSPECT_SEX == "FEMALE", 0, 1)]

# convert all remaining character columns to factor
char.cols <- which(sapply(sqf, is.character))
sqf[, (char.cols) := lapply(.SD, as.factor), .SDcols = char.cols]

# create factor columns
sqf$SUSPECT_RACE_DESCRIPTION <- factor(sqf$SUSPECT_RACE_DESCRIPTION,
                                            levels = c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC",
                                                       "WHITE","ASIAN / PACIFIC ISLANDER",
                                                       "MIDDLE EASTERN/SOUTHWEST ASIAN",
                                                       "AMERICAN INDIAN/ALASKAN NATIVE"))
sqf$STOP_LOCATION_BORO_NAME <- factor(sqf$STOP_LOCATION_BORO_NAME)
sqf$STOP_WAS_INITIATED <- factor(sqf$STOP_WAS_INITIATED)
sqf$MONTH2 <- factor(sqf$MONTH2)
sqf$DAY2 <- factor(sqf$DAY2)
sqf$SUSPECT_BODY_BUILD_TYPE <- factor(sqf$SUSPECT_BODY_BUILD_TYPE)
sqf$SUSPECT_EYE_COLOR <- factor(sqf$SUSPECT_EYE_COLOR)
sqf$SUSPECT_HAIR_COLOR <- factor(sqf$SUSPECT_HAIR_COLOR)

# creating groups for the race variable
other_racegroup <- c("ASIAN / PACIFIC ISLANDER","AMERICAN INDIAN/ALASKAN NATIVE", "MIDDLE EASTERN/SOUTHWEST ASIAN")
sqf[, SUSPECT_RACE_DESCRIPTION := ifelse(SUSPECT_RACE_DESCRIPTION %in% other_racegroup, "OTHER", SUSPECT_RACE_DESCRIPTION)]
sqf$SUSPECT_RACE_DESCRIPTION <- factor(sqf$SUSPECT_RACE_DESCRIPTION, levels = c("1", "2", "3", "4", "OTHER"),
                         labels = c("BLACK", "WHITE HISPANIC", "BLACK HISPANIC","WHITE", "OTHER"))

# binning of stop time of the day
# 6 - 12: AM
# 12 - 18: PM
# 18 - 22: evening
# 22 - 6 : night
# convert times to numeric values
sqf <- sqf |> 
  mutate(STOP_FRISK_TIME = sub(":", ".", substr(STOP_FRISK_TIME, 1, 5))) |> 
  mutate(STOP_FRISK_TIME = round(as.numeric(STOP_FRISK_TIME))) |> 
  mutate(STOP_FRISK_TIME = ifelse(STOP_FRISK_TIME == 24, 0, STOP_FRISK_TIME))

# bin time
sqf$STOP_FRISK_TIME <- cut(
  sqf$STOP_FRISK_TIME,
  breaks = c(0, 6, 12, 18, 24),
  labels = c("night", "AM", "PM", "evening"),
  right = FALSE
)

# remove levels that are very rare
hair.color <- c("GRN", "PLE", "PNK", "ORG", "SDY")
eye.color <- c("MAR", "MUL", "PNK", "OTH")

sqf <- sqf |> 
  filter(!SUSPECT_HAIR_COLOR %in% hair.color) |>
  filter(!SUSPECT_EYE_COLOR %in% eye.color)

# filter out complete cases
sqf.filtered <- sqf[complete.cases(sqf), ]

# create a column that is 1 if any of the targets columns is equal to "Y" and 0 otherwise
# sqf$TARGET <- ifelse(rowSums(sqf[, ..targets] == "Y") > 0, "Y", "N")

# remove targets columns from the data
# sqf <- sqf[, -..targets]



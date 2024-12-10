library(glmnet)
library(smurf)

sqf.2023 <- read_excel("data/sqf-2023.xlsx")
setDT(sqf.2023)
n <- nrow(sqf.2023)
sqf.2023[sqf.2023 == "(null)"] <- NA

# delete a few columns that wouldn't make sense as features
cols.to.remove <- c("STOP_ID", "STOP_FRISK_DATE", "STOP_FRISK_TIME", "DEMEANOR_OF_PERSON_STOPPED",
                    "SUSPECT_OTHER_DESCRIPTION", "STOP_LOCATION_FULL_ADDRESS", "STOP_LOCATION_X",
                    "STOP_LOCATION_Y", "STOP_LOCATION_APARTMENT", "YEAR2", "RECORD_STATUS_CODE", "STOP_LOCATION_STREET_NAME",
                    "SEARCH_BASIS_OUTLINE_FLAG", )
sqf.2023[ , (cols.to.remove) := NULL]

# convert al columns with numbers to numeric
sqf.2023[, (col.names) := lapply(.SD, function(x) {
  if (all(is.na(x) | grepl("[[:digit:]]+", x))) as.numeric(x) else x
}), .SDcols = col.names]

# convert all the columns that end on FLAG or FLG to factor
flag.cols <- grep("FLAG$|FLG$", names(sqf.2023))
sqf.2023[, (flag.cols) := lapply(.SD, as.factor), .SDcols = flag.cols]


# convert al remaining character columns to factor
char.cols <- which(sapply(sqf.2023, is.character))
sqf.2023[, (char.cols) := lapply(.SD, as.factor), .SDcols = char.cols]



# fit a logistic regression with LASSO regularization
set.seed(123)
sqf.2023 <- droplevels(sqf.2023)
sort(colSums(is.na(complete.cases)))
sqf.2023 <- na.omit(sqf.2023)
sqf.2023$PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG <- NULL

x <- model.matrix(SUSPECT_ARRESTED_FLAG ~ ., data = sqf.2023)
y <- sqf.2023$SUSPECT_ARRESTED_FLAG

lasso.cv <- cv.glmnet(x, y, family = binomial(link = "logit"), alpha = 1)
lambda.lasso <- lasso.cv$lambda.min
lasso.fit <- glmnet(x, y, family = binomial(link = "logit"), alpha = 1, lambda = lambda.lasso)
summary(lasso.fit)

coef(lasso.fit)

res <- apply(sqf.2023, 2, function(x) (length(levels(x))))
apply(sqf.2023, 2, \(x) length(unique(x)))
which(res == 1)

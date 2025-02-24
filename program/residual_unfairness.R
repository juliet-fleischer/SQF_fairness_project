
# Attempt to estimate the joint distribution of X,A for the target population

# 1. Target population (T = 1) ----
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

# remove the total population rows for each borough
target_pop <- target_pop |> 
  filter(group != "Total population") |>
  mutate(race_group = ifelse(group %in% c("Hispanic", "Black non-Hispanic"), "POC", "White")) |> 
  mutate(count = as.numeric(count)) |>
  group_by(race_group, borough) |>
  summarise(n = sum(count)) |> 
  ungroup() |> 
  mutate(prop = n / sum(n)) |> 
  rename("PA_GROUP" = race_group, "STOP_LOCATION_BORO_NAME" = borough) |> 
  select(STOP_LOCATION_BORO_NAME, PA_GROUP, everything()) |>  # change the entries to all upper case
  mutate(STOP_LOCATION_BORO_NAME = toupper(STOP_LOCATION_BORO_NAME))
  

# 2. Training population (Z = 1) ----
train_pop <- data2023 |> 
  group_by(STOP_LOCATION_BORO_NAME, PA_GROUP) |>
  mutate(STOP_LOCATION_BORO_NAME = as.character(STOP_LOCATION_BORO_NAME)) |>
  summarise(n = n()) |>
  ungroup() |> 
  mutate(prop = n / sum(n))

# 3. Weighing ----
# create the weights as ratio between probabilities
# join the data
weight_df <- left_join(
  target_pop,
  train_pop,
  by = c("STOP_LOCATION_BORO_NAME", "PA_GROUP"),
  suffix = c("_target", "_train")
)
weight_df <- weight_df |> 
  mutate(weight = prop_target / prop_train)


# 4. Match the weights ----
# have to get the PREDICITON of the people to count tp or fp
data2023_weights <- data2023 |> 
  left_join(weight_df, by = c("STOP_LOCATION_BORO_NAME", "PA_GROUP")) |>
  mutate(weight = ifelse(is.na(weight), 0, weight))
# predict on the whole dataset
train_preds_2023 <- lrn_rf_2023$predict(task_arrest_ex_ante, row_ids = data2023_weights$row_ids)
train_preds_2023_df <- as.data.table(train_preds_2023)
# combine the predictions with the data
data2023_weights <- cbind(data2023_weights, train_preds_2023_df)

# 5. Comparison ----
# compare the test error rates with estimated target error rates
calcGroupwiseMetrics(base_mrs_assistive, task_arrest_ex_ante, train_preds_2023)
calcGroupwiseMetrics(base_mrs_punitive, task_arrest_ex_ante, train_preds_2023)
getErrorRates(data2023_weights, "POC", "White")




# Attempt to estimate the joint distribution of X,A for the target population

# 1. Target population (T = 1) ----
target_pop <- lapply(1:5, \(i) assign(paste0("data_", i), read_xlsx("data/target_pop_data.xlsx", sheet = i)))
names(target_pop) <- c("Brooklyn", "Manhattan", "Bronx", "Queens", "Staten Island")
# for each tibble in target_pop select only the first three columns
target_pop <- lapply(target_pop, \(x) select(x, 1:2))
# rename the columns for each tibble
target_pop <- lapply(target_pop, function(x) {
  colnames(x) <- c("group", "count")
  return(x)
})
# bind everything together to one data frame
target_pop <- do.call(rbind, lapply(names(target_pop), \(x) cbind(target_pop[[x]], borough = x)))
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
train_pop <- complete_cases |> 
  group_by(STOP_LOCATION_BORO_NAME, PA_GROUP) |>
  mutate(STOP_LOCATION_BORO_NAME = as.character(STOP_LOCATION_BORO_NAME)) |>
  summarise(n = n()) |>
  ungroup() |> 
  mutate(prop = n / sum(n))

# 3. Weighing
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


# 4. Match the weights
# have to get the PREDICITON of the people to count tp or fp
complete_cases_weights <- complete_cases |> 
  left_join(weight_df, by = c("STOP_LOCATION_BORO_NAME", "PA_GROUP")) |>
  mutate(weight = ifelse(is.na(weight), 0, weight))
# predict on the whole dataset
train_preds <- lrn_rf$predict(task_arrest, row_ids = complete_cases_weights$row_ids)
train_preds <- as.data.table(train_preds)
# combine the predictions with the data
complete_cases_weights <- cbind(complete_cases_weights, train_preds)
d1 <- complete_cases_weights[truth == "Y" & response == "Y", .N,
                       by = c("PA_GROUP", "STOP_LOCATION_BORO_NAME", "weight")][order(PA_GROUP, STOP_LOCATION_BORO_NAME)]

d1 |> 
  group_by(PA_GROUP) |>
  summarise(numerator = sum(weight * N))

d2 <- complete_cases_weights[truth == "Y", .N,
                             by = c("PA_GROUP", "STOP_LOCATION_BORO_NAME", "weight")][order(PA_GROUP, STOP_LOCATION_BORO_NAME)]

d2 |>
  group_by(PA_GROUP) |>
  summarise(denominator = sum(weight * N))

# match the weights to observations in complete_cases df
# multiply the weights by counts and get the fpr for one PA_group

poc_positives <- complete_cases_weights[PA_GROUP == "POC" & truth == "Y"]
poc_numerator <- sum(poc_positives$weight[poc_positives$response == "Y"])
poc_denominator <- sum(poc_positives$weight)
poc_tpr <- poc_numerator / poc_denominator
poc_tpr

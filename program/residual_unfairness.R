
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
train_preds_2023 <- lrn_rf_2023$predict(task_arrest, row_ids = data2023_weights$row_ids)
calcGroupwiseMetrics(base_mrs_assistive, task_arrest, train_preds_2023)
calcGroupwiseMetrics(base_mrs_punitive, task_arrest, train_preds_2023)

train_preds_2023 <- as.data.table(train_preds_2023)
# combine the predictions with the data
data2023_weights <- cbind(data2023_weights, train_preds_2023)

# tpr for black people
data2023_weights |> 
  select(truth, response, PA_GROUP, weight, STOP_LOCATION_BORO_NAME) |>
  filter(truth == "Y", response == "Y", PA_GROUP == "POC") |>
  summarise(nominator = mean(weight))

data2023_weights |> 
  select(truth, response, PA_GROUP, weight, STOP_LOCATION_BORO_NAME) |>
  filter(truth == "Y", response == "N", PA_GROUP == "POC") |>
  summarise(denominator_1 = mean(weight))
# 0.5299261 for black

data2023_weights |> 
  select(truth, response, PA_GROUP, weight, STOP_LOCATION_BORO_NAME) |>
  filter(truth == "Y", response == "Y", PA_GROUP == "White") |>
  summarise(nominator = mean(weight))

data2023_weights |> 
  select(truth, response, PA_GROUP, weight, STOP_LOCATION_BORO_NAME) |>
  filter(truth == "Y", response == "N", PA_GROUP == "White") |>
  summarise(denominator_1 = mean(weight))
# 0.4937023 for white 




# Numerator: sum of weights where A=a, Y=1, predicted=1
num <- data2023_weights |>
  filter(PA_GROUP == "POC", truth == "Y", response == "Y") |>
  summarize(num = sum(weight)) |>
  pull(num)

# Denominator: sum of weights where A=a, Y=1 (regardless of predicted label)
den <- data2023_weights |>
  filter(PA_GROUP == "POC", truth == "Y") |>
  summarize(den = sum(weight)) |>
  pull(den)

num / den

# Numerator: sum of weights where A=a, Y=1, predicted=1
num2 <- data2023_weights |>
  filter(PA_GROUP == "White", truth == "Y", response == "Y") |>
  summarize(num = sum(weight)) |>
  pull(num)

# Denominator: sum of weights where A=a, Y=1 (regardless of predicted label)
den2 <- data2023_weights |>
  filter(PA_GROUP == "White", truth == "Y") |>
  summarize(den = sum(weight)) |>
  pull(den)

num2 / den2






d1 <- data2023_weights[truth == "Y" & response == "Y", .N,
                       by = c("PA_GROUP", "STOP_LOCATION_BORO_NAME", "weight")][order(PA_GROUP, STOP_LOCATION_BORO_NAME)]

d1 |> 
  group_by(PA_GROUP) |>
  summarise(numerator = sum(weight * N))

d2 <- data2023_weights[truth == "Y", .N,
                             by = c("PA_GROUP", "STOP_LOCATION_BORO_NAME", "weight")][order(PA_GROUP, STOP_LOCATION_BORO_NAME)]

d2 |>
  group_by(PA_GROUP) |>
  summarise(denominator = sum(weight * N))

# match the weights to observations in data2023 df
# multiply the weights by counts and get the fpr for one PA_group

poc_positives <- data2023_weights[PA_GROUP == "POC" & truth == "Y"]
poc_numerator <- sum(poc_positives$weight[poc_positives$response == "Y"])
poc_denominator <- sum(poc_positives$weight)
poc_tpr <- poc_numerator / poc_denominator
poc_tpr




# Attempt to estimate the joint distribution of X,A for the target population

# Target population T = 1
target_pop <- lapply(1:5, \(i) assign(paste0("data_", i), read_xlsx("data/target_pop_data.xlsx", sheet = i)))
names(target_pop) <- c("Brooklyn", "Manhattan", "Bronx", "Queens", "Staten_Island")
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
  mutate(count = as.numeric(count)) |>
  mutate(prop = count / sum(count))

# Training population Z = 1
sqf |> 
  group_by(STOP_LOCATION_BORO_NAME, SUSPECT_RACE_DESCRIPTION) |> 
  summarise(count = n()) |>
  ungroup() |> 
  mutate(prop = count / sum(count))

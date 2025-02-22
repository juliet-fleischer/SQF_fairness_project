theme_set(
  theme_minimal()
)


# Import target population data
# The data is from NYC Population FactFinder
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
other_racegroup <- c("Non-Hispanic of two or more races", "Some other race non-Hispanic")
target_pop <- target_pop |> 
  filter(group != "Total population") |>
  mutate(count = as.numeric(count)) |>
  mutate(group = ifelse(group %in% other_racegroup, "Other", group)) |> 
  mutate(group = factor(group,
                           levels = c("Black non-Hispanic", "Hispanic", "White non-Hispanic",
                                      "Asian non-Hispanic", "Other"),
                           labels = c("Black", "Hispanic", "White", "Asian", "Other")))
target_pop$count <- as.integer(gsub("\\.", "", target_pop$count))

# Target population vs. 2023 ----
# description for reduced data
p4 <- ggplot(data2023, aes(x = STOP_LOCATION_BORO_NAME, fill = SUSPECT_RACE_DESCRIPTION)) +
  geom_bar(position = "dodge") +
  labs(title = "SQF 2021",
       x = "",
       y = "Proportion of stops") +
  theme(legend.title = element_blank())

p5 <- ggplot(target_pop, aes(x = borough, y = count, fill = group)) +
  geom_col(position = "dodge") +
  labs(title = "target population",
       x = "",
       y = "Proportion of arrests") + # remove the legend completely
  theme(legend.position = "none")
p45 <- p4 / p5

p6 <- ggplot(data2023, aes(x = SUSPECT_RACE_DESCRIPTION)) +
  geom_bar() +
  xlab("Race distribution in SQF data 2021")
p7 <- target_pop |> 
  group_by(group) |> 
  summarise(count = sum(count)) |>
  ggplot(aes(x = group, y = count)) +
  geom_col() +
  xlab("Race distribution in NYC according to 2020 census") +
  scale_y_continuous(labels = scales::comma)
# put p6 and p7 underneath each other
p8 <- p6 / p7

# 2011 data ----
p11 <- ggplot(data2011, aes(x = city, fill = race)) +
  geom_bar(position = "fill") +
  labs(title = "SQF 2011",
       x = "",
       y = "Proportion of stops")

p12 <- ggplot(data2011, aes(x = race)) +
  geom_bar() +
  xlab("Race distribution in SQF data 2011") + # make readable y axis without scientific notation
  scale_y_continuous(labels = scales::comma)

ggplot(data2011, aes(x = race, fill = arstmade)) +
  geom_bar(position = "fill") +
  labs(title = "SQF 2011",
       x = "",
       y = "Proportion of arrests")

# Arrestment rates ----
## 2023 data ----
p13 <- data2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarise(arrest_rate = mean(SUSPECT_ARRESTED_FLAG == "Y")) |>
  ggplot(aes(x = SUSPECT_RACE_DESCRIPTION, y = arrest_rate)) +
  geom_col() +
  ylim(c(0, 0.4)) +
  xlab("2023")
## 2011 data ----
p14 <- data2011 |> 
  group_by(race) |> 
  summarise(arrest_rate = mean(arstmade == "Y")) |>
  ggplot(aes(x = race, y = arrest_rate)) +
  geom_col() +
  xlab("2011") +
  ylim(c(0, 0.4)) +
  theme(axis.text.y = element_blank())
p15 <- p13 + p14

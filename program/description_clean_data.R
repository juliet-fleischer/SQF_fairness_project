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
  geom_bar(position = "fill") +
  labs(title = "SQF 2021",
       x = "",
       y = "Proportion of stops") +
  theme(legend.title = element_blank())

p5 <- ggplot(target_pop, aes(x = borough, y = count, fill = group)) +
  geom_col(position = "fill") +
  labs(title = "NYC population",
       x = "",
       y = "Proportion") + # remove the legend completely
  theme(legend.position = "none")
p45 <- p4 / p5

p6 <- data2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  reframe(n = n()) |>
  ungroup() |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = SUSPECT_RACE_DESCRIPTION, y = prop)) +
  geom_col() +
  xlab("Sop, Question, Frisk data 2023") + # add percentage y legend
  scale_y_continuous(labels = scales::percent)

p7 <- target_pop |> 
  group_by(group) |> 
  reframe(count = sum(count)) |>
  ungroup() |> 
  mutate(prop = count / sum(count)) |>
  ggplot(aes(x = group, y = prop)) +
  geom_col() +
  xlab("census 2020") +
  scale_y_continuous(labels = scales::percent)
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
p16 <- data2023 |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  summarise(arrest_rate = mean(SUSPECT_ARRESTED_FLAG == "Y")) |>
  ggplot(aes(x = STOP_LOCATION_BORO_NAME, y = arrest_rate)) +
  geom_col() +
  xlab("2023")

# estimation of crime rates by borough
## problem is that we normalise based on 2020 census data
# but the crime rates come from 2024
target_pop_total <- target_pop |> 
  group_by(borough) |>
  reframe(total_n = sum(count))

bronx2024 <- 30223
brooklyn2024 <- 14906 + 15847
manhattan <-  14252 + 17135
queens <- 16782 + 11105
staten_island <- 3453

target_pop_total$crime_count <- c(bronx2024, brooklyn2024, manhattan, queens, staten_island)
target_pop_total$crime_rate <- (target_pop_total$crime_count / target_pop_total$total_n) * 100000

p17 <- ggplot(target_pop_total, aes(x = borough, y = crime_rate)) +
  geom_col() +
  xlab("") +
  ylab("Crime rate per 100,000 citizens")

# proportion of stops by borough
p18 <- data2023 |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  reframe(n = n()) |> 
  ungroup() |> 
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = STOP_LOCATION_BORO_NAME, y = prop)) +
  geom_col() +
  xlab("") +
  ylab("Proportion of stops") +
  scale_y_continuous(labels = scales::percent)

# combine thee plots p5, p17, p18
p19 <- p17 / p18

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

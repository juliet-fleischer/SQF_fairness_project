theme_set(
  theme_minimal(base_size = 30) +  # Increase the base size (default is 11)
    theme(legend.position = "top",
          plot.title = element_blank(),
          # plot.margin = unit(c(1, 1, 1, 1), "cm"),
          # panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()
    )
)


# Borough-wise Statistics ----
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

p11 <- ggplot(data2011, aes(x = city, fill = race)) +
  geom_bar(position = "fill") +
  labs(title = "SQF 2011",
       x = "",
       y = "Proportion of stops")

p18 <- data2023 |>
  mutate(STOP_LOCATION_BORO_NAME = factor(STOP_LOCATION_BORO_NAME,
                                          levels = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
                                          labels = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))) |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  reframe(n = n()) |> 
  ungroup() |> 
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = STOP_LOCATION_BORO_NAME, y = prop)) +
  geom_col() +
  xlab("Stop, Question, and Frisk data 2023") +
  ylab("prop") +
  scale_y_continuous(labels = scales::percent)

# Ethnic-wise Statistics ----
p6 <- data2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  reframe(n = n()) |>
  ungroup() |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = SUSPECT_RACE_DESCRIPTION, y = prop)) +
  geom_col() +
  xlab("Stop, Question, and Frisk data 2023") +
  scale_y_continuous(labels = scales::percent)

p7 <- target_pop |> 
  group_by(group) |> 
  reframe(count = sum(count)) |>
  ungroup() |> 
  mutate(prop = count / sum(count)) |>
  ggplot(aes(x = group, y = prop)) +
  geom_col() +
  xlab("Census 2020") +
  scale_y_continuous(labels = scales::percent)
# put p6 and p7 underneath each other
p8 <- p6 / p7

p12 <- ggplot(data2011, aes(x = race)) +
  geom_bar() +
  xlab("Race distribution in SQF data 2011") +
  scale_y_continuous(labels = scales::comma)


# Arrest Statistics ----
p13 <- data2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarise(arrest_rate = mean(SUSPECT_ARRESTED_FLAG == "Y")) |>
  ggplot(aes(x = SUSPECT_RACE_DESCRIPTION, y = arrest_rate)) +
  geom_col() +
  ylim(c(0, 0.4)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("") +
  ylab("prop")
p13_table <- data2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarise(arrest_rate = mean(SUSPECT_ARRESTED_FLAG == "Y")) |> 
  mutate(arrest_rate = scales::percent(arrest_rate)) |> 
  rename("group" = SUSPECT_RACE_DESCRIPTION, "prop" = arrest_rate)

p16 <- data2023 |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  summarise(arrest_rate = mean(SUSPECT_ARRESTED_FLAG == "Y")) |>
  ggplot(aes(x = STOP_LOCATION_BORO_NAME, y = arrest_rate)) +
  geom_col()
p14 <- data2011 |> 
  group_by(race) |> 
  summarise(arrest_rate = mean(arstmade == "Y")) |>
  ggplot(aes(x = race, y = arrest_rate)) +
  geom_col() +
  xlab("2011") +
  ylim(c(0, 0.4))
p14_table <- data2011 |> 
  group_by(race) |> 
  summarise(arrest_rate = mean(arstmade == "Y")) |>
  mutate(arrest_rate = scales::percent(arrest_rate)) |>
  rename("group" = race, "prop" = arrest_rate)

# Estimation of crime rates by borough ----
## we normalise based on 2020 census data but the crime rates come from 2024
target_pop_total <- target_pop |> 
  group_by(borough) |>
  reframe(total_n = sum(count))

# the numbers come from the yearly NYPD report: https://www.nyc.gov/site/nypd/stats/crime-statistics/borough-and-precinct-crime-stats.page
bronx <- 30223
brooklyn <- 14906 + 15847
manhattan <-  14252 + 17135
queens <- 16782 + 11105
staten_island <- 3453

target_pop_total$crime_count <- c(bronx, brooklyn, manhattan, queens, staten_island)
target_pop_total$crime_rate <- (target_pop_total$crime_count / target_pop_total$total_n) * 100000

p17 <- ggplot(target_pop_total, aes(x = borough, y = crime_rate)) +
  geom_col() +
  xlab("Estimated crime rate") +
  ylab("rate/100,000")


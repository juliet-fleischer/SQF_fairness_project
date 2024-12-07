## Descriptive Analysis


# age
ggplot(sqf.2023, aes(x = SUSPECT_REPORTED_AGE)) +
  geom_histogram() +
  ggtitle("Age distribution") +
  xlab("Age") +
  ylab("Frequency")
# geopgraphy
table(sqf.2023$STOP_LOCATION_BORO_NAME)
sqf.2023 |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  summarise(count = n()) |> 
  mutate(prop = count / sum(count)) |> 
  ggplot(aes(x = STOP_LOCATION_BORO_NAME, y = prop)) +
  geom_col() +
  ggtitle("Borough distribution") +
  xlab("Borough") +
  ylab("Relative Frequency")
# sex distribution
table(sqf.2023$SUSPECT_SEX) # extreme sec unbalance, 94% male and 6% female
# race distribution
sqf.2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarise(count = n()) |>
  mutate(prop = count / sum(count)) |>
  ggplot(aes(x = SUSPECT_RACE_DESCRIPTION, y = prop)) +
  geom_col() +
  ggtitle("Race distribution") +
  xlab("Race") +
  ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# improve: write a function and use apply family to iteratively apply it
# description of all possible targets by race
prop.data <- list()
for(a in protected.a[-1]) {
  prop.data[[a]] <- list()
  for (t in targets) {
    p <- sqf.2023 |> 
      mutate(a = factor(.data[[a]], levels = rev(levels(.data[[a]])))) |> 
      ggplot(aes(x = .data[[t]], fill = .data[[a]])) +
      geom_bar(position = "fill") +
      labs(x = t, fill = a, title = paste("Distribution of", t, "by", a)) +
      theme_minimal()
    print(p)
    
    prop.data[[a]][[t]] <- sqf.2023 |> 
      group_by(.data[[t]], .data[[a]]) |> 
      summarise(count = n()) |>
      mutate(prop = count / sum(count))
  }
}


# descrpition of arrested by race and boro name
ggplot(sqf.2023, aes(x = STOP_LOCATION_BORO_NAME, y = SUSPECT_ARRESTED_FLAG, fill = SUSPECT_RACE_DESCRIPTION)) +
  geom_col()

sqf.2023 |> 
  group_by(STOP_LOCATION_BORO_NAME, SUSPECT_RACE_DESCRIPTION) |>
  summarise(count_arrested = sum(SUSPECT_ARRESTED_FLAG)) |>
  group_by(STOP_LOCATION_BORO_NAME) |>
  mutate(prop = count_arrested / sum(count_arrested)) |> View()



# proportion of frisked by sex in the imputed and original data is virtually the same
# (so the imputed dataset is probably safe to use and doesn't introduce additional bias)
glimpse(sqf.2023)
xtabs(FRISKED_FLAG ~ SUSPECT_SEX, data = sqf.2023)
imputed_data_frisked |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(FRISKED_FLAG) / n())
sqf.2023 |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(FRISKED_FLAG) / n())
# proportion of arrested by sex in the imputed and original data is virtually the same
# for arrested too
imputed_data_arrested |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SUSPECT_ARRESTED_FLAG) / n())
sqf.2023 |>
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SUSPECT_ARRESTED_FLAG) / n())
# for searched
sqf.2023 |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SEARCHED_FLAG) / n())
imputed_data_searched |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SEARCHED_FLAG) / n())

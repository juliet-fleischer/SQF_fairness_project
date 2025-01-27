## Descriptive Analysis
# set a general plot theme

# theme_minimal() +
#   theme(plot.caption = element_text(hjust = 0))

## classifiation of attributes based to their percentage of missing values
p1 <- ggplot(na.count.df, aes(x = na.count.binned, fill = color_code)) +
  geom_bar() +
  labs(x = "Percentage of missing values", y = "Number of attributes",
       caption = "Fig. x: classifiation of variables based to their percentage of missing values")

## RACE
# turn the x axis ticks by 45 degrees
p2 <- ggplot(sqf, aes(x = SUSPECT_RACE_DESCRIPTION, fill = SUSPECT_ARRESTED_FLAG)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3a <- ggplot(sqf, aes(x = SUSPECT_RACE_DESCRIPTION, fill = SUSPECT_ARRESTED_FLAG)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("True label by race of the whole dataset")
# put p2 and p3 next to each other in one plot panel
grid.arrange(p2, p3, ncol = 2)
p3b <- ggplot(predictions_dt, aes(x = SUSPECT_RACE_DESCRIPTION, fill = response)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Predictions by race of the test dataset")
ggplot(predictions_dt, aes(x = SUSPECT_RACE_DESCRIPTION, fill = truth)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("True label by race of the test dataset")


# put the proportion of each race group in the sample against the ethnic distribution og NYC
# in general
t1 <- sqf |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarise(n = n()) |> 
  mutate(prop = n / sum(n))



# SEX
sqf |> 
  select(STOP_ID, SUSPECT_ARRESTED_FLAG, SUSPECT_SEX) |>
  mutate(across(everything(), as.factor)) |> 
  group_by(SUSPECT_ARRESTED_FLAG, SUSPECT_SEX) |> 
  reframe(count = n()) |> 
  mutate(prop = count / sum(count)) |> 
  ggplot(aes(x = SUSPECT_SEX, y = prop, fill = SUSPECT_ARRESTED_FLAG)) +
  geom_col(position = "dodge")
  
  

# age
ggplot(sqf, aes(x = SUSPECT_REPORTED_AGE)) +
  geom_histogram() +
  ggtitle("Age distribution") +
  xlab("Age") +
  ylab("Frequency")
# geopgraphy
table(sqf$STOP_LOCATION_BORO_NAME)
sqf |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  summarise(count = n()) |> 
  mutate(prop = count / sum(count)) |> 
  ggplot(aes(x = STOP_LOCATION_BORO_NAME, y = prop)) +
  geom_col() +
  ggtitle("Borough distribution") +
  xlab("Borough") +
  ylab("Relative Frequency")
# sex distribution
table(sqf$SUSPECT_SEX) # extreme sec unbalance, 94% male and 6% female


# improve: write a function and use apply family to iteratively apply it
# description of all possible targets by race
prop.data <- list()
for(a in protected.a[-1]) {
  prop.data[[a]] <- list()
  for (t in targets) {
    p <- sqf |> 
      mutate(a = factor(.data[[a]], levels = rev(levels(.data[[a]])))) |> 
      ggplot(aes(x = .data[[t]], fill = .data[[a]])) +
      geom_bar(position = "fill") +
      labs(x = t, fill = a, title = paste("Distribution of", t, "by", a)) +
      theme_minimal()
    print(p)
    
    prop.data[[a]][[t]] <- sqf |> 
      group_by(.data[[t]], .data[[a]]) |> 
      summarise(count = n()) |>
      mutate(prop = count / sum(count))
  }
}


# descrpition of arrested by race and boro name
ggplot(sqf, aes(x = STOP_LOCATION_BORO_NAME, y = SUSPECT_ARRESTED_FLAG, fill = SUSPECT_RACE_DESCRIPTION)) +
  geom_col()

sqf |> 
  group_by(STOP_LOCATION_BORO_NAME, SUSPECT_RACE_DESCRIPTION) |>
  summarise(count_arrested = sum(SUSPECT_ARRESTED_FLAG)) |>
  group_by(STOP_LOCATION_BORO_NAME) |>
  mutate(prop = count_arrested / sum(count_arrested)) |> View()



# proportion of frisked by sex in the imputed and original data is virtually the same
# (so the imputed dataset is probably safe to use and doesn't introduce additional bias)
glimpse(sqf)
xtabs(FRISKED_FLAG ~ SUSPECT_SEX, data = sqf)
imputed_data_frisked |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(FRISKED_FLAG) / n())
sqf |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(FRISKED_FLAG) / n())
# proportion of arrested by sex in the imputed and original data is virtually the same
# for arrested too
imputed_data_arrested |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SUSPECT_ARRESTED_FLAG) / n())
sqf |>
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SUSPECT_ARRESTED_FLAG) / n())
# for searched
sqf |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SEARCHED_FLAG) / n())
imputed_data_searched |> 
  group_by(SUSPECT_SEX) |> 
  summarise(sum(SEARCHED_FLAG) / n())

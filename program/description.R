## Descriptive Analysis


# age
ggplot(sqf.complete.2023, aes(x = SUSPECT_REPORTED_AGE)) +
  geom_histogram() +
  ggtitle("Age distribution") +
  xlab("Age") +
  ylab("Frequency")
# geopgraphy
table(sqf.complete.2023$STOP_LOCATION_BORO_NAME)
sqf.complete.2023 |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  summarise(count = n()) |> 
  mutate(prop = count / sum(count)) |> 
  ggplot(aes(x = STOP_LOCATION_BORO_NAME, y = prop)) +
  geom_col() +
  ggtitle("Borough distribution") +
  xlab("Borough") +
  ylab("Relative Frequency")
# sex
table(sqf.complete.2023$SUSPECT_SEX) # extreme sec unbalance, 94% male and 6% female
# race
sqf.complete.2023 |> 
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
prop.data <- list()
for(a in protected.a[-1]) {
  prop.data[[a]] <- list()
  for (t in targets) {
    p <- sqf.complete.2023 |> 
      mutate(a = factor(.data[[a]], levels = rev(levels(.data[[a]])))) |> 
      ggplot(aes(x = .data[[t]], fill = .data[[a]])) +
      geom_bar(position = "fill") +
      labs(x = t, fill = a, title = paste("Distribution of", t, "by", a)) +
      theme_minimal()
    print(p)
    
    prop.data[[a]][[t]] <- sqf.complete.2023 |> 
      group_by(.data[[t]], .data[[a]]) |> 
      summarise(count = n()) |>
      mutate(prop = count / sum(count))
  }
}






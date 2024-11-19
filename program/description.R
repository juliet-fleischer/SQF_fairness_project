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


# people stopped because suspected to have a weapon and whether they actually had a weapon
sqf.2023[SUSPECTED_CRIME_DESCRIPTION == "CPW", sum(WEAPON_FOUND_FLAG, na.rm = TRUE) / .N]

sqf.2023[, .(prop = sum(WEAPON_FOUND_FLAG, na.rm = TRUE) / .N), by = SUSPECT_RACE_DESCRIPTION][order(prop)]
sqf.2023[, .(prop = sum(WEAPON_FOUND_FLAG, na.rm = TRUE) / .N), by = SUSPECT_SEX]

sqf.2023 |> 
  filter(!is.na(SUSPECT_RACE_DESCRIPTION)) |> 
  ggplot(aes(x = WEAPON_FOUND_FLAG, fill = SUSPECT_RACE_DESCRIPTION)) +
  geom_bar()
table(sqf.2023[WEAPON_FOUND_FLAG == 0, ]$SUSPECT_RACE_DESCRIPTION)

sqf.2023 |> 
  filter(WEAPON_FOUND_FLAG == 0) |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count))

sqf.2023 |> 
  filter(WEAPON_FOUND_FLAG == 1) |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count))


sqf.2023 |> 
  filter(WEAPON_FOUND_FLAG == 0) |> 
  group_by(SUSPECT_SEX) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count))

sqf.2023 |> 
  filter(WEAPON_FOUND_FLAG == 1) |> 
  group_by(SUSPECT_SEX) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count))


# WEAPON_FOUND_FLAG ~ protected attributes, build, stop_in_out, officer in uniform, weight, height, month, day, STOP_WAS_INITIATED

form.basic <-  WEAPON_FOUND_FLAG ~ SUSPECT_REPORTED_AGE + STOP_LOCATION_BORO_NAME +
  SUSPECT_SEX + SUSPECT_RACE_DESCRIPTION + LOCATION_IN_OUT_CODE + STOP_WAS_INITIATED +
  MONTH2 + DAY2 + STOP_DURATION_MINUTES + OFFICER_EXPLAINED_STOP_FLAG +
  OFFICER_IN_UNIFORM_FLAG + ASK_FOR_CONSENT_FLG + CONSENT_GIVEN_FLG +
  SUSPECT_HEIGHT + SUSPECT_WEIGHT + SUSPECT_BODY_BUILD_TYPE + SUSPECT_EYE_COLOR +
  SUSPECT_HAIR_COLOR + STOP_LOCATION_PRECINCT

x <- model.matrix(form.basic, data = sqf.2023)
# Extract the row indices by checking the row names of 'x'
used_indices <- as.numeric(rownames(x))
# Subset y using the same indices
y <- sqf.2023$WEAPON_FOUND_FLAG[used_indices]

lambda.lasso <- cv.glmnet(x = x, y = y, family = "binomial", alpha = 1)$lambda.min

model.lasso <- glmnet(x = x, y = y, family = "binomial", alpha = 1, lambda = lambda.lasso)
model.lasso$beta



# predict with this model on training data + test fairness defs


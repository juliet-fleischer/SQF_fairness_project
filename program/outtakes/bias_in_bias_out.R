# filter only searched individuals
data2023_searched <- data2023 |>
  filter(SEARCHED_FLAG == "Y")
# randomly split data into two paritions
set.seed(123)
split1_idx <- sample(1:nrow(data2023_searched), size = nrow(data2023_searched) * 0.5)
data2023_searched_split1 <- data2023_searched[split1_idx, ]
data2023_searched_split2 <- data2023_searched[-split1_idx, ]

# Stage 1: the officers decision to stop someone
train_data_1 <- data2023_searched_split1 |> 
  select(SUSPECTED_CRIME_DESCRIPTION, SUSPECT_SEX, PA_GROUP,
         SUSPECT_REPORTED_AGE, SUSPECT_BODY_BUILD_TYPE, STOP_LOCATION_BORO_NAME,
         WEAPON_FOUND_FLAG)
# fit a logistic regression with contrabn as the target
model_1 <- glm(WEAPON_FOUND_FLAG ~ ., data = train_data_1, family = "binomial")

# Stage 2: the automated stopping rule based on a biased algortihm
# make predictions with model_1 on data2023_searched_split2
data2023_searched_split2$y_hat <- predict(model_1, data2023_searched_split2, type = "response")
# summary statistics of the prediction scores per race group
yhat_stats_df <- data2023_searched_split2 |>
  group_by(PA_GROUP) |>
  reframe(quantiel_values = quantile(y_hat, probs = c(0.50, 0.45, 0.40, 0.35))) |> 
  mutate(quantile_names = rep(c("0.95", "0.90", "0.85", "0.80"), 2))
# yhat_stats_df <- yhat_stats_df |> 
#   pivot_wider(names_from = quantile_names, values_from = quantiel_values)

# the automates search rule
tau <- c(0.05, 0.1, 0.15, 0.20)
# Initialize an empty list to store results
results_list <- list()
searched_count_vector <- c()

for (i in seq_along(tau)) {
  browser()
  t <- tau[i]
  c_POC <- yhat_stats_df$quantiel_values[[i]]
  
  data2023_searched_split2_poc <- data2023_searched_split2 |>
    filter(PA_GROUP == "POC") |>
    mutate(searched_automated = ifelse(y_hat > c_POC, "Y", "N"))
  
  data2023_searched_split2_white <- data2023_searched_split2 |>
    filter(PA_GROUP == "White") |>
    mutate(searched_automated = ifelse(y_hat > c_POC + t, "Y", "N"))  # <- Use 't' instead of 'tau'
  
  data2023_searched_split2 <- rbind(data2023_searched_split2_poc, data2023_searched_split2_white)
  # get count of searched individuals
  searched_count[i] <- data2023_searched_split2 |> 
    group_by(PA_GROUP) |>
    reframe(searched_count = sum(searched_automated == "Y")) |> 
    pull(searched_count)
  
  # Re-estimate based on people searched by automated rule
  train_data_2 <- data2023_searched_split2 |> 
    filter(searched_automated == "Y") |> 
    select(SUSPECT_SEX, PA_GROUP,
           SUSPECT_REPORTED_AGE, SUSPECT_BODY_BUILD_TYPE, STOP_LOCATION_BORO_NAME,
           WEAPON_FOUND_FLAG)
  
  model_2 <- glm(WEAPON_FOUND_FLAG ~ ., data = train_data_2, family = "binomial")
  
  # Predict on the second split
  data2023$y_hat_re <- predict(model_2, data2023, type = "response")
  
  median_risk <- median(data2023$y_hat_re)
  
  # Compute summary statistics
  res_df <- data2023 |> 
    group_by(PA_GROUP) |>
    reframe(top_risk_count = sum(y_hat_re > median_risk))
    # reframe(quantile_values = quantile(y_hat_re, probs = c(0.5, 0.75, 0.9, 0.95))) |> 
    # mutate(quantile_names = rep(c("0.50", "0.75", "0.90", "0.95"), 2))
  
  # Store results
  results_list[[as.character(t)]] <- res_df
  
  # return searched_count for each round
}

# Combine all results into a single dataframe after the loop
final_results <- bind_rows(results_list, .id = "tau")

# Print or return final results
print(final_results)
print(searched_count)
# Repeat for varying group-specific thresholds



# I cannot replicate the study
data2023 |> 
  group_by(PA_GROUP) |> 
  reframe(n = n(),
          searched_count = sum(SEARCHED_FLAG == "Y"),
          weapon_count = sum(WEAPON_FOUND_FLAG == "Y"),
          arrested_count = sum(SUSPECT_ARRESTED_FLAG == "Y"))
# The thing is that, from a descriptive standpoint,
# relatively speaking more african americans were searched
# around 43% of african americans were searched
# while around 41% of white people were searched.
# Going the next step, for around 17% of African Americans
# a weapon was found while only 9% of white people had a weapon.
# So this would actually say that white people are slightly less often searched. yes
# but they also carry a weapon less often. And relatively seen
# they are actually more "too often" searched than PoC are to often searched.

# As a comparison it would be interesting to see how things behave for arrestment
# because for arrestment, in the data descriptively, the arrestment rate for
# white people is higher than for black people. 
# So simply put, balck people are more often stopped and also carry a weapon more often than white pople.
# but also are arrested less often than white people.
# but now when I formulate is like this it really does not seem discriminatory against
# blck people.



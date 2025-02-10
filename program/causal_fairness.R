
# Step 1: Create a toy dataset
set.seed(42) # For reproducibility
toy_data <- data.frame(
  A = rbinom(100, 1, 0.5),  # Protected attribute (e.g., 0 = "Group 1", 1 = "Group 2")
  X = rnorm(100, mean = 50, sd = 10), # Covariate (e.g., age, income)
  Y = rnorm(100, mean = 10, sd = 2)   # Outcome variable (e.g., salary)
)

# Step 2: Calculate the stabilized weights
# We'll assume we have a logistic model predicting A based on X
library(ipw)  # For calculating inverse probability weights

# Fit a logistic regression to predict A from X
weight_model <- glm(A ~ X, data = toy_data, family = binomial)

# Predicted probabilities for A
prob_A_given_X <- predict(weight_model, type = "response")

# Calculate the stabilized weights (swi)
# Pr(A = a) is the marginal probability (mean of A)
p_A <- mean(toy_data$A == 1)
toy_data$weights <- ifelse(toy_data$A == 1,
                           p_A / prob_A_given_X,    # For A = 1
                           (1 - p_A) / (1 - prob_A_given_X)) # For A = 0

# Step 3: Perform a weighted regression
# Regress Y on A and X, using the stabilized weights
weighted_model <- lm(Y ~ A + X, data = toy_data, weights = toy_data$weights)

# Step 4: Interpret the results
summary(weighted_model)

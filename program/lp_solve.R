# Install and load lpSolve if not already installed:
# install.packages("lpSolve")
library(lpSolve)

# Estinate form the data:
alpha0 <- 0.6   # P(Y=0) in Group A; truly negative in group A
beta0  <- 0.4   # P(Y=1) in Group A; truly negative in group B
alpha1 <- 0.5   # P(Y=0) in Group B; truly positive in group A
beta1  <- 0.5   # P(Y=1) in Group B; truly positive in group B

# Objective function coefficients:
# We want to minimize: 0.6 * p00 - 0.4 * p10 + 0.5 * p01 - 0.5 * p11.
# The order of variables is: p00, p10, p01, p11.
f.obj <- c(alpha0, -beta0, alpha1, -beta1)

# Construct the constraint matrix.
# Constraint 1 (FPR equality): alpha0 * p00 - alpha1 * p01 = 0
# Constraint 2 (TPR equality): beta0 * p10 - beta1 * p11 = 0
f.con <- matrix(c(alpha0,  0,   -alpha1,   0,
                  0,      beta0,  0,     -beta1), 
                nrow = 2, byrow = TRUE)

# Right-hand side of the constraints:
f.rhs <- c(0, 0)

# Constraint directions:
f.dir <- c("=", "=")

# Set lower and upper bounds for each variable (between 0 and 1)
lower_bounds <- rep(0, 4)
upper_bounds <- rep(1, 4)

# Solve the linear program:
solution <- lp(direction = "min",
               objective.in = f.obj,
               const.mat = f.con,
               const.dir = f.dir,
               const.rhs = f.rhs,
               all.int = FALSE,
               compute.sens = TRUE,
               transpose.constraints = FALSE,
               lower = lower_bounds,
               upper = upper_bounds)

# Display the optimal decision variables:
optimal_p <- solution$solution
names(optimal_p) <- c("p00 (GrpA, Y=0)", "p10 (GrpA, Y=1)",
                      "p01 (GrpB, Y=0)", "p11 (GrpB, Y=1)")
print(optimal_p)

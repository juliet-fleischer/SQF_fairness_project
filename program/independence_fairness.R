# Independence Fairness analysis
# PA: Sex (1 = male, 0 = female)

# 3.1.1. Statistical Parity for SUSPECT_ARRESTED_FLAG
# P(d = 1 | Y = 1) = P(d = 1 | Y = 0)
# Results are somewhats unexpected and one reason can be the class imbalance!

test.set[, .(prop = mean(response)), by = .(SUSPECT_SEX)]
# It is more likely for females to be arrested
test.set[, .(prop = mean(response)), by = SUSPECT_RACE_DESCRIPTION][order(prop)]
test.set[, .N, by = SUSPECT_RACE_DESCRIPTION]

# Conditional statistical parity
test.set[, .(prop = mean(response)), by = .(SUSPECT_SEX, STOP_LOCATION_BORO_NAME)][order(SUSPECT_SEX)]
# WEAPON_FOUND_FLAG another reasonable variable tot ake into account


# 3.2.1. Predictive parity
test.set[, .(prop = mean(SUSPECT_ARRESTED_FLAG)), by = .(SUSPECT_SEX, response)]

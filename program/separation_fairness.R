# False positive error rate balance/ Predictive equality
test.set[, .(prop = mean(response)), by = .(SUSPECT_ARRESTED_FLAG, SUSPECT_SEX)]

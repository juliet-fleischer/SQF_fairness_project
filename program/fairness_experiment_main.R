set.seed(024)

theme_set(
  theme_minimal(base_size = 30) +  # Increase the base size (default is 11)
    theme(legend.position = "top",
          plot.title = element_blank(),
          panel.grid.minor = element_blank()
    )
)

# 1. Fairness metrics ----
## punitive base measures ----
base_mrs_punitive <- list(
  fpr = msr("classif.fpr"),
  ppv = msr("classif.ppv"),
  fdr = msr("classif.fdr")
)

# assistive base measures ----
base_mrs_assistive <- list(
  tpr = msr("classif.tpr"),
  npv = msr("classif.npv")
)

# mixed base measures ----
base_mrs_other <- list(
  acc = msr("classif.acc")
)

# punitive mlr3 measures ----
fairness_msr_punitive <- msrs(c("fairness.fpr","fairness.tnr","fairness.ppv"))

# assistive mlr3 measures ----
fairness_msr_assistive <- msrs(c("fairness.fnr","fairness.tpr", "fairness.npv",
                                 "fairness.fomr"))
# mixed mlr3 measures ---- 
fairness_msr_other <- msrs(c("fairness.acc", "fairness.cv", "fairness.eod"))

# 2. Data, learner and task ----

## regular learner ----
lrn_rf_2023 = lrn("classif.ranger", predict_type = "prob")
lrn_rf_2023$id = "ranger_rf"

## pre-processing: reweighing ----
l1 = as_learner(mlr3pipelines::`%>>%`(po("reweighing_wts"), lrn("classif.rpart")))
l1$id = "reweight"

## post-processing: Equalized Odds ----
l2 = as_learner(po("learner_cv", lrn("classif.ranger")) %>>%
                  po("EOd"))
l2$id = "EOd"

## inprocessing: fair logistic regression ----
l3 = as_learner(po("collapsefactors") %>>% lrn("classif.fairzlrm"))
l3$id = "fairzlrm"

## classification task ----
task_arrest_2023 <- as_task_classif(data2023, target = "SUSPECT_ARRESTED_FLAG",
                                       positive = "Y", response_type = "prob")
# set the PA
task_arrest_2023$set_col_roles("PA_GROUP", "pta")
# split the data
data2023_split <- partition(task_arrest_2023)


# 2. Fairness Auditing ----
## regular RF 2023 ----
lrn_rf_2023 <- readRDS("data/trained_rf_2023.rds") # read in the trained learner
# lrn_rf_2023$train(task_arrest_2023, data2023_split$train) # alternatively train the learner
preds_2023 <- lrn_rf_2023$predict(task_arrest_2023, data2023_split$test)
preds_2023_dt <- as.data.table(preds_2023)
preds_2023_dt$pa_group <- data2023[data2023_split$test, PA_GROUP]

p1_rf <- fairness_prediction_density(preds_2023, task = task_arrest_2023) +
  xlim(0, 1)

p2_rf <- compare_metrics(preds_2023,
                         msrs(c("fairness.acc", "fairness.fpr", "fairness.ppv", "fairness.tpr")),
                      task = task_arrest_2023) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

res1 <- calcGroupwiseMetrics(base_mrs_assistive, task_arrest_2023, preds_2023)
res2 <- calcGroupwiseMetrics(base_mrs_punitive, task_arrest_2023, preds_2023)
res3 <- calcGroupwiseMetrics(base_mrs_other, task_arrest_2023, preds_2023)
res_all <- cbind(res1, res2, res3)

## regular RF 2011 ----
lrn_rf_2011 <- readRDS("data/trained_rf_2011.rds") # see analysis_2011.R for the training

# 3. Experiment ----
lrns = list(lrn_rf_2023, lrn_rf_2011, l1, l2, l3)
bmr <- readRDS("data/bmr_results.rds") # read in the benchmark results
# bmr = benchmark(benchmark_grid(task_arrest_2023, lrns, rsmp("cv", folds = 5))) # alternatively run the benchmark
meas = msrs(c("classif.acc", "fairness.eod", "fairness.tpr"))
bmr$aggregate(meas)[, .(learner_id, classif.acc, fairness.equalized_odds)]

p3 <- fairness_accuracy_tradeoff(bmr, fairness_measure = msr("fairness.tpr"),
                           accuracy_measure = msr("classif.ce")) +
  scale_color_viridis_d("Learner") +
  theme(legend.position = "right") +
  geom_point(size = 4) +
  theme_minimal(base_size = 26)


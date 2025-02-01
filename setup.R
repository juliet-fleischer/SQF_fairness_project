# setup script
set.seed(024)
options(java.parameters = "-Xmx12g") # Allocate 8 GB of memory (adjust as needed)
library(data.table)
library(readxl)
library(ggplot2)
library(ggmosaic)
library(pROC)
library(tidyverse)
library(mlr3verse)
library(mlr3fairness)
library(mlr3measures)
library(mlr3learners)
library(mlr3pipelines)
library(checkmate)
library(iml)
library(fairml)
library(linprog)
library(CVXR)
library(gridExtra)
library(rJava)
# library(randomForest)
# library(mlr3extralearners)
# options(java.parameters = "-Xmx8G")  # Zuweisung auf 8 GB
# library(RWeka)


source("program/functions.R")
source("program/data_cleaning.R")
# source("program/pre_training.R")


# FIREARM_FLAG until SEARCH_BASIS_OUTLINE_FLAG set all the NA to "N"
# 
# imputed_data_arrested$SUSPECT_ARRESTED_FLAG <- as.numeric(imputed_data_arrested$SUSPECT_ARRESTED_FLAG)
# imputed_data_arrested$FRISKED_FLAG <- as.numeric(imputed_data_arrested$FRISKED_FLAG)



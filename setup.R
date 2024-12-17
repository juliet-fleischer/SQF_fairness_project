# setup script
set.seed(024)
library(data.table)
library(readxl)
library(ggplot2)
library(pROC)
library(tidyverse)
library(mlr3verse)
library(mlr3fairness)
library(mice)
library(checkmate)
library(iml)
library(fairml)
library(linprog)
library(CVXR)
library(randomForest)
library(mlr3extralearners)


source("program/functions.R")
source("program/data_cleaning.R")
source("program/pre_training.R")


# FIREARM_FLAG until SEARCH_BASIS_OUTLINE_FLAG set all the NA to "N"
# 
# imputed_data_arrested$SUSPECT_ARRESTED_FLAG <- as.numeric(imputed_data_arrested$SUSPECT_ARRESTED_FLAG)
# imputed_data_arrested$FRISKED_FLAG <- as.numeric(imputed_data_arrested$FRISKED_FLAG)


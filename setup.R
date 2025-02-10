# setup script
set.seed(024)
options(java.parameters = "-Xmx12g") # Allocate 8 GB of memory (adjust as needed)
library(data.table)
library(readxl)
library(pROC)
library(tidyverse)
library(patchwork)
library(mlr3fairness)
library(mlr3measures)
library(mlr3learners)
library(mlr3pipelines)
library(checkmate)
library(iml)
# library(CVXR) # this package also uses the "%>>%" operator - be careful when loading it with mlr3pipelines
# library(conflicted) shows conflicts when operators defined by mutiple packages are used
library(gridExtra)
library(rJava)


source("program/functions.R")
source("program/data_cleaning.R")


# FIREARM_FLAG until SEARCH_BASIS_OUTLINE_FLAG set all the NA to "N"
# 
# imputed_data_arrested$SUSPECT_ARRESTED_FLAG <- as.numeric(imputed_data_arrested$SUSPECT_ARRESTED_FLAG)
# imputed_data_arrested$FRISKED_FLAG <- as.numeric(imputed_data_arrested$FRISKED_FLAG)



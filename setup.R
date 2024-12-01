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


sqf.2023 <- read_excel("data/sqf-2023.xlsx")

source("program/data_cleaning.R")
source("program/functions.R")



imputed_data <- read_excel("data/imputed_data_frisked.xlsx")
imputed_data_arrested <- read_excel("data/imputed_data_arrested.xlsx")
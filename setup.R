# setup script

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

source("program/data_cleaning.R")
source("program/functions.R")
sqf.2023 <- read_excel("data/sqf-2023.xlsx")
imputed_data <- read_excel("data/imputed_data.xlsx")
imputed_data[, sapply(imputed_data, is.character)] <- lapply(imputed_data[, sapply(imputed_data, is.character)], as.factor)
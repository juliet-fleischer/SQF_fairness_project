# load necessary packages
set.seed(024)
options(java.parameters = "-Xmx12g")
library(data.table)
library(readxl)
library(tidyverse)
library(patchwork)
library(mlr3fairness)
library(mlr3measures)
library(mlr3learners)
library(mlr3pipelines)
library(checkmate)
library(iml)
library(lubridate)
library(conflicted)
library(rJava)

# source custom functions
source("program/functions.R")

# read in the clean datasets
data2011 <- readRDS("data/data2011.rds")
data2023 <- readRDS("data/data2023.rds")





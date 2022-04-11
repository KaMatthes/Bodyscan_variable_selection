library(dplyr)
library(lubridate)
library(openxlsx)
library(coxed)
library(tidyverse)

bootnum <- 2000
setseed <- 20210519 # I would not change this, you need this to get always the same results by running bootstrapping

#### load R skript
source("R/data.R") # load data
source("R/Bootstrapping.R")  # load bootstrapping function
source("R/lineareRegression.R")  # load bootstrapping function


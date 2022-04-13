library(dplyr)
library(lubridate)
library(openxlsx)
library(coxed)
library(tidyverse)

bootnum <- 2000
setseed <- 20210519 # for bootstrapping

#### load R skript
source("R/data.R") # load data
source("R/Bootstrapping.R")  # load bootstrapping function
source("R/lineareRegression.R")  # load bootstrapping function


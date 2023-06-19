library(dplyr)
library(lubridate)
library(openxlsx)
library(coxed)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(caret)
library(cowplot)


bootnum <- 2000
setseed <- 20210519 # for bootstrapping
ctrl <- trainControl(method = "cv", number = 5)
boot_val <- trainControl(method = "boot", number =2000)

#### load R skript
source("R/data.R") # load data
source("R/Bootstrapping.R")  # load bootstrapping function
source("R/function_data.R")  # load function for data analyis
source("R/lineareRegression.R") # lineare regression
source("R/Table1_wilcox_chi2tests.R") # Table 1 Paper
source("R/plots_body_composition.R") # Boxplots
source("R/Appendix_Table1.R") # Table 1 Appendix


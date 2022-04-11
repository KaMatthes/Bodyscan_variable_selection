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

#### Variable selection
#VFM
# Men VFM V1
Results_MenVFM1 <- funktion_bootstrap(data.selection=MenVFM1,Age_pred="No",name.data="MenVFM1",response="visceral.adipose.tissue.value",setseed=setseed,bootnum=bootnum)
# Men VFM V2
Results_MenVFM2 <- funktion_bootstrap(data.selection=MenVFM2, Age_pred="No", name.data="MenVFM2",response="visceral.adipose.tissue.value",setseed=setseed,bootnum=bootnum)
# Women VFM V1
Results_WomenVFM1 <- funktion_bootstrap(data.selection=WomenVFM1, Age_pred="No", name.data="WomenVFM1",response="visceral.adipose.tissue.value",setseed=setseed,bootnum=bootnum)
# Women VFM V2 
Results_WomenVFM2 <- funktion_bootstrap(data.selection=WomenVFM2, Age_pred="No", name.data="WomenVFM2",response="visceral.adipose.tissue.value",setseed=setseed,bootnum=bootnum)
#AFM
# Men AFM V1
Results_MenAFM1 <- funktion_bootstrap(data.selection=MenAFM1, Age_pred="No", name.data="MenAFM1",response="Absolute.fat.mass.value",setseed=setseed,bootnum=bootnum)
# Men AFM V2
Results_MenAFM2 <- funktion_bootstrap(data.selection=MenAFM2, Age_pred="No", name.data="MenAFM2",response="Absolute.fat.mass.value",setseed=setseed,bootnum=bootnum)
# Women AFM V1
Results_WomenAFM1 <- funktion_bootstrap(data.selection=WomenAFM1, Age_pred="No", name.data="WomenAFM1",response="Absolute.fat.mass.value",setseed=setseed,bootnum=bootnum)
# Women AFM V2
Results_WomenAFM2 <- funktion_bootstrap(data.selection=WomenAFM2, Age_pred="No", name.data="WomenAFM2",response="Absolute.fat.mass.value",setseed=setseed,bootnum=bootnum)
#RFM
# Men RFM V1
Results_MenRFM1 <- funktion_bootstrap(data.selection=MenRFM1, Age_pred="No", name.data="MenRFM1",response="Relative.fat.mass.value",setseed=setseed,bootnum=bootnum)
# Men RFM V2
Results_MenRFM2 <- funktion_bootstrap(data.selection=MenRFM2, Age_pred="No", name.data="MenRFM2",response="Relative.fat.mass.value",setseed=setseed,bootnum=bootnum)
# Women RFM V1
Results_WomenRFM1 <- funktion_bootstrap(data.selection=WomenRFM1, Age_pred="No", name.data="WomenRFM1",response="Relative.fat.mass.value",setseed=setseed,bootnum=bootnum)
# Women RFM V2
Results_WomenRFM2 <- funktion_bootstrap(data.selection=WomenRFM2, Age_pred="No", name.data="WomenRFM2",response="Relative.fat.mass.value",setseed=setseed,bootnum=bootnum)
#SMM
#Men Skeletal muscle mass V1
Results_MenSMM1 <- funktion_bootstrap(data.selection=MenSMM1, Age_pred="No", name.data="MenSMM1",response="Skeletal.muscle.mass.value",setseed=setseed,bootnum=bootnum)
#Men Skeletal muscle mass V2
Results_MenSMM2 <- funktion_bootstrap(data.selection=MenSMM2, Age_pred="No", name.data="MenSMM2",response="Skeletal.muscle.mass.value",setseed=setseed,bootnum=bootnum)
#Women Skeletal muscle mass V1
Results_WomenSMM1 <- funktion_bootstrap(data.selection=WomenSMM1, Age_pred="No", name.data="WomenSMM1",response="Skeletal.muscle.mass.value",setseed=setseed,bootnum=bootnum)
#Women Skeletal muscle mass V2
Results_WomenSMM2 <- funktion_bootstrap(data.selection=WomenSMM2, Age_pred="No", name.data="WomenSMM2",response="Skeletal.muscle.mass.value",setseed=setseed,bootnum=bootnum)
#SMI
#Men Skeletal muscle mass index
Results_MenSMI1 <- funktion_bootstrap(data.selection=MenSMI1, Age_pred="No", name.data="MenSMI1",response="SMI",setseed=setseed,bootnum=bootnum)
#SMI
#Women Skeletal muscle mass index
Results_WomenSMI1 <- funktion_bootstrap(data.selection=WomenSMI1, Age_pred="No", name.data="WomenSMI1",response="SMI",setseed=setseed,bootnum=bootnum)


#### lineare Regression with variables of variable selection


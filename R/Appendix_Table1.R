
dataM <- function_data_analysis(Sexv="M") %>%
  select(-ID, -Sex, - PArec, -education, -healthyscore, - Body.heightM, - Age_cat,
         -Education_cat, - Diet_cat, - Physical.Activity_cat)

dataW <- function_data_analysis(Sexv="F") %>%
  select(-ID, -Sex, - PArec, -education, -healthyscore, - Body.heightM, - Age_cat,
         -Education_cat, - Diet_cat, - Physical.Activity_cat)

RhoMen <- cor(dataM,use = "complete.obs", method = "spearman") %>%
  data.frame() %>%
  select(Relative.fat.mass.value, SMI) 

RhoWomen <- cor(dataW,use = "complete.obs", method = "spearman") %>%
  data.frame() %>%
  select(Relative.fat.mass.value, SMI) 

Tab_M <- dataM %>%
  tidyr::pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd)) %>%
  cbind(RhoMen)
 
Tab_W <- dataW %>%
  tidyr::pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd)) %>%
  cbind(RhoWomen)


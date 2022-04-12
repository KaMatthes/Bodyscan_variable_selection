
funktion_bootstrap <- function(data.selection, response, Age_pred,setseed,bootnum,name.data) {
  data.selection <- data.selection %>%
    filter(complete.cases(.))
  pred_var <- names(data.selection)[-1] # define your independent variables
  if(Age_pred=="No"){
  pred_var <-   pred_var[-1]}
  formula <- paste(response,"~", paste(pred_var , collapse = "+")) 
  full_mod <- lm(formula, data = data.selection, x = T, y = T) # fullmodel
  full_est <- coef(full_mod)
  full_se <- coef(summary(full_mod))[, "Std. Error"]
  sum_full_mod <- summary(full_mod)
  
  # Selected model, backward, start with the full model
  
  sel_mod <- step(lm(formula, data = data.selection,  x=T,y=T), 
                direction = "backward",
                trace = 0) 
  sum_sel_mod <- summary(sel_mod)
  
  lm_anova <- anova(sel_mod)
  lm_anova <- lm_anova[-dim(lm_anova)[1],]
  perc.expl <- data.frame(variable=rownames(lm_anova),perc.variation=round(100*lm_anova$"Sum Sq"/sum(lm_anova$"Sum Sq"),1)) %>%
  arrange(desc(perc.variation))
  
  sel_est <- coef(sel_mod)
  sel_se <- coef(summary(sel_mod))[, "Std. Error"]

  # Bootstrap 
  boot_est <-  boot_se <- matrix(0, ncol = length(coef(full_mod)), nrow = bootnum,
                                             dimnames = list(NULL, names(coef(full_mod))))
  terms.boot <-  perc.expl <-  matrix(FALSE, nrow=bootnum, length(attr(full_mod$terms, "term.labels")))
  colnames(terms.boot) <- attr(full_mod$terms, "term.labels")
  colnames(perc.expl) <- attr(full_mod$terms, "term.labels")
  
  set.seed(setseed)
  for (i in 1:bootnum) {
  data_id <- sample(1:dim(data.selection)[1], replace = T)
  if(Age_pred=="No") {
  boot_mod <- step(lm(formula, data = data.selection[data_id, ], 
                      x=T, y=T),
                   direction = "backward", 
                   trace = 0)
  }
  else{
    boot_mod <- step(lm(formula, data = data.selection[data_id, ], 
                        x=T, y=T),
                     direction = "backward", 
                     scope = list(upper = formula, 
                                  lower = formula(response~Age)),
                     trace = 0)
    
  }
  boot_est[i, names(coef(boot_mod))] <- coef(boot_mod)
  boot_se[i, names(coef(boot_mod))] <- coef(summary(boot_mod))[, "Std. Error"]
  terms.boot[i, attr(boot_mod$terms, "term.labels")] <- TRUE
  perc.expl[i, attr(boot_mod$terms, "term.labels")] <- TRUE
  
  lm_anova <- anova(boot_mod)
  lm_anova <- lm_anova[-dim(lm_anova)[1],]
  perc.expl[i, row.names(lm_anova)]  <- c(round(100*lm_anova$"Sum Sq"/sum(lm_anova$"Sum Sq"),1))
  
  }
  
  boot_inclusion <- apply( terms.boot, 2, function(x) sum(x) / length(x) * 100)
  boot_per_var <- apply(perc.expl, 2, median)
  boot_per_var025 <- apply(perc.expl, 2, function(x) quantile(x, 0.025))
  boot_per_var975 <- apply(perc.expl, 2, function(x) quantile(x, 0.975))
  
  Inclusion <- round(cbind(boot_inclusion,boot_per_var,boot_per_var025,boot_per_var975), 4) %>%
    as.data.frame(.) %>%
    arrange(desc(boot_per_var))
  
  
  Inclusion_min70 <- Inclusion %>%
    filter(boot_inclusion >69) %>%
    arrange(desc(boot_per_var))
  
  boot_median <- apply(boot_est, 2, median)
  boot_025per <- apply(boot_est, 2, function(x) quantile(x, 0.025))
  boot_975per <- apply(boot_est, 2, function(x) quantile(x, 0.975))
  boot_bca <- apply(boot_est, 2, function(x) bca(x,conf.level = .95))
  boot_bca_l <- boot_bca[1,]
  boot_bca_u <- boot_bca[2,]
  
  
  overview <- data.frame(terms=row.names(as.data.frame(full_est)),full_est=round(as.data.frame(full_est),2)) %>%
    left_join(data.frame(terms=row.names(as.data.frame(full_se)),full_se=round(as.data.frame(full_se),2))) %>%
    left_join(data.frame(terms=row.names(as.data.frame(sel_est)),sel_est=round(as.data.frame(sel_est),2))) %>%
    left_join(data.frame(terms=row.names(as.data.frame(sel_se)),sel_se=round(as.data.frame(sel_se),2))) %>%
    left_join(data.frame(terms=row.names(as.data.frame(boot_median)),boot_median=round(as.data.frame(boot_median),2))) %>%
    left_join(data.frame(terms=row.names(as.data.frame( boot_025per)), boot_025per=round(as.data.frame( boot_025per),2))) %>%
    left_join(data.frame(terms=row.names(as.data.frame( boot_975per)), boot_025per=round(as.data.frame( boot_975per),2))) %>%
    left_join(data.frame(terms=row.names(as.data.frame(  boot_bca_l)),  boot_bca_l=round(as.data.frame( boot_bca_l),2))) %>%
    left_join(data.frame(terms=row.names(as.data.frame(  boot_bca_u)),  boot_bca_l=round(as.data.frame( boot_bca_u),2))) %>%
    arrange(desc(sel_est)) %>%
    filter(!terms=="(Intercept)")
  
  
  # coefficents 
  boot_01 <- (boot_est != 0) * 1
  boot_inclusion_coef <- as.data.frame(apply(boot_01, 2, function(x) sum(x) / length(x) * 100)) %>%
    filter(.[,1]>69)
  boot_inclusion_coef <- row.names( boot_inclusion_coef)[-1]
  
  boot_est_i <- boot_est[,boot_inclusion_coef]
  pdf(file =paste0("output/",today(),"_",name.data,"_", response,"Age_",Age_pred,"_Coef_plot.pdf"),width = 20, height =15)  
  par(mfrow=c(5,7))
  for(i in 1:ncol(boot_est_i)){
  hist(boot_est_i[,i], breaks=50,
       main=colnames(boot_est_i)[i],
       xlab="")
  abline(v=0,lwd=2, col="red")
  }
  dev.off()
  
  coef_selected <- overview
  row.names(coef_selected) <-  coef_selected$terms
  coef_selected <-   coef_selected[boot_inclusion_coef,] %>%
    select(terms,boot_median,boot_bca_l,boot_bca_u)

  results <- list(Percentage_inclusion=Inclusion,  Inclusion_min70= Inclusion_min70,   overview=overview,coef_selected=coef_selected)
  
  wb <- createWorkbook()
  addWorksheet(wb, "Percentage_inclusion")
  addWorksheet(wb, "Inclusion_min70")
  addWorksheet(wb, "overview")
  addWorksheet(wb, "coef_selected")
  

  writeData(wb, "Percentage_inclusion", Inclusion,rowNames = TRUE)
  writeData(wb, "Inclusion_min70", Inclusion_min70,rowNames = TRUE)
  writeData(wb, "overview", overview)
  writeData(wb, "coef_selected", coef_selected)
  
  saveWorkbook(wb,file =paste0("output/",today(),"_",name.data,"_", response,"Age_",Age_pred,".xlsx"),overwrite = TRUE)
  
  return(results)
  
}


#### Variable selection

Results_MenVFM1 <- funktion_bootstrap(data.selection=MenVFM1,Age_pred="No",name.data="MenVFM1",response="visceral.adipose.tissue.value",setseed=setseed,bootnum=bootnum)
Results_WomenVFM1 <- funktion_bootstrap(data.selection=WomenVFM1, Age_pred="No", name.data="WomenVFM1",response="visceral.adipose.tissue.value",setseed=setseed,bootnum=bootnum)
Results_MenAFM1 <- funktion_bootstrap(data.selection=MenAFM1, Age_pred="No", name.data="MenAFM1",response="Absolute.fat.mass.value",setseed=setseed,bootnum=bootnum)
Results_WomenAFM1 <- funktion_bootstrap(data.selection=WomenAFM1, Age_pred="No", name.data="WomenAFM1",response="Absolute.fat.mass.value",setseed=setseed,bootnum=bootnum)
Results_MenRFM1 <- funktion_bootstrap(data.selection=MenRFM1, Age_pred="No", name.data="MenRFM1",response="Relative.fat.mass.value",setseed=setseed,bootnum=bootnum)
Results_WomenRFM1 <- funktion_bootstrap(data.selection=WomenRFM1, Age_pred="No", name.data="WomenRFM1",response="Relative.fat.mass.value",setseed=setseed,bootnum=bootnum)
Results_MenSMM1 <- funktion_bootstrap(data.selection=MenSMM1, Age_pred="No", name.data="MenSMM1",response="Skeletal.muscle.mass.value",setseed=setseed,bootnum=bootnum)
Results_WomenSMM1 <- funktion_bootstrap(data.selection=WomenSMM1, Age_pred="No", name.data="WomenSMM1",response="Skeletal.muscle.mass.value",setseed=setseed,bootnum=bootnum)
Results_MenSMI1 <- funktion_bootstrap(data.selection=MenSMI1, Age_pred="No", name.data="MenSMI1",response="SMI",setseed=setseed,bootnum=bootnum)
Results_WomenSMI1 <- funktion_bootstrap(data.selection=WomenSMI1, Age_pred="No", name.data="WomenSMI1",response="SMI",setseed=setseed,bootnum=bootnum)






dat_men <-function_data_analysis(Sexv="M")
dat_women <- function_data_analysis(Sexv="F")
lwd_size <- 1
col_plot <- "black"

# VAT men
vat_men <- train(visceral.adipose.tissue.value~ Waist.girth..cm. + Hip.girth..cm. + WHR + Volume.Belly..l. + Volume.Hip..l. + Age,
                 data=dat_men,method = "lm", trControl = boot_val)

pred_vat_men <- predict(vat_men, newdata =dat_men) %>%
  as.data.frame() %>%
  rename(pred = ".") %>%
  cbind(dat_men) %>%
  mutate(Difference = visceral.adipose.tissue.value - pred,
         Mean_var = (visceral.adipose.tissue.value+pred)/2)
mean_diff <- mean(pred_vat_men$Difference)
sd_var <- sd(pred_vat_men$Difference)
CIl <- mean_diff -1.96*sd_var
CIu <- mean_diff +1.96*sd_var

pred_vat_men <- pred_vat_men %>%
  mutate(mean_diff,
         sd_var =sd_var,
         CIl = CIl,
         CIu = CIu)
         
lm(pred_vat_men$pred ~ pred_vat_men$visceral.adipose.tissue.value)

figure_vat_men <- ggplot(pred_vat_men) +
  geom_point(aes(x=visceral.adipose.tissue.value, y=pred)) +
  geom_smooth(aes(x=visceral.adipose.tissue.value, y=pred), method="lm", col="black") +
  annotate("text", x=1, y=6.5, label= "R2 = 0.89", size=5) + 
  annotate("text", x=2, y=6.0, label= "y = 0.23 + 0.91*x", size=5) + 
  ylim(0, 8)+
  xlim(0,8)+
  geom_abline(intercept = 0, slope = 1, col="darkgrey")+
  xlab("VAT") +
  ylab(" VAT predicted") +
  ggtitle("VAT Men") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())
  

bland_vat_men <- ggplot(pred_vat_men) +
  geom_line(aes(x=Mean_var, y=mean_diff), col=col_plot , lwd=lwd_size) +
  geom_line(aes(x=Mean_var, y=CIl), col=col_plot ,linetype = "dashed", lwd=lwd_size)+
  geom_line(aes(x=Mean_var, y=CIu),col=col_plot ,linetype = "dashed", lwd=lwd_size) +
  geom_point(aes(x=Mean_var, y=Difference), col="black", size=2)+
  geom_smooth(aes(x=Mean_var, y=Difference), method="lm", col="black",lwd=1.5) +
  xlab("Mean") +
  ylab("Difference") +
  ggtitle("VAT Men") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())
  
  
  
# VAT women
vat_women <- train(visceral.adipose.tissue.value~ Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + Upper.arm.girth.left..cm. +Thigh.girth.left..horizontal...cm. + WHR + Volume.Hip..l.+Hip.girth..cm. + Age,
                   data=dat_women,method = "lm", trControl =  boot_val)

pred_vat_women <- predict(vat_women, newdata =dat_women) %>%
  as.data.frame() %>%
  rename(pred = ".") %>%
  cbind(dat_women) %>%
  mutate(Difference = visceral.adipose.tissue.value - pred,
         Mean_var = (visceral.adipose.tissue.value+pred)/2)
mean_diff <- mean(pred_vat_women$Difference)
sd_var <- sd(pred_vat_women$Difference)
CIl <- mean_diff -1.96*sd_var
CIu <- mean_diff +1.96*sd_var

pred_vat_women <- pred_vat_women %>%
  mutate(mean_diff,
         sd_var =sd_var,
         CIl = CIl,
         CIu = CIu)

lm(pred_vat_women$pred ~ pred_vat_women$visceral.adipose.tissue.value)


figure_vat_women <- ggplot(pred_vat_women) +
  geom_point(aes(x=visceral.adipose.tissue.value, y=pred)) +
  geom_smooth(aes(x=visceral.adipose.tissue.value, y=pred), method="lm", col="black") +
  annotate("text", x=1, y=6.5, label= "R2 = 0.81", size=5) + 
  annotate("text", x=2, y=6.0, label= "y = 0.11 + 0.87*x", size=5) + 
  ylim(0, 8)+
  xlim(0,8)+
  geom_abline(intercept = 0, slope = 1, col="darkgrey")+
  ggtitle("VAT Women") +
  xlab("VAT") +
  ylab(" VAT predicted") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())



bland_vat_women <- ggplot(pred_vat_women) +
  geom_line(aes(x=Mean_var, y=mean_diff), col=col_plot , lwd=lwd_size) +
  geom_line(aes(x=Mean_var, y=CIl), col=col_plot ,linetype = "dashed", lwd=lwd_size)+
  geom_line(aes(x=Mean_var, y=CIu),col=col_plot ,linetype = "dashed", lwd=lwd_size) +
  geom_point(aes(x=Mean_var, y=Difference), col="black", size=2)+
  geom_smooth(aes(x=Mean_var, y=Difference), method="lm", col="black",lwd=1.5) +
  xlab("Mean") +
  ylab("Difference") +
  ggtitle("VAT Women") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())

# RFM men
rfm_men <- train(Relative.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + High.waist.girth..cm. + Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + WHR + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age,
                 dat_men,method = "lm", trControl = boot_val)

pred_rfm_men <- predict(rfm_men, newdata =dat_men) %>%
  as.data.frame() %>%
  rename(pred = ".") %>%
  cbind(dat_men) %>%
  mutate(Difference =Relative.fat.mass.value - pred,
         Mean_var = (Relative.fat.mass.value+pred)/2)
mean_diff <- mean(pred_rfm_men$Difference)
sd_var <- sd(pred_rfm_men$Difference)
CIl <- mean_diff -1.96*sd_var
CIu <- mean_diff +1.96*sd_var

pred_rfm_men <- pred_rfm_men %>%
  mutate(mean_diff,
         sd_var =sd_var,
         CIl = CIl,
         CIu = CIu)

lm(pred_rfm_men$pred ~ pred_rfm_men$Relative.fat.mass.value)

figure_rfm_men <- ggplot(pred_rfm_men) +
  geom_point(aes(x=Relative.fat.mass.value, y=pred)) +
  geom_smooth(aes(x=Relative.fat.mass.value, y=pred), method="lm", col="black") +
  annotate("text", x=6, y=45, label= "R2 = 0.76", size=5) + 
  annotate("text", x=11.9, y=40, label= "y = 4.06 + 0.82*x", size=5) + 
  ylim(0,50)+
  xlim(0,50)+
  geom_abline(intercept = 0, slope = 1, col="darkgrey")+
  xlab("RFM") +
  ylab(" RFM predicted") +
  ggtitle("RFM Men") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())

bland_rfm_men <- ggplot(pred_rfm_men) +
  geom_line(aes(x=Mean_var, y=mean_diff), col=col_plot , lwd=lwd_size) +
  geom_line(aes(x=Mean_var, y=CIl), col=col_plot ,linetype = "dashed", lwd=lwd_size)+
  geom_line(aes(x=Mean_var, y=CIu),col=col_plot ,linetype = "dashed", lwd=lwd_size) +
  geom_point(aes(x=Mean_var, y=Difference), col="black", size=2)+
  geom_smooth(aes(x=Mean_var, y=Difference), method="lm", col="black",lwd=1.5) +
  xlab("Mean") +
  ylab("Difference") +
  ggtitle("RFM Men") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())



# RFM women
rfm_women <- train(Relative.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + High.waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. + WHtR + WHR  + Volume.Belly..l. + Volume.Hip..l. + BMIscan+ Age,
                   dat_women,method = "lm", trControl = boot_val)

pred_rfm_women <- predict(rfm_women, newdata =dat_women) %>%
  as.data.frame() %>%
  rename(pred = ".") %>%
  cbind(dat_women) %>%
  mutate(Difference =Relative.fat.mass.value - pred,
         Mean_var = (Relative.fat.mass.value+pred)/2)
mean_diff <- mean(pred_rfm_women$Difference)
sd_var <- sd(pred_rfm_women$Difference)
CIl <- mean_diff -1.96*sd_var
CIu <- mean_diff +1.96*sd_var

pred_rfm_women <- pred_rfm_women %>%
  mutate(mean_diff,
         sd_var =sd_var,
         CIl = CIl,
         CIu = CIu)

lm(pred_rfm_women$pred ~ pred_rfm_women$Relative.fat.mass.value)

figure_rfm_women <- ggplot(pred_rfm_women) +
  geom_point(aes(x=Relative.fat.mass.value, y=pred)) +
  geom_smooth(aes(x=Relative.fat.mass.value, y=pred), method="lm", col="black") +
  annotate("text", x=6, y=45, label= "R2 = 0.82", size=5) + 
  annotate("text", x=11.9, y=40, label= "y = 3.61 + 0.88*x", size=5) + 
  ylim(0,50)+
  xlim(0,50)+
  geom_abline(intercept = 0, slope = 1, col="darkgrey")+
  xlab("RFM")+
  ylab("RFM predicted") +
  ggtitle("RFM Women") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())

bland_rfm_women <- ggplot(pred_rfm_women) +
  geom_line(aes(x=Mean_var, y=mean_diff), col=col_plot , lwd=lwd_size) +
  geom_line(aes(x=Mean_var, y=CIl), col=col_plot ,linetype = "dashed", lwd=lwd_size)+
  geom_line(aes(x=Mean_var, y=CIu),col=col_plot ,linetype = "dashed", lwd=lwd_size) +
  geom_point(aes(x=Mean_var, y=Difference), col="black", size=2)+
  geom_smooth(aes(x=Mean_var, y=Difference), method="lm", col="black",lwd=1.5) +
  xlab("Mean") +
  ylab("Difference") +
  ggtitle("RFM Women") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())


# SMI men
smi_men <-train(SMI~Maximum.belly.circumference..cm.+ Forearm.girth.left..cm. + Thigh.girth.left..horizontal...cm. + Volume.Thigh.Left..l.+ Volume.Chest..l.+ Volume.Belly..l.+ Volume.Hip..l.+ BMIscan + Age,
                dat_men,method = "lm", trControl = boot_val)

pred_smi_men <- predict(smi_men, newdata =dat_men) %>%
  as.data.frame() %>%
  rename(pred = ".") %>%
  cbind(dat_men) %>%
  mutate(Difference =SMI - pred,
         Mean_var = (SMI+pred)/2)
mean_diff <- mean(pred_smi_men$Difference)
sd_var <- sd(pred_smi_men$Difference)
CIl <- mean_diff -1.96*sd_var
CIu <- mean_diff +1.96*sd_var

pred_smi_men <- pred_smi_men %>%
  mutate(mean_diff,
         sd_var =sd_var,
         CIl = CIl,
         CIu = CIu)

lm(pred_smi_men$pred ~ pred_smi_men$SMI)

figure_smi_men <- ggplot(pred_smi_men) +
  geom_point(aes(x=SMI, y=pred)) +
  geom_smooth(aes(x=SMI ,y=pred), method="lm", col="black") +
  annotate("text", x=2, y=13, label= "R2 = 0.85", size=5) + 
  annotate("text", x=3.8, y=11, label= "y = 1.06 + 0.89*x", size=5) + 
  ylim(0,15) +
  xlim(0,15) +
  geom_abline(intercept = 0, slope = 1, col="darkgrey")+
  xlab("SMI") +
  ylab(" SMI predicted") +
  ggtitle("SMI Men") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())

bland_smi_men <- ggplot(pred_smi_men) +
  geom_line(aes(x=Mean_var, y=mean_diff), col=col_plot , lwd=lwd_size) +
  geom_line(aes(x=Mean_var, y=CIl), col=col_plot ,linetype = "dashed", lwd=lwd_size)+
  geom_line(aes(x=Mean_var, y=CIu),col=col_plot ,linetype = "dashed", lwd=lwd_size) +
  geom_point(aes(x=Mean_var, y=Difference), col="black", size=2)+
  geom_smooth(aes(x=Mean_var, y=Difference), method="lm", col="black",lwd=1.5) +
  xlab("Mean") +
  ylab("Difference") +
  ggtitle("SMI Men") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())


# SMI women
smi_women <- train(SMI~ Waist.girth..cm. + WHtR + Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm.+ Volume.Hip..l.+BMIscan+ Age,
                   dat_women,method = "lm", trControl = boot_val)

pred_smi_women <- predict(smi_women, newdata =dat_women) %>%
  as.data.frame() %>%
  rename(pred = ".") %>%
  cbind(dat_women) %>%
  mutate(Difference =SMI - pred,
         Mean_var = (SMI+pred)/2)
mean_diff <- mean(pred_smi_women$Difference)
sd_var <- sd(pred_smi_women$Difference)
CIl <- mean_diff -1.96*sd_var
CIu <- mean_diff +1.96*sd_var

pred_smi_women <- pred_smi_women %>%
  mutate(mean_diff,
         sd_var =sd_var,
         CIl = CIl,
         CIu = CIu)

lm(pred_smi_women$pred ~ pred_smi_women$SMI)

figure_smi_women <- ggplot(pred_smi_women) +
  geom_point(aes(x=SMI, y=pred)) +
  geom_smooth(aes(x=SMI, y=pred), method="lm", col="black") +
  annotate("text", x=2, y=13, label= "R2 = 0.64", size=5) + 
  annotate("text", x=3.8, y=11, label= "y = 2.05 + 0.71*x", size=5) +
  ylim(0,15) +
  xlim(0,15) +
  geom_abline(intercept = 0, slope = 1, col="darkgrey")+
  xlab("SMI") +
  ylab(" SMI predicted") +
  ggtitle("SMI Women") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())

bland_smi_women <- ggplot(pred_smi_women) +
  geom_line(aes(x=Mean_var, y=mean_diff), col=col_plot , lwd=lwd_size) +
  geom_line(aes(x=Mean_var, y=CIl), col=col_plot ,linetype = "dashed", lwd=lwd_size)+
  geom_line(aes(x=Mean_var, y=CIu),col=col_plot ,linetype = "dashed", lwd=lwd_size) +
  geom_point(aes(x=Mean_var, y=Difference), col="black", size=2)+
  geom_smooth(aes(x=Mean_var, y=Difference), method="lm", col="black",lwd=1.5) +
  xlab("Mean") +
  ylab("Difference") +
  ggtitle("SMI Women") +
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank())


Figure1<- cowplot::plot_grid(figure_vat_men ,  figure_vat_women ,
                              # bland_vat_men, bland_rfm_men, bland_smi_men,
                             figure_rfm_men, figure_rfm_women , 
                              figure_smi_men , figure_smi_women,
                              # bland_vat_women, bland_rfm_women, bland_smi_women,
                              nrow=3, ncol=2)

cowplot::save_plot("output/Figure1.pdf", Figure1,base_height=15,base_width=10)

Figure2<- cowplot::plot_grid(bland_vat_men,  bland_vat_women,
                             bland_rfm_men, bland_rfm_women, 
                             bland_smi_men,bland_smi_women,
                             nrow=3, ncol=2)

cowplot::save_plot("output/Figure2.pdf", Figure2,base_height=15,base_width=10)




dataM <- function_data_analysis(Sexv="M") %>%
  mutate(Waist_cat = cut(Waist.girth..cm.,c(0,93.99,101.99,+Inf), 
                         label=c("Low RMC","Increased RMC","Substantially increased RMC")),
         WHR_cat = cut(WHR,c(0,0.8999,0.999,+Inf), 
                       label=c("Normal Weight","Over-weight","Obesity")),
         Sex = "Male")


dataW <- function_data_analysis(Sexv="F") %>%
  mutate(Waist_cat = cut(Waist.girth..cm.,c(0,79.99,87.99,+Inf), 
                         label=c("Low RMC","Increased RMC","Substantially increased RMC")),
         WHR_cat = cut(WHR,c(0,0.7999,0.84999,+Inf), 
                       label=c("Normal Weight","Over-weight","Obesity")),
         Sex = "Female")

data_t <- rbind(dataM, dataW) %>%
  mutate( BMI_cat = cut(BMIscan,c(0,18.5,25,30,40), 
                        label=c("Underweight","Normal weight","Overweight","Obesity")),
          WHtR_cat = cut(WHtR,c(0,0.5,0.6,1), 
                         label=c("No increased health risk","Increased Health risk","Greatly increased Health risk")))

#Table for Table 1 in the paper

tab_des <- tableby(Sex~Age_cat+Education_cat+Physical.Activity_cat+Diet_cat+BMI_cat+WHtR_cat+Waist_cat+WHR_cat,
                   data = data_t,digits=1)
tab_des <- summary(tab_des,text=TRUE)
tab_des <- as.data.frame(tab_des)


##################################################################################
#Statistics test

#Age wilcox
wilcox.test(Age~Sex, data=data_t)

#BMI wilcox
wilcox.test(BMIscan~Sex, data=data_t)

#Body height wilcox
wilcox.test(Body.height..cm.~Sex, data=data_t)

#Education cat chi2

E <- matrix(c(37,35,68,61), ncol = 2, dimnames =
               list(c("Men", "Women"), c("Primary/Secondary", "Tertiary")))
chisq.test(E)

#Physical activity cat chi2

PA <- matrix(c(16,12,32,50,57,34), ncol = 3, dimnames =
               list(c("Men", "Women"), c("Light", "Moderate","Heavy")))
chisq.test(PA)

#Diet cat chi2

D <- matrix(c(63,37,36,43,6,16), ncol = 3, dimnames =
               list(c("Men", "Women"), c("Unhealthy", "Medium","Healthy")))

chisq.test(D)

#VAT wilcox
wilcox.test(visceral.adipose.tissue.value ~ Sex,data=data_t)

#AFM wilcox
wilcox.test(Absolute.fat.mass.value ~ Sex, data=data_t)

#RFM wilcox
wilcox.test(Relative.fat.mass.value ~ Sex, data=data_t)

#SMM wilcox

wilcox.test(Skeletal.muscle.mass.value ~ Sex, data=data_t)

#SMI wilcox
wilcox.test(SMI ~ Sex, data=data_t)

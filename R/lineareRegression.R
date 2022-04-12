function_lineare_regression <- function(Sexv) {

  Healthscore <- read.csv("data/healthscore.csv", sep=";") %>% #
    mutate(ID=id)
  
  datal <- read.csv("data/data_study.csv", sep=";") %>%
    rename(ID=ï..ID ) %>%
    full_join(Healthscore) %>%
    mutate(Body.heightM=Body.height..cm./100,
           BMIscan=Weight..kg./(Body.heightM^2),
           Age_cat = cut(Age,c(16,35,65,100), label=c("35&Less","Between36to65","Over65")),
           Education_cat = cut(education,c(0,4,8), label=c("Primary/Secondary", "Tertiary")),
           Diet_cat = cut(healthyscore,c(-1,1,3,5), label=c("unhealthy", "medium","healthy")),
           Physical.Activity_cat=cut(PArec,c(0,2,3,5), label=c("light", "moderate","heavy")),
           SMI=Skeletal.muscle.mass.value/(Body.heightM^2)) %>%
    filter(!(ID == 4 | ID == 18 | ID == 38 | ID == 43 | ID == 44 | ID == 55| ID == 62|
             ID == 71 | ID == 85 | ID == 90 | ID == 117 | ID == 123 | ID == 135 | ID == 204 |  ID == 216)) %>%
    select(-Fat.free.mass.value) %>%
    filter(complete.cases(.))
  

if(Sexv=="M") {
datal <- datal %>%
  filter(Sex=="M")
}
else if (Sexv=="F"){
datal <- datal %>%
  filter(Sex=="F")
}
return(datal)

}


# Visceral adipose tissuen(kg)
summary(lm(visceral.adipose.tissue.value~BMIscan,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~BMIscan + Age,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat, function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm., function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. + Age, function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~WHtR,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~WHtR + Age,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~ Waist.girth..cm. + Hip.girth..cm. + WHR + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~ Waist.girth..cm. + Hip.girth..cm. + WHR + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="M")))
summary(lm(visceral.adipose.tissue.value~ Waist.girth..cm. + Hip.girth..cm. + WHR + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))


summary(lm(visceral.adipose.tissue.value~BMIscan,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~BMIscan + Age,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~BMIscan + Age + Education_cat + Physical.Activity_cat +Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm.,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. +Age,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat +Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~WHtR, function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~WHtR + Age,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~ Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + Body.height..cm.+ High.waist.girth..cm. + Upper.arm.girth.left..cm. +Thigh.girth.left..horizontal...cm. + WHR + Volume.Hip..l.,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~ Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + Body.height..cm.+ High.waist.girth..cm. + Upper.arm.girth.left..cm. +Thigh.girth.left..horizontal...cm. + WHR + Volume.Hip..l.+ Age,function_lineare_regression(Sexv="F")))
summary(lm(visceral.adipose.tissue.value~ Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + Body.height..cm.+ High.waist.girth..cm. + Upper.arm.girth.left..cm. +Thigh.girth.left..horizontal...cm. + WHR + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))



# Relative fat mass (%)
summary(lm(Relative.fat.mass.value~BMIscan,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~BMIscan + Age,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~WHtR,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~WHtR + Age,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + High.waist.girth..cm. + Hip.girth..cm.+ Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + WHR + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + High.waist.girth..cm. + Hip.girth..cm.+ Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + WHR + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + High.waist.girth..cm. + Hip.girth..cm.+ Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + WHR + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))


summary(lm(Relative.fat.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(Relative.fat.mass.value~BMIscan,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~BMIscan + Age,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~WHtR,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~WHtR + Age,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + High.waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. + WHtR + WHR  + Volume.Belly..l. + Volume.Hip..l. + BMIscan,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + High.waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. + WHtR + WHR  + Volume.Belly..l. + Volume.Hip..l. + BMIscan+ Age,function_lineare_regression(Sexv="F")))
summary(lm(Relative.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + High.waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. + WHtR + WHR  + Volume.Belly..l. + Volume.Hip..l. + BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

# Skeletal muscle mass index (kg/m2)
summary(lm(SMI~BMIscan,function_lineare_regression(Sexv="M")))
summary(lm(SMI~BMIscan + Age,function_lineare_regression(Sexv="M")))
summary(lm(SMI~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(SMI~Waist.girth..cm.,function_lineare_regression(Sexv="M")))
summary(lm(SMI~Waist.girth..cm. + Age,function_lineare_regression(Sexv="M")))
summary(lm(SMI~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(SMI~WHtR,function_lineare_regression(Sexv="M")))
summary(lm(SMI~WHtR + Age,function_lineare_regression(Sexv="M")))
summary(lm(SMI~Maximum.belly.circumference..cm.+ Forearm.girth.left..cm. + Thigh.girth.left..horizontal...cm. + WHtR + Waist.girth..cm. + Volume.Thigh.Left..l.+ Volume.Chest..l.+ Volume.Belly..l.+ Volume.Hip..l.+ BMIscan,function_lineare_regression(Sexv="M")))
summary(lm(SMI~Maximum.belly.circumference..cm.+ Forearm.girth.left..cm. + Thigh.girth.left..horizontal...cm. + WHtR + Waist.girth..cm. + Volume.Thigh.Left..l.+ Volume.Chest..l.+ Volume.Belly..l.+ Volume.Hip..l.+ BMIscan + Age,function_lineare_regression(Sexv="M")))
summary(lm(SMI~Maximum.belly.circumference..cm.+ Forearm.girth.left..cm. + Thigh.girth.left..horizontal...cm. + WHtR + Waist.girth..cm. + Volume.Thigh.Left..l.+ Volume.Chest..l.+ Volume.Belly..l.+ Volume.Hip..l.+ BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))


summary(lm(SMI~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))
summary(lm(SMI~BMIscan,function_lineare_regression(Sexv="F")))
summary(lm(SMI~BMIscan + Age,function_lineare_regression(Sexv="F")))
summary(lm(SMI~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(SMI~Waist.girth..cm.,function_lineare_regression(Sexv="F")))
summary(lm(SMI~Waist.girth..cm. + Age,function_lineare_regression(Sexv="F")))
summary(lm(SMI~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(SMI~WHtR,function_lineare_regression(Sexv="F")))
summary(lm(SMI~WHtR + Age,function_lineare_regression(Sexv="F")))
summary(lm(SMI~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
summary(lm(SMI~  Waist.girth..cm. + WHtR +Weight..kg.+ Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm.+  Volume.Thigh.Left..l.+ Volume.Hip..l.+BMIscan ,function_lineare_regression(Sexv="F")))
summary(lm(SMI~ Waist.girth..cm. + WHtR +Weight..kg.+ Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm.+  Volume.Thigh.Left..l.+ Volume.Hip..l.+BMIscan+ Age,function_lineare_regression(Sexv="F")))
summary(lm(SMI~ Waist.girth..cm. + WHtR +Weight..kg.+ Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm.+  Volume.Thigh.Left..l.+ Volume.Hip..l.+BMIscan+ Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

function_lineare_regression <- function(Sexv) {

  Healthscore <- read.csv("data/healthscore.csv", sep=";") %>% #
    mutate(ID=id)
  
  datal <- read.csv("data/datcomb.csv", sep=",") %>%
    full_join(Healthscore) %>%
    mutate(Body.heightM=Body.height..cm./100,
           BMIscan=Weight..kg./(Body.heightM^2),
           Age_cat = cut(Age,c(16,35,65,100), label=c("35&Less","Between36to65","Over65")),
           Education_cat = cut(education,c(0,4,8), label=c("Primary/Secondary", "Tertiary")),
           Diet_cat = cut(healthyscore,c(-1,1,3,5), label=c("unhealthy", "medium","healthy")),
           Physical.Activity_cat=cut(PArec,c(0,2,3,5), label=c("light", "moderate","heavy")),
           SMI=Skeletal.muscle.mass.value/(Body.heightM^2)) %>%
    filter(!(ID == 4 | ID == 18 | ID == 38 | ID == 43 | ID == 44 | ID == 55| ID == 62|
             ID == 71 | ID == 85 | ID == 90 | ID == 117 | ID == 123 | ID == 135 | ID == 204 |  ID == 216))

  
completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }  
  
datal <- completeFun(datal, c(20,21,23,108,5,194,199,205,213,242,249,264,270,286,288,299,302,307,313,315,317,336,338,340,344,346,351,352,368,370,380,382,390,393,395,399))

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



#VFM
#VFM~BMI Men
summary(lm(visceral.adipose.tissue.value~BMIscan,function_lineare_regression(Sexv="M")))

#VFM~BMI+Age Men
summary(lm(visceral.adipose.tissue.value~BMIscan + Age,function_lineare_regression(Sexv="M")))

#VFM~BMI+Age Men + lifestyle
summary(lm(visceral.adipose.tissue.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat, function_lineare_regression(Sexv="M")))

#VFM~WC Men
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm., function_lineare_regression(Sexv="M")))

#VFM~WC+Age Men
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. + Age, function_lineare_regression(Sexv="M")))

#VFM~WC+Age Men + lifestyle
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#VFM~WHtR Men
summary(lm(visceral.adipose.tissue.value~WHtR,function_lineare_regression(Sexv="M")))

#VFM~WHtR+Age Men
summary(lm(visceral.adipose.tissue.value~WHtR + Age,function_lineare_regression(Sexv="M")))

#VFM~WHtR+Age Men + lifestyle
summary(lm(visceral.adipose.tissue.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#VFM~BMI Women
summary(lm(visceral.adipose.tissue.value~BMIscan,function_lineare_regression(Sexv="F")))

#VFM~BMI+Age Women
summary(lm(visceral.adipose.tissue.value~BMIscan + Age,function_lineare_regression(Sexv="F")))

#VFM~BMI+Age Women + lifestyle
summary(lm(visceral.adipose.tissue.value~BMIscan + Age + Education_cat + Physical.Activity_cat +Diet_cat,function_lineare_regression(Sexv="F")))

#VFM~WC Women
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm.,function_lineare_regression(Sexv="F")))

#VFM~WC+Age Women
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. +Age,function_lineare_regression(Sexv="F")))

#VFM~WC+Age Women + lifestyle
summary(lm(visceral.adipose.tissue.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat +Diet_cat,function_lineare_regression(Sexv="F")))

#VFM~WHtR Women
summary(lm(visceral.adipose.tissue.value~WHtR, function_lineare_regression(Sexv="F")))

#VFM~WHtR+Age Women
summary(lm(visceral.adipose.tissue.value~WHtR + Age,function_lineare_regression(Sexv="F")))

#VFM~WHtR+Age Women + lifestyle
summary(lm(visceral.adipose.tissue.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

############################################################
#AFM
#AFM~BMI  Men
summary(lm(Absolute.fat.mass.value~BMIscan,function_lineare_regression(Sexv="M")))

#AFM~BMI+Age Men
summary(lm(Absolute.fat.mass.value~BMIscan + Age,function_lineare_regression(Sexv="M")))

#AFM~BMI+Age Men + lifestyle
summary(lm(Absolute.fat.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#AFM~WC  Men
summary(lm(Absolute.fat.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="M")))

#AFM~WC+Age Men
summary(lm(Absolute.fat.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="M")))

#AFM~WC+Age Men + lifestyle
summary(lm(Absolute.fat.mass.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#AFM~WHtR  Men
summary(lm(Absolute.fat.mass.value~WHtR,function_lineare_regression(Sexv="M")))

#AFM~WHtR+Age Men
summary(lm(Absolute.fat.mass.value~WHtR + Age,function_lineare_regression(Sexv="M")))

#AFM~WHtR+Age Men + lifestyle
summary(lm(Absolute.fat.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#AFM~BMI  Women
summary(lm(Absolute.fat.mass.value~BMIscan,function_lineare_regression(Sexv="F")))

#AFM~BMI+Age Women
summary(lm(Absolute.fat.mass.value~BMIscan + Age,function_lineare_regression(Sexv="F")))

#AFM~BMI+Age Women + lifestyle
summary(lm(Absolute.fat.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#AFM~WC  Women
summary(lm(Absolute.fat.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="F")))

#AFM~WC+Age Women
summary(lm(Absolute.fat.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="F")))

#AFM~WC+Age Women + lifestyle
summary(lm(Absolute.fat.mass.value~Waist.girth..cm. +Age + Education_cat +Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#AFM~WHtR  Women
summary(lm(Absolute.fat.mass.value~WHtR,function_lineare_regression(Sexv="F")))

#AFM~WHtR+Age Women
summary(lm(Absolute.fat.mass.value~WHtR + Age,function_lineare_regression(Sexv="F")))

#AFM~WHtR+Age Women + lifestyle
summary(lm(Absolute.fat.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))
############################################################
#RFM
#RFM~BMI  Men
summary(lm(Relative.fat.mass.value~BMIscan,function_lineare_regression(Sexv="M")))

#RFM~BMI+Age Men
summary(lm(Relative.fat.mass.value~BMIscan + Age,function_lineare_regression(Sexv="M")))

#RFM~BMI+Age Men + lifestyle
summary(lm(Relative.fat.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#RFM~WC  Men
summary(lm(Relative.fat.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="M")))

#RFM~WC+Age Men
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="M")))

#RFM~WC+Age Men + lifestyle
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#RFM~WHtR  Men
summary(lm(Relative.fat.mass.value~WHtR,function_lineare_regression(Sexv="M")))

#RFM~WHtR+Age Men
summary(lm(Relative.fat.mass.value~WHtR + Age,function_lineare_regression(Sexv="M")))

#RFM~WHtR+Age Men + lifestyle
summary(lm(Relative.fat.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#RFM~BMI  Women
summary(lm(Relative.fat.mass.value~BMIscan,function_lineare_regression(Sexv="F")))

#RFM~BMI+Age Women
summary(lm(Relative.fat.mass.value~BMIscan + Age,function_lineare_regression(Sexv="F")))

#RFM~BMI+Age Women + lifestyle
summary(lm(Relative.fat.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#RFM~WC  Women
summary(lm(Relative.fat.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="F")))

#RFM~WC+Age Women
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="F")))

#RFM~WC+Age Women + lifestyle
summary(lm(Relative.fat.mass.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#RFM~WHtR  Women
summary(lm(Relative.fat.mass.value~WHtR,function_lineare_regression(Sexv="F")))

#RFM~WHtR+Age Women
summary(lm(Relative.fat.mass.value~WHtR + Age,function_lineare_regression(Sexv="F")))

#RFM~WHtR+Age Women + lifestyle
summary(lm(Relative.fat.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#############################################################
#SMM
#SMM~BMI  Men
summary(lm(Skeletal.muscle.mass.value~BMIscan,function_lineare_regression(Sexv="M")))

#SMM~BMI+Age Men
summary(lm(Skeletal.muscle.mass.value~BMIscan + Age,function_lineare_regression(Sexv="M")))

#SMM~BMI+Age Men + lifestyle
summary(lm(Skeletal.muscle.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#SMM~WC  Men
summary(lm(Skeletal.muscle.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="M")))

#SMM~WC+Age Men
summary(lm(Skeletal.muscle.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="M")))

#SMM~WC+Age Men + lifestyle
summary(lm(Skeletal.muscle.mass.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#SMM~WHtR  Men
summary(lm(Skeletal.muscle.mass.value~WHtR,function_lineare_regression(Sexv="M")))

#SMM~WHtR+Age Men
summary(lm(Skeletal.muscle.mass.value~WHtR + Age,function_lineare_regression(Sexv="M")))

#SMM~WHtR+Age Men + lifestyle
summary(lm(Skeletal.muscle.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#SMM~BMI  Women
summary(lm(Skeletal.muscle.mass.value~BMIscan,function_lineare_regression(Sexv="F")))

#SMM~BMI+Age Women
summary(lm(Skeletal.muscle.mass.value~BMIscan + Age,function_lineare_regression(Sexv="F")))

#SMM~BMI+Age Women + lifestyle
summary(lm(Skeletal.muscle.mass.value~BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#SMM~WC  Women
summary(lm(Skeletal.muscle.mass.value~Waist.girth..cm.,function_lineare_regression(Sexv="F")))

#SMM~WC+Age Women
summary(lm(Skeletal.muscle.mass.value~Waist.girth..cm. + Age,function_lineare_regression(Sexv="F")))

#SMM~WC+Age Women + lifestyle
summary(lm(Skeletal.muscle.mass.value~Waist.girth..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#SMM~WHtR  Women
summary(lm(Skeletal.muscle.mass.value~WHtR,function_lineare_regression(Sexv="F")))

#SMM~WHtR+Age Women
summary(lm(Skeletal.muscle.mass.value~WHtR + Age,function_lineare_regression(Sexv="F")))

#SMM~WHtR+Age Women + lifestyle
summary(lm(Skeletal.muscle.mass.value~WHtR + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#################################################################
#SMM~Girth forearm L  Men
summary(lm(Skeletal.muscle.mass.value~Forearm.girth.left..cm.,function_lineare_regression(Sexv="M")))

#SMM~Girth forearm L+Age Men
summary(lm(Skeletal.muscle.mass.value~Forearm.girth.left..cm. + Age,function_lineare_regression(Sexv="M")))

#SMM~Girth forearm L+Age Men + lifestyle
summary(lm(Skeletal.muscle.mass.value~Forearm.girth.left..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#SMM~Girth Thigh L  Men
summary(lm(Skeletal.muscle.mass.value~Thigh.girth.left..horizontal...cm.,function_lineare_regression(Sexv="M")))

#SMM~Girth Thigh L+Age Men
summary(lm(Skeletal.muscle.mass.value~Thigh.girth.left..horizontal...cm. + Age,function_lineare_regression(Sexv="M")))

#SMM~Girth Thigh L+Age Men + lifestyle
summary(lm(Skeletal.muscle.mass.value~Thigh.girth.left..horizontal...cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#SMM~Girth forearm L  Women
summary(lm(Skeletal.muscle.mass.value~Forearm.girth.left..cm.,function_lineare_regression(Sexv="F")))

#SMM~Girth forearm L+Age Women
summary(lm(Skeletal.muscle.mass.value~Forearm.girth.left..cm. + Age,function_lineare_regression(Sexv="F")))

#SMM~Girth forearm L+Age Women + lifestyle
summary(lm(Skeletal.muscle.mass.value~Forearm.girth.left..cm. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

#SMM~Girth Thigh L  Women
summary(lm(Skeletal.muscle.mass.value~Thigh.girth.left..horizontal...cm.,function_lineare_regression(Sexv="F")))

#SMM~Girth Thigh L+Age Women
summary(lm(Skeletal.muscle.mass.value~Thigh.girth.left..horizontal...cm. + Age,function_lineare_regression(Sexv="F")))

#SMM~Girth Thigh L+Age Women + lifestyle
summary(lm(Skeletal.muscle.mass.value~Thigh.girth.left..horizontal...cm. + Age + Education_cat + Physical.Activity_cat +Diet_cat,function_lineare_regression(Sexv="F")))


#################################################################################
#multiple linear regression 

#VFM
#Men VFM
#Scan variables
summary(lm(visceral.adipose.tissue.value~ Waist.girth..cm. + Hip.girth..cm. + WHR + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="M")))

#Scan variables + age
summary(lm(visceral.adipose.tissue.value~ Waist.girth..cm. + Hip.girth..cm. + WHR + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="M")))

#Scan variables + age + lifestyle
summary(lm(visceral.adipose.tissue.value~ Waist.girth..cm. + Hip.girth..cm. + WHR + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#Women VFM
#Scan variables
summary(lm(visceral.adipose.tissue.value~ Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. +Thigh.girth.left..horizontal...cm. + WHR + Volume.Hip..l.,function_lineare_regression(Sexv="F")))

#Scan variables + age
summary(lm(visceral.adipose.tissue.value~ Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. +Thigh.girth.left..horizontal...cm. + WHR + Volume.Hip..l. + Age,function_lineare_regression(Sexv="F")))

#Scan variables + age+ lifestyle
summary(lm(visceral.adipose.tissue.value~ Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. +Thigh.girth.left..horizontal...cm. + WHR + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

########################
#AFM

#Men Absolute fat mass
#Scan variables
summary(lm(Absolute.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm.+ High.waist.girth..cm. +  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + Volume.Thigh.Left..l. + Volume.Lower.Leg.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="M")))

#Scan variables + age
summary(lm(Absolute.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm.+ High.waist.girth..cm. +  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + Volume.Thigh.Left..l. + Volume.Lower.Leg.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="M")))

#Scan variables + age + lifestyle
summary(lm(Absolute.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm.+ High.waist.girth..cm. +  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + Volume.Thigh.Left..l. + Volume.Lower.Leg.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#Women Absolute fat mass
#Scan variables
summary(lm(Absolute.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. +  Upper.arm.girth.left..cm. + WHtR + Volume.Thigh.Left..l.  + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="F")))

#Scan variables + age
summary(lm(Absolute.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. +  Upper.arm.girth.left..cm. + WHtR + Volume.Thigh.Left..l.  + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="F")))

#Scan variables + age + lifestyle
summary(lm(Absolute.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. +  Upper.arm.girth.left..cm. + WHtR + Volume.Thigh.Left..l.  + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

################################
#RFM
#Men RFM
#Scan variables
summary(lm(Relative.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + High.waist.girth..cm. +  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + WHR + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="M")))

#Scan variables + age
summary(lm(Relative.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + High.waist.girth..cm. +  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + WHR + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="M")))

#Scan variables + age + lifestyle
summary(lm(Relative.fat.mass.value~ Body.height..cm. + Mid.neck.girth..cm. + High.waist.girth..cm. +  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + WHR + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#Women RFM
#Scan variables
summary(lm(Relative.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + High.waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. + WHtR + WHR  + Volume.Belly..l. + Volume.Hip..l. + BMIscan,function_lineare_regression(Sexv="F")))

#Scan variables + age
summary(lm(Relative.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + High.waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. + WHtR + WHR  + Volume.Belly..l. + Volume.Hip..l. + BMIscan + Age,function_lineare_regression(Sexv="F")))

#Scan variables + age + lifestyle
summary(lm(Relative.fat.mass.value~ Mid.neck.girth..cm. + Bust.chest.girth..horizontal...cm. + Neck.to.waist.center.back..cm. + Waist.girth..cm. + High.waist.girth..cm. + Hip.girth..cm. + Upper.arm.girth.left..cm. + WHtR + WHR  + Volume.Belly..l. + Volume.Hip..l. + BMIscan + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))

##########################################
#SMM
#Men SMM
#Scan variables
summary(lm(Skeletal.muscle.mass.value ~  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + Thigh.girth.left..horizontal...cm. + Weight..kg. + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="M")))

#Scan variables + age
summary(lm(Skeletal.muscle.mass.value ~  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + Thigh.girth.left..horizontal...cm. + Weight..kg. + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="M")))

#Scan variables + age + lifestyle
summary(lm(Skeletal.muscle.mass.value ~  Maximum.belly.circumference..cm. + Forearm.girth.left..cm. + Thigh.girth.left..horizontal...cm. + Weight..kg. + Volume.Thigh.Left..l. + Volume.Chest..l. + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="M")))

#Women SMM
#Scan variables
summary(lm(Skeletal.muscle.mass.value~ Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm. + Waist.girth..cm.  + WHtR + Volume.Forearm.Left..l.  + Volume.Belly..l. + Volume.Hip..l.,function_lineare_regression(Sexv="F")))

#Scan variables + age
summary(lm(Skeletal.muscle.mass.value~ Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm. + Waist.girth..cm.  + WHtR + Volume.Forearm.Left..l.  + Volume.Belly..l. + Volume.Hip..l. + Age,function_lineare_regression(Sexv="F")))

#Scan variables + age + lifestyle
summary(lm(Skeletal.muscle.mass.value~ Mid.neck.girth..cm. + Cross.shoulder.over.neck..cm. + Waist.girth..cm.  + WHtR + Volume.Forearm.Left..l.  + Volume.Belly..l. + Volume.Hip..l. + Age + Education_cat + Physical.Activity_cat + Diet_cat,function_lineare_regression(Sexv="F")))


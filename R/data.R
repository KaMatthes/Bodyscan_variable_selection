#Remove People


Healthscore <- read.csv("data/healthscore.csv", sep=";") %>% #
  mutate(ID=id)

data <- read.csv("data/data_study.csv", sep=";") %>%
  rename(ID=ï..ID ) %>%
  full_join(Healthscore) %>%
  mutate(Body.heightM=Body.height..cm./100,
         BMIscan=Weight..kg./(Body.heightM^2),
         Education_cat = cut(education,c(0,4,8), label=c("Primary/Secondary", "Tertiary")),
         Diet_cat = cut(healthyscore,c(-1,1,3,5), label=c("unhealthy", "medium","healthy")),
         Physical.Activity_cat=cut(PArec,c(0,2,3,5), label=c("light", "moderate","heavy")),
         SMI=Skeletal.muscle.mass.value/(Body.heightM^2)) %>%
  filter(!(ID == 38 | ID == 62| ID == 90 | ID == 216))
  
         
#Sex group
Men <- data %>%
  filter(Sex =="M")

Women <- data %>%
  filter(Sex =="F")

MenV1 <- Men %>%
  select(-Education_cat, -Diet_cat, -Physical.Activity_cat, -Age, -Sex, -ID)


WomenV1 <- Women %>%
  select(-Education_cat, -Diet_cat, -Physical.Activity_cat, -Age, -Sex, -ID)


#SELECT COMPLETE CASES FOR THE MODELS
#First model (V1) (body composition + Age + scan mesures selected)

#Selection MenV1 SMM, remove the 3 other body composition variables

MenSMM1<- MenV1 %>%
  select(-1,-2,-4,-5)

#Selection Men SMM

MenSMM2<- Men %>%
  select(-1,-2,-4,-5)

#Selection WomenV1 SMM

WomenSMM1<-WomenV1 %>%
  select(-1,-2,-4,-5)


#Selection Women SMM

WomenSMM2<- Women %>%
  select(-1,-2,-4,-5)

#Selection MenV1 SMI
MenSMI1<- MenV1 %>%
  select(-1,-2,-3,-4)
#Selection WomenV1 SMI

WomenSMI1 <- WomenV1%>%
  select(-1,-2,-3,-4)

#Selection MenV1 RFM

MenRFM1<- MenV1 %>%
  select(-2,-3,-4,-5)


#Selection Men RFM

MenRFM2<- Men %>%
  select(-2,-3,-4,-5)

#Selection WomenV1 RFM

WomenRFM1<- WomenV1 %>%
  select(-2,-3,-4,-5)

#Selection Women RFM

WomenRFM2<-Women %>%
  select(-2,-3,-4,-5)

#Selection MenV1 AFM

MenAFM1<- MenV1 %>%
  select(-1,-3,-4,-5)

#Selection Men AFM

MenAFM2 <- Men %>%
  select(-1,-3,-4,-5)

#Selection WomenV1 AFM

WomenAFM1<- WomenV1 %>%
  select(-1,-3,-4,-5)

#Selection Women AFM

WomenAFM2<- Women %>%
  select(-1,-3,-4,-5)

#Selection MenV1 VFM

MenVFM1<- MenV1 %>%
  select(-1,-2,-3,-5)


#Selection Men VFM

MenVFM2<- Men %>%
  select(-1,-2,-3,-5)

#Selection WomenV1 VFM
WomenVFM1<- WomenV1 %>%
  select(-1,-2,-3,-5)

#Selection Women VFM
WomenVFM2<- Women %>%
  select(-1,-2,-3,-5)
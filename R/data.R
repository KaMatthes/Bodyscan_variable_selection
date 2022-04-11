#Remove People


Healthscore <- read.csv("data/healthscore.csv", sep=";") %>% #
  mutate(ID=id)

data <- read.csv("data/datcomb.csv", sep=",") %>%
  full_join(Healthscore) %>%
  mutate(Body.heightM=Body.height..cm./100,
         BMIscan=Weight..kg./(Body.heightM^2),
         Education_cat = cut(education,c(0,4,8), label=c("Primary/Secondary", "Tertiary")),
         Diet_cat = cut(healthyscore,c(-1,1,3,5), label=c("unhealthy", "medium","healthy")),
         Physical.Activity_cat=cut(PArec,c(0,2,3,5), label=c("light", "moderate","heavy")),
         SMI=Skeletal.muscle.mass.value/(Body.heightM^2)) %>%
  filter(!(ID == 38 | ID == 62| ID == 90 | ID == 216))
  
         
#Gender group
Men <- data %>%
  filter(Sex =="M")

Women <- data %>%
  filter(Sex =="F")


#SELECT COMPLETE CASES FOR THE MODELS
#First model (V1) (body composition + Age + scan mesures selected)
MenV1 <- Men %>%
  select(20,21,23,108,403,5,194,199,205,213,242,249,264,270,286,288,299,302,307,313,315,317,336,338,340,344,346,351,352,368,370,380,382,390,393,395,399) %>%
  na.omit(.)
  

WomenV1<- Women %>%
  select(20,21,23,108,403,5,194,199,205,213,242,249,264,270,286,288,299,302,307,313,315,317,336,338,340,344,346,351,352,368,370,380,382,390,393,395,399) %>%
  na.omit(.)

#Second model (V2) (body composition + Age + scan mesures selected + lifestyle)
MenV2<- Men %>%
  select(20,21,23,108,403,5,194,199,205,213,242,249,264,270,286,288,299,302,307,313,315,317,336,338,340,344,346,351,352,368,370,380,382,390,393,395,399:402)%>%
  na.omit(.)

WomenV2 <- Women %>%
  select(20,21,23,108,403,5,194,199,205,213,242,249,264,270,286,288,299,302,307,313,315,317,336,338,340,344,346,351,352,368,370,380,382,390,393,395,399:402) %>%
  na.omit(.)

#Selection MenV1 SMM, remove the 3 other body composition variables

MenSMM1<- MenV1 %>%
  select(-1,-2,-4,-5)

#Selection MenV2 SMM

MenSMM2<- MenV2 %>%
  select(-1,-2,-4,-5)

#Selection WomenV1 SMM

WomenSMM1<-WomenV1 %>%
  select(-1,-2,-4,-5)


#Selection WomenV2 SMM

WomenSMM2<- WomenV2 %>%
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


#Selection MenV2 RFM

MenRFM2<- MenV2 %>%
  select(-2,-3,-4,-5)

#Selection WomenV1 RFM

WomenRFM1<- WomenV1 %>%
  select(-2,-3,-4,-5)

#Selection WomenV2 RFM

WomenRFM2<-WomenV2 %>%
  select(-2,-3,-4,-5)

#Selection MenV1 AFM

MenAFM1<- MenV1 %>%
  select(-1,-3,-4,-5)

#Selection MenV2 AFM

MenAFM2 <- MenV2 %>%
  select(-1,-3,-4,-5)

#Selection WomenV1 AFM

WomenAFM1<- WomenV1 %>%
  select(-1,-3,-4,-5)
#Selection WomenV2 AFM

WomenAFM2<- WomenV2 %>%
  select(-1,-3,-4,-5)

#Selection MenV1 VFM

MenVFM1<- MenV1 %>%
  select(-1,-2,-3,-5)


#Selection MenV2 VFM

MenVFM2<- MenV2 %>%
  select(-1,-2,-3,-5)

#Selection WomenV1 VFM
WomenVFM1<- WomenV1 %>%
  select(-1,-2,-3,-5)

#Selection WomenV2 VFM
WomenVFM2<- WomenV2 %>%
  select(-1,-2,-3,-5)
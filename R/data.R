#Remove People


Healthscore <- read.csv("data/healthscore.csv", sep=";") %>% #
  rename(ID=id)

data <- read.csv("data/data_study.csv", sep=";",fileEncoding="UTF-8-BOM") %>%
  # rename(I=`ï..ID` ) %>%
  full_join(Healthscore) %>%
  mutate(Body.heightM=Body.height..cm./100,
         BMIscan=Weight..kg./(Body.heightM^2),
         Education_cat = cut(education,c(0,4,8), label=c("Primary/Secondary", "Tertiary")),
         Diet_cat = cut(healthyscore,c(-1,1,3,5), label=c("unhealthy", "medium","healthy")),
         Physical.Activity_cat=cut(PArec,c(0,2,3,5), label=c("light", "moderate","heavy")),
         SMI=Skeletal.muscle.mass.value/(Body.heightM^2)) %>%
  filter(!(ID == 38 | ID == 62| ID == 90 | ID == 216)) %>%
  select(-Body.heightM, -PArec, -education,-healthyscore)
  
         
#Sex group
Men <- data %>%
  filter(Sex =="M") %>%
  select(-Sex, -ID)

Women <- data %>%
  filter(Sex =="F") %>%
  select(-Sex, -ID)


MenV1 <- Men %>%
  select(-Education_cat, -Diet_cat, -Physical.Activity_cat)

WomenV1 <- Women %>%
  select(-Education_cat, -Diet_cat, -Physical.Activity_cat)


#Selection SNMM

MenSMM1<- MenV1 %>%
  select(-2,-3,-4,-6, -38)
# 
MenSMM2<- Men %>%
  select(-2,-3,-4,-6, -41)

WomenSMM1<-WomenV1 %>%
  select(-2,-3,-4,-6, -38)

WomenSMM2<- Women %>% 
  select(-2,-3,-4,-6, -41)



#Selection SMI
MenSMI1<- MenV1 %>%
  select(-2,-3,-4,-5,-6)

WomenSMI1 <- WomenV1%>%
  select(-2,-3,-4,-5,-6)

#Selection RFM

MenRFM1<- MenV1 %>%
  select(-3,-4,-5,-6,-38)

MenRFM2<- Men %>%
  select(-3,-4,-5,-6,-41)

WomenRFM1<- WomenV1 %>%
  select(-3,-4,-5,-6,-38)

WomenRFM2<-Women %>%
  select(-3,-4,-5,-6,-41)

#Selection AFM

MenAFM1<- MenV1 %>%
  select(-2,-4,-5,-6,-38)

MenAFM2 <- Men %>%
  select(-2,-4,-5,-6,-41)

WomenAFM1<- WomenV1 %>%
  select(-2,-4,-5,-6,-38)
WomenAFM2<- Women %>%
  select(-2,-4,-5,-6,-41)


# VFM
MenVFM1<- MenV1 %>%
  select(-2,-3,-4,-5,-38)

MenVFM2<- Men %>%
  select(-2,-3,-4,-5,-41)

WomenVFM1<- WomenV1 %>%
  select(-2,-3,-4,-5,-38)

WomenVFM2<- Women %>%
  select(-2,-3,-4,-5,-41)

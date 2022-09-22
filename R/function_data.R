function_data_analysis <- function(Sexv) {
  
  Healthscore <- read.csv("data/healthscore.csv", sep=";") %>% #
    rename(ID=id)
  
  data <- read.csv("data/data_study.csv", sep=";",fileEncoding="UTF-8-BOM") %>%
    full_join(Healthscore) %>%
    mutate(Body.heightM=Body.height..cm./100,
           BMIscan=Weight..kg./(Body.heightM^2),
           Age_cat = cut(Age,c(16,35,65,100), label=c("<36","36 - 65",">66")),
           Education_cat = cut(education,c(0,4,8), label=c("Primary/Secondary", "Tertiary")),
           Diet_cat = cut(healthyscore,c(-1,1,3,5), label=c("unhealthy", "medium","healthy")),
           Physical.Activity_cat=cut(PArec,c(0,2,3,5), label=c("light", "moderate","heavy")),
           SMI=Skeletal.muscle.mass.value/(Body.heightM^2)) %>%
    filter(!(ID == 4 | ID == 18 | ID == 38 | ID == 43 | ID == 44 | ID == 55| ID == 62|
               ID == 71 | ID == 85 | ID == 90 | ID == 117 | ID == 123 | ID == 135 | ID == 204 |  ID == 216)) %>%
    select(-Fat.free.mass.value) %>%
    filter(complete.cases(.))
  
  
  if(Sexv=="M") {
    data <- data %>%
      filter(Sex=="M")
  }
  else if (Sexv=="F"){
    data <- data %>%
      filter(Sex=="F")
  }
  return(data)
  
}
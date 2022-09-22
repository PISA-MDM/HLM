############# PISA 2018 ######################
### Analysis with merged SPSS Dataset ########
###############################################

library(haven)
library(tidyverse)
library(srvyr)
library(survey)
library(sjPlot)
library(WeMix)
library(lme4)
library(flexplot)


# setting wd to data folder
setwd("C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Frühjahr 2022/Project Consulting Course/PISA/Data/SPSS")

# getting time needed to read data in
start_time <- Sys.time()
stu <- read_spss('CY07_MSU_STU_QQQ.sav')
Sys.time() - start_time # runtime for reading in data

stuDE <- stu %>% subset(CNT == "DEU")

###############################
## School Data   ###
################################

start_time <- Sys.time()
sch <- read_spss('CY07_MSU_SCH_QQQ.sav')
Sys.time() - start_time # runtime for reading in data
# Time difference of 1.624626 secs

schDE <- sch %>% subset(CNT == "DEU")


#########################################################
# Merging Data Sets  #########################################################
#########################################################


# the principals' and students' datasets can be linked to each other 
#using the identification combination of CNTRYID or CNT, CNTSCHID, 
# and CNTSTUID

pisa.merged <- left_join(x = stuDE,y = schDE, by = c('CNTSCHID'), suffix = c("",""))

# Writing German subset
write_sav(data = pisa.merged, path = 'pisa.merged.sav')


###################################################
# Code to read merged dataset
pisa.merged <- read_spss('pisa.merged.sav')


global.scales <- c("GCSELFEFF",#Self-efficacy regarding global issues (WLE)
                                                              "GCAWARE",#Student's awareness of global issues (WLE)
                                                              "PERSPECT",#Perspective-taking (WLE)
                                                              "COGFLEX",#Cognitive flexibility/adaptability (WLE)
                                                              "AWACOM",#Awareness of intercultural communication (WLE)
                                                              "INTCULT",#Student's interest in learning about other cultures (WLE)
                                                              "RESPECT",#Respect for people from other cultures (WLE)
                                                              "GLOBMIND",#Global-mindedness (WLE)
                                                              "ATTIMM")
pv <- c("PV1READ" , "PV2READ", "PV3READ", "PV4READ", "PV5READ" , "PV6READ", "PV7READ", "PV8READ", "PV9READ" , "PV10READ")


id.vars <- c("CNTSCHID","CNTSTUID")


wt.vars <- c("W_FSTUWT", #FINAL TRIMMED NONRESPONSE ADJUSTED STUDENT WEIGHT
             "W_SCHGRNRABWT", #  GRADE NONRESPONSE ADJUSTED SCHOOL BASE WEIGHT
             "W_FSTUWT_SCH_SUM") # Sum of W_FSTUW

control.vars <- c("ST001D01T",#Grade
                  "ST004D01T",#Student (Standardized) Gender
                  "HISCED",#Highest Education of parents (ISCED)
                  "HISEI",#Highest International Socio-Economic Index of Occupational Status
                  "PARED",#Index highest parental education in years of schooling
                  "IMMIG",#Index Immigration status
                  "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                  "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
                  "REPEAT", # GRADE REPETITION
                  "PROGN",  # School classification
                  "SC048Q01NA") # Percentage <national modal grade for 15-year-olds>: Students whose <heritage language> is different from <test language



pisa.subset <- pisa.merged %>% select(global.scales,control.vars,id.vars,pv,wt.vars, W_FSTURWT1:W_FSTURWT80)
  

pisa.subset$DUMMYWT <- 1

pisa.subset <- pisa.subset %>% mutate(
                          cluster = as.factor(CNTSCHID))

#########################################################
#### Converting to survey design object #################
#########################################################

# #Tidyverse
# d2018_survey <- pisa.merged %>%
#   as_survey(type = "Fay",
#             repweights = starts_with("W_FSTURWT"),
#             weights = W_FSTUWT,   # student weight
#             rho = .5)


#Standard R
#test.survey<-svrepdesign(weights = ~W_FSTUWT, repweights = "W_FSTURWT[0-9]+",
#                 type = "Fay", rho = .5, data = pisa.merged)






# Using WeMix for weighted Null-model
# School weight provided
baseline.wemix <- mix(PV1READ ~ 1 |CNTSCHID,  data = pisa.subset, 
                      weights = c("DUMMYWT","W_SCHGRNRABWT"))
summary(baseline.wemix)


# Null model
baseline.w <- lmer(PV1READ ~ 1 |CNTSCHID,  data = pisa.subset,
                   REML = F,weights = W_SCHGRNRABWT)
summary(baseline.w) 
estimates(baseline.w) # from flexplot
tab_model(baseline.w, show.r2 = F) # from sjPlot

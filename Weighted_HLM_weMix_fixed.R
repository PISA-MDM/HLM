## Ed Survey / WeMix weighted models

library(EdSurvey)
library(WeMix)
library(lme4)
library(tidyverse)


#####################################
# Read data 
###################################

# Path needs to be adjusted by user
sdf <- readPISA(path = "C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Frühjahr 2022/Project Consulting Course/Data/PISA/2018",countries="DEU")
#sdf <- readPISA(path = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",countries="DEU")


global.scales <- c("GCSELFEFF")#Self-efficacy regarding global issues (WLE)
#  "GCAWARE",#Student's awareness of global issues (WLE)
# "PERSPECT",#Perspective-taking (WLE)
#  "COGFLEX",#Cognitive flexibility/adaptability (WLE)
#  "AWACOM",#Awareness of intercultural communication (WLE)
#  "INTCULT",#Student's interest in learning about other cultures (WLE)
#  "RESPECT",#Respect for people from other cultures (WLE)
#  "GLOBMIND",#Global-mindedness (WLE)
#  "ATTIMM")
global.scales <- str_to_lower(global.scales)

pv <- c("PV1READ" , "PV2READ", "PV3READ", "PV4READ", "PV5READ" , "PV6READ", "PV7READ", "PV8READ", "PV9READ" , "PV10READ")
pv <- str_to_lower(pv)


id.vars <- c("cntryid","cnt","cntschid","cntstuid")


wt.vars <- c("w_fstuwt", #FINAL TRIMMED NONRESPONSE ADJUSTED STUDENT WEIGHT
             "w_schgrnrabwt", #  GRADE NONRESPONSE ADJUSTED SCHOOL BASE WEIGHT
             "w_fstuwt_sch_sum") # Sum of W_FSTUW

control.vars <- c("ST001D01T",#Grade
                  "ST004D01T",#Student (Standardized) Gender
                  # "HISCED",#Highest Education of parents (ISCED)
                  "HISEI",#Highest International Socio-Economic Index of Occupational Status
                  #  "PARED",#Index highest parental education in years of schooling
                  "IMMIG",#Index Immigration status
                  #  "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                  #  "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
                  "repeatgrade",
                  "progn",  # School classification
                  "SC048Q01NA") # Percentage <national modal grade for 15-year-olds>: Students whose <heritage language> is different from <test language

control.vars <- str_to_lower(control.vars)

### Get Data

pisa.sel <- EdSurvey::getData(data = sdf,
                              varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F, # Do not drop omitted levels
                              returnJKreplicates = T) # Return replicate weights




# Object can be used in EdSurvey functions, if addAttributes = True
# pisa.sel2 <- EdSurvey::getData(data = sdf,
#                                varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
#                               omittedLevels = F,
#                               returnJKreplicates = TRUE, # Necessary to make functions work
#                               addAttributes = T) # dataframe can be used for EdSurvey functions



############################################
# Demonstrating difference between pisa.sel & pisa.sel2

# lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel) # Does not work without rebinding attribures first
# m.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel2) # EdSurvey functions can be used, because of  returnJKreplicates = TRUE and  addAttributes = TRUE
###########################################


############################################
#### Start of data preparation ##############
###########################################



########### mutate progn -Tatjana ##############
attributes(pisa.sel$progn)$levels
class(pisa.sel$cntschid)
is.numeric(pisa.sel$progn)


pisa.sel<- pisa.sel%>%
  mutate(progn_ad = factor(case_when(progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: VOCATIONAL SCHOOL" ~ "7", #Berufsschule
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY (SPECIAL EDUCATION)" ~ "1", #Förderschule
                                     progn == "GERMANY: UPPER SECONDARY (VOCATIONAL), QUALIFYING FOR SUBJECT-SPECIFIC TERTIARY EDUCATIO" ~ "4", #Gymnasium
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY; BASIC GENERAL EDUCATION" ~ "2", #Hauptschule
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY NO ACCESS TO UPPER; BASIC GENERAL EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; EXTENSIVE GENERAL EDUCATION" ~ "3", #Realschule
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION (EXCLUSIVELY STU" ~ "4", #Gymnasium
                                     progn == "GERMANY: LOWER SECONDARY, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (STUDENTS OF DIFFE" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: UPPER SECONDARY (EXCLUSIVELY STUDENTS OF THE SAME TRACK [CF. KEY 4])" ~ "4", #Gymnasium
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION (STUDENTS OF DIFF" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACHIEVEMENT-BASED ACCESS TO UPPER SECONDARY (WIT" ~ "5", #Integrierte Gesamtschule
                                     progn == "GERMANY: LOWER SECONDARY WITH ACCESS TO UPPER (WALDORF SCHOOL)" ~ "5", #Integrierte Gesamtschule
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (DIF" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: PRE-VOCATIONAL TRAINING YEAR UPPER SECONDARY LEVEL" ~ "7", #Berufsschule
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: VOCATIONAL SCHOOL UPPER SECONDARY LEVEL" ~ "7"))) #Berufsschule


# Relevel to baseline 2
pisa.sel$progn_ad <- relevel(pisa.sel$progn_ad, ref="2")

#is progn a factor?
class(pisa.sel$progn_ad)
#yes!

# 1. Förderschule (1), 
# 2. Hauptschule(2), 
# 3. Realschule(3), 
# 4. Gymnasium(4,5,21), 
# 5. Integrierte Gesamtschule(6-7,16-17), 
# 6. Schule mit mehreren Bildungsgängen(8-15) and 
# 7. Berufsschule (18-20)



### PROGN with German school names


pisa.sel<- pisa.sel%>%
  mutate(progn_de = factor(case_when(progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: VOCATIONAL SCHOOL" ~ "Berufsschule", #7
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY (SPECIAL EDUCATION)" ~ "Förderschule", #1
                                     progn == "GERMANY: UPPER SECONDARY (VOCATIONAL), QUALIFYING FOR SUBJECT-SPECIFIC TERTIARY EDUCATIO" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY; BASIC GENERAL EDUCATION" ~ "Hauptschule", # 2
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY NO ACCESS TO UPPER; BASIC GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; EXTENSIVE GENERAL EDUCATION" ~ "Realschule", # 3
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION (EXCLUSIVELY STU" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (STUDENTS OF DIFFE" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: UPPER SECONDARY (EXCLUSIVELY STUDENTS OF THE SAME TRACK [CF. KEY 4])" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION (STUDENTS OF DIFF" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACHIEVEMENT-BASED ACCESS TO UPPER SECONDARY (WIT" ~ "Integrierte Gesamtschule", # 5
                                     progn == "GERMANY: LOWER SECONDARY WITH ACCESS TO UPPER (WALDORF SCHOOL)" ~ "Integrierte Gesamtschule", #5
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (DIF" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: PRE-VOCATIONAL TRAINING YEAR UPPER SECONDARY LEVEL" ~ "Berufsschule", # 7
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: VOCATIONAL SCHOOL UPPER SECONDARY LEVEL" ~ "Berufsschule"))) # 7


# Relevel to baseline Hauptschule
pisa.sel$progn_de <- relevel(pisa.sel$progn_de, ref="Hauptschule")



####### mutate ST001D01T",#Grade  -  Tatjana #########
pisa.sel<- pisa.sel%>%
  mutate(#st001d01t = as.numeric(st001d01t),
    st001d01t_ad = factor(case_when(st001d01t <= 9 ~ "Grade 7-9",
                                    st001d01t >= 10 ~ "Grade 10-12")))

# Relevel to baseline Hauptschule
pisa.sel$st001d01t_ad <- relevel(pisa.sel$st001d01t_ad, ref="Grade 7-9")


# calculate school hisei
# hisei_gc = group-mean centered
pisa.sel <- pisa.sel %>% group_by(cntschid) %>% mutate(avg_hisei = mean(hisei, na.rm = TRUE),
                                                       hisei_gc = hisei - avg_hisei) %>% ungroup()


# Check group mean centering
pisa.hisei <- pisa.sel %>% select(hisei, avg_hisei, hisei_gc)

# Show school average hisei
pisa.sel %>% 
  group_by(cntschid) %>% 
  summarise(avg_hisei = mean(hisei, na.rm = TRUE)) %>% ungroup

##########################################################
######### Rebinding attributes to use EdSurvey functions
##########################################################
pisa.sel <- rebindAttributes(pisa.sel, sdf)


#############################################
# Test - get summary statistics for new variables

summary2(data = pisa.sel, variable = "progn_de")
summary2(data = pisa.sel, variable = "st001d01t_ad")
summary2(data = pisa.sel, variable = "avg_hisei")
# EdSurvey functions work fine


##################################
## Prepare complete case analyis
#################################

#delete cases with missing values
omitted2018 <- getAttributes(sdf,'omittedLevels')

# save full dataset separately
pisa.full <- pisa.sel

# Create copy for regressions
pisa.sel2 <- pisa.sel


for (i in 1:ncol(pisa.sel2)) {
  pisa.sel2 <- pisa.sel2[!pisa.sel2[,i] %in% omitted2018,]
}


full.cases <- pisa.sel2
length(full.cases$cntstuid) # 2034 obs

# Number of schools
length(unique(pisa.sel2$cntschid))

# Descriptive statistics of full cases
t <- pisa.sel2 %>% group_by(cntschid) %>% summarize(number_stu = n()) %>% ungroup
summary(t$number_stu)
sd(t$number_stu)


# Create dummywt for HLM
pisa.sel2$dummywt <- 1




############################################
#### End of data preparation ##############
###########################################


###########################################
###### Analysis with unscaled weights
###########################################



##################################
##### Null model ################
#############################


# Alternative use WeMix directly
# Unscaled weights provided
baseline.unscaled <- mix(pv1read ~ 1 + (1|cntschid), data = pisa.sel2, 
                      weights = c("w_fstuwt","w_schgrnrabwt"), cWeights = FALSE)
summary(baseline.unscaled)

WeMix::waldTest(baseline.unscaled, type = "beta")


##################################
##### Simple model ###############
##################################


# Model with Global competence as predictor
gcselfeff.unscaled <- mix(pv1read ~ gcselfeff + (1|cntschid), data = pisa.sel2, 
                       weights = c("w_fstuwt","w_schgrnrabwt"))
summary(gcselfeff.unscaled)

# help("waldTest")
WeMix::waldTest(gcselfeff.unscaled, type="beta", coefs= "gcselfeff")


##################################
##### Control model ##############
##################################

######################
# Raw scores hisei ###
control.unscaled <-  mix(pv1read ~ st001d01t_ad + st004d01t + hisei  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                      data = pisa.sel2,
                      weights = c("w_fstuwt","w_schgrnrabwt"))


summary(control.unscaled)

WeMix::waldTest(control.unscaled, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "hisei")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(control.unscaled, type = "beta", coefs = "sc048q01na")


######################
# Group centered hisei ###
control.unscaled.centered <-  mix(pv1read ~ st001d01t_ad + st004d01t + hisei_gc  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                         data = pisa.sel2,
                         weights = c("w_fstuwt","w_schgrnrabwt"))


summary(control.unscaled.centered)

WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "hisei_gc")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(control.unscaled.centered, type = "beta", coefs = "sc048q01na")



##################################
##### Full model ################
#############################


######################
# Raw scores hisei ###
full.unscalsed <-  mix(pv1read ~ gcselfeff + st001d01t_ad + st004d01t + hisei  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                       data = pisa.sel2,
                       weights = c("w_fstuwt","w_schgrnrabwt"))


summary(full.unscalsed)

WeMix::waldTest(full.unscalsed, type = "beta", coefs = "gcselfeff")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "hisei")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(full.unscalsed, type = "beta", coefs = "sc048q01na")


######################
# Group centered hisei ###
full.unscalsed.centered <-  mix(pv1read ~ gcselfeff + st001d01t_ad + st004d01t + hisei_gc  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                       data = pisa.sel2,
                       weights = c("w_fstuwt","w_schgrnrabwt"))


summary(full.unscalsed.centered)

WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "gcselfeff")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "hisei_gc")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(full.unscalsed.centered, type = "beta", coefs = "sc048q01na")


#########################################
###### Analysis with schoolweight only #
######################################


##################################
##### Null model ################
#############################


# Alternative use WeMix directly
# School weight provided
baseline.wemix <- mix(pv1read ~ 1 + (1|cntschid), data = pisa.sel2, 
            weights = c("dummywt","w_schgrnrabwt"), cWeights = T)
summary(baseline.wemix)

WeMix::waldTest(baseline.wemix, type = "beta")


##################################
##### Simple model ###############
##################################


# Model with Global competence as predictor
gcselfeff.wemix <- mix(pv1read ~ gcselfeff + (1|cntschid), data = pisa.sel2, 
                 weights = c("dummywt","w_schgrnrabwt"), cWeights = T)
summary(gcselfeff.wemix)

#help("waldTest")
WeMix::waldTest(gcselfeff.wemix, type="beta", coefs= "gcselfeff")


##################################
##### Control model ##############
##################################

######################
# Raw scores hisei ###
control.wemix <-  mix(pv1read ~ st001d01t_ad + st004d01t + hisei  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                     data = pisa.sel2,
                     weights = c("dummywt","w_schgrnrabwt"), cWeights = T)


summary(control.wemix)

WeMix::waldTest(control.wemix, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(control.wemix, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(control.wemix, type = "beta", coefs = "hisei")
WeMix::waldTest(control.wemix, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(control.wemix, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(control.wemix, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(control.wemix, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(control.wemix, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(control.wemix, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(control.wemix, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(control.wemix, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(control.wemix, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(control.wemix, type = "beta", coefs = "sc048q01na")



######################
# Group centered hisei ###
control.wemix.centered <-  mix(pv1read ~ st001d01t_ad + st004d01t + hisei_gc  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                      data = pisa.sel2,
                      weights = c("dummywt","w_schgrnrabwt"), cWeights = T)


summary(control.wemix.centered)

WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "hisei_gc")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(control.wemix.centered, type = "beta", coefs = "sc048q01na")





##################################
##### Full model ################
#############################

######################
# Raw scores hisei ###
full.wemix <-  mix(pv1read ~ gcselfeff + st001d01t_ad + st004d01t + hisei  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                      data = pisa.sel2,
                      weights = c("dummywt","w_schgrnrabwt"), cWeights = T)


summary(full.wemix)

WeMix::waldTest(full.wemix, type = "beta", coefs = "gcselfeff")
WeMix::waldTest(full.wemix, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(full.wemix, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(full.wemix, type = "beta", coefs = "hisei")
WeMix::waldTest(full.wemix, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(full.wemix, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(full.wemix, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(full.wemix, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(full.wemix, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(full.wemix, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(full.wemix, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(full.wemix, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(full.wemix, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(full.wemix, type = "beta", coefs = "sc048q01na")





######################
# Group centered hisei ###

full.wemix.centered <-  mix(pv1read ~ gcselfeff + st001d01t_ad + st004d01t + hisei_gc  + immig + repeatgrade + progn_de +  avg_hisei + sc048q01na + (1|cntschid), 
                   data = pisa.sel2,
                   weights = c("dummywt","w_schgrnrabwt"), cWeights = T)


summary(full.wemix.centered)

WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "gcselfeff")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "st001d01t_adGrade 10-12")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "st004d01tMALE")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "hisei_gc")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "immigSECOND-GENERATION")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "immigFIRST-GENERATION")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "repeatgradeREPEATED A  GRADE")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "progn_deBerufsschule")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "progn_deGymnasium")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "progn_deIntegrierte Gesamtschule")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "progn_deRealschule")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "progn_deSchule mit mehreren Bildungsgängen")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "avg_hisei")
WeMix::waldTest(full.wemix.centered, type = "beta", coefs = "sc048q01na")



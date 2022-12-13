# Unweighted Hierarchical Linear Modelling
# In the following section unweighted HLM (chapter 7) will be performed.


# Download necessary packages if not installed
list.of.packages <- c("EdSurvey", "lme4","WeMix","flexplot","tidyverse","sjPlot","jtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)


library(EdSurvey)
library(lme4)
library(nlme)
library(lmerTest)
library(WeMix)
library(flexplot) # get statistics for hlm
library(tidyverse)
library(sjPlot) # For plotting models only lme4
library(jtools) # For plotting models only lme4

# Please make sure data is downloaded
# Data can be downloaded at https://www.oecd.org/pisa/data/2018database/
# As an alternative the Edsurvey package provides a download function that will download data for all countries. Thus the download will take some time
# see help("downloadPISA") for more information


#####################################
# Read data 
###################################

# Path needs to be adjusted by user - path needs to be download folder created by EdSurvey function downloadPISA
sdf <- readPISA(path = "C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Frühjahr 2022/Project Consulting Course/Data/PISA/2018",countries="DEU")
#sdf <- readPISA(path = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",countries="DEU")


global.scales <- c("GCSELFEFF") #Self-efficacy regarding global issues (WLE)
global.scales <- str_to_lower(global.scales) # Edsurvey takes variables as lower case letters

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
                  "repeatgrade",
                  "progn",  # School classification
                  "SC048Q01NA") # Percentage <national modal grade for 15-year-olds>: Students whose <heritage language> is different from <test language

control.vars <- str_to_lower(control.vars)

### Get Data and variables defined above

pisa.sel <- EdSurvey::getData(data = sdf,
                              varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F, # Do not drop omitted levels
                              returnJKreplicates = T) # Return replicate weights





############################################
#### Start of data preparation ##############
###########################################



########### mutate progn -Tatjana ##############
# Recode progn variable to reflect Germany schooltypes
# assign numeric characters

# attributes(pisa.sel$progn)$levels
# class(pisa.sel$cntschid)
# is.numeric(pisa.sel$progn)


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


# Relevel to baseline 2 (=Hauptschule)
pisa.sel$progn_ad <- relevel(pisa.sel$progn_ad, ref="2")

#is progn a factor?
#class(pisa.sel$progn_ad)
#yes!

# 1. Förderschule (1), 
# 2. Hauptschule(2), 
# 3. Realschule(3), 
# 4. Gymnasium(4,5,21), 
# 5. Integrierte Gesamtschule(6-7,16-17), 
# 6. Schule mit mehreren Bildungsgängen(8-15) and 
# 7. Berufsschule (18-20)



### PROGN with German school names
# Recode progn variable to reflect Germany schooltypes


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
# Dichotomize Grade variable because of small cell sizes
summary2(sdf, "st001d01t")

pisa.sel<- pisa.sel%>%
  mutate(st001d01t_ad = factor(case_when(st001d01t <= 9 ~ "Grade 7-9",
                                         st001d01t >= 10 ~ "Grade 10-12")))

# Relevel to baseline Hauptschule
pisa.sel$st001d01t_ad <- relevel(pisa.sel$st001d01t_ad, ref="Grade 7-9")


# calculate school hisei
# hisei_gc = group-mean centered
pisa.sel <- pisa.sel %>% group_by(cntschid) %>% mutate(avg_hisei = mean(hisei, na.rm = TRUE),
                                                       hisei_gc = hisei - avg_hisei) %>% ungroup()


# Check group mean centering
#pisa.sel %>% select(hisei, avg_hisei, hisei_gc)

# Show school average hisei
pisa.sel %>% 
  group_by(cntschid) %>% 
  summarise(avg_hisei = mean(hisei, na.rm = TRUE)) %>% ungroup


##########################################################
######### Rebinding attributes to use EdSurvey functions
##########################################################
pisa.sel <- rebindAttributes(pisa.sel, sdf)



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
length(full.cases$cntstuid) # 2989 obs

# Number of schools
length(unique(pisa.sel2$cntschid))

# Descriptive statistics of full cases
t <- pisa.sel2 %>% group_by(cntschid) %>% summarize(number_stu = n()) %>% ungroup
summary(t$number_stu)
sd(t$number_stu)


############################################
#### End of data preparation ##############
###########################################

#######################################
### Start of HLM #####################
######################################


# intercept only model
intercept.only <- gls(pv1read~1, data = pisa.sel2, method = "ML")
summary(intercept.only)


# Null model
baseline.unw <- lme4::lmer(pv1read ~ 1 + (1|cntschid), data = pisa.sel2)
summary(baseline.unw) 
estimates(baseline.unw) # from flexplot

# Getting p-values from lmerTest
summary(lmerTest::lmer(pv1read ~ 1 + (1|cntschid), data = pisa.sel2,REML = F))

# tab model with sjPlot
#tab_model(baseline.unw, show.r2 = FALSE) # from sjPlot

# Export to word
#tab_model(baseline.unw, show.r2 = FALSE, file = "baseline.unw.doc") # from sjPlot


# Visualize with flexplot
visualize(baseline.unw, plot = "residuals")
#visualize(baseline.unw, sample = 174, plot = "model")



# gcselfeff
gcselfeff.unw <-  lme4::lmer(pv1read ~ gcselfeff + (1|cntschid), data = pisa.sel2)
summary(gcselfeff.unw)
estimates(gcselfeff.unw)
tab_model(gcselfeff.unw, show.r2 = FALSE)

# Getting p-values from lmerTest
summary(lmerTest::lmer(pv1read ~ gcselfeff + (1|cntschid), data = pisa.sel2))


# Visualize with flexplot
visualize(gcselfeff.unw,  sample = 174, plot = "model")

# Alternative to plot fitted vs resid
plot(gcselfeff.unw)

##################################
# Important model comparison
model.comparison(baseline.unw,gcselfeff.unw)

tab_model(baseline.unw,gcselfeff.unw, show.r2 = FALSE)



#######################
# Control variables
######################
# Raw scores of hisei
control.unw <-  lme4::lmer(pv1read ~ progn_de + st001d01t_ad + st004d01t + hisei + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
                  data = pisa.sel2)
flexplot::estimates(control.unw)
summary(control.unw)

# Getting p-values from lmerTest
summary(lmerTest::lmer(pv1read ~ progn_de + st001d01t_ad + st004d01t + hisei + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
                   data = pisa.sel2))



# Visualize with flexplot
#visualize(control.unw, sample = 174)

# Alternative to plot fitted vs resid
plot(control.unw)

##################################
# Important model comparison
model.comparison(baseline.unw,control.unw)


######################
# group centered hisei
######################

# control.centered <-  lme4::lmer(pv1read ~ progn_de + st001d01t_ad + st004d01t + hisei_gc + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
#                      data = pisa.sel2)
# estimates(control.centered)
# summary(control.centered)
# 
# # Getting p-values from lmerTest
# summary(lmerTest::lmer(pv1read ~ progn_de + st001d01t_ad + st004d01t + hisei_gc + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
#                        data = pisa.sel2))
# 
# # Alternative to plot fitted vs resid
# plot(control.centered)
# 
# ##################################
# # Important model comparison
# model.comparison(baseline.unw,control.centered)




###################
# Full models
###################
# Raw scores of hisei
full.unw <-  lme4::lmer(pv1read ~ gcselfeff + progn_de + st001d01t_ad + st004d01t + hisei + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
                  data = pisa.sel2)
flexplot::estimates(full.unw)
summary(full.unw)

# Getting p-values from lmerTest
summary(lmerTest::lmer(pv1read ~ gcselfeff + progn_de + st001d01t_ad + st004d01t + hisei + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
                   data = pisa.sel2))

# Alternative to plot fitted vs resid
plot(full.unw)

##################################
# Important model comparison
model.comparison(control.unw, full.unw)




######################
# group centered hisei
######################

# # Raw scores of hisei
# full.unw.centered <-  lme4::lmer(pv1read ~ gcselfeff + progn_de + st001d01t_ad + st004d01t + hisei_gc + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
#                            data = pisa.sel2)
# estimates(full.unw.centered)
# summary(full.unw.centered)
# 
# 
# # Getting p-values from lmerTest
# summary(lmerTest::lmer(pv1read ~ gcselfeff + progn_de + st001d01t_ad + st004d01t + hisei_gc + avg_hisei + immig + repeatgrade +  sc048q01na + (1|cntschid), 
#                        data = pisa.sel2))
# 
# 
# ##################################
# # Important model comparison
# model.comparison(control.unw, full.unw)


# 
# # Alternative to plot fitted vs resid
# plot(full.unw.centered)
# 
# 
# ##################################
# # Important model comparison
# model.comparison(control.centered, full.unw.centered)



# tab models
# tab_model(baseline.unw,gcselfeff.unw,control.unw, full.unw, show.r2 = FALSE)
# tab_model(baseline.unw,gcselfeff.unw,control.unw, full.unw, show.r2 = FALSE, file = "hlm_unweighted.doc")





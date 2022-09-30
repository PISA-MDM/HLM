## Ed Survey / WeMix weighted models

library(EdSurvey)
library(lme4)
library(WeMix)
#library(flexplot)
library(tidyverse)


#####################################
# Read data 
###################################

sdf <- readPISA(path = "C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Fr?hjahr 2022/Project Consulting Course/Data/PISA/2018",countries="DEU")

#ISR
sdf <- readPISA(path = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",countries="DEU")


global.scales <- c("GCSELFEFF",#Self-efficacy regarding global issues (WLE)
                   "GCAWARE",#Student's awareness of global issues (WLE)
                   "PERSPECT",#Perspective-taking (WLE)
                   "COGFLEX",#Cognitive flexibility/adaptability (WLE)
                   "AWACOM",#Awareness of intercultural communication (WLE)
                   "INTCULT",#Student's interest in learning about other cultures (WLE)
                   "RESPECT",#Respect for people from other cultures (WLE)
                   "GLOBMIND",#Global-mindedness (WLE)
                   "ATTIMM")
global.scales <- str_to_lower(global.scales)

pv <- c("PV1READ" , "PV2READ", "PV3READ", "PV4READ", "PV5READ" , "PV6READ", "PV7READ", "PV8READ", "PV9READ" , "PV10READ")
pv <- str_to_lower(pv)


id.vars <- c("cntschid","cntstuid")


wt.vars <- c("w_fstuwt", #FINAL TRIMMED NONRESPONSE ADJUSTED STUDENT WEIGHT
             "w_schgrnrabwt", #  GRADE NONRESPONSE ADJUSTED SCHOOL BASE WEIGHT
             "w_fstuwt_sch_sum") # Sum of W_FSTUW

control.vars <- c("ST001D01T",#Grade
                  "ST004D01T",#Student (Standardized) Gender
                  "HISCED",#Highest Education of parents (ISCED)
                  "HISEI",#Highest International Socio-Economic Index of Occupational Status
                  "PARED",#Index highest parental education in years of schooling
                  "IMMIG",#Index Immigration status
                  "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                  "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
                  "repeatgrade",
                  "progn",  # School classification %>% 
                  "SC048Q01NA") # Percentage <national modal grade for 15-year-olds>: Students whose <heritage language> is different from <test language>

control.vars <- str_to_lower(control.vars)

### Get Data
pisa.sel <- EdSurvey::getData(data = sdf,
                              varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F, # Do not drop omitted levels
                              returnJKreplicates = F) # don?t return replicate weights

# Complete case analysis - delete cases with missing values
omitted2018 <- getAttributes(sdf,'omittedLevels')

# save full dataset separately
pisa.full <- pisa.sel


for (i in 1:ncol(pisa.sel)) {
  pisa.sel <- pisa.sel[!pisa.sel[,i] %in% omitted2018,]
}
#2034 complete cases


# Immigration dummy variable
pisa.sel$dummy.immig <- ifelse(pisa.sel$immig %in% c("SECOND-GENERATION",
                                                     "FIRST-GENERATION"), "MIGRATION-BACKROUND",
                               "NATIVE")
# Numeric immig dummy
pisa.sel$dummy.immig.num <- ifelse(pisa.sel$dummy.immig == "NATIVE",0,1)


# Dummy variables for grade 9/10
pisa.sel <- pisa.sel %>% 
  mutate(G9 = ifelse(st001d01t == "GRADE 9",1,0),
         G9_fct = factor(G9,labels = c("Other","G9")),
         G10 = ifelse(st001d01t == "GRADE 10", 1,0),
         G10_fct = factor(G10, labels = c("Other","G10")))


# calculate school hisei
pisa.sel <- pisa.sel %>% 
  group_by(cntschid) %>% 
  mutate(avg_hisei = mean(hisei)) %>% ungroup()


# Add required school weight variable
pisa.sel$w_fschwt <- pisa.sel$w_schgrnrabwt
pisa.sel$dummywt <- 1


# Rebind attributes to use EdSurvey functions
pisa.rebind <- rebindAttributes(pisa.sel,sdf)

##################################
##### Null model ################
#############################


# Use function with EdSurvey
# No weights provided
m1 <- mixed.sdf(pv1read ~ 1 + (1|cntschid), data = pisa.rebind)
summary(m1)

# School weight provided
baseline.schoolw <- mixed.sdf(pv1read ~ 1 + (1|cntschid), data = pisa.rebind, 
                 weightVars = c("dummywt","w_schgrnrabwt"), weightTransformation = T)
summary(baseline.schoolw)

# Using scaled weights
baseline.transformed <- mixed.sdf(pv1read ~ 1 + (1|cntschid), data = pisa.rebind,
                 weightVars = c("w_fstuwt","w_schgrnrabwt"), weightTransformation = T)

summary(baseline.schoolw.transformed)



# Alternative use WeMix directly
# School weight provided
baseline.wemix <- mix(pv1read ~ 1 + (1|cntschid), data = pisa.rebind, 
            weights = c("dummywt","w_schgrnrabwt"))
summary(baseline.wemix)



##################################
##### Simple model ################
#############################


# Model with Global competence as predictor
gcselfeff.schoolw <- mixed.sdf(pv1read ~ gcselfeff + (1|cntschid), data = pisa.rebind, 
                 weightVars = c("dummywt","w_schgrnrabwt"))
summary(m2.gcselfeff)



# Using scaled weights
gcselfeff.transformed <- mixed.sdf(pv1read ~ gcselfeff + (1|cntschid), data = pisa.rebind,
                                          weightVars = c("w_fstuwt","w_schgrnrabwt"), weightTransformation = T)

summary(gcselfeff.transformed)




##################################
##### Full model ################
#############################



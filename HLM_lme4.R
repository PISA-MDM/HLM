# HLM unweighted

library(EdSurvey)
library(lme4)
library(WeMix)
library(flexplot)
library(tidyverse)
library(sjPlot)
library(jtools)
#####################################
# Read data 
###################################

sdf <- readPISA(path = "C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Frühjahr 2022/Project Consulting Course/Data/PISA/2018",countries="DEU")


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
                              returnJKreplicates = F) # don´t return replicate weights# Complete case analysis - 
#delete cases with missing values
omitted2018 <- getAttributes(sdf,'omittedLevels')

# save full dataset separately
pisa.full <- pisa.sel


for (i in 1:ncol(pisa.sel)) {
  pisa.sel <- pisa.sel[!pisa.sel[,i] %in% omitted2018,]
}


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
pisa.sel <- pisa.sel %>% group_by(cntschid) %>% mutate(avg_hisei = mean(hisei)) %>% ungroup()

# Null model
baseline.unw <- lmer(pv1read ~ 1 + (1|cntschid), data = pisa.sel,REML = F)
summary(baseline.unw) 
estimates(baseline.unw) # from flexplot

# tab model with sjPlot
tab_model(baseline.unw, show.r2 = FALSE) # from sjPlot

# Export to word
tab_model(baseline.unw, show.r2 = FALSE, file = "baseline.unw.doc") # from sjPlot



# Visualize with flexplot
Visualize model
visualize(baseline, plot = "residuals")



# gcselfeff
gcselfeff.unw <-  lmer(pv1read ~ gcselfeff + (1|cntschid), data = pisa.sel,REML = F)
summary(gcselfeff.unw)
estimates(gcselfeff.unw)
tab_model(gcselfeff.unw, show.r2 = FALSE)

# Visualize with flexplot
visualize(gcselfeff.unw)
# Alternative to plot fitted vs resid
plot(gcselfeff.unw)

##################################
# Important model comparison
model.comparison(baseline.unw,gcselfeff.unw)

tab_model(baseline.unw,gcselfeff.unw)



# Full model
# Needs to be adjusted 
full.unw <-  lmer(pv1read ~ gcselfeff + st001d01t + st004d01t + hisei + avg_hisei + immig + repeatgrade + progn + sc048q01na + (1|cntschid), data = pisa.sel)
estimates(full.unw)
summary(full.unw)


# tab the 2 models
tab_model(baseline.unw,gcselfeff.unw,full.unw, show.r2 = FALSE)

tab_model(baseline.unw,gcselfeff.unw,full.unw, show.r2 = FALSE, file = "hlm_unweighted.doc")


# Model comparison
model.comparison(gcselfeff.unw,full.unw)




#########################################################################
#### Weighted analysi using lme4 with school weights
#########################################################################

# Null model
baseline.w <- lmer(pv1read ~ 1 + (1|cntschid), data = pisa.sel,REML = F,weights = w_schgrnrabwt)
summary(baseline.w) 
estimates(baseline.w) # from flexplot
tab_model(baseline.w, show.r2 = F) # from sjPlot


# Visualize model
visualize(baseline.w, plot = "residuals")



# gcselfeff
gcselfeff.w <-  lmer(pv1read ~ gcselfeff + (1|cntschid), data = pisa.sel,REML = F, weights = w_schgrnrabwt)
summary(gcselfeff.w)
estimates(gcselfeff.w)
tab_model(gcselfeff.w, show.r2 = F)


##################################
# Important model comparison
model.comparison(baseline.w,gcselfeff.w)

tab_model(baseline.w,gcselfeff.w , show.r2 = F)



# Full model
# Needs to be adjusted
full.w <-  lmer(pv1read ~ gcselfeff + st001d01t + st004d01t + hisei + avg_hisei + immig + repeatgrade + progn + sc048q01na + (1|cntschid), data = pisa.sel,REML = F, weights = w_schgrnrabwt)

estimates(full.w)
summary(full.w)


tab_model(baseline.w,gcselfeff.w,full.w, show.r2 = F)
tab_model(baseline.w,gcselfeff.w,full.w, show.r2 = F, file = "hlm_lme_weighted.doc")


# Model comparison
model.comparison(gcselfeff.w,full.w)


# Tab unweighted vs weighted models
tab_model(baseline.unw,baseline.w,show.r2 = F, file = "baseline_unw_lme_weighted.doc")
tab_model(gcselfeff.unw,gcselfeff.w,show.r2 = F, file = "gcseleff_unw_lme_weighted.doc")
tab_model(full.unw,full.w,show.r2 = F, file = "full_unw_lme_weighted.doc")








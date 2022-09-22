
#########################################
# Reading Data in package intsvy ####
############################################


library(haven)
library(devtools)
library(intsvy)
library(stringr)
library(EdSurvey)
library(sjPlot)
library(WeMix)
library(flexplot)



# Print variable labels and names of participating countries
# PISA
# pisa.var.label(folder = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",  student.file = "CY07_MSU_SCH_QQQ.sav",
#                 school.file= "CY07_MSU_STU_QQQ.sav", output = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data/" )

# Import PISA Data
pisa.de <- pisa.select.merge(folder = "C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Frühjahr 2022/Project Consulting Course/PISA/Data/SPSS",
                              school.file = "CY07_MSU_SCH_QQQ.sav",
                              student.file = "CY07_MSU_STU_QQQ.sav",
                              student =c("ST001D01T",#Grade
                                         "ST004D01T",#Student (Standardized) Gender
                                         "HISCED",#Highest Education of parents (ISCED)
                                         "HISEI",#Highest In-ternational Socio-Economic Index of Occupational Status
                                         "PARED",#Index highest parental education in years of schooling
                                         "IMMIG",#Index Immigration status
                                         "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                                         "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
                                         "REPEAT",# Grade repetition
                                         "GCSELFEFF",#Self-efficacy regarding global issues (WLE)
                                         "GCAWARE",#Student's awareness of global issues (WLE)
                                         "PERSPECT",#Perspective-taking (WLE)
                                         "COGFLEX",#Cognitive flexibility/adaptability (WLE)
                                         "AWACOM",#Awareness of intercultural communication (WLE)
                                         "INTCULT",#Student's interest in learning about other cultures (WLE)
                                         "RESPECT",#Respect for people from other cultures (WLE)
                                         "GLOBMIND",#Global-mindedness (WLE)
                                         "ATTIMM"#Student's attitudes towards immigrants (WLE)
                                         ),
                              school = c("W_SCHGRNRABWT"),
                              countries = c("DEU"))



pisa.de$DUMMYWT <- 1

# Using WeMix for weighted Null-model
# School weight provided
baseline.wemix <- mix(PV1READ ~ 1 |CNTSCHID,  data = pisa.de, 
                      weights = c("W_FSTUWT","W_SCHGRNRABWT"))
summary(baseline.wemix)

# summary(baseline.wemix)
# Call:
#   mix(formula = PV1READ ~ 1 | CNTSCHID, data = pisa.de, weights = c("W_FSTUWT", 
#                                                                     "W_SCHGRNRABWT"))
# 
# Variance terms:
#   Level    Group        Name Variance Std. Error Std.Dev.
# 2 CNTSCHID (Intercept)     6153      550.5    78.44
# 1 Residual                 5794      140.3    76.12
# Groups:
#   Level    Group n size mean wgt sum wgt
# 2 CNTSCHID    223    58.51   13047
# 1      Obs   5451   134.82  734915
# 
# Fixed Effects:
#   Estimate Std. Error t value
# (Intercept)  467.384      6.176   75.68
# 
# lnl= -4251118.93 
# Intraclass Correlation= 0.515 
# 

baseline.wemix <- mix(PV1READ ~ 1 |CNTSCHID,  data = pisa.de, 
                      weights = c("DUMMYWT","W_SCHGRNRABWT"))
summary(baseline.wemix)


# Call:
#   mix(formula = PV1READ ~ 1 | CNTSCHID, data = pisa.de, weights = c("DUMMYWT", 
#                                                                     "W_SCHGRNRABWT"))
# 
# Variance terms:
#   Level    Group        Name  Variance Std. Error  Std.Dev.
# 2 CNTSCHID (Intercept) 5.247e-19  2.147e-18 7.244e-10
# 1 Residual             1.129e+04  4.158e+02 1.063e+02
# Groups:
#   Level    Group n size mean wgt sum wgt
# 2 CNTSCHID    223    58.51   13047
# 1      Obs   5451     1.00    5451
# 
# Fixed Effects:
#   Estimate Std. Error t value
# (Intercept)  500.852      5.065   98.89
# 
# lnl= -33168.17 
# Intraclass Correlation= 4.648e-23 
# 







 
# # Null model
baseline.w <- lmer(PV1READ ~ 1 |CNTSCHID,  data = pisa.de,
                   REML = F,weights = W_FSTUWT)
summary(baseline.w) 
estimates(baseline.w) # from flexplot
tab_model(baseline.w, show.r2 = F) # from sjPlot


# 
# > # Null model
#   > baseline.w <- lmer(PV1READ ~ 1 |CNTSCHID,  data = pisa.de,
#                        +                    REML = F,weights = W_SCHGRNRABWT)
# > summary(baseline.w) 
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: PV1READ ~ 1 | CNTSCHID
# Data: pisa.de
# Weights: W_SCHGRNRABWT
# 
# AIC      BIC   logLik deviance df.resid 
# 64322.4  64342.2 -32158.2  64316.4     5448 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -5.4591 -0.5868  0.0301  0.6234  4.6384 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# CNTSCHID (Intercept)   5924    76.96  
# Residual             307166   554.23  
# Number of obs: 5451, groups:  CNTSCHID, 223
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)  490.628      5.283   92.87
# > estimates(baseline.w) # from flexplot
# Fixed Effects: 
#   (Intercept) 
# 490.628 
# 
# 
# Random Effects: 
#   Groups   Name        Std.Dev.
# CNTSCHID (Intercept)  76.964 
# Residual             554.226 
# 
# 
# ICC and Design Effect: 
#   icc design.effect 
# 0.01891957    1.44354934 
# 
# 
# R Squared: 
#   
#   (Intercept)    Residual 
# 0           0 
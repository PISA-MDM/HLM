
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
                      weights = c("DUMMYWT","W_SCHGRNRABWT"))
summary(baseline.wemix)


# Null model
baseline.w <- lmer(PV1READ ~ 1 |CNTSCHID,  data = pisa.de,
                   REML = F,weights = W_SCHGRNRABWT)
summary(baseline.w) 
estimates(baseline.w) # from flexplot
tab_model(baseline.w, show.r2 = F) # from sjPlot
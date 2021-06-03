#lmes for MFQ analyses

#inital R set up 
rm(list = ls()) # command to clear all variables from R environment
library(dplyr)
library(readxl)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(gapminder)
library(writexl)
library(lme4)
library(nlme)

FinalDatabase <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/MFQAnalysesDatabase.xlsx")

#Demographic Information
finalN <- FinalDatabase %>% group_by(SDAN) %>% slice(n=1)
mean(finalN$Age_at_visit)
sd(finalN$Age_at_visit)
femaleonly <- finalN %>% filter(SEX == "FEMALE")
maleonly <- finalN %>% filter(SEX == "MALE")
98/130
famhx <- finalN %>% filter(dep_immed == 1)
nofamhx <- finalN %>% filter(dep_immed == 0)
96/130


#doing the LMEs

#true null
truenull <- lme(s_mfq_tot ~ antidepressants + TimeBetween + InpatientDuring + Age_at_visit + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(truenull)

#Model 1
previousMFQonly <- lme(s_mfq_tot ~ antidepressants + TimeBetween + MFQtminus1 + InpatientDuring + Age_at_visit + SEX  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(previousMFQonly)

#Model 2
fhmfqmodel <- lme(s_mfq_tot ~ dep_immed*TimeBetween + MFQtminus1 + InpatientDuring + Age_at_visit + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhmfqmodel)

#Model 3
fhonlymodel <- lme(s_mfq_tot ~ dep_immed*TimeBetween + InpatientDuring + Age_at_visit + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhonlymodel)





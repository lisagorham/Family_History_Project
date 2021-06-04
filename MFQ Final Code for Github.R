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

MFQDatabase <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lily Eisner/Research/FH/Lisa Files/Family History Interview/FINALMFQDATABASEFORPAPER.xlsx")
#If you were loading in the PII free dataset, use the following line of code instead:
#MFQDatabase <- read_xlsx("./data/PII Free MFQ Dataset.xlsx" )

#remove the outlier (900 days between pair of visits)
FinalDatabase <- MFQDatabase %>% filter(SDAN != 23501)

#Demographic Information
finalN <- FinalDatabase %>% group_by(SDAN) %>% slice_head(n=1)
mean(finalN$PreviousAge)
sd(finalN$PreviousAge)
femaleonly <- finalN %>% filter(SEX == "FEMALE")
maleonly <- finalN %>% filter(SEX == "MALE")
97/129
famhx <- finalN %>% filter(dep_immed == 1)
nofamhx <- finalN %>% filter(dep_immed == 0)
96/129
antidep <- finalN %>% filter(antidepressants ==1)
noantidep <- finalN %>% filter(antidepressants == 0)
56/129
othermed <- finalN %>% filter(OtherMeds == 1)
noothermed <- finalN %>% filter(OtherMeds == 0)
34/129

#calculating mean mfq 
meanperperson <- FinalDatabase %>% group_by(SDAN) %>% summarise_at(vars(s_mfq_tot), list(name = mean))
mean(meanperperson$name)
sd(meanperperson$name)
hist(FinalDatabase$s_mfq_tot,
     main = "Histogram of MFQ Scores Across All Participants and Time Points",
     xlab = "MFQ Score")

#doing the LMEs

#true null
truenull <- lme(s_mfq_tot ~ antidepressants + TimeBetween + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(truenull)

#Model 1
previousMFQonly <- lme(s_mfq_tot ~ antidepressants + TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(previousMFQonly)

#Model 2
fhmfqmodel <- lme(s_mfq_tot ~ dep_immed*TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhmfqmodel)

#Model 3
fhonlymodel <- lme(s_mfq_tot ~ dep_immed*TimeBetween + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhonlymodel)

#Model 4
as.numeric(FinalDatabase$TimeBetween)
values <- as.numeric(FinalDatabase$TimeBetween)
values2 <- values^2
FinalDatabase$Time2 <- values2

fhmodel2 <- lme(s_mfq_tot ~ dep_immed*TimeBetween + dep_immed:Time2 + Time2 + MFQtminus1 + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhmodel2)


#Creating a Baseline MFQ Score
first <- finalN %>% select(SDAN, MFQtminus1) %>% rename("BaselineMFQScore" = "MFQtminus1")
total <- merge(FinalDatabase, first, by = "SDAN")

#Model 5
BaselineMFQ <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + InpatientDuring + PreviousAge + SEX +  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQ)

#Model 6
BaselineMFQFH <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + dep_immed + dep_immed:TimeBetween + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQFH)

#Model 7
BaselineMFQPreviousMFQ <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + MFQtminus1 + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQPreviousMFQ)

#Model 8
BaselineMFQPreviousMFQFH <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + dep_immed + dep_immed:TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQPreviousMFQFH)


































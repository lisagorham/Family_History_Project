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

#MFQDatabase <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lily Eisner/Research/FH/Lisa Files/Family History Interview/FINALMFQDATABASEFORPAPER.xlsx")
MFQDatabase <- read_xlsx("./data/FINALMFQDATABASEFORPAPER_PLUSDX_PLUSANX.xlsx")
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

# Sensitivity analysis with formal diagnosis only for supplement

#doing the LMEs

#true null
truenull_dx <- lme(s_mfq_tot ~ antidepressants + TimeBetween + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(truenull_dx)

#Model 1
previousMFQonly_dx <- lme(s_mfq_tot ~ antidepressants + TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(previousMFQonly_dx)

#Model 2
fhmfqmodel_dx <- lme(s_mfq_tot ~ dep_immed_dx*TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhmfqmodel_dx)

#Model 3
fhonlymodel_dx <- lme(s_mfq_tot ~ dep_immed_dx*TimeBetween + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhonlymodel_dx)

#Model 4
as.numeric(FinalDatabase$TimeBetween)
values <- as.numeric(FinalDatabase$TimeBetween)
values2 <- values^2
FinalDatabase$Time2 <- values2

fhmodel2 <- lme(s_mfq_tot ~ dep_immed_dx*TimeBetween + dep_immed_dx:Time2 + Time2 + MFQtminus1 + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase)
summary(fhmodel2_dx)


#Model 5
BaselineMFQ_dx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + InpatientDuring + PreviousAge + SEX +  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQ_dx)

#Model 6
BaselineMFQFH_dx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + dep_immed_dx + dep_immed_dx:TimeBetween + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQFH_dx)

#Model 7
BaselineMFQPreviousMFQ_dx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + MFQtminus1 + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQPreviousMFQ_dx)

#Model 8
BaselineMFQPreviousMFQFH_dx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + dep_immed_dx + dep_immed_dx:TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total)
summary(BaselineMFQPreviousMFQFH_dx)


#------------------------------------------------

# Anxiety Exploratory Analyses

MFQDatabase_anx <- read_xlsx("./data/FINALMFQDATABASEFORPAPER_PLUSDX_PLUSANX.xlsx")

#remove the outlier (900 days between pair of visits)
FinalDatabase_anx <- MFQDatabase_anx %>% filter(SDAN != 23501)

#Demographic Information
finalN_anx <- FinalDatabase_anx %>% group_by(SDAN) %>% slice_head(n=1)
mean(finalN_anx$PreviousAge)
sd(finalN_anx$PreviousAge)
femaleonly_anx <- finalN_anx %>% filter(SEX == "FEMALE")
maleonly_anx <- finalN_anx %>% filter(SEX == "MALE")
97/129
famhx_anx <- finalN_anx %>% filter(anx_immed == 1)
nofamhx_anx <- finalN_anx %>% filter(anx_immed == 0)
96/129
antidep_anx <- finalN_anx %>% filter(antidepressants ==1)
noantidep_anx <- finalN_anx %>% filter(antidepressants == 0)
56/129
othermed_anx <- finalN_anx %>% filter(OtherMeds == 1)
noothermed_anx <- finalN_anx %>% filter(OtherMeds == 0)
34/129

#calculating mean mfq 
meanperperson_anx <- FinalDatabase_anx %>% group_by(SDAN) %>% summarise_at(vars(s_mfq_tot), list(name = mean))
mean(meanperperson_anx$name)
sd(meanperperson_anx$name)
hist(FinalDatabase_anx$s_mfq_tot,
     main = "Histogram of MFQ Scores Across All Participants and Time Points (Anxeity Analysis)",
     xlab = "MFQ Score")

#doing the LMEs

#true null
truenull_anx <- lme(s_mfq_tot ~ antidepressants + TimeBetween + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_anx)
summary(truenull_anx)

#Model 1
previousMFQonly_anx <- lme(s_mfq_tot ~ antidepressants + TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_anx)
summary(previousMFQonly_anx)

#Model 2
fhmfqmodel_anx <- lme(s_mfq_tot ~ anx_immed*TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_anx)
summary(fhmfqmodel_anx)

#Model 3
fhonlymodel_anx <- lme(s_mfq_tot ~ anx_immed*TimeBetween + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_anx)
summary(fhonlymodel_anx)

#Model 4
as.numeric(FinalDatabase_anx$TimeBetween)
values <- as.numeric(FinalDatabase_anx$TimeBetween)
values2 <- values^2
FinalDatabase_anx$Time2 <- values2

fhmodel2_anx <- lme(s_mfq_tot ~ anx_immed*TimeBetween + anx_immed:Time2 + Time2 + MFQtminus1 + InpatientDuring + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_anx)
summary(fhmodel2_anx)

#Creating a Baseline MFQ Score
first_anx <- finalN_anx %>% select(SDAN, MFQtminus1) %>% rename("BaselineMFQScore" = "MFQtminus1")
total_anx <- merge(FinalDatabase_anx, first_anx, by = "SDAN")

#Model 5
BaselineMFQ_anx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + InpatientDuring + PreviousAge + SEX +  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_anx)
summary(BaselineMFQ_anx)

#Model 6
BaselineMFQFH_anx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + anx_immed + anx_immed:TimeBetween + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_anx)
summary(BaselineMFQFH_anx)

#Model 7
BaselineMFQPreviousMFQ_anx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + MFQtminus1 + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_anx)
summary(BaselineMFQPreviousMFQ_anx)

#Model 8
BaselineMFQPreviousMFQFH_anx <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + anx_immed + anx_immed:TimeBetween + MFQtminus1 + InpatientDuring + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_anx)
summary(BaselineMFQPreviousMFQFH_anx)

#------------------------------------------------

# No Inpatient sensitivity analysis

MFQDatabase_noinpatient <- read_xlsx("./data/FINALMFQDATABASEFORPAPER_NOINPATIENT.xlsx")

#remove the outlier (900 days between pair of visits)
FinalDatabase_noinpatient <- MFQDatabase_noinpatient %>% filter(SDAN != 23501)

#Demographic Information
finalN_noinpatient <- FinalDatabase_noinpatient %>% group_by(SDAN) %>% slice_head(n=1)
mean(finalN_noinpatient$PreviousAge)
sd(finalN_noinpatient$PreviousAge)
femaleonly_noinpatient <- finalN_noinpatient %>% filter(SEX == "FEMALE")
maleonly_noinpatient <- finalN_noinpatient %>% filter(SEX == "MALE")
97/129
famhx_noinpatient <- finalN_noinpatient %>% filter(dep_immed == 1)
nofamhx_noinpatient <- finalN_noinpatient %>% filter(dep_immed == 0)
96/129
antidep_noinpatient <- finalN_noinpatient %>% filter(antidepressants ==1)
noantidep_noinpatient <- finalN_noinpatient %>% filter(antidepressants == 0)
56/129
othermed_noinpatient <- finalN_noinpatient %>% filter(OtherMeds == 1)
noothermed_noinpatient <- finalN_noinpatient %>% filter(OtherMeds == 0)
34/129

#calculating mean mfq 
meanperperson_noinpatient <- FinalDatabase_noinpatient %>% group_by(SDAN) %>% summarise_at(vars(s_mfq_tot), list(name = mean))
mean(meanperperson_noinpatient$name)
sd(meanperperson_noinpatient$name)
hist(FinalDatabase_noinpatient$s_mfq_tot,
     main = "Histogram of MFQ Scores Across All Participants and Time Points (No Inpatient Sensitivity Analysis)",
     xlab = "MFQ Score")

#doing the LMEs

#true null
truenull_noinpatient <- lme(s_mfq_tot ~ antidepressants + TimeBetween + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_noinpatient)
summary(truenull_noinpatient)

#Model 1
previousMFQonly_noinpatient <- lme(s_mfq_tot ~ antidepressants + TimeBetween + MFQtminus1 + PreviousAge + SEX  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_noinpatient)
summary(previousMFQonly_noinpatient)

#Model 2
fhmfqmodel_noinpatient <- lme(s_mfq_tot ~ dep_immed*TimeBetween + MFQtminus1 + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_noinpatient)
summary(fhmfqmodel_noinpatient)

#Model 3
fhonlymodel_noinpatient <- lme(s_mfq_tot ~ dep_immed*TimeBetween + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_noinpatient)
summary(fhonlymodel_noinpatient)

#Model 4
as.numeric(FinalDatabase_noinpatient$TimeBetween)
values <- as.numeric(FinalDatabase_noinpatient$TimeBetween)
values2 <- values^2
FinalDatabase_noinpatient$Time2 <- values2

fhmodel2_noinpatient <- lme(s_mfq_tot ~ dep_immed*TimeBetween + dep_immed:Time2 + Time2 + MFQtminus1 + PreviousAge + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = FinalDatabase_noinpatient)
summary(fhmodel2_noinpatient)

#Creating a Baseline MFQ Score
first_noinpatient <- finalN_noinpatient %>% select(SDAN, MFQtminus1) %>% rename("BaselineMFQScore" = "MFQtminus1")
total_noinpatient <- merge(FinalDatabase_noinpatient, first_noinpatient, by = "SDAN")

#Model 5
BaselineMFQ_noinpatient <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + PreviousAge + SEX +  + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_noinpatient)
summary(BaselineMFQ_noinpatient)

#Model 6
BaselineMFQFH_noinpatient <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + dep_immed + dep_immed:TimeBetween + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_noinpatient)
summary(BaselineMFQFH_noinpatient)

#Model 7
BaselineMFQPreviousMFQ_noinpatient <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + MFQtminus1 + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_noinpatient)
summary(BaselineMFQPreviousMFQ_noinpatient)

#Model 8
BaselineMFQPreviousMFQFH_noinpatient <- lme(s_mfq_tot ~ antidepressants + TimeBetween*BaselineMFQScore + dep_immed + dep_immed:TimeBetween + MFQtminus1 + PreviousAge + SEX + OtherMeds + postpandemic, random = (~TimeBetween | SDAN), data = total_noinpatient)
summary(BaselineMFQPreviousMFQFH_noinpatient)


#linear regressions for weeks of depression analyses

rm(list = ls()) # command to clear all variables from R environment

# Load necessary packages 
library(dplyr)
library(readxl)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(gapminder)
library(writexl)

#dataset3 <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lily Eisner/Research/FH/Lisa Files/Family History Interview/FINALWODDATASETFORPAPER.xlsx" )
dataset3 <- read_xlsx("./data/FINALWODDATASETFORPAPER_PLUSDX_PLUSANX.xlsx" )
#If you were loading in the PII free dataset, use the following line of code instead:
#dataset3 <- read_xlsx("./data/PII Free WOD Dataset.xlsx" )

#Demographic Information
mean(dataset3$AgeatV1)
sd(dataset3$AgeatV1)
female <- dataset3 %>% filter(SEX == "FEMALE")
male <- dataset3 %>% filter(SEX == "MALE")
52/72
famhx <- dataset3 %>% filter(dep_immed == 1)
nofamhx <- dataset3 %>% filter(dep_immed == 0)
51/72
baseantidep <- dataset3 %>% filter(BaselineAntiDep == 1)
nobaseantidep <- dataset3 %>% filter(BaselineAntiDep == 0)
34/72
baseothermed <- dataset3 %>% filter(BaselineOtherMeds == 1)
nobaseothermed <- dataset3 %>% filter(BaselineOtherMeds == 0)
23/72
mean(dataset3$v1MFQScore)
sd(dataset3$v1MFQScore)
dataset3$c_ksadsdx_epset_annual_weeks_mdd <- as.numeric(dataset3$c_ksadsdx_epset_annual_weeks_mdd)
hist(dataset3$c_ksadsdx_epset_annual_weeks_mdd,
     main = "Histogram of Weeks of Depression",
     xlab = "Weeks of Depression",
     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

#Linear Regressions

#Null Model 
truenull <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(truenull)

#Model 1
nullwithbaselinemfq <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(nullwithbaselinemfq)

#Model 2
baselinemfqplusfh <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(baselinemfqplusfh)

#Model 3
mfqfhcase <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + (dep_immed*s_case__neg_tot) + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(mfqfhcase)

#Model 4
familyhistoryonly <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(familyhistoryonly)


# sensitivity analysis with only formal diagnosis of depression for supplemental analysis

#Linear Regressions

#Null Model 
truenull_dx <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(truenull_dx)

#Model 1
nullwithbaselinemfq_dx <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(nullwithbaselinemfq_dx)

#Model 2
baselinemfqplusfh_dx <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + dep_immed_dx + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(baselinemfqplusfh_dx)

#Model 3
mfqfhcase_dx <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + (dep_immed_dx*s_case__neg_tot) + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(mfqfhcase_dx)

#Model 4
familyhistoryonly_dx <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ dep_immed_dx + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3)
summary(familyhistoryonly_dx)

#------------------------------------------------

# Anxiety Exploratory Analyses

#dataset3 <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lily Eisner/Research/FH/Lisa Files/Family History Interview/FINALWODDATASETFORPAPER.xlsx" )
dataset3_anx <- read_xlsx("./data/FINALWODDATASETFORPAPER_PLUSDX_PLUSANX.xlsx" )
#If you were loading in the PII free dataset, use the following line of code instead:
#dataset3 <- read_xlsx("./data/PII Free WOD Dataset.xlsx" )

#Demographic Information
mean(dataset3_anx$AgeatV1)
sd(dataset3_anx$AgeatV1)
female_anx <- dataset3_anx %>% filter(SEX == "FEMALE")
male_anx <- dataset3_anx %>% filter(SEX == "MALE")
52/72
famhx_anx <- dataset3_anx %>% filter(anx_immed == 1)
nofamhx_anx <- dataset3_anx %>% filter(anx_immed == 0)
51/72
baseantidep_anx <- dataset3_anx %>% filter(BaselineAntiDep == 1)
nobaseantidep_anx <- dataset3_anx %>% filter(BaselineAntiDep == 0)
34/72
baseothermed_anx <- dataset3_anx %>% filter(BaselineOtherMeds == 1)
nobaseothermed_anx <- dataset3_anx %>% filter(BaselineOtherMeds == 0)
23/72
mean(dataset3_anx$v1MFQScore)
sd(dataset3_anx$v1MFQScore)
dataset3_anx$c_ksadsdx_epset_annual_weeks_mdd <- as.numeric(dataset3_anx$c_ksadsdx_epset_annual_weeks_mdd)
hist(dataset3_anx$c_ksadsdx_epset_annual_weeks_mdd,
     main = "Histogram of Weeks of Depression (Anxiety Analysis)",
     xlab = "Weeks of Depression",
     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

#Linear Regressions

#Null Model 
truenull_anx <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_anx)
summary(truenull_anx)

#Model 1
nullwithbaselinemfq_anx <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_anx)
summary(nullwithbaselinemfq_anx)

#Model 2
baselinemfqplusfh_anx <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + anx_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_anx)
summary(baselinemfqplusfh_anx)

#Model 3
mfqfhcase_anx <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + (anx_immed*s_case__neg_tot) + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_anx)
summary(mfqfhcase_anx)

#Model 4
familyhistoryonly_anx <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ anx_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_anx)
summary(familyhistoryonly_anx)

#------------------------------------------------

# No Inpatient Sensitivity Analysis

#dataset3 <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lily Eisner/Research/FH/Lisa Files/Family History Interview/FINALWODDATASETFORPAPER.xlsx" )
dataset3_noinpatient <- read_xlsx("./data/FINALWODDATASETFORPAPER_NOINPATIENT.xlsx" )
#If you were loading in the PII free dataset, use the following line of code instead:
#dataset3 <- read_xlsx("./data/PII Free WOD Dataset.xlsx" )

#Demographic Information
mean(dataset3_noinpatient$AgeatV1)
sd(dataset3_noinpatient$AgeatV1)
female_noinpatient <- dataset3_noinpatient %>% filter(SEX == "FEMALE")
male_noinpatient <- dataset3_noinpatient %>% filter(SEX == "MALE")
52/72
famhx_noinpatient <- dataset3_noinpatient %>% filter(dep_immed == 1)
nofamhx_noinpatient <- dataset3_noinpatient %>% filter(dep_immed == 0)
51/72
baseantidep_noinpatient <- dataset3_noinpatient %>% filter(BaselineAntiDep == 1)
nobaseantidep_noinpatient <- dataset3_noinpatient %>% filter(BaselineAntiDep == 0)
34/72
baseothermed_noinpatient <- dataset3_noinpatient %>% filter(BaselineOtherMeds == 1)
nobaseothermed_noinpatient <- dataset3_noinpatient %>% filter(BaselineOtherMeds == 0)
23/72
mean(dataset3_noinpatient$v1MFQScore)
sd(dataset3_noinpatient$v1MFQScore)
dataset3_noinpatient$c_ksadsdx_epset_annual_weeks_mdd <- as.numeric(dataset3_noinpatient$c_ksadsdx_epset_annual_weeks_mdd)
hist(dataset3_noinpatient$c_ksadsdx_epset_annual_weeks_mdd,
     main = "Histogram of Weeks of Depression (No Inpatient Analysis)",
     xlab = "Weeks of Depression",
     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

#Linear Regressions

#Null Model 
truenull_noinpatient <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_noinpatient)
summary(truenull_noinpatient)

#Model 1
nullwithbaselinemfq_noinpatient <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_noinpatient)
summary(nullwithbaselinemfq_noinpatient)

#Model 2
baselinemfqplusfh_noinpatient <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_noinpatient)
summary(baselinemfqplusfh_noinpatient)

#Model 3
mfqfhcase_noinpatient <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + (dep_immed*s_case__neg_tot) + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_noinpatient)
summary(mfqfhcase_noinpatient)

#Model 4
familyhistoryonly_noinpatient <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + AgeatV1 + postpandemic, data = dataset3_noinpatient)
summary(familyhistoryonly_noinpatient)


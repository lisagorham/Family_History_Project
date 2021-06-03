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

dataset3 <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/DatasetforTest1.xlsx")

#Demographic Information
mean(dataset3$Age_at_visit)
sd(dataset3$Age_at_visit)
female <- dataset3 %>% filter(SEX == "FEMALE")
male <- dataset3 %>% filter(SEX == "MALE")
52/72
famhx <- dataset3 %>% filter(dep_immed == 1)
nofamhx <- dataset3 %>% filter(dep_immed == 0)
51/72

#Linear Regressions

#Null Model 
truenull <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(truenull)

#Model 1
nullwithbaselinemfq <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(nullwithbaselinemfq)

#Model 2
baselinemfqplusfh <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(baselinemfqplusfh)

#Model 3
mfqfhcase <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + (dep_immed*s_case__neg_tot) + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(mfqfhcase)

#Model 4
familyhistoryonly <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(familyhistoryonly)




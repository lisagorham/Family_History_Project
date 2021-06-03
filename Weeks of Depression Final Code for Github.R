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









#92 person dataset for supplmental analyses 
largerdf <- read.csv("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/WeeksofDepression5.24.2021reduced.csv")

#Demographic Information
mean(largerdf$AgeatV1)
sd(largerdf$AgeatV1)
female2 <- largerdf %>% filter(SEX == "FEMALE")
male2 <- largerdf %>% filter(SEX == "MALE")
66/92
famhx2 <- largerdf %>% filter(dep_immed == 1)
nofamhx2 <- largerdf %>% filter(dep_immed == 0)
68/92
baseantidep2 <- largerdf %>% filter(BaselineAntiDep == 1)
nobaseantidep2 <- largerdf %>% filter(BaselineAntiDep == 0)
40/92
baseothermed2 <- largerdf %>% filter(BaselineOtherMeds == 1)
nobaseothermed2 <- largerdf %>% filter(BaselineOtherMeds == 0)
27/92
mean(largerdf$v1MFQScore)
sd(largerdf$v1MFQScore)


#BSL_Null 
BSL_Null <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineAntiDep + BaselineOtherMeds + SEX + AgeatV1 + postpandemic, data = largerdf)
summary(BSL_Null)

#BSL_FH
BSL_FH <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ dep_immed + BaselineAntiDep + BaselineOtherMeds + SEX + AgeatV1 + postpandemic, data = largerdf)
summary(BSL_FH)

#BSL_MFQ
BSL_MFQ <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ v1MFQScore + BaselineAntiDep + BaselineOtherMeds + SEX + AgeatV1 + postpandemic, data = largerdf)
summary(BSL_MFQ)

#BSL_MFQ+FH
BSL_MFQFH <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ dep_immed + v1MFQScore + BaselineAntiDep + BaselineOtherMeds + SEX + AgeatV1 + postpandemic, data = largerdf)
summary(BSL_MFQFH)


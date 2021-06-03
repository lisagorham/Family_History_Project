#Equivalence Testing for Cross Validated Weeks of Depression Models

rm(list = ls())
#install.packages("TOSTER")
library(TOSTER)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)

#first, load in the outputs of the cross validation 
equivalencedf <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/PUTNAMEINHERE.xlsx")

#NOTE: in this dataset, here are the models:
#Null model: weeks of depression~ BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit
#Model 1: weeks of depression~ Baseline MFQ + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit
#Model 2: weeks of depression~ Baseline MFQ + Family History + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit
#Model 3: weeks of depression~ Baseline MFQ + Family History*CASE + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit
#Model 4: weeks of depression~ Family History + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit

#make sure all values are numeric to avoid errors later on 
equivalencedf$actualvalues <- as.numeric(equivalencedf$actualvalues)
equivalencedf$truenullpredictions <- as.numeric(equivalencedf$truenullpredictions)
equivalencedf$nullwithbaselinemfqpredictions <- as.numeric(equivalencedf$nullwithbaselinemfqpredictions)
equivalencedf$baselinemfqplusfhpredictions <- as.numeric(equivalencedf$baselinemfqplusfhpredictions)
equivalencedf$mfqfhcasepredictions <- as.numeric(equivalencedf$mfqfhcasepredictions)
equivalencedf$familyhistoryonlypredictions <- as.numeric(equivalencedf$familyhistoryonlypredictions)

#calculate the RMSE for each of the different models
equivalencedf <- equivalencedf %>% mutate(nullRMSE = sqrt((equivalencedf$truenullpredictions - equivalencedf$actualvalues)^2)) %>% 
  mutate(model1RMSE = sqrt((equivalencedf$nullwithbaselinemfqpredictions - equivalencedf$actualvalues)^2)) %>%
  mutate(model2RMSE = sqrt((equivalencedf$baselinemfqplusfhpredictions - equivalencedf$actualvalues)^2)) %>%
  mutate(model3RMSE = sqrt((equivalencedf$mfqfhcasepredictions - equivalencedf$actualvalues)^2)) %>%
  mutate(model4RMSE = sqrt((equivalencedf$familyhistoryonlypredictions - equivalencedf$actualvalues)^2))

#calculate the mean and sd of the RMSEs across participants for each of the models
nullmean <- mean(equivalencedf$nullRMSE)
nullsd <- sd(equivalencedf$nullRMSE)
model1mean <- mean(equivalencedf$model1RMSE)
model1sd <- sd(equivalencedf$model1RMSE)
model2mean <- mean(equivalencedf$model2RMSE)
model2sd <- sd(equivalencedf$model2RMSE)
model3mean <- mean(equivalencedf$model3RMSE)
model3sd <- sd(equivalencedf$model3RMSE)
model4mean <- mean(equivalencedf$model4RMSE)
model4sd <- sd(equivalencedf$model4RMSE)

#compare all of the different pairs of models using toster
nullvs1<- TOSTtwo(m1 = nullmean, m2 = model1mean, sd1 = nullsd, sd2 = model1sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
nullvs2<- TOSTtwo(m1 = nullmean, m2 = model2mean, sd1 = nullsd, sd2 = model2sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
nullvs3<- TOSTtwo(m1 = nullmean, m2 = model3mean, sd1 = nullsd, sd2 = model3sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
nullvs4<- TOSTtwo(m1 = nullmean, m2 = model4mean, sd1 = nullsd, sd2 = model4sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
onevs2<- TOSTtwo(m1 = model1mean, m2 = model2mean, sd1 = model1sd, sd2 = model2sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
onevs3<- TOSTtwo(m1 = model1mean, m2 = model3mean, sd1 = model1sd, sd2 = model3sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
onevs4<- TOSTtwo(m1 = model1mean, m2 = model4mean, sd1 = model1sd, sd2 = model4sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
twovs3<- TOSTtwo(m1 = model2mean, m2 = model3mean, sd1 = model2sd, sd2 = model3sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
twovs4<- TOSTtwo(m1 = model2mean, m2 = model4mean, sd1 = model2sd, sd2 = model4sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)
threevs4<- TOSTtwo(m1 = model3mean, m2 = model4mean, sd1 = model3sd, sd2 = model4sd, n1 = 73, n2 = 73, low_eqbound_d = -2, high_eqbound_d = 2, alpha = 0.05, var.equal = FALSE, plot = FALSE)











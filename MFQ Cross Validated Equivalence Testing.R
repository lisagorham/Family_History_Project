#Equivalence Testing for Cross Validated MFQ Models

rm(list = ls())
#install.packages("TOSTER")
library(TOSTER)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)

#first, load in the outputs of the cross validation 
equivalencedf2 <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/PUTNAMEINHERE.xlsx")

#NOTE: here are the models:
#Null model: lme(s_mfq_tot ~ antidepressants + TimeBetween + MFQtminus1 + InpatientDuring + Age_at_visit + SEX  + OtherMeds + postpandemic, random = (~TimeBetween | Initials)
#Family History Model: lme(s_mfq_tot ~ dep_immed*TimeBetween + MFQtminus1 + InpatientDuring + Age_at_visit + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | Initials) 
#Family History 2 Model: lme(s_mfq_tot ~ dep_immed*TimeBetween + dep_immed*Time2 + MFQtminus1 + InpatientDuring + Age_at_visit + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | Initials)

#make sure everything is numeric like it should be
equivalencedf2$actualvalues <- as.numeric(equivalencedf2$actualvalues)
equivalencedf2$nullmodelpredictions <- as.numeric(equivalencedf2$nullmodelpredictions)
equivalencedf2$fhmodelpredictions <- as.numeric(equivalencedf2$fhmodelpredictions)
equivalencedf2$fhmodel2predictions <- as.numeric(equivalencedf2$fhmodel2predictions)

#calculate the RMSE for each model 
equivalencedf2 <- equivalencedf2 %>% mutate(nullRMSE = sqrt((equivalencedf2$nullmodelpredictions - equivalencedf2$actualvalues)^2)) %>% 
  mutate(fhmodelRMSE = sqrt((equivalencedf2$fhmodelpredictions - equivalencedf2$actualvalues)^2)) %>%
  mutate(fhmodel2RMSE = sqrt((equivalencedf2$fhmodel2predictions - equivalencedf2$actualvalues)^2)) 

#calculate the mean RMSE for each person, since each person has a variable number of pairs of scores
nullmeansperperson <- equivalencedf2 %>% group_by(sdans) %>% summarise_at(vars(nullRMSE), list(name = mean))
fhmodelmeansperperson <- equivalencedf2 %>% group_by(sdans) %>% summarise_at(vars(fhmodelRMSE), list(name = mean))
fhmodel2meansperperson <- equivalencedf2 %>% group_by(sdans) %>% summarise_at(vars(fhmodel2RMSE), list(name = mean))

#calculate the mean RMSE for each model across all people 
nulltotalmean <- mean(nullmeansperperson$name)
nulltotalsd <- sd(nullmeansperperson$name)
fhmodeltotalmean <- mean(fhmodelmeansperperson$name)
fhmodeltotalsd <- sd(fhmodelmeansperperson$name)
fhmodel2totalmean <- mean(fhmodel2meansperperson$name)
fhmodel2totalsd <- sd(fhmodel2meansperperson$name)

#compare the models using toster 
nullvsfhmodel<- TOSTtwo(m1 = nulltotalmean, m2 = fhmodeltotalmean, sd1 = nulltotalsd, sd2 = fhmodeltotalsd, n1 = 130, n2 = 130, low_eqbound_d = -7, high_eqbound_d = 7, alpha = 0.05, var.equal = FALSE, plot = FALSE)
nullvsfhmodel2<- TOSTtwo(m1 = nulltotalmean, m2 = fhmodel2totalmean, sd1 = nulltotalsd, sd2 = fhmodel2totalsd, n1 = 130, n2 = 130, low_eqbound_d = -7, high_eqbound_d = 7, alpha = 0.05, var.equal = FALSE, plot = FALSE)
fhmodelvsfhmodel2<- TOSTtwo(m1 = fhmodeltotalmean, m2 = fhmodel2totalmean, sd1 = fhmodeltotalsd, sd2 = fhmodel2totalsd, n1 = 130, n2 = 130, low_eqbound_d = -7, high_eqbound_d = 7, alpha = 0.05, var.equal = FALSE, plot = FALSE)










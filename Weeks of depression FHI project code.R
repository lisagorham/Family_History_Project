#Making a database for test #1 which looks at weeks of depression 

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

# Load master database
fhi_database <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/CompleteClinicalFamHX.xlsx")

#load behavioral database
behavioral_database <- read_xlsx("W:/string-mbd/Database/Master Psychometric Database/MASTER_DATABASE_BEHAVIOURAL.xlsx")
taskdata <- behavioral_database %>% select(Initials, SDAN, PLUSID, Clinical_Visit_Date, Clinical_Visit_Type, Task_Name, Task_Number, Task_Date, Task_Visit_Type)

taskonerow <- taskdata %>%  group_by(Initials, Task_Visit_Type) %>% slice(n=1)

#merging into one dataset

weeksfhidataset <- merge(x=fhi_database, y=taskonerow, by= c("Initials", "SDAN", "PLUSID", "Clinical_Visit_Date", "Clinical_Visit_Type"))
weeksfhidataset$Task_Name <- NULL
weeksfhidataset$Task_Number <- NULL

#fixing order of columns

weeksfhidataset <- weeksfhidataset[, c(1,2,3,12,13,14,4,5,27,28,24,25,26,6, 7,8,9,10,11,15,16,17,18,19,20,21,22,23,29)]

#filtering for just v1 and v4
filtered <- weeksfhidataset %>% drop_na(Task_Visit_Type)

#write_xlsx(filtered, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/LookingAtVDescriptives.xlsx")

#creating a dataset for analyzing frequency of weeks of depression
weeksdataset <- filtered %>% drop_na(c_ksadsdx_epset_annual_weeks_mdd)




justv1v4 <- subset(filtered, Task_Visit_Type == "v1" | Task_Visit_Type == "v4")

write_xlsx(justv1v4, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/WeeksFHIDataset.xlsx")


#now we want to create a doc that has one row per person- one year follow up data and baseline data on the same row 
justv1data <- subset(justv1v4, Task_Visit_Type == "v1")
justv4data <- subset(justv1v4, Task_Visit_Type == "v4")

justv1MFQonly <- justv1data %>% select(Initials, SDAN, s_mfq_date, s_mfq_tot)
#library(tidyverse)
namesfixed <- justv1MFQonly %>% rename("BaselineMFQScore" = "s_mfq_tot", "BaselineMFQDate" = "s_mfq_date")


combined <- merge(justv4data, namesfixed, by= c("Initials", "SDAN"))

#making a dataset that only has people with a family history interview, a value for weeks of depression

dataset <- combined %>% drop_na(c_ksadsdx_epset_annual_weeks_mdd)

baselinemeds <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/BaselineMedData.xlsx")
followupmeds <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/1YRFUMedData.xlsx")

namesfixed2 <- baselinemeds %>% rename("BaselineAntiDep" = "antidepressants", "BaselineOtherMeds" = "OtherMeds")
namesfixed3 <- followupmeds %>% rename("FUAntiDep" = "antidepressants", "FUOtherMeds" = "OtherMeds") 
getridofextra <- namesfixed3 %>% select(SDAN, FUAntiDep, FUOtherMeds)
medsdatabase <- merge(namesfixed2, getridofextra, by = "SDAN")

inpatientdatabase <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/InpatientData.xlsx")

datasetwithmeds <- merge(x=dataset, y=medsdatabase, by= "SDAN", all.x = TRUE)

datasetwithmedsandinpatient <- merge(datasetwithmeds, inpatientdatabase, by= "Initials")

#if you split that further to only get people who have MFQs and a family history

dataset2 <- datasetwithmedsandinpatient %>% drop_na(BaselineMFQScore) %>% drop_na(s_case__neg_tot) %>% drop_na(dep_immed) %>% drop_na(BaselineAntiDep)

datasetmdd <- subset(dataset2, Participant_Type2 == "MDD")

dataset3 <- datasetmdd %>% select(Initials, SDAN, Participant_Type2, SEX, Age_at_visit, Inpatient, BaselineAntiDep, BaselineOtherMeds, FUAntiDep, FUOtherMeds, dep_immed, BaselineMFQScore, s_case__neg_tot, c_ksadsdx_epset_annual_weeks_mdd)

write_xlsx(dataset3, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/DatasetforTest1.xlsx")

#making the boxplots 

#boxplot(BaselineMFQScore ~ dep_immed, data = dataset2, main= "Baseline MFQ Score as a Function of Family History", xlab = "Do they have an immediate family member with depression?", ylab= "MFQ Score at Baseline", col= (c("light blue", "pink")), names = c("No", "Yes"))
#boxplot(BaselineMFQScore ~ dep_immed*Participant_Type2, data = dataset2, main= "Baseline MFQ Score as a Function of Family History", xlab = "Do they have an immediate family member with depression?", ylab= "MFQ Score at Baseline", col= (c("light blue", "pink")), names = c("HV/No", "HV/Yes", "MDD/No", "MDD/Yes"))
#boxplot(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ dep_immed, data = dataset2, main= "Weeks of Depression as a Function of Family History", xlab = "Do they have an immediate family member with depression?", ylab= "Weeks of Depression Between Baseline and 1 Year FU", col= (c("light blue", "pink")), names = c("No", "Yes"))
#boxplot(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ dep_immed*Participant_Type2, data = dataset2, main= "Weeks of Depression as a Function of Family History", xlab = "Do they have an immediate family member with depression?", ylab= "Weeks of Depression Between Baseline and 1 Year FU", col= (c("light blue", "pink")), names = c("HV/No", "HV/Yes", "MDD/No", "MDD/Yes"))

trythis <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(trythis)

question <- lm(BaselineMFQScore ~ dep_immed, data = dataset3)
summary(question)
plot(question)
boxplot(BaselineMFQScore ~ dep_immed, data = dataset3)

#doing the linear regressions

truenull <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(truenull)
truenull
truenullpredictions <- predict.lm(truenull)

nullwithbaselinemfq <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(nullwithbaselinemfq)
nullwithbaselinemfqpredictions <- predict.lm(nullwithbaselinemfq)
#plot(model1)

baselinemfqplusfh <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(baselinemfqplusfh)
baselinemfqplusfhpredictions <- predict.lm(baselinemfqplusfh)
#plot(model2)

mfqfhcase <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + (dep_immed*s_case__neg_tot) + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(mfqfhcase)
#plot(model3)
mfqfhcasepredictions <- predict.lm(mfqfhcase)

familyhistoryonly <- lm(c_ksadsdx_epset_annual_weeks_mdd  ~ dep_immed + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient + SEX + Age_at_visit, data = dataset3)
summary(familyhistoryonly)
familyhistoryonlypredictions <- predict.lm(familyhistoryonly)


actualvalues <- dataset3$c_ksadsdx_epset_annual_weeks_mdd
sdans <- dataset3$SDAN

equivalencedf <- cbind(data.frame(sdans), data.frame(actualvalues), data.frame(truenullpredictions), data.frame(nullwithbaselinemfqpredictions), data.frame(baselinemfqplusfhpredictions), data.frame(mfqfhcasepredictions), data.frame(familyhistoryonlypredictions))

write_xlsx(equivalencedf, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/Equivalence Dataframe.xlsx")

#anova(model1, model2)

#model4 <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + (dep_immed*s_case__neg_tot), data = dataset2)
#summary(model4)

#calculating percent female

female <- dataset3 %>% filter(SEX == "FEMALE")
male <- dataset3 %>% filter(SEX == "MALE")

#calculating mean age
age <- mean(dataset3$Age_at_visit)
age
sd <- sd(dataset3$Age_at_visit)
sd

#calculating percent with a family history
famhx <- dataset3 %>% filter(dep_immed == 1)
nofamhx <- dataset3 %>% filter(dep_immed == 0)


#make graph for poster
boxplot(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ dep_immed, data = dataset3, main= "Weeks of Depression as a Function of Family History", xlab = "Do they have an immediate family member with a history of depression?", ylab= "Weeks of Depression Between Baseline and 1 Year FU", col= (c("light blue", "pink")), names = c("No", "Yes"))
hist(as.numeric(dataset3$c_ksadsdx_epset_annual_weeks_mdd), breaks = c(0,1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))
p <- ggplot(data = dataset3, aes(x = BaselineMFQScore, y = as.numeric(c_ksadsdx_epset_annual_weeks_mdd), group= dep_immed, color = dep_immed))
p + geom_line()

copy <- ifelse(dataset3$dep_immed == 0, "No", "Yes")
dataset3$rewritten <- copy

ggplot(dataset3, aes(x=BaselineMFQScore, y=as.numeric(c_ksadsdx_epset_annual_weeks_mdd), color=rewritten)) + geom_point(size=3) + labs(y= "Weeks of Depression", x= "Baseline MFQ Score", title = "Weeks of Depression as a Function of Family History", color = "Do they have an immediate family \nmember with a history of MDD?") + geom_smooth(method="lm", se=TRUE) + theme(legend.position="bottom")+ theme(plot.title = element_text(hjust=.5))


#looking at just the MDD kids


#model5 <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Age_at_visit + SEX + Inpatient, data = dataset3)
#summary(model5)
#plot(model5)
#anova(model5)

#model6 <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + dep_immed + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Inpatient + Age_at_visit + SEX, data = dataset3)
#summary(model6)
#plot(model6)
#anova(model6)

#model7 <- lm(c_ksadsdx_epset_annual_weeks_mdd ~ dep_immed + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Age_at_visit + SEX + Inpatient, data = dataset3)
#summary(model7)

#anova(model5, model6)

#trying robust linear model for just the MDD kids

#library(MASS)

#model8 <- polr(as.factor(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore, data= dataset3, Hess = TRUE)
#summary(model8)

#model9 <- polr(as.factor(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + dep_immed, data = dataset3, Hess = TRUE)
#summary(model9)

#model10 <- polr(as.factor(c_ksadsdx_epset_annual_weeks_mdd) ~ dep_immed, data= dataset3, Hess = TRUE)
#summary(model10)

#trying glm for just the MDD kids

#model11 <- glm(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Inpatient+ Age_at_visit + SEX, family= "poisson", data = dataset3)
#summary(model11)
#plot(model11)

#model12 <- glm(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Inpatient + Age_at_visit + SEX + dep_immed, family= "poisson", data = dataset3)
#summary(model12)
#plot(model12)


#model13 <- glm(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Inpatient + Age_at_visit + SEX + dep_immed, family= "poisson", data = dataset3)
#summary(model13)


#loading caret

#install.packages("caret")
#install.packages("rlang")


library(rlang)
library(caret)

#trying out using caret for cross validation

set.seed(23)
truenull <- train(
  as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ SEX + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Age_at_visit + Inpatient,
  dataset3,
  method = "lm",
  trControl= trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

print(truenull)
truenull$resample




set.seed(23)
model1 <- train(
  as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + SEX + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Age_at_visit + Inpatient,
  dataset3,
  method = "lm",
  trControl= trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

print(model1)
model1$resample

set.seed(23)
model2 <- train(
  as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + dep_immed + SEX + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient+ Age_at_visit,
  dataset3,
  method = "lm",
  trControl= trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

model2$resample
print(model2)

set.seed(23)
model3 <- train(
  as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + (dep_immed*s_case__neg_tot) + SEX + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Inpatient+ Age_at_visit,
  dataset3,
  method = "lm",
  trControl= trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

model3$resample
print(model3)


set.seed(23)
fhonly <- train(
  as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ dep_immed + SEX + BaselineAntiDep + BaselineOtherMeds + FUAntiDep + FUOtherMeds + Age_at_visit + Inpatient,
  dataset3,
  method = "lm",
  trControl= trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

print(fhonly)






# 
# caretstats <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/caretstats.xlsx")
# 
# group_by(caretstats, model) %>% summarise(count = n(), mean = mean(rmse, na.rm = TRUE))
# group1 <- subset(caretstats, model == 1, rmse, drop = TRUE)
# group2 <- subset(caretstats, model == 2, rmse, drop = TRUE)
# group3 <- subset(caretstats, model == 3, rmse, drop = TRUE)
# 
# results <- t.test(group1, group3, paired = TRUE)
# results
# 
# 
# res <- t.test(rmse ~ model, data = caretstats, paired = TRUE)
# res
# 
# 
# 
# compare_models(model1, model2)
# 
# doesthiswork$results
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #trying bootstrapping
# 
# set.seed(42)
# jan15test <- train(
#   as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Age_at_visit, 
#   dataset3,
#   method = "lm",
#   trControl= trainControl(
#     method = "boot",
#     number = 10,
#     verboseIter = TRUE
#   )
# )
# 
# print(jan15test)
# doesthiswork$resample
# 
# set.seed(42)
# jan15test2 <- train(
#   as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + dep_immed + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Age_at_visit, 
#   dataset3,
#   method = "lm",
#   trControl= trainControl(
#     method = "boot",
#     number = 10,
#     verboseIter = TRUE
#   )
# )
# 
# 
# print(jan15test2)
# 
# 
# model_list <- list(jan15test, jan15test2)
# res <- resamples(model_list)
# summary(res)
# compare_models(jan15test, jan15test2)
# 
# #modifying Neda's bootstrapping code
# 
# d <- dataset3
# k1 <- dataset3[1:14, ]
# k2<- dataset3[15:28, ]
# k3<- dataset3[29:42, ]
# k4<- dataset3[43:57, ]
# k5<- dataset3[58:72, ]
# 
# d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#  fit1=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[1:57,])
#     #predict y for the left out data
#     predicted=predict(fit1, newdata=d[58:72,])
#     actual=d[58:72,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss1=abs(predicted - actual)
#     rss1
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold2=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[15:72,])
#     #predict y for the left out data
#     predicted=predict(fit1fold2, newdata=d[1:14,])
#     actual=d[1:14,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss2=abs(predicted - actual)
#     rss2
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold3=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[c(1:42, 58:72),])
#     #predict y for the left out data
#     predicted=predict(fit1fold3, newdata=d[43:57,])
#     actual=d[43:57,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss3=abs(predicted - actual)
#     rss3
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold4=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[c(1:14, 29:72),])
#     #predict y for the left out data
#     predicted=predict(fit1fold4, newdata=d[15:28,])
#     actual=d[15:28,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss4=abs(predicted - actual)
#     rss4
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold5=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[c(1:28, 43:72),])
#     #predict y for the left out data
#     predicted=predict(fit1fold5, newdata=d[29:42,])
#     actual=d[29:42,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss5=abs(predicted - actual)
#     rss5
#     
#     totalformodel1 <- c(rss1, rss2, rss3, rss4, rss5)
#     
#     totalformodel1
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + dep_immed + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[1:57,])
#     #predict y for the left out data
#     predicted=predict(fit1, newdata=d[58:72,])
#     actual=d[58:72,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss1a=abs(predicted - actual)
#     rss1a
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold2=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + dep_immed+ SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[15:72,])
#     #predict y for the left out data
#     predicted=predict(fit1fold2, newdata=d[1:14,])
#     actual=d[1:14,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss2a=abs(predicted - actual)
#     rss2a
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold3=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore+ dep_immed + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[c(1:42, 58:72),])
#     #predict y for the left out data
#     predicted=predict(fit1fold3, newdata=d[43:57,])
#     actual=d[43:57,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss3a=abs(predicted - actual)
#     rss3a
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold4=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore+ dep_immed + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[c(1:14, 29:72),])
#     #predict y for the left out data
#     predicted=predict(fit1fold4, newdata=d[15:28,])
#     actual=d[15:28,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss4a=abs(predicted - actual)
#     rss4a
#     
#     d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
#     fit1fold5=lm(c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + dep_immed+ SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU, data=d[c(1:28, 43:72),])
#     #predict y for the left out data
#     predicted=predict(fit1fold5, newdata=d[29:42,])
#     actual=d[29:42,]$c_ksadsdx_epset_annual_weeks_mdd
#     actual
#     #actual=actual[[1]] #since it's tibble, to extract a single column
#     rss5a=abs(predicted - actual)
#     rss5a
#     
#     totalformodel2 <- c(rss1a, rss2a, rss3a, rss4a, rss5a)
#     
#     totalformodel2
#     
#     totalformodel1
#     
#     #rss is really absolute value of the residuals 
#     
#     t.test(totalformodel1-totalformodel2)
#     
#     mean(totalformodel1-totalformodel2)
#     
#     #calculate rmse
#     rmseOnLeftOutData=sqrt(mean((predicted-actual)^2))
#     
#     #calculate r-square
#     u <- if (attr(fit1$terms, "intercept"))
#       mean(actual) else 0
#     num=sum((actual - predicted)^2)
#     den=sum((actual - u)^2)
#     R2=1-(num/den)
#     
#     #calculate adjusted r-square
#     df.int <- if (attr(fit1$terms, "intercept")) 1L else 0L
#     rdf <- fit1$df.residual
#     n=dim(d)[1]
#     adjR2 <- 1 - (1 - R2) * ((n - df.int)/rdf)
#     
#     theta.star[i,] <- c(coef(fit1), rmseOnLeftOutData, R2, adjR2)
#   }
#   
#   colnames(theta.star)=c(names(coef(lm1)), "rmse","r-square","adj-r-square")
#   return(theta.star)
# }
# 
# 
# #comparing model1 and model2
# compareModels=function(model1, model2,parameterToCompare){
#   
#   paramdiff=model1[,parameterToCompare]-model2[,parameterToCompare]
#   #get confidence intervals using percentile method, 
#   #we might want to try bias-corrected accelerated (BCa) method for estimating confidence interval which is better if the data is skewed
#   paramci=quantile(paramdiff, probs = c(.025, .975)) #get 95% confidence interval
#   #uncomment if you want to see the histograms
#   # print(hist(paramdiff, breaks=100))
#   # abline(v=paramci[[1]], col="blue")
#   # abline(v=paramci[[2]], col="blue")
#   # abline(v=mean(paramdiff),col="red")
#   
#   #for calculating p-value I used the code from the following site, I'll look more into this
#   #http://www.stat.umn.edu/geyer/old/5601/examp/tests.html
#   ltpv= mean(paramdiff <= 0)
#   pvalue=2 * min(ltpv, 1 - ltpv)
#   
#   print(paste("***", parameterToCompare, " comparision - test whether model 1",  parameterToCompare, "minus model 2", parameterToCompare, "is different than zero ***"))
#   print(paste(parameterToCompare, "of first model:", sprintf("%.3f",mean(model1[,parameterToCompare])), ", second model: ", sprintf("%.3f",mean(model2[,parameterToCompare]))))
#   print(paste("mean of difference","se","2.5th percentile","97.5th percentile","p-value", sep=","))
#   print(sprintf("%.3f %.3f %.3f %.3f %.3f", mean(paramdiff), sd(paramdiff), paramci[[1]],paramci[[2]], pvalue))
# }
# 
# #check whether model parameters are significantly different than zero
# printSummaryStats=function(model1){
#   numberOfparameteters=(dim(model1)[2])
#   summarystats=matrix(nrow=numberOfparameteters, ncol=5)
#   colnames(summarystats)=c("mean","se","2.5th percentile","97.5th percentile","p-value")
#   rownames(summarystats)=colnames(model1)[1:numberOfparameteters]
#   for(i in 1:numberOfparameteters){
#     bmean=mean(model1[,i])
#     bse=sd(model1[,i]) #bootstrap se is the sd of the bootstrap distribution
#     ltpv= mean(model1[,i] <= 0)
#     pvalue=2 * min(ltpv, 1 - ltpv)
#     ci=quantile(model1[,i], probs = c(.025, .975))
#     summarystats[i,]=c(bmean,bse,ci,pvalue)
#     #print(sprintf("%.3f %.3f %.4f %.3f %.3f", bmean, bse, ci[[1]],ci[[2]],pvalue))
#   }
#   print(summarystats)
# }
# 
# #replace with your model, d is your dataframe
# d$c_ksadsdx_epset_annual_weeks_mdd=as.numeric(d$c_ksadsdx_epset_annual_weeks_mdd)
# model1=calculateCoefsAndRMSE(formula=c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore, ks, d) 
# lm1=lm(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore, data=d) #fitting using the original data
# 
# #replace with your model
# model2=calculateCoefsAndRMSE(formula=c_ksadsdx_epset_annual_weeks_mdd ~ BaselineMFQScore + dep_immed, ks, d)
# lm2=lm(as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + dep_immed, data=d) #fitting using the original data
# 
# 
# print(paste("*********** Coefficient estimation ***************"))
# printSummaryStats(model1) #using bootstrap distribution
# print(summary(lm1)) #using original sample, for comparision
# 
# print(paste("*********** Coefficient estimation ***************"))
# printSummaryStats(model2) #using bootstrap distribution
# print(summary(lm2)) #using original sample, for comparision
# 
# #example of comparing model1 and model2 rmse
# compareModels(model1, model2, "rmse")
# 
# #example of comparing model1 and model2 r-squares
# compareModels(model1, model2, "r-square")
# 
# #example of comparing model1 and model2 adjusted r-squares
# compareModels(model1, model2, "adj-r-square")
# 
# summary(model1)
# summary(model2)
# 
# 
# #trying to do cross validation with tidyverse
# 
# install.packages("tidyverse")
# install.packages("broom.mixed")
# install.packages("dotwhisker")
# install.packages("readr")
# install.packages("rstanarm")
# install.packages("tidymodels")
# install.packages("dplyr")
# library(tidyverse)
# library(tidymodels)
# 
# 
# 





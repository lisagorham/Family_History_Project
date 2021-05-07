#setting up for the second statistical question, MFQ score change over time

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

#creating a clean database for analysis
originaldatabase <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/CompleteClinicalFamHx.xlsx")

#fix LYDO problem manually 
row_number <- which((originaldatabase$Initials == "LYDO") & (originaldatabase$Age_at_visit == 15.65))
originaldatabase[row_number, 13] <- "i23"
row_number2 <- which((originaldatabase$Initials == "LYDO") & (originaldatabase$Age_at_visit == 15.67))
originaldatabase[row_number2, 13] <- "i24"
row_number3 <- which((originaldatabase$Initials == "LYDO") & (originaldatabase$Age_at_visit == 15.69))
originaldatabase[row_number3, 13] <- "i25"
row_number4 <- which((originaldatabase$Initials == "LYDO") & (originaldatabase$Age_at_visit == 15.71))
originaldatabase[row_number4, 13] <- "i26"
row_number5 <- which((originaldatabase$Initials == "LYDO") & (originaldatabase$Age_at_visit == 15.73))
originaldatabase[row_number5, 13] <- "i27"
row_number6 <- which((originaldatabase$Initials == "LYDO") & (originaldatabase$Age_at_visit == 15.75))
originaldatabase[row_number6, 13] <- "i28"
row_number7 <- which((originaldatabase$Initials == "LYDO") & (originaldatabase$Age_at_visit == 15.77))
originaldatabase[row_number7, 13] <- "i29"

#adding in inpatient variable
inpatientadd <- originaldatabase %>% select(-c_ksadsdx_clin_name, -c_ksadsdx_date, -c_ksadsdx_epset_annual_weeks_mdd, -c_ksadsdx_visit_type) %>% drop_na(dep_immed)
inpatientdates <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/InpatientDates.xlsx")
inpatientadd2 <- merge(x= inpatientadd, y= inpatientdates, by="Initials", all.x = TRUE)
originaldatabaseclean <- inpatientadd2 %>% drop_na(s_mfq_tot)

#add in normalized date code 

fhi_normalized_first <- originaldatabaseclean %>% group_by(Initials) %>% arrange(as.Date(s_mfq_date)) %>% slice(n=1)
fhi_normalized_first$`Baseline Visit` <- as.Date(fhi_normalized_first$s_mfq_date)
originaldatabaseclean$`Baseline Visit` <- NA 
for (row in 1:nrow(originaldatabaseclean)){
  participant <- originaldatabaseclean$Initials[row]
  ind <- which(fhi_normalized_first$Initials == participant)
  originaldatabaseclean$`Baseline Visit`[row] <- fhi_normalized_first$`Baseline Visit`[ind] 
}
originaldatabaseclean$`Baseline Visit` <- as.Date(originaldatabaseclean$`Baseline Visit`, origin="1970-01-01")
database <- originaldatabaseclean %>% mutate(Date_Normalized = as.Date(s_mfq_date) - as.Date(`Baseline Visit`))

#add in a pandemic variable
pandemic_start_date <- as.Date("2020-03-17")
comparison <- database$Clinical_Visit_Date > pandemic_start_date
database$postpandemic <- comparison

#adding in the medication data
medsdatabase <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/BaselineMedData.xlsx")
databasewithmeds <- merge(x= database, y= medsdatabase, by= "SDAN", all.x = TRUE)

#figuring out discrepancy
#check5 <- smallerdatabase %>% group_by(Initials, Participant_Type2) %>% slice(n=1)
#n_occur <- data.frame(table(check5$Initials))
#kids that switched from HV to MDD are EALE, HLKC, JALS, MNMR

#making the dataset in the right format that I need 
ordered <- databasewithmeds %>% arrange(Initials, Age_at_visit)

#you can add more variables to mutate function so like mutate(s_mfq_tot_tminus1=lag(s_mfq_tot), med1_tminus1=lag(med1), ...), med1 wasn't in the ordered dataset so I didn't include it here
test <- ordered %>% group_by(Initials) %>% mutate(MFQtminus1=lag(s_mfq_tot), Datetminus1 = lag(Date_Normalized), PreviousDate = lag(s_mfq_date))

#checking that it worked
selecting <- test %>% select(Initials, SDAN, Participant_Type2, SEX, dep_immed, postpandemic, Clinical_Visit_Type, Clinical_Visit_Date, Age_at_visit, s_mfq_date, PreviousDate, s_mfq_tot, MFQtminus1, Date_Normalized, Datetminus1, antidepressants, OtherMeds, `Inpatient Start`, `Inpatient End`, `Second Inpatient Start`, `Second Inpatient End` )
intermediate <- selecting %>% mutate(TimeBetween = Date_Normalized - Datetminus1)
intermediate2 <- intermediate %>% select(-Date_Normalized, -Datetminus1)
intermediate3 <- intermediate2 %>% drop_na(MFQtminus1)


#Now I need to add a clean variable that gives us true false variables for inpatient 
#try the witin function within the lubridate package, see: https://rdrr.io/cran/lubridate/man/within-interval.html

intermediate3["InpatientDuring1"] <- FALSE
testing <- (intermediate3$PreviousDate <= intermediate3$`Inpatient End`) & (intermediate3$s_mfq_date >= intermediate3$`Inpatient Start`)
intermediate3$InpatientDuring1 <- testing
intermediate3$InpatientDuring1[is.na(intermediate3$InpatientDuring1)] <- FALSE
intermediate3["InpatientDuring2"] <- FALSE
testing2 <- (intermediate3$PreviousDate <= intermediate3$`Second Inpatient End`) & (intermediate3$s_mfq_date >= intermediate3$`Second Inpatient Start`)
intermediate3$InpatientDuring2 <- testing2
intermediate3$InpatientDuring2[is.na(intermediate3$InpatientDuring2)] <- FALSE
combined <- (intermediate3$InpatientDuring1 == TRUE) | (intermediate3$InpatientDuring2 == TRUE)
intermediate3$InpatientDuring <- combined


#assign final dataset to UseMeDatabase
UseMeDatabase <- intermediate3 %>% select(Initials, SDAN, Participant_Type2, Clinical_Visit_Date, s_mfq_tot, MFQtminus1, antidepressants, OtherMeds, InpatientDuring, postpandemic, Age_at_visit, SEX, dep_immed, TimeBetween)
UseMeDatabase2 <- UseMeDatabase %>% drop_na(antidepressants) 

UseMeDatabase2$SEX <- toupper(UseMeDatabase2$SEX)

write_xlsx(UseMeDatabase2, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/DatabaseforSEM.xlsx")



numberofpeople2 <- UseMeDatabase2 %>% group_by(Initials) %>% slice(n=1)
FinalDatabase <- UseMeDatabase2 %>% filter(Participant_Type2 == "MDD")
finalN <- FinalDatabase %>% group_by(Initials) %>% slice(n=1)
mean(finalN$Age_at_visit)
sd(finalN$Age_at_visit)
femaleonly <- finalN %>% filter(SEX == "FEMALE")
maleonly <- finalN %>% filter(SEX == "MALE")
famhx <- finalN %>% filter(dep_immed == 1)
nofamhx <- finalN %>% filter(dep_immed == 0)

#checking baseline correlation
fixed <- originaldatabaseclean %>% select(Initials, Participant_Type2, Clinical_Visit_Date, Clinical_Visit_Type, s_mfq_date, s_mfq_tot, dep_immed)
baselinedata <- fixed %>% arrange(Initials, as.Date(s_mfq_date))
oneeach <- baselinedata %>% group_by(Initials) %>% slice(n=1)
MDDbaseline <- oneeach %>% filter(Participant_Type2 == "MDD")
HVbaseline <- oneeach %>% filter(Participant_Type2 == "HV")
MDDpositive <- MDDbaseline %>% filter(dep_immed == 1)
MDDnegative <- MDDbaseline %>% filter(dep_immed == 0)
HVpositive <- HVbaseline %>% filter(dep_immed == 1)
HVnegative <- HVbaseline %>% filter(dep_immed == 0)

question <- lm(s_mfq_tot ~ dep_immed*Participant_Type2, data = oneeach)
summary(question)
plot(question)
boxplot(s_mfq_tot ~ dep_immed*Participant_Type2, data = oneeach)

question2 <- lm(s_mfq_tot ~ dep_immed, data = HVbaseline)
summary(question2)
boxplot(s_mfq_tot ~ dep_immed, data = HVbaseline)
plot(question2)

question3 <- lm(s_mfq_tot ~ dep_immed, data=oneeach)
summary(question3)

question4 <- chisq.test(table(oneeach$Participant_Type2, oneeach$dep_immed))
summary(question4)
question4

#building a model 

#goal model is as follows:
#H0: MFQt ~ MFQt-1 + Mdt-1 + Mdt + Mot-1 + Mot + I + At  + Sex + (1 | subject) + (1 | t)

library(lme4)
library(nlme)

#from Dylan
#mfq_t2 ~ fh  * days + mfq_t1 + med_bsl + age + sex + inpatient + pandemic + (days| subject)

nullmodel <- lme(s_mfq_tot ~ antidepressants + TimeBetween + MFQtminus1 + InpatientDuring + Age_at_visit + SEX  + OtherMeds + postpandemic, random = (~TimeBetween | Initials), data = FinalDatabase)
summary(nullmodel)
anova(nullmodel, type = "marginal")
plot(nullmodel)

nullmodelpredictions <- predict(nullmodel)

fhmodel <- lme(s_mfq_tot ~ dep_immed*TimeBetween + MFQtminus1 + InpatientDuring + Age_at_visit + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | Initials), data = FinalDatabase)
summary(fhmodel)
anova(fhmodel, type = "marginal")
plot(fhmodel)

fhmodelpredictions <- predict(fhmodel)

as.numeric(FinalDatabase$TimeBetween)
values <- as.numeric(FinalDatabase$TimeBetween)
values2 <- values^2
FinalDatabase$Time2 <- values2

fhmodel2 <- lme(s_mfq_tot ~ dep_immed*TimeBetween + dep_immed*Time2 + MFQtminus1 + InpatientDuring + Age_at_visit + SEX + antidepressants + OtherMeds + postpandemic, random = (~TimeBetween | Initials), data = FinalDatabase)
summary(fhmodel2)
anova(fhmodel2, type = "marginal")
plot(fhmodel2)

fhmodel2predictions <- predict(fhmodel2)

actualvalues <- FinalDatabase$s_mfq_tot
sdans <- FinalDatabase$SDAN

equivalencedf2 <- cbind(data.frame(sdans), data.frame(actualvalues), data.frame(nullmodelpredictions), data.frame(fhmodelpredictions), data.frame(fhmodel2predictions))

write_xlsx(equivalencedf2, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/Equivalence Dataframe2.xlsx")










#trying cross validation

library(rlang)
library(caret)

#trying out using caret for cross validation

#https://rstudio-pubs-static.s3.amazonaws.com/241926_6490553c521743529a1d64607be948c3.html

set.seed(23)
model3 <- train(
  s_mfq_tot ~ MFQtminus1 + InpatientDuring + Age_at_visit + SEX + TimeBetween, random = (~1|Initials), 
  UseMeDatabase,
  method = "lme",
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
  as.numeric(c_ksadsdx_epset_annual_weeks_mdd) ~ BaselineMFQScore + dep_immed + SEX + AntiDepBaseline + OtherMedBaseline + AntiDepFU + OtherMedFU + Inpatient+ Age_at_visit, 
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


caretstats <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/caretstats.xlsx")

group_by(caretstats, model) %>% summarise(count = n(), mean = mean(rmse, na.rm = TRUE))
group1 <- subset(caretstats, model == 1, rmse, drop = TRUE)
group2 <- subset(caretstats, model == 2, rmse, drop = TRUE)

results <- t.test(group1, group2, paired = TRUE)
results


res <- t.test(rmse ~ model, data = caretstats, paired = TRUE)
res



compare_models(model1, model2)

doesthiswork$results
























#code from Neda

model3 = lme(s_mfq_tot ~ MFQtminus1 + InpatientDuring + Age_at_visit + SEX + TimeBetween, random = ~1|Initials, data = MDDonly, method = "ML")
newdat.lme = data.frame(MFQtminus1 = MDDonly$MFQtminus1, InpatientDuring = MDDonly$InpatientDuring, Age_at_visit = MDDonly$Age_at_visit, SEX = MDDonly$SEX, TimeBetween = MDDonly$TimeBetween, Participant_Type2 = MDDonly$Participant_Type2)
newdat.lme$predlme = predict(model3, newdata = newdat.lme, level = 0)
fixef(model3)
ranef(model3)
head(newdat.lme$predlme)
head(MDDonly)
head(newdat.lme)

ggplot(UseMeDatabase, aes(x = MFQtminus1, y = s_mfq_tot, color = Participant_Type2) ) +
  #geom_rug(sides = "b", size = 1) +
  #geom_point(data=mfqData4, aes(x=numberOfDaysSincePandemic, y=s_mfq_tot, group=SDAN, color=Participant_Type2 ))+
  geom_point(data = UseMeDatabase , aes(x=MFQtminus1, y=s_mfq_tot, group=SDAN, color=Participant_Type2)) +
  geom_point(data = newdat.lme, aes(y = predlme), size = 1, color = "green")+
  labs(x="Previous MFQ", y="Self MFQ") +
  scale_color_manual(values=c("#CC0000", "#0000CC")) +
  theme_minimal() +
  guides(color=guide_legend("Diagnosis:"))


summary(model3)

model4 = lme(s_mfq_tot ~ MFQtminus1 + InpatientDuring + Age_at_visit + SEX + TimeBetween + dep_immed, random = ~1|Initials, data = MDDonly, method = "ML")
newdat.lme2 = data.frame(MFQtminus1 = MDDonly$MFQtminus1, InpatientDuring = MDDonly$InpatientDuring, Age_at_visit = MDDonly$Age_at_visit, SEX = MDDonly$SEX, TimeBetween = MDDonly$TimeBetween, Participant_Type2 = MDDonly$Participant_Type2, dep_immed = MDDonly$dep_immed)
newdat.lme2$predlme = predict(model4, newdata = newdat.lme2, level = 0)
fixef(model4)
ranef(model4)
head(newdat.lme2$predlme)
head(MDDonly)
head(newdat.lme2)

summary(model4)

anova(model3, model4)



#+ antidepressants+ AntiDeptminus1 + OtherMeds+ OtherMedstminus1 




#read into lme 
#try cross validation
















#trying to do equivalence testing

rm(list = ls())
#install.packages("TOSTER")
library(TOSTER)
library(dplyr)
library(tidyverse)


equivalencedf <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/Equivalence Dataframe.xlsx")

equivalencedf$actualvalues <- as.numeric(equivalencedf$actualvalues)
equivalencedf$truenullpredictions <- as.numeric(equivalencedf$truenullpredictions)
equivalencedf$nullwithbaselinemfqpredictions <- as.numeric(equivalencedf$nullwithbaselinemfqpredictions)
equivalencedf$baselinemfqplusfhpredictions <- as.numeric(equivalencedf$baselinemfqplusfhpredictions)
equivalencedf$mfqfhcasepredictions <- as.numeric(equivalencedf$mfqfhcasepredictions)
equivalencedf$familyhistoryonlypredictions <- as.numeric(equivalencedf$familyhistoryonlypredictions)


equivalencedf <- equivalencedf %>% mutate(nullRMSE = sqrt((equivalencedf$truenullpredictions - equivalencedf$actualvalues)^2)) %>% 
  mutate(model1RMSE = sqrt((equivalencedf$nullwithbaselinemfqpredictions - equivalencedf$actualvalues)^2)) %>%
  mutate(model2RMSE = sqrt((equivalencedf$baselinemfqplusfhpredictions - equivalencedf$actualvalues)^2)) %>%
  mutate(model3RMSE = sqrt((equivalencedf$mfqfhcasepredictions - equivalencedf$actualvalues)^2)) %>%
  mutate(model4RMSE = sqrt((equivalencedf$familyhistoryonlypredictions - equivalencedf$actualvalues)^2))

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


equivalencedf2 <- read_xlsx("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/Equivalence Dataframe2.xlsx")

equivalencedf2$actualvalues <- as.numeric(equivalencedf2$actualvalues)
equivalencedf2$nullmodelpredictions <- as.numeric(equivalencedf2$nullmodelpredictions)
equivalencedf2$fhmodelpredictions <- as.numeric(equivalencedf2$fhmodelpredictions)
equivalencedf2$fhmodel2predictions <- as.numeric(equivalencedf2$fhmodel2predictions)


equivalencedf2 <- equivalencedf2 %>% mutate(nullRMSE = sqrt((equivalencedf2$nullmodelpredictions - equivalencedf2$actualvalues)^2)) %>% 
  mutate(fhmodelRMSE = sqrt((equivalencedf2$fhmodelpredictions - equivalencedf2$actualvalues)^2)) %>%
  mutate(fhmodel2RMSE = sqrt((equivalencedf2$fhmodel2predictions - equivalencedf2$actualvalues)^2)) 

nullmeansperperson <- equivalencedf2 %>% group_by(sdans) %>% summarise_at(vars(nullRMSE), list(name = mean))
fhmodelmeansperperson <- equivalencedf2 %>% group_by(sdans) %>% summarise_at(vars(fhmodelRMSE), list(name = mean))
fhmodel2meansperperson <- equivalencedf2 %>% group_by(sdans) %>% summarise_at(vars(fhmodel2RMSE), list(name = mean))

nulltotalmean <- mean(nullmeansperperson$name)
nulltotalsd <- sd(nullmeansperperson$name)
fhmodeltotalmean <- mean(fhmodelmeansperperson$name)
fhmodeltotalsd <- sd(fhmodelmeansperperson$name)
fhmodel2totalmean <- mean(fhmodel2meansperperson$name)
fhmodel2totalsd <- sd(fhmodel2meansperperson$name)

nullvsfhmodel<- TOSTtwo(m1 = nulltotalmean, m2 = fhmodeltotalmean, sd1 = nulltotalsd, sd2 = fhmodeltotalsd, n1 = 130, n2 = 130, low_eqbound_d = -7, high_eqbound_d = 7, alpha = 0.05, var.equal = FALSE, plot = FALSE)
nullvsfhmodel2<- TOSTtwo(m1 = nulltotalmean, m2 = fhmodel2totalmean, sd1 = nulltotalsd, sd2 = fhmodel2totalsd, n1 = 130, n2 = 130, low_eqbound_d = -7, high_eqbound_d = 7, alpha = 0.05, var.equal = FALSE, plot = FALSE)
fhmodelvsfhmodel2<- TOSTtwo(m1 = fhmodeltotalmean, m2 = fhmodel2totalmean, sd1 = fhmodeltotalsd, sd2 = fhmodel2totalsd, n1 = 130, n2 = 130, low_eqbound_d = -7, high_eqbound_d = 7, alpha = 0.05, var.equal = FALSE, plot = FALSE)










# m1: mean of group 1
# m2: mean of group 2
# sd1: standard deviation of group 1
# sd2: standard deviation of group 2
# n1: sample size in group 1
# n2: sample size in group 2
# low_eqbound_d: lower equivalence bounds (e.g., -0.5) expressed in standardized mean difference (Cohen's d)
# high_eqbound_d: upper equivalence bounds (e.g., 0.5) expressed in standardized mean difference (Cohen's d)
# alpha: alpha level (default = 0.05)
# var.equal: logical variable indicating whether equal variances assumption is assumed to be TRUE or FALSE. Defaults to FALSE.
# plot: set whether results should be plotted (plot = TRUE) or not (plot = FALSE) - defaults to TRUE
# verbose: logical variable indicating whether text output should be generated (verbose = TRUE) or not (verbose = FALSE) - default to TRUE



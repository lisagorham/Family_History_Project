
# Run a Linear Discriminant Analysis for FH Paper Revisions
# Updated 10/1/2021

# Step 1: Load in packages
library(readxl)
library(writexl)
library(tidyverse)
library(utils)
library(splitstackshape)
library(dplyr)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(gapminder)
library(broom)

# Step 2: Load in database
MFQ_data_v1 <- read.csv("./data/FINALMFQDATASETFORLDA.csv")

# Step 3: Run LDA for MFQ

# Load in more packages for LDA
library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)

# Visualize by Immediate Family Depressed
pairs.panels(MFQ_data_v1[3:16],
             gap = 0,
             bg = c("red", "green")[MFQ_data_v1$dep_immed],
             pch = 21, 
             jiggle=TRUE, 
             factor=1,
             main="Depression Data by Family History (MFQ Dataset)")

# Create training and testing datasets for analysis
MFQ_data_lda <- subset(MFQ_data_v1, select = -SDAN)
MFQ_data_lda <- subset(MFQ_data_lda, select = -X)

set.seed(123)
ind <- sample(2, nrow(MFQ_data_lda),
              replace = TRUE,
              prob = c(0.6, 0.4))
training_mfq <- MFQ_data_lda[ind==1,]
testing_mfq <- MFQ_data_lda[ind==2,]

# Run linear discriminant analysis
linear_mfq <- lda(dep_immed~., training_mfq)
linear_mfq

p_mfq <- predict(linear_mfq, training_mfq)
ldahist(data = p_mfq$x[,1], g = training_mfq$dep_immed)

#partimat(dep_immed~., data = training_mfq, method = "lda")
#partimat(dep_immed~., data = training_mfq, method = "qda")

p1_mfq <- predict(linear_mfq, training_mfq)$class
tab_mfq <- table(Predicted = p1_mfq, Actual = training_mfq$dep_immed)
tab_mfq

sum(diag(tab_mfq))/sum(tab_mfq)

p2_mfq <- predict(linear_mfq, testing_mfq)$class
tab1_mfq <- table(Predicted = p2_mfq, Actual = testing_mfq$dep_immed)
tab1_mfq

sum(diag(tab1_mfq))/sum(tab1_mfq)

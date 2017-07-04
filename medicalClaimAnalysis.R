#Libraries
library(caTools)
library("ROCR")

setwd("D:/Code/Analytics/Datasets")
quality <- read.csv("quality.csv")
str(quality)

summary(quality$InpatientDays)
str(quality$InpatientDays)

plot(quality)

plot(quality$OfficeVisits, quality$InpatientDays)

hist(quality$OfficeVisits)
hist(quality$InpatientDays)

#Setting seed for homogenity in split
set.seed(88)
#Logicals for training dataset split
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
split

qualityTrain <- subset(quality, split == TRUE)
summary(qualityTrain)
str(qualityTrain)

qualityTest <- subset(quality, split == FALSE)

nrow(qualityTrain)
nrow(qualityTest)

qualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(qualityLog)

predictTrain <- predict(qualityLog, type = "response")
summary(predictTrain)
table(qualityTrain$PoorCare , predictTrain > 0.5)

#Best results
#Sensitivity -> 0.92
#Specificity -> 0.99
table(qualityTrain$PoorCare , predictTrain > 0.8)

table(qualityTrain$PoorCare , predictTrain > 0.2)

ROCpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCperf)
plot(ROCperf, colorize = TRUE)
plot(ROCperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
plot(ROCperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(0.5,1.2))

predictTest <- predict(qualityLog, type = "response", newdata = qualityTest)
summary(predictTest)

#Calculating AUC
auc.perf <- performance(ROCpred, measure = "auc")
auc.perf

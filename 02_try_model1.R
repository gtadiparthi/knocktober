# Analytics Vidhya Knocktober hackathon
# In this code, we do any preprocessing to the data such as imputations etc.
# Step 0: Clear everything on the workspace and memory

rm(list=ls())
graphics.off()

setwd("/Users/gopalakrishnatadiparthi/Documents/RPrograms/analyticsvidhya/knocktober")

###### Include all the libraries

load("data/all_data.RData")

train = subset(all_data, !is.na(outcome))
test = subset(all_data, is.na(outcome))


##### Try GLM MODEL
library(caTools)
set.seed(188)

table(train$outcome)
split = sample.split(train$outcome, SplitRatio = 0.75)
split

qualityTrain = subset(train, split==TRUE)
qualityTest = subset(train, split==FALSE)

##### First test it on a training data set
outcomeLogistic = glm(outcome ~ Var1 + Var2 + Var3 +Var4 +Var5 +Category1 + Category2 + Category3 ,
                      data= qualityTrain,
                      family= binomial)

summary(outcomeLogistic)

# Look at the performance on the train data
predictLogistic = predict(outcomeLogistic, type="response")
table(qualityTrain$outcome, predictLogistic >= 0.1)


library(ROCR)

ROCRpred = prediction(predictLogistic, qualityTrain$outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# Look at the performance on the quality test data
predictTest = predict(outcomeLogistic, type="response",newdata = qualityTest)
table(qualityTest$outcome, predictTest >= 0.4)
ROCRpred = prediction(predictTest, qualityTest$outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
# 0.7848746

##### Run the same model for all train data

outcomeLog2 = glm(outcome ~ Var1 + Var2 + Var3 +Var4 +Var5 +Category1 + Category2 + Category3 ,
                      data= train,
                      family= binomial)
summary(outcomeLog2)
predictTest2 = predict(outcomeLog2, type="response",newdata = test)

mysubmission = data.frame(Patient_ID = test$Patient_ID,	Health_Camp_ID = test$Health_Camp_ID,	Outcome = predictTest2)

write.csv(mysubmission, "output/sub_log1.csv",row.names = FALSE)

#Leaderboard auc
0.774291

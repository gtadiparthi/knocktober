# Analytics Vidhya Knocktober hackathon
# Use random forest and take care of the class imbalance problem
# Step 0: Clear everything on the workspace and memory

rm(list=ls())
graphics.off()

library(caret)
library(pROC)
setwd("/Users/gopalakrishnatadiparthi/Documents/RPrograms/analyticsvidhya/knocktober")

###### Include all the libraries

load("data/all_data.RData")

train = subset(all_data, !is.na(outcome))
test = subset(all_data, is.na(outcome))
train$outcome = as.factor(train$outcome)
#####

levels(train$outcome) <- make.names(levels(factor(train$outcome)))
table(train$outcome)
nmin <- sum(train$outcome == "X1")
nmin

ctrl <- trainControl(method = "cv", classProbs = TRUE,summaryFunction = twoClassSummary)


set.seed(2)
rfDownsampled <- train(outcome ~ Var1 + Var2 + Var3 +Var4 +Var5 +Category1 + Category2 + Category3
                       , data = train, method = "rf",ntree = 1500,
                         tuneLength = 5,metric = "ROC", trControl = ctrl,
                                                strata = train$outcome, sampsize = rep(nmin,2))
getTrainPerf(rfDownsampled)

set.seed(2)
rfUnbalanced <- train(outcome ~ Var1 + Var2 + Var3 +Var4 +Var5 +Category1 + Category2 + Category3
                      , data = train,
                                             method = "rf",
                                               ntree = 1500,
                                               tuneLength = 5,
                                               metric = "ROC",
                                               trControl = ctrl)


downProbs <- predict(rfDownsampled, train, type = "prob")[,1]
downsampledROC <- roc(response = train$outcome, 
                        predictor = downProbs,
                        levels = rev(levels(train$outcome)))


auc(downsampledROC)

unbalProbs <- predict(rfUnbalanced, train, type = "prob")[,1]
unbalROC <- roc(response = train$outcome, predictor = unbalProbs, levels = rev(levels(train$outcome)))


##### Try Random Forest model
library(randomForest)
set.seed(188)

outcomeRF1 = randomForest(as.factor(outcome) ~ Var1 + Var2 + Var3 +Var4 +Var5 +Category1 + Category2 + Category3 ,
                      data= train,
                      do.trace = 100)
summary(outcomeRF1)

# Look at the performance on the train data
predictRF1 = predict(outcomeRF1, type="prob")
print(table(train$outcome, predictRF1[,2]>=0.3))

library(ROCR)
ROCRpred = prediction(predictRF1[,2], train$outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
###0.77 for train auc

vu = varUsed(outcomeRF1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(outcomeRF1$forest$xlevels[vusorted$ix]))

##### Run the same model for all train data

predictTest2 = predict(outcomeRF1, type="prob",newdata = test)
#Check the values of predictTest2
predictTest2[predictTest2 <0]=0
mysubmission = data.frame(Patient_ID = test$Patient_ID,	Health_Camp_ID = test$Health_Camp_ID,	Outcome = predictTest2[,2])

write.csv(mysubmission, "output/sub_RF1.csv",row.names = FALSE)

#Leaderboard auc
#0.78
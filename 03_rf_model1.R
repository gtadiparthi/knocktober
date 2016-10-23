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
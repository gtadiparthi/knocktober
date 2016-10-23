# Analytics Vidhya Knocktober hackathon
# In this code, we read all the data and prepare it for modeling
# Step 0: Clear everything on the workspace and memory

rm(list=ls())
graphics.off()

setwd("/Users/gopalakrishnatadiparthi/Documents/RPrograms/analyticsvidhya/knocktober")


######## CLEAN UP HEALTH CAMP DATA #############
health_camps <- read.csv("data/Train/Health_Camp_Detail.csv",
                         colClasses=c("character", "character","character","factor","factor","factor"))
health_camps$start <- as.Date(health_camps$Camp_Start_Date,format='%d-%b-%y')
health_camps$end <- as.Date(health_camps$Camp_End_Date,format='%d-%b-%y')
health_camps$duration <- health_camps$end - health_camps$start
health_camps$Camp_Start_Date <- NULL
health_camps$Camp_End_Date <- NULL
str(health_camps)


####### CLEAN UP PATIENT PROFILE #######
#Don't read profile data using colclasses as you have to do a lot of changes after merging to the train and test
#Especially since the data is sparce, we have to have  afactor NA for records that are missing

profile <- read.csv("data/Train/Patient_Profile.csv",colClasses=c("character", "factor", "factor", "factor","factor",
                                                                  "factor","character","character",
                                                                  "character","factor","factor"))
profile[which(profile$Education_Score=="None"),]$Education_Score <- "0"
profile[which(profile$Age=="None"),]$Age <- "0"
table(profile$Age)
table(profile$Education_Score)
profile$Education_Score <- as.numeric(profile$Education_Score)
profile$Age <- as.numeric(profile$Age)
profile[which(profile$Education_Score==0),]$Education_Score <- NA
profile[which(profile$Age==0),]$Age <- NA
str(profile)

#### Read first health camp

first_camp <- read.csv("data/Train/First_Health_Camp_Attended.csv",colClasses=c("character", "character", "numeric", "numeric"))
names(first_camp)[names(first_camp)=="Health_Score"] <- "outcome"
first_camp$X <- NULL
first_camp$Donation <- NULL
str(first_camp)

#### Read SECOND health camp

second_camp <- read.csv("data/Train/Second_Health_Camp_Attended.csv",colClasses=c("character", "character", "numeric"))
names(second_camp)[names(second_camp)=="Health.Score"] <- "outcome"
str(second_camp)


#### Read Third health camp

third_camp <- read.csv("data/Train/Third_Health_Camp_Attended.csv",colClasses=c("character", "character", "numeric","numeric"))
#names(third_camp)[names(third_camp)=="Health.Score"] <- "health2"

third_camp$outcome <- 0
third_camp[which(third_camp$Number_of_stall_visited>0),]$outcome <- 1
third_camp$Number_of_stall_visited<- NULL
third_camp$Last_Stall_Visited_Number<- NULL

str(third_camp)

outcomes<- rbind(first_camp, second_camp, third_camp)

train <- read.csv("data/Train/Train.csv", stringsAsFactors = FALSE)
train$Patient_ID <- as.character(train$Patient_ID)
train$Health_Camp_ID <- as.character(train$Health_Camp_ID)
str(train)

train1 = merge(train, outcomes, by = c("Patient_ID","Health_Camp_ID"),all.x = TRUE)
str(train1)

train1[is.na(train1$outcome),]$outcome <- 0
train1[train1$outcome>0,]$outcome <- 1
table(train1$outcome)

str(train1)
ntrain = nrow(train1)
######### Read the test data set

test <- read.csv("data/Test_D7W1juQ.csv", stringsAsFactors = FALSE)
test$Patient_ID <- as.character(test$Patient_ID)
test$Health_Camp_ID <- as.character(test$Health_Camp_ID)
ntest = nrow(test)
test$outcome <- rep(NA,ntest)
test$outcome <- as.numeric(test$outcome)
str(test)

# Test and train are the same data set.
#Now, it is easy to apply transformations to both test and train
all <- rbind(train1, test)
all$reg_date <- as.Date(all$Registration_Date,format='%d-%b-%y')
#all$Registration_Date <- NULL
all1 <- merge(all, health_camps,by="Health_Camp_ID")
all1$reg_duration <- as.numeric(all1$reg_date - all1$start)
str(all1)
#####

##### Join profile
all_data = merge (all1, profile, by="Patient_ID",all.x = TRUE)

save(all_data,file="data/all_data.RData")

###########
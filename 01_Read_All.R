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
health_camps$duration <- as.numeric(health_camps$end - health_camps$start)
health_camps$start_wkday <- as.factor(weekdays(health_camps$start))
health_camps$Camp_Start_Date <- NULL
health_camps$Camp_End_Date <- NULL
health_camps$end <- NULL
str(health_camps)


####### CLEAN UP PATIENT PROFILE #######
#Don't read profile data using colclasses as you have to do a lot of changes after merging to the train and test
#Especially since the data is sparce, we have to have  afactor NA for records that are missing

profile <- read.csv("data/Train/Patient_Profile.csv",stringsAsFactors = FALSE)
str(profile)
table(profile$Income)
profile[which(profile$Education_Score=="None"),]$Education_Score <- "0"
 profile[which(profile$Age=="None"),]$Age <- "0"
# table(profile$Age)
# table(profile$Education_Score)
 profile$Education_Score <- as.numeric(profile$Education_Score)
 profile$Age <- as.numeric(profile$Age)
 profile[which(profile$Education_Score==0),]$Education_Score <- NA
 profile[which(profile$Age==0),]$Age <- NA
 profile$Online_Follower <- as.factor(profile$Online_Follower)
 profile$LinkedIn_Shared <- as.factor(profile$LinkedIn_Shared)
 profile$Twitter_Shared <- as.factor(profile$Twitter_Shared)
 profile$Facebook_Shared <- as.factor(profile$Facebook_Shared)
 profile$City_Type <- as.factor(profile$City_Type)
 profile$Income <- as.factor(profile$Income)
 profile$Employer_Category<- as.factor(profile$Employer_Category)
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

all1 <- merge(all, health_camps,by="Health_Camp_ID")
all1[which(all1$Registration_Date ==""),]$reg_date = all1[which(all1$Registration_Date ==""),]$start
all1$reg_duration <- as.numeric(all1$reg_date - all1$start)
all1$reg_wkday <- as.factor(weekdays(all1$reg_date))
hist(all1$reg_duration)
all1$start <- NULL
all1$Registration_Date <- NULL
str(all1)
summary(all1)
#####

##### Join profile
all_data = merge (all1, profile, by="Patient_ID",all.x = TRUE)
all_data$first_date <- as.Date(all_data$First_Interaction,format='%d-%b-%y')
all_data[which(all_data$First_Interaction ==""),]$first_date = all_data[which(all_data$First_Interaction ==""),]$reg_date
all_data$first_duration <- as.numeric(all_data$reg_date - all_data$first_date)
hist(all_data$first_duration)
all_data$reg_date <- NULL
all_data$first_date <- NULL
all_data$First_Interaction <- NULL
str(all_data)
names(all_data)
#Imputation using mice
library(mice)
simple = all_data[c("Var1", "Var2","Var3","Var4","Var5","Category1","Category2", "Category3",
                     "duration","start_wkday","reg_duration","reg_wkday",
                    "Online_Follower",   "LinkedIn_Shared",   "Twitter_Shared", "Facebook_Shared",
                    "Income" , "Education_Score",  "Age" ,  "City_Type" ,   "Employer_Category",
                    "first_duration"  )]
summary(simple)
set.seed(144)
imputed = complete(mice(simple, me = c("", "","","","","","", "",
                                       "","","","",
                                       "logreg",   "logreg",   "logreg", "logreg",
                                       "polyreg" , "pmm",  "pmm" ,  "polyreg" ,   "polyreg",
                                       "" )))



#third_camp$Number_of_stall_visited<- NULL
#third_camp$Last_Stall_Visited_Number<- NULL

save(all_data,file="data/all_data.RData")

###########
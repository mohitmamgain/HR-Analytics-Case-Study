#############################HR ANALYTICS CASE STUDY###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:
# HIgh level of attrition makes the company to investigate on
# the probable factors as it impacting effort in the project deliverables and company reputation,
# recruting and training to new staff etc.

## AIM:

# To understand  factors behind attrittion, the company should focus on, in order to curb attrition.

# Data understanding and Preparation
# Set Working Directory
# Loading all the libraries.
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(lubridate)

# Loading 5 files
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F )

# Examine the strucute of data i.e observation and variable
str(general_data)    # 4410 obs of 24 variables with target variable
str(employee_survey_data) # 4410 obs of 4 variables
str(manager_survey_data) # 4410 obs of 3 variables
str(out_time) # 4410 of 262 variable
str(in_time) # 4410 of 262 variable

# changing the name of first column in in_time and out_time as it is not mentioned in dataset.
names(out_time)[1] <- "EmployeeID"
names(in_time)[1] <- "EmployeeID"

# in_time and out_time data is record of 12 months span.
# All the columns are not necessary in both data sets, therefore computing office_hour_duration from both data.

# Remove all columns which have all NA value i.e holidays from in and out data frame
holidays_column <- function(x){
  holiday <- subset(x, select = -c(X2015.01.01, X2015.01.14, X2015.01.26, X2015.03.05, X2015.05.01, X2015.07.17,
                                   X2015.09.17, X2015.10.02, X2015.11.09, X2015.11.10, X2015.11.11, X2015.12.25))
  return(holiday)
}

in_time <- holidays_column(in_time)
out_time <- holidays_column(out_time)

# Converting all the date columns from character to DateTime format
in_time[,2:250] <- lapply(in_time[,2:250], function(x) as_datetime(x))
out_time[,2:250] <- lapply(out_time[,2:250], function(x) as_datetime(x))

# Calculation of the work hours duration for each employee everyday i.e. outtime-intime
work_hours <- out_time[,2:250]- in_time[,2:250]
work_hours[,1:249] <- lapply(work_hours[,1:249], function(work_hours) as.numeric(work_hours))

# Calculaing the avg work hours of each employee
work_hours$avg_work_hours <- rowMeans(work_hours[,1:249], na.rm = TRUE)

# Creating a new dataframe consisting of Employee ID and avg_work_hrs.
office_time_duration <- data.frame(in_time$EmployeeID, work_hours$avg_work_hours)
colnames(office_time_duration) <- c("EmployeeID", "Avg_work_Hrs")
# Rounding off to avg_eork_Hrs to two decimal.
office_time_duration$Avg_work_Hrs <- round(office_time_duration$Avg_work_Hrs, 2)


# Collate the data together in one single file
length(unique(tolower(general_data$EmployeeID)))    # 4410, confirming customerID is key 
length(unique(tolower(employee_survey_data$EmployeeID))) # 4410, confirming customerID is key
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, confirming customerID is key
length(unique(tolower(office_time_duration$EmployeeID))) # 4410, confirming customerID is key


setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EMPLOYEEID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EMPLOYEEID across these datasets
setdiff(general_data$EmployeeID,office_time_duration$EmployeeID) # Identical EMPLOYEEID across these datasets

# Merging all the data frames in one by common column EMPLOYEEID.
hr_analytics <- merge(general_data, employee_survey_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, manager_survey_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, office_time_duration, by= "EmployeeID", all = F)
# hr_analtics dataframe after merging has 4410 obs with 30 variable.


# Creating new derived column "Over_time" with Overtime = 1 indicates yes while 0 = no
hr_analytics$Over_time <- ifelse(hr_analytics$Avg_work_Hrs > 8, 1, 0)

#Creating new derived column less_work_time with YES = 1 and No = 0
hr_analytics$less_work_time <- ifelse(hr_analytics$Avg_work_Hrs < 7, 1, 0)
#Inadiquate time means the employee is working mush less than the required hours on average. 

#creating new derived column leaves. which is caluclated by counting no. of NA's in out_time - in_time
for(i in 1:4410)
{
  hr_analytics$leaves[i]<-sum(is.na(work_hours[i,]))
}

View(hr_analytics)   # master_file
# hr_analytics is master file having 4410 observations with 33 variable.
str(hr_analytics)

# cleaning the individual NA values from the data set

names(which(sapply(hr_analytics, anyNA)))
# NUmcompanyworked, Totalworkinghours, enviornmentsatisfaction, Jobsatisfaction, worklifebalance columns
# having NA values.

# cleaning NA for EnvironmentSatisfaction replace with mean as there is no outlers in it
hr_analytics$EnvironmentSatisfaction[which(is.na(hr_analytics$EnvironmentSatisfaction))]<-mean(hr_analytics$EnvironmentSatisfaction,na.rm = TRUE)
hr_analytics$EnvironmentSatisfaction <- round(hr_analytics$EnvironmentSatisfaction, 0)

# cleaning NA for JobSatisfaction replace with mean as there is no outliers
hr_analytics$JobSatisfaction[which(is.na(hr_analytics$JobSatisfaction))]<-mean(hr_analytics$JobSatisfaction,na.rm = TRUE)
hr_analytics$JobSatisfaction <- round(hr_analytics$JobSatisfaction, 0)

#cleaning NA for WorkLifeBalance replace with mean as there is no outliers
hr_analytics$WorkLifeBalance[which(is.na(hr_analytics$WorkLifeBalance))]<-mean(hr_analytics$WorkLifeBalance,na.rm = TRUE)
hr_analytics$WorkLifeBalance <- round(hr_analytics$WorkLifeBalance, 0)

#cleaning NA for NumCompaniesWorked replace with median as there are outliers
hr_analytics$NumCompaniesWorked[which(is.na(hr_analytics$NumCompaniesWorked))]<-median(hr_analytics$NumCompaniesWorked,na.rm = TRUE)
hr_analytics$NumCompaniesWorked <- round(hr_analytics$NumCompaniesWorked, 0)

#cleaning NA for TotalWorkingYears replace with median as there are outliers
hr_analytics$TotalWorkingYears[which(is.na(hr_analytics$TotalWorkingYears))]<-median(hr_analytics$TotalWorkingYears,na.rm = TRUE)
hr_analytics$TotalWorkingYears <- round(hr_analytics$TotalWorkingYears, 0)

sum(is.na(hr_analytics)) # No NA in whole dataframe.

# converting education column into actual name better for analysis.
hr_analytics$Education[((hr_analytics$Education == 1))] <- c("below_college")
hr_analytics$Education[((hr_analytics$Education == 2))] <- c("college")
hr_analytics$Education[((hr_analytics$Education == 3))] <- c("bachelor")
hr_analytics$Education[((hr_analytics$Education == 4))] <- c("master")
hr_analytics$Education[((hr_analytics$Education == 5))] <- c("doctor")

# converting enviornmental satisfaction column into actual name better for analysis.
hr_analytics$EnvironmentSatisfaction[((hr_analytics$EnvironmentSatisfaction == 1))] <- c("low")
hr_analytics$EnvironmentSatisfaction[((hr_analytics$EnvironmentSatisfaction == 2))] <- c("medium")
hr_analytics$EnvironmentSatisfaction[((hr_analytics$EnvironmentSatisfaction == 3))] <- c("high")
hr_analytics$EnvironmentSatisfaction[((hr_analytics$EnvironmentSatisfaction == 4))] <- c("very)high")

# converting jobinvolvementcolumn into actual name better for analysis.
hr_analytics$JobInvolvement[((hr_analytics$JobInvolvement == 1))] <- c("low")
hr_analytics$JobInvolvement[((hr_analytics$JobInvolvement == 2))] <- c("medium")
hr_analytics$JobInvolvement[((hr_analytics$JobInvolvement == 3))] <- c("high")
hr_analytics$JobInvolvement[((hr_analytics$JobInvolvement == 4))] <- c("very)high")

# converting jobsatisfaction column into actual name better for analysis.
hr_analytics$JobSatisfaction[((hr_analytics$JobSatisfaction == 1))] <- c("low")
hr_analytics$JobSatisfaction[((hr_analytics$JobSatisfaction == 2))] <- c("medium")
hr_analytics$JobSatisfaction[((hr_analytics$JobSatisfaction == 3))] <- c("high")
hr_analytics$JobSatisfaction[((hr_analytics$JobSatisfaction == 4))] <- c("very)high")

# converting performance rating column into actual name better for analysis.
hr_analytics$PerformanceRating[((hr_analytics$PerformanceRating == 1))] <- c("low")
hr_analytics$PerformanceRating[((hr_analytics$PerformanceRating == 2))] <- c("good")
hr_analytics$PerformanceRating[((hr_analytics$PerformanceRating == 3))] <- c("exellent")
hr_analytics$PerformanceRating[((hr_analytics$PerformanceRating == 4))] <- c("outstanding")

# converting worklife balance column into actual name better for analysis.
hr_analytics$WorkLifeBalance[((hr_analytics$WorkLifeBalance == 1))] <- c("bad")
hr_analytics$WorkLifeBalance[((hr_analytics$WorkLifeBalance == 2))] <- c("good")
hr_analytics$WorkLifeBalance[((hr_analytics$WorkLifeBalance == 3))] <- c("better")
hr_analytics$WorkLifeBalance[((hr_analytics$WorkLifeBalance == 4))] <- c("best")

str(hr_analytics)
#------------------------------------------------------------------------------------------------------------
# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

# Plot1 
plot1 <- plot_grid(ggplot(hr_analytics, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1, 
                   ggplot(hr_analytics, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
                   ggplot(hr_analytics, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
                   ggplot(hr_analytics, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
                   ggplot(hr_analytics, aes(x=JobRole,fill=Attrition))+ geom_bar() + bar_theme1,
                   ggplot(hr_analytics, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
                   align = "h")
plot1

plot2 <- plot_grid(ggplot(hr_analytics, aes(x=Education,fill=Attrition))+ geom_bar()+ bar_theme1, 
                   ggplot(hr_analytics, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
                   ggplot(hr_analytics, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
                   ggplot(hr_analytics, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
                   ggplot(hr_analytics, aes(x=JobInvolvement,fill=Attrition))+ geom_bar() + bar_theme1,
                   ggplot(hr_analytics, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
                   align = "h")
plot2

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot3 <- plot_grid(ggplot(hr_analytics, aes(Age))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot3

plot4 <- plot_grid(ggplot(hr_analytics, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot4

plot5 <- plot_grid(ggplot(hr_analytics, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot5

plot6 <- plot_grid(ggplot(hr_analytics, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot6

plot7 <- plot_grid(ggplot(hr_analytics, aes(StockOptionLevel))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot7

plot8 <- plot_grid(ggplot(hr_analytics, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot8

plot9 <- plot_grid(ggplot(hr_analytics, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot9

plot10 <- plot_grid(ggplot(hr_analytics, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
                   ggplot(hr_analytics, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                   align = "v",ncol = 1)
plot10

plot11 <- plot_grid(ggplot(hr_analytics, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
                    ggplot(hr_analytics, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
plot11

plot12 <- plot_grid(ggplot(hr_analytics, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
                    ggplot(hr_analytics, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
plot12

plot13 <- plot_grid(ggplot(hr_analytics, aes(leaves))+ geom_histogram(binwidth = 10),
                    ggplot(hr_analytics, aes(x="",y=leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
plot13
#-----------------------------------------------------------------------------------------------------



# Categorical attributes with 2 levels -- Attrition and gender

#Converting the "Attrition,Gender and Over18" attributes with 2 levels into numbers(0,1)
hr_analytics$Attrition <- ifelse(hr_analytics$Attrition == "Yes", 1,0)
hr_analytics$Gender <- ifelse(hr_analytics$Gender == "Female",1,0)

# Attributes with more than 2 levels:
# EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, JobRole, MaritalStatus,  
# BusinessTravel, Department,Education, EducationField, JobInvolvement, JobLevel, PerformanceRating.

# Creating dataframe of categorical attributes with more than 2 levels
hr_analytics_fact <- hr_analytics[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                                     "BusinessTravel","Department","EducationField", "Education",
                                     "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                                     "PerformanceRating")]

# Converting categorical attributes to factors
hr_analytics_fact <- data.frame(sapply(hr_analytics_fact, function(x) factor(x)))
str(hr_analytics_fact)

# Creating dummy attributes for factor attributes
dummies <- data.frame(sapply(hr_analytics_fact, function(x)
  data.frame(model.matrix(~x-1, data = hr_analytics_fact))[,-1]))

# Removing the categorical attributes and adding the corresponding dummy attributes.
# Also removing column named EmploymentID as it is necessary
# Also removing column named over18, Standard hours and Employment count as it has only one levels
# i.e only Yes for over18, 8 for standard hours, and 1 for Employment count. threfore removing it as
# it will not affect out modeling.
hr_analytics <- cbind(hr_analytics[,-c(1,4,5,7,8,9,11,12,13,16,18,25,26,27,28,29)], dummies)
View(hr_analytics)  # 4410 observations with 57 attributes.

# We are not doing any outliers treatment as it will bound to happen in company.

summary(hr_analytics)
str(hr_analytics)

# Normalising continuous features 
hr_analytics$Age <- scale(hr_analytics$Age) 
hr_analytics$DistanceFromHome <- scale(hr_analytics$DistanceFromHome)
hr_analytics$MonthlyIncome <- scale(hr_analytics$MonthlyIncome)
hr_analytics$NumCompaniesWorked <- scale(hr_analytics$NumCompaniesWorked)
hr_analytics$PercentSalaryHike <- scale(hr_analytics$PercentSalaryHike)
hr_analytics$StockOptionLevel <- scale(hr_analytics$StockOptionLevel)
hr_analytics$TotalWorkingYears <- scale(hr_analytics$TotalWorkingYears)
hr_analytics$TrainingTimesLastYear <- scale(hr_analytics$TrainingTimesLastYear)
hr_analytics$YearsAtCompany <- scale(hr_analytics$YearsAtCompany)
hr_analytics$YearsSinceLastPromotion <- scale(hr_analytics$YearsSinceLastPromotion)
hr_analytics$YearsWithCurrManager <- scale(hr_analytics$YearsWithCurrManager)
hr_analytics$Avg_work_Hrs <- scale(hr_analytics$Avg_work_Hrs)
hr_analytics$leaves <- scale(hr_analytics$leaves)

Attrition <- sum(hr_analytics$Attrition)/nrow(hr_analytics)
Attrition
# 16.12 % Attrition rate.
# splitting the data between train and test
set.seed(100)

indices = sample.split(hr_analytics$Attrition, SplitRatio = 0.7)

train = hr_analytics[indices,]

test = hr_analytics[!(indices),]


########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC = 2102.5

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
sort(vif(model_2))

# Remove Educationfield.xlife.sciences as VIF = 15.50 and p value = 0.153
model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                 WorkLifeBalance.xbetter + WorkLifeBalance.xgood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
               family = "binomial", data = train)
summary(model_3)
sort(vif(model_3))


# Remove YearsAtCompany, VIF =  4.72, p- value = 0.030
model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                 WorkLifeBalance.xbetter + WorkLifeBalance.xgood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
               family = "binomial", data = train)
summary(model_4)
sort(vif(model_4))

# Remove MaritalStatus.xMarried, VIF = 2.15 , p-value = 0.123

model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                 WorkLifeBalance.xbetter + WorkLifeBalance.xgood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
               family = "binomial", data = train)
summary(model_5)
sort(vif(model_5))

# Remove BusinessTravel.xTravel_rarely as it has high VIF = 4.56 but it is significant.
model_6 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                 WorkLifeBalance.xbetter + WorkLifeBalance.xgood + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
               family = "binomial", data = train)
summary(model_6)
sort(vif(model_6))

# LEts remove Department.xsales as it has high VIF value = 4.30 but it is also significant.
# So lets see what it it effect on AIC after removing this variable.
model_7 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                 WorkLifeBalance.xbetter + WorkLifeBalance.xgood + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
               family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))

# LEts remove worklikebalance.xgood, VIF = 3.06, its significant but doesnt have much effect on AIC.
model_8 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                 WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
               family = "binomial", data = train)
summary(model_8)
sort(vif(model_8))

# LEts remove WorkLifeBalance.xbest  p-value = 0.410097 
model_9 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                 WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
               family = "binomial", data = train)
summary(model_9)
sort(vif(model_9))

# LEts remove JobRole.xResearch.Scientist p-value = 0.312266
model_10 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
                family = "binomial", data = train)
summary(model_10)
sort(vif(model_10))

# Remove EducationField.xMedical p-value = 0.269428
model_11 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
                family = "binomial", data = train)
summary(model_11)
sort(vif(model_11))

# Remove DistanceFromHome p-value = 0.14247
model_12 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.xbelow_college + Education.xcollege + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
                family = "binomial", data = train)
summary(model_12)
sort(vif(model_12))

# Remove Education.xbelow_college p-value =  0.14311
model_13 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.xcollege + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
                family = "binomial", data = train)
summary(model_13)
sort(vif(model_13))

# Remove MonthlyIncome p-value =  0.12629 
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.xcollege + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.xlow + JobInvolvement.xvery.high + JobLevel.x2, 
                family = "binomial", data = train)
summary(model_14)
sort(vif(model_14))

# Remove JobInvolvement.xlow p-value = 0.138745  
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.xcollege + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.xvery.high + JobLevel.x2, 
                family = "binomial", data = train)
summary(model_15)
sort(vif(model_15))

# Remove JobInvolvement.xvery.high p-value = 0.15842
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.xcollege + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_16)
sort(vif(model_16))

# Remove Education.xcollege p-value = 0.086773 
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_17)
sort(vif(model_17))

# Remove EducationField.xTechnical.Degree p-value = 0.054621  
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xOther + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_18)
sort(vif(model_18))

# Remove Department.xResearch...Development p-value= 0.045386
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  EducationField.xMarketing + EducationField.xOther + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_19)
sort(vif(model_19))

# Remove EducationField.xMarketing p-value=  0.16393
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  EducationField.xOther + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_20)
sort(vif(model_20))

# Remove EducationField.xOther p-value=  0.03598 *
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_21)
sort(vif(model_21))

# Remove JobRole.xManufacturing.Director p-value=  0.01322 *
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_22)
sort(vif(model_22))

# Remove EnvironmentSatisfaction.xvery.high p-value= 0.040090 * 
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_23)
sort(vif(model_23))

# Remove JobLevel.x2  p-value = 0.009870 **
model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_24)
sort(vif(model_24))

# Remove JobRole.xResearch.Director p-value= 0.003364 ** 
model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_25)
sort(vif(model_25))

# Remove JobRole.xSales.Executive p-value= 0.002961 ** 
model_26 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Over_time + EnvironmentSatisfaction.xlow + 
                  JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                  WorkLifeBalance.xbetter + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_26)
sort(vif(model_26))
# With 16 significant variables in the model

final_model<- model_26

### Model Evaluation

### Test Data ####
# Predicting probability of Attrition 1 for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

test_predict_attrition<-factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Finding the Optimal Probability Cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
dev.off()

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,1,col=c(2,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff

#Optimal P_Cutoff
test_cutoff_attrition<-factor(ifelse(test_pred >= .1616, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#           Accuracy    : 0.7619 
#           Sensitivity : 0.7511          
#           Specificity : 0.7639

### KS -statistic - Test Data ######
test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.5151

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attrition, test_pred, groups = 10)
Churn_decile


plot14 <- ggplot(Churn_decile, aes(x = bucket, y = Cumlift)) + geom_line() + geom_point()+
  labs(x="Decile", y="Cumulative Lift") + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))+
  geom_text(aes(label=round(Cumlift,2)),hjust=-0.2, vjust=-1)
plot14

plot15 <- ggplot(Churn_decile, aes(x = bucket, y = Gain)) + geom_line() + geom_point() +
  labs(x="Decile", y="Gain") + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))+
  geom_vline(xintercept = 4)+ geom_hline(yintercept = 81.2)+ geom_text(aes(label=round(Gain,1)),hjust=-0.2, vjust=1)
plot15
  

########################################################################
#Conclusion
#Environment Satisfaction, Job Satisfaction and Work life balance,
#the better these are for employees the less are their chances of leaving the company.

#The more an employee works overtime on an average the more are the chances that he/she will leave the company.

#If an employee works with the same manager for a longer period of time the lesser 
#are the chances that employee will leave the company.

#Hire people with more experience as they are less likely to leave the company. 

#But if the person has worked in many companies then the chances that he/she will leave the company increases.

#Employees who are unmarried are prone to leaving the company.
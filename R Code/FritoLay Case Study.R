library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071) # Naive Bayes
library(class) # KNN

# Read in CaseStudy1 file
caseStudy <- read.csv(file.choose())

#----------------------------------------------------------------

# Dimensions (rows x columns)
dim(caseStudy)

# Total number of missing values
sum(is.na(caseStudy))

# Total number of missing values in each column
colSums(is.na(caseStudy))

# Summary of stats
summary(caseStudy)

# Check for duplicate rows
sum(duplicated(caseStudy))

# Check structure of the dataset
str(caseStudy)

#----------------------------------------------------------------

# Calculate total number of people and number of people who left within each WorkLifeBalance group
attrition_summary <- caseStudy %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(count = n()) %>%
  group_by(WorkLifeBalance) %>%
  mutate(total = sum(count),
         proportion_left = count / total) %>%
  filter(Attrition == "Yes")  # Filter to only those who left

# Bar plot to display proportion of people who left for each WorkLifeBalance group
ggplot(attrition_summary, aes(x=factor(WorkLifeBalance), y=proportion_left, fill=factor(WorkLifeBalance))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) + # Shows percentages rather than values (ex: shows 10% rather than .1) +
  labs(title="Proportion of People Who Left by WorkLifeBalance", 
       x="Work Life Balance", 
       y="Percentage of People Who Left (%)") +
  theme_minimal()
  
#----------------------------------------------------------------

# Calculate total number of people and number of people who left within each JobSatisfaction group
job_satisfaction_summary <- caseStudy %>%
  group_by(JobSatisfaction, Attrition) %>%
  summarise(count = n()) %>%
  group_by(JobSatisfaction) %>%
  mutate(total = sum(count),
         percentage_left = count / total) %>%
  filter(Attrition == "Yes")  # Filter to only those who left

# Bar plot to display proportion of people who left for each JobSatisfaction group
ggplot(job_satisfaction_summary, aes(x=factor(JobSatisfaction), y=percentage_left, fill=factor(JobSatisfaction))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) + # Shows percentages rather than values (ex: shows 10% rather than .1) +
labs(title="Percentage of People Who Left by JobSatisfaction", 
     x="Level of Job Satisfaction", 
     y="Percentage of People Who Left (%)") +
  theme_minimal()  

#----------------------------------------------------------------

min(caseStudy$HourlyRate)
max(caseStudy$HourlyRate)

# Define custom breakpoints for Hourly Rate, adjusting based on the actual data range
caseStudy$HourlyRateGroup <- cut(caseStudy$HourlyRate,
                            breaks = c(30, 50, 70, 90, 100),  # Custom breakpoints based on the data
                            labels = c("30-50", "50-70", "70-90", "90-100"),  # Labels for the ranges
                            include.lowest = TRUE)


# Create bins for Hourly Rate (you can adjust the number of bins as needed)
##caseStudy$HourlyRateGroup <- cut(caseStudy$HourlyRate,
##                            breaks = 3,  # Number of bins (3 for low, medium, high)
##                            labels = c("Low", "Medium", "High"))

# Calculate proportion of people who left within each Hourly Rate group
hourly_rate_summary <- caseStudy %>%
  group_by(HourlyRateGroup, Attrition) %>%
  summarise(count = n()) %>%                # Count the number of people in each group
  group_by(HourlyRateGroup) %>%
  mutate(total = sum(count),                # Calculate total number of people in each Hourly Rate group
         percentage_leftHR = count / total) %>%  # Calculate proportion of people who left (Attrition = "Yes")
  filter(Attrition == "Yes")                # Only keep rows where attrition = "Yes"


# Bar plot of proportion of people who left by Hourly Rate group
ggplot(hourly_rate_summary, aes(x=factor(HourlyRateGroup), y=percentage_leftHR, fill=factor(HourlyRateGroup))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Percentage of People Who Left by Hourly Rate Group", 
       x="Hourly Rate Group", 
       y="Percentage of People Who Left (%)") +
  theme_minimal()

#----------------------------------------------------------------
  
# Calculate proportion of people who left within each combination of WorkLifeBalance and HourlyRateGroup
worklife_hourly_summary <- caseStudy %>%
  group_by(WorkLifeBalance, HourlyRateGroup, Attrition) %>%
  summarise(count = n()) %>%   # Count the number of people in each group
  group_by(WorkLifeBalance, HourlyRateGroup) %>%
  mutate(total = sum(count),   # Calculate the total number of people in each group
         percentage_Attrition_WLB_HR = count / total) %>%  # Calculate the proportion who left
  filter(Attrition == "Yes")   # Only keep rows where Attrition = "Yes"

# Bar graph: Compare WorkLifeBalance and Hourly Rate Group for people who left
ggplot(worklife_hourly_summary, aes(x=factor(WorkLifeBalance), y=percentage_Attrition_WLB_HR, fill=factor(HourlyRateGroup))) +
  geom_bar(stat="identity", position="dodge") +  # Use 'dodge' to place bars side by side
  scale_y_continuous(labels=scales::percent) +   # Convert y-axis to percentage format
  labs(title="Attrition Percentage by WorkLifeBalance and Hourly Rates", 
       x="Work-Life Balance", 
       y="Percent of Attrition",
       fill="Hourly Rates Grouped") +
  theme_minimal()

#----------------------------------------------------------------
  
# Calculate proportion of people who left within each combination of JobSatisfaction and HourlyRateGroup
job_satisfaction_hourly_summary <- caseStudy %>%
  group_by(JobSatisfaction, HourlyRateGroup, Attrition) %>%
  summarise(count = n()) %>%                # Count the number of people in each group
  group_by(JobSatisfaction, HourlyRateGroup) %>%
  mutate(total = sum(count),                # Calculate the total number of people in each group
         percentage_Attrition_JS_HR = count / total) %>%  # Calculate the proportion who left (Attrition = "Yes")
  filter(Attrition == "Yes")                # Only keep rows where attrition = "Yes"

# Bar graph: Compare Job Satisfaction and Hourly Rate Group for people who left
ggplot(job_satisfaction_hourly_summary, aes(x=factor(JobSatisfaction), y=percentage_Attrition_JS_HR, fill=factor(HourlyRateGroup))) +
  geom_bar(stat="identity", position="dodge") +  # Use 'dodge' to place bars side by side
  scale_y_continuous(labels=scales::percent) +   # Convert y-axis to percentage format
  labs(title="Attrition Percentage by Job Satisfaction and Hourly Rates", 
       x="Job Satisfaction", 
       y="Percent of Attrition",
       fill="Hourly Rates Grouped") +
  theme_minimal()

#----------------------------------------------------------------
    
##ggplot(caseStudy, aes(x=factor(JobSatisfaction), fill=factor(Attrition))) +
##  geom_bar(position="stack") +
##  labs(title="Job Satisfaction vs Attrition", x="Job Satisfaction", fill="Attrition")

##ggplot(caseStudy, aes(x=factor(MonthlyIncome), fill=factor(Attrition))) +
##  geom_bar(position="stack") +
##  labs(title="Monthly Income vs Attrition", x="Monthly Income", fill="Attrition")

## Bar plot of average Monthly Income per Job Level
##ggplot(caseStudy, aes(x=factor(JobLevel), y=MonthlyIncome)) +
##  stat_summary(fun=mean, geom="bar", fill="lightblue") +
##  labs(title="Bar Plot: Average Monthly Income by Job Level", x="Job Level", y="Average Monthly Income")





## Scatterplot with jitter for WorkLifeBalance vs Attrition
##ggplot(caseStudy, aes(x=factor(Attrition), y=WorkLifeBalance)) +
##  geom_jitter(width=0.2, height=0.2, color="darkblue", alpha=0.6) +
##  labs(title="Scatterplot with Jitter: WorkLifeBalance vs Attrition", x="Attrition", y="WorkLifeBalance")

#----------------------------------------------------------------

predictor_vars <- c("WorkLifeBalance", "JobSatisfaction", "HourlyRate")  

data_model <- caseStudy[, c(predictor_vars, "Attrition")]

caseStudy$Attrition <- as.factor(caseStudy$Attrition)

set.seed(123)
trainIndex <- createDataPartition(data_model$Attrition, p=0.7, list=FALSE)
trainData <- data_model[trainIndex, ]
testData <- data_model[-trainIndex, ]

#-------------------------------KNN---------------------------------

# Train KNN Model (with k = 5 as an example)
train_x <- trainData[, -which(names(trainData) == "Attrition")]  # Exclude target variable
test_x <- testData[, -which(names(testData) == "Attrition")]
train_y <- trainData$Attrition
test_y <- testData$Attrition

# KNN Prediction (set k = 5, adjust as necessary)
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 5)

# Confusion Matrix for KNN
knn_conf_mat <- confusionMatrix(factor(knn_pred), factor(test_y))

# View KNN Model Metrics
print(knn_conf_mat)

# Another way to print the metrics (only the specific ones needed)
# Extract metrics for KNN
knn_accuracy <- knn_conf_mat$overall['Accuracy']
knn_sensitivity <- knn_conf_mat$byClass['Sensitivity']
knn_specificity <- knn_conf_mat$byClass['Specificity']
knn_f1 <- 2 * ((knn_conf_mat$byClass['Precision'] * knn_conf_mat$byClass['Sensitivity']) /
                 (knn_conf_mat$byClass['Precision'] + knn_conf_mat$byClass['Sensitivity']))

# Display KNN Metrics
cat("KNN Model Metrics:\n", 
    "Accuracy:", knn_accuracy, 
    "\nSensitivity:", knn_sensitivity, 
    "\nSpecificity:", knn_specificity, 
    "\nF1 Score:", knn_f1, "\n")

#-------------------------------NB---------------------------------

# Train Naive Bayes Model
nb_model <- naiveBayes(Attrition ~ ., data = trainData)

# Make predictions
nb_pred <- predict(nb_model, testData)

# Confusion Matrix for Naive Bayes
nb_conf_mat <- confusionMatrix(factor(nb_pred), factor(testData$Attrition))

# View Naive Bayes Model Metrics
print(nb_conf_mat)


#--------------Adjusting Threshold---------------#
# Predict probabilities with Naive Bayes
nb_prob <- predict(nb_model, testData, type = "raw")[, "Yes"]  # Assuming "Yes" is the positive class

# Set a new threshold (e.g., 0.7) to adjust for specificity/sensitivity trade-off
threshold <- 0.159
nb_pred_adjusted <- ifelse(nb_prob > threshold, "Yes", "No")

# Convert to factors for confusionMatrix
nb_pred_adjusted <- factor(nb_pred_adjusted, levels = levels(testData$Attrition))

# Confusion Matrix for adjusted Naive Bayes predictions
nb_conf_mat_adjusted <- confusionMatrix(nb_pred_adjusted, testData$Attrition)
print(nb_conf_mat_adjusted)
#------------------------------------------------#


# Another way to print the metrics (only the ones needed)
# Extract metrics for Naive Bayes
nb_accuracy <- nb_conf_mat$overall['Accuracy']
nb_sensitivity <- nb_conf_mat$byClass['Sensitivity']
nb_specificity <- nb_conf_mat$byClass['Specificity']
nb_f1 <- 2 * ((nb_conf_mat$byClass['Precision'] * nb_conf_mat$byClass['Sensitivity']) /
                (nb_conf_mat$byClass['Precision'] + nb_conf_mat$byClass['Sensitivity']))

# Display Naive Bayes Metrics
cat("Naive Bayes Model Metrics:\n", 
    "Accuracy:", nb_accuracy, 
    "\nSensitivity:", nb_sensitivity, 
    "\nSpecificity:", nb_specificity, 
    "\nF1 Score:", nb_f1, "\n")



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

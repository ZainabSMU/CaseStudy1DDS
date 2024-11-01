library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071) # Naive Bayes
library(class) # KNN

# Read in CaseStudy1 file
caseStudy2 <- read.csv(file.choose())

# Dimensions (rows x columns)
dim(caseStudy2)

# Check for NAs
sum(is.na(caseStudy2))

# Summary of stats
summary(caseStudy2)

# Check for duplicate rows
sum(duplicated(caseStudy2))

# Check structure/data types of the dataset
str(caseStudy2)

#----------Pointless, just use is.character instead----------#
# Since variables had the character data type, we have to convert them to factors for the "sapply" to work
# Sapply finds the data type
# Lapply changes the data type

# Changes the variables that have a character data type to be a factor, and stores it in all variables with data type of character
#caseStudy2[sapply(caseStudy2, is.character)] <- lapply(caseStudy2[sapply(caseStudy2, is.character)], as.factor)
#------------------------------------------------------------#

#---------------------------Separating numerical and categorical vars---------------------------------#

# Find and separate the numeric data types and store them in a variable
numericValues <- names(caseStudy2)[sapply(caseStudy2, is.numeric)]

# Find and separate the categorical data types and store them in a variable
#categoricalValues <- names(caseStudy2)[sapply(caseStudy2, is.character)]

# Print out values
print(numericValues)

#print(categoricalValues)

# Check data types of dataset
#str(caseStudy2)

#--------------------------Run a t-test on all numeric variables against attrition------------------------------#

# Make a data frame to store the variables with their given p-values
tTestResults <- data.frame(Variable = character(), p_value = numeric(), stringsAsFactors = FALSE)

# Run t-tests for each numeric variable in order to find the p-value and compare each variable
for (var in numericValues) 
{
  # Check for variability within each level of Attrition (Originally getting an error saying data was constant; this fixed the issue)
  if (length(unique(caseStudy2[[var]][caseStudy2$Attrition == "Yes"])) > 1 &&
      length(unique(caseStudy2[[var]][caseStudy2$Attrition == "No"])) > 1) 
    {
    # Run a t-test for the current iterating variable against Attrition
    t_test <- t.test(caseStudy2[[var]] ~ caseStudy2$Attrition)
    
    # Store the variable name and the p-value in the results data frame (bind results to the data frame)
    tTestResults <- rbind(tTestResults, data.frame(Variable = var, p_value = t_test$p.value))
    }
  else 
    {
    # If the variable is constant in one or both groups (of Attrition)
    cat("Skipped: ", var, ": no variability in Attrition groups\n")
    }
}

# Print out p-values 
print(tTestResults)

# Sorting the p-values in order
# Smaller p-value (<.05): Related to Attrition
# Larger p-value (>.05): Might not be related to Attrition
tTestResultsSorted <- tTestResults[order(tTestResults$p_value), ]
print(tTestResultsSorted)

#--------------------------Run an anova on all categorical variables against attrition------------------------------#

# Anova uses categorical variables and they have to be factors, so we convert the characters into factors using lapply
#caseStudy2[sapply(caseStudy2, is.character)] <- lapply(caseStudy2[sapply(caseStudy2, is.character)], as.factor)

# Better way to convert character data types to factors wherever found
caseStudy2 <- caseStudy2 %>% 
  mutate(across(where(is.character), as.factor))

# Find and separate the categorical data types and store them in a variable
categoricalValues <- names(caseStudy2)[sapply(caseStudy2, is.factor)]

# Print out values
print(categoricalValues)

#as.factor(categoricalValues)

# Make a data frame to store the variables with their given p-values
anovaResults <- data.frame(Variable = character(), p_value = numeric(), stringsAsFactors = FALSE)

# Run anova for each categorical variable in order to find the p-value and compare each variable
for (var in categoricalValues) 
  {
  # Don't use the actual "Attrition" variable, and skip variables with only one level (was getting error saying anova can be done with vars with 2 or more levels)
  if (var != "Attrition" && length(unique(caseStudy2[[var]])) > 1) 
    { 
    # Run an ANOVA for the current iterating variable against Attrition
    anovaTest <- aov(as.numeric(Attrition) ~ caseStudy2[[var]], data = caseStudy2)
    
    # Grab p-value from summary (1st element, column name, 1st item)
    p_value <- summary(anovaTest)[[1]][["Pr(>F)"]][1]
    
    # Store the variable name and the p-value in the results data frame ("bind" results to the data frame)
    anovaResults <- rbind(anovaResults, data.frame(Variable = var, p_value = p_value))
  } else {
    # If the variable is skipped b/c of only one level
    cat("Skipped:", var, ": only one level\n")
  }
}

# View ANOVA results, sorted by p-value
anovaResultsSorted <- anovaResults[order(anovaResults$p_value), ]
print(anovaResultsSorted)

#---------------------------Combine P-Value Results----------------------------#

# Combine t-test and anova results
allResults <- rbind(tTestResultsSorted, anovaResultsSorted)

# Sort by p-value
allResultsSorted <- allResults[order(allResults$p_value), ]
print(allResultsSorted)

# Give first few
head(allResultsSorted)


#--------------------------Choosing top variables and plotting------------------------------#

#--------------------------#
# Calculate total number of people and number of people who left within each OverTime group
OverTimeSummary <- caseStudy2 %>%
  group_by(OverTime, Attrition) %>%
  summarise(count = n()) %>%
  group_by(OverTime) %>%
  mutate(total = sum(count),
         proportion_left = count / total) %>%
  filter(Attrition == "Yes")  # Filter to only those who left

# Bar plot to display proportion of people who left for each OverTime group
ggplot(OverTimeSummary, aes(x=factor(OverTime), y=proportion_left, fill=factor(OverTime))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) + # Shows percentages rather than values (ex: shows 10% rather than .1) +
labs(title="Proportion of People Who Left by Over Time", 
     x="Over Time", 
     y="Percentage of People Who Left (%)") +
  theme_minimal()
#--------------------------#

#--------------------------#
# Calculate total number of people and number of people who left within each JobRole group
JobRoleSummary <- caseStudy2 %>%
  group_by(JobRole, Attrition) %>%
  summarise(count = n()) %>%
  group_by(JobRole) %>%
  mutate(total = sum(count),
         proportion_left = count / total) %>%
  filter(Attrition == "Yes")  # Filter to only those who left

# Bar plot to display proportion of people who left for each JobRole group
ggplot(JobRoleSummary, aes(x=factor(JobRole), y=proportion_left, fill=factor(JobRole))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) + # Shows percentages rather than values (ex: shows 10% rather than .1) +
labs(title="Proportion of People Who Left by Job Role", 
     x="Job Role", 
     y="Percentage of People Who Left (%)") +
  theme_minimal()
#--------------------------#

#--------------------------#
# Calculate total number of people and number of people who left within each MaritalStatus group
MaritalStatusSummary <- caseStudy2 %>%
  group_by(MaritalStatus, Attrition) %>%
  summarise(count = n()) %>%
  group_by(MaritalStatus) %>%
  mutate(total = sum(count),
         proportion_left = count / total) %>%
  filter(Attrition == "Yes")  # Filter to only those who left

# Bar plot to display proportion of people who left for each MaritalStatus group
ggplot(MaritalStatusSummary, aes(x=factor(MaritalStatus), y=proportion_left, fill=factor(MaritalStatus))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) + # Shows percentages rather than values (ex: shows 10% rather than .1) +
labs(title="Proportion of People Who Left by Marital Status", 
     x="Marital Status", 
     y="Percentage of People Who Left (%)") +
  theme_minimal()
#--------------------------#

#--------------------------#
min(caseStudy2$MonthlyIncome)
max(caseStudy2$MonthlyIncome)
# Define custom breakpoints for MonthlyIncome, adjusting based on the actual data range
caseStudy2$MonthlyIncomeGroup <- cut(caseStudy2$MonthlyIncome,
                                 breaks = c(1081, 5000, 10000, 15000, 19999),  # Custom breakpoints based on the data
                                 labels = c("1081-4999", "5000-9999", "10000-14999", "15000-19999"),  # Labels for the ranges
                                 include.lowest = TRUE)

# Calculate proportion of people who left within each MonthlyIncome group
MonthlyIncomeSummary <- caseStudy2 %>%
  group_by(MonthlyIncomeGroup, Attrition) %>%
  summarise(count = n()) %>%                # Count the number of people in each group
  group_by(MonthlyIncomeGroup) %>%
  mutate(total = sum(count),                # Calculate total number of people in each MonthlyIncome group
         percentage_leftHR = count / total) %>%  # Calculate proportion of people who left (Attrition = "Yes")
  filter(Attrition == "Yes")                # Only keep rows where attrition = "Yes"


# Bar plot of proportion of people who left by MonthlyIncome group
ggplot(MonthlyIncomeSummary, aes(x=factor(MonthlyIncomeGroup), y=percentage_leftHR, fill=factor(MonthlyIncomeGroup))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Percentage of People Who Left by Monthly Income Groups", 
       x="Monthly Income Groups", 
       y="Percentage of People Who Left (%)") +
  theme_minimal()
#--------------------------#

#--------------------------#
# Calculate total number of people and number of people who left within each JobLevel group
JobLevelSummary <- caseStudy2 %>%
  group_by(JobLevel, Attrition) %>%
  summarise(count = n()) %>%
  group_by(JobLevel) %>%
  mutate(total = sum(count),
         proportion_left = count / total) %>%
  filter(Attrition == "Yes")  # Filter to only those who left

# Bar plot to display proportion of people who left for each JobLevel group
ggplot(JobLevelSummary, aes(x=factor(JobLevel), y=proportion_left, fill=factor(JobLevel))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) + # Shows percentages rather than values (ex: shows 10% rather than .1) +
  labs(title="Proportion of People Who Left by Job Level", 
       x="Job Level", 
       y="Percentage of People Who Left (%)") +
  theme_minimal()
#--------------------------#

#--------------------------#
min(caseStudy2$TotalWorkingYears)
max(caseStudy2$TotalWorkingYears)
# Define custom breakpoints for TotalWorkingYears, adjusting based on the actual data range
caseStudy2$TotalWorkingYearsGroup <- cut(caseStudy2$TotalWorkingYears,
                                     breaks = c(0, 8, 16, 24, 32, 40),  # Custom breakpoints based on the data
                                     labels = c("0-7", "8-15", "16-23", "24-31", "32-40"),  # Labels for the ranges
                                     include.lowest = TRUE)

# Calculate proportion of people who left within each TotalWorkingYears group
TotalWorkingYearsSummary <- caseStudy2 %>%
  group_by(TotalWorkingYearsGroup, Attrition) %>%
  summarise(count = n()) %>%                # Count the number of people in each group
  group_by(TotalWorkingYearsGroup) %>%
  mutate(total = sum(count),                # Calculate total number of people in each TotalWorkingYears group
         percentage_leftHR = count / total) %>%  # Calculate proportion of people who left (Attrition = "Yes")
  filter(Attrition == "Yes")                # Only keep rows where attrition = "Yes"


# Bar plot of proportion of people who left by TotalWorkingYears group
ggplot(TotalWorkingYearsSummary, aes(x=factor(TotalWorkingYearsGroup), y=percentage_leftHR, fill=factor(TotalWorkingYearsGroup))) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Percentage of People Who Left by Total Working Years Groups", 
       x="Total Working Years Groups", 
       y="Percentage of People Who Left (%)") +
  theme_minimal()
#--------------------------#


# Create age ranges and update the 'age' column in the original data frame
caseStudy2$AgeGroups <- cut(caseStudy2$Age, 
                    breaks = seq(10, 100, by = 10), 
                    labels = paste(seq(10, 90, by = 10), seq(20, 100, by = 10), sep = "-"),
                    right = FALSE, 
                    include.lowest = TRUE)

# View the updated dataset
print(caseStudy2)


#-------------------------------KNN---------------------------------#

# Train KNN Model
##trainX <- trainData[, -which(names(trainData) == "Attrition")]  # Exclude actual "Attrition" variable
##testX <- testData[, -which(names(testData) == "Attrition")]
##trainY <- trainData$Attrition
##testY <- testData$Attrition

# Convert factors to numeric (trainX and testX were showing errors saying NAs introduced, but it was b/c some variables were factors)
##trainX <- trainData %>%
##  select(-Attrition) %>%
##  mutate(across(where(is.factor), ~ as.numeric(as.factor(.))))

##testX <- testData %>%
##  select(-Attrition) %>%
##  mutate(across(where(is.factor), ~ as.numeric(as.factor(.))))

# Prediction
##knnPrediction <- knn(train = trainX, test = testX, cl = trainY, k = 5)

# Confusion Matrix
##knnConfusionMatrix <- confusionMatrix(factor(knnPrediction), factor(testY))

# View KNN Model Metrics
##print(knnConfusionMatrix)

#--------------Adjusting Threshold---------------#
##threshold <- 0.6

##knnProbability <- attr(knnPrediction, "probabilities")

##knnPredictionAdjusted <- ifelse(knnProbability[, "Yes"] > threshold, "Yes", "No")

# Convert adjusted predictions to a factor
##knnPredictionAdjusted <- factor(knnPredictionAdjusted, levels = levels(testY))

# Create confusion matrix for adjusted predictions
##knnConfusionMatrixAdjusted <- confusionMatrix(knnPredictionAdjusted, testY)
##print(knnConfusionMatrixAdjusted)

# Display sensitivity and specificity
##sensitivity <- knnConfusionMatrixAdjusted$byClass["Sensitivity"]
##specificity <- knnConfusionMatrixAdjusted$byClass["Specificity"]

##print(paste("Sensitivity:", sensitivity))
##print(paste("Specificity:", specificity))
#------------------------------------------------#

# Another way to print the metrics (only the specific ones needed)
# Extract metrics for KNN
#knnAccuracy <- knnConfusionMatrix$overall['Accuracy']
#knnSensitivity <- knnConfusionMatrix$byClass['Sensitivity']
#knnSpecificity <- knnConfusionMatrix$byClass['Specificity']
#knnF1 <- 2 * ((knnConfusionMatrix$byClass['Precision'] * knnConfusionMatrix$byClass['Sensitivity']) /
#                 (knnConfusionMatrix$byClass['Precision'] + knnConfusionMatrix$byClass['Sensitivity']))

# Display KNN Metrics
#cat("KNN Model Metrics:\n", 
#    "Accuracy:", knnAccuracy, 
#    "\nSensitivity:", knnSensitivity, 
#    "\nSpecificity:", knnSpecificity, 
#    "\nF1 Score:", knnF1, "\n")

#--------------------------Modeling------------------------------#

predictorVars <- c("WorkLifeBalance", "JobSatisfaction", "HourlyRate", "AgeGroups",
                   "YearsInCurrentRole", "JobInvolvement",
                   "YearsWithCurrManager", "YearsAtCompany", "Department",
                   "OverTime", "JobRole", "MaritalStatus", "MonthlyIncome", "JobLevel", "TotalWorkingYears")  

dataModel <- caseStudy2[, c(predictorVars, "Attrition")]

caseStudy2$Attrition <- as.factor(caseStudy2$Attrition)

set.seed(123)
trainIndex <- createDataPartition(dataModel$Attrition, p=0.8, list=FALSE, times=1)
trainData <- dataModel[trainIndex, ]
testData <- dataModel[-trainIndex, ]

#-------------------------------NB---------------------------------#

# Train Naive Bayes Model
nbModel <- naiveBayes(Attrition ~ ., data = trainData, laplace=1)

# Make predictions
nbPrediction <- predict(nbModel, testData)

# Confusion Matrix for Naive Bayes
nbConfusionMatrix <- confusionMatrix(factor(nbPrediction), factor(testData$Attrition))

# View Naive Bayes Model Metrics
print(nbConfusionMatrix)

#summary(nbConfusionMatrix)


#--------------Adjusting Threshold---------------#
# Predict probabilities with Naive Bayes
nbProbability <- predict(nbModel, testData, type = "raw")[, "Yes"]  # Assuming "Yes" is the positive class

threshold <- 0.5
nbPredictionAdjusted <- ifelse(nbProbability > threshold, "Yes", "No")

# Convert to factors for confusionMatrix
nbPredictionAdjusted <- factor(nbPredictionAdjusted, levels = levels(testData$Attrition))

# Confusion Matrix for adjusted Naive Bayes predictions
nbConfusionMatrixAdjusted <- confusionMatrix(nbPredictionAdjusted, testData$Attrition)
print(nbConfusionMatrixAdjusted)
#------------------------------------------------#

# Another way to print the metrics (only the ones needed)
# Extract metrics for Naive Bayes
nbAccuracy <- nbConfusionMatrix$overall['Accuracy']
nbSensitivity <- nbConfusionMatrix$byClass['Sensitivity']
nbSpecificity <- nbConfusionMatrix$byClass['Specificity']
nbF1 <- 2 * ((nbConfusionMatrix$byClass['Precision'] * nbConfusionMatrix$byClass['Sensitivity']) /
                (nbConfusionMatrix$byClass['Precision'] + nbConfusionMatrix$byClass['Sensitivity']))

# Display Naive Bayes Metrics
cat("Naive Bayes Model Metrics:\n", 
    "Accuracy:", nbAccuracy, 
    "\nSensitivity:", nbSensitivity, 
    "\nSpecificity:", nbSpecificity, 
    "\nF1 Score:", nbF1, "\n")
#------------------------------------------------#
# Read in NoAttrition file
competition <- read.csv(file.choose())

head(competition)

# Make predictions
compPrediction <- predict(nbModel, competition) 

compResults <- data.frame(ID=competition$ID, Attrition = compPrediction)
compResults$Attrition <- ifelse(compResults$Attrition == 1, "Yes", "No")
print(compResults)

#summary(nbConfusionMatrix)

write.csv(compResults, "/Users/Zainab/Desktop/FritoLayCaseStudyAttrition.csv", row.names=FALSE)

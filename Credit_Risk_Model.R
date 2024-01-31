# Code For Data Science and Analytics
# Student Name: Edward Duffy
# Student Number: R00257226
# Module: Data8001
# Continuous Assessment: Assignment 1


# IMPORTANT NOTE: This code needs to be run in one go to get the results as seen in report

# Synopsis of task at hand
# Siobhán, a financial institution manager, 
#needs your help assessing future customer creditworthiness 
#using a dataset of 904 past loan cases with 14 attributes, 
#including financial status, loan purpose, employment, demographics, and more, 
#to classify them as good or bad loans.

# Data Details:

#Checking Acct - What level of regular checking account does the customer have
#–No acct, 0balance, low(balance), high (balance)

#Credit History – All paid – no credit taken or all credit paid back duly

#Bank Paid – All credit at this bank paid back

#Current – Existing loan/credit paid back duly till now

#Critical – Risky account or other credits at other banks

#Delay – Delay in paying back credit/loan in the past

#Months Acct – The number of months the customer has an account with the bank.

#Credibility score – A score given to applicants to reflect the credibility of them repaying the loan, using a formula 
#created by a data analyst and had access to all historical data.

#Check – The data analyst created this field as a check on Credit Standing and had access to all historical data.

# Installing libraries if you need to
#install.packages("outliers")
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("dummies")
#install.packages("combiant")

# Loading libraries
library(readxl)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)
library(outliers)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(corrplot)
library(tree)
library(combinat)

#library(dummies)


# Q1: Exploratory Data Analysis (EDA): - Carry out EDA on the data set; do you notice anything unusual
#(missing data, outliers, duplicates etc.) or any patterns with the data set?

# Here I will be performing the following 
# 1. Reading in the CSV file / Dataset 
# 2. Getting a summary of the data before any EDA
# 3. Using R to explore the dataset and try to find outliers, dupes, missing values
# 4. Getting my results and making decisons on whether to remove / impute or one hot encode

# Reading in my dataset "Credit_Risk_32_final.csv"
Credit_Risk_Dataset = read.csv("Credit_Risk_32_final.csv")

Clean_copy = Credit_Risk_Dataset

# Get the number of rows, which corresponds to the number of data points
num_data_points = nrow(Clean_copy)
print(num_data_points)

str(Credit_Risk_Dataset)


# ANALYZING RELATIONSHIP BETWEEN VARIABLES

# 1) Credit Standing vs Credit History:
# Exploring relationship using a bar plot with the ggplot library

ggplot(Credit_Risk_Dataset, aes(x = Credit.History, fill = Credit.Standing))+
  geom_bar(position = "dodge")+
  labs(title = "Credit Standing vs Credit History", x = "Credit History", y = "Count")+
  theme_minimal()

# 2) Distribution of Credibility Scores
# Using a histogram to explore distribution of credibility scores

# Histogram of Credibility Scores

ggplot(Credit_Risk_Dataset, aes(x = Credibility_score, fill = Credit.Standing)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  labs(title = "Distribution of Credibility Scores", x = "Credibility Score", y = "Frequency") +
  theme_minimal()

# 3) Employment Duration vs Credit Standing
# Bar plot to understand relationship between employment duration and credit standing

ggplot(Credit_Risk_Dataset, aes(x = Employment, fill = `Credit.Standing`)) +
  geom_bar(position = "dodge") +
  labs(title = "Employment Duration and Credit Standing", x = "Employment Duration", y = "Count") +
  theme_minimal()


# 4) Age Distribution 
# A histogram to show the Age Distribution of customers

ggplot(Credit_Risk_Dataset, aes(x = Age, fill = `Credit.Standing`)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  labs(title = "Age Distribution of Customers", x = "Age", y = "Frequency") +
  theme_minimal()





# Creating a variable that will return a number of duplicate rows within the dataset
# It will return the following to indicate whether or not there are duplicate rows <"n"rows> (or 0-length row.names) 
Credit_Risk_Duplicate_Rows = Credit_Risk_Dataset[duplicated(Credit_Risk_Dataset),]
print(Credit_Risk_Duplicate_Rows)


unique_duplicates_in_Credit_Risk_Dataset = unique(Credit_Risk_Duplicate_Rows)
print(unique_duplicates_in_Credit_Risk_Dataset)

# Checking for any missing values contained within the table
# Decided to separate this into numerical and nominal 
# Numerical columns are the following which was gotten from str(Credit_Risk_Dataset):
#$ ID                                : int  1 2 3 4 5 6 7 8 9 10 ...
#$ Months.since.Checking.Acct.opened : int  7 16 25 31 7 13 22 25 25 13 ...
#$ Residence.Time.In.current.district: int  3 2 2 4 4 2 3 4 4 4 ...
#$ Age                               : num  50.8 28 28 35.4 40.3 28.5 36.9 34.4 67.4 44.6 ...
#$ Credibility_score                 : num  127.5 70.1 70.2 89.3 101 ...
#$ check                             : int  1 0 0 1 1 1 1 1 1 1 ...

# ID is a unique identifier so all I needed to check was that every value is unique



# Iteratively going through every numeric column


numeric_data = data.frame(
  ID_column = Credit_Risk_Dataset$ID,
  Months_since_check_acc_opened_col = Credit_Risk_Dataset$Months.since.Checking.Acct.opened,
  Residence_col = Credit_Risk_Dataset$Residence.Time.In.current.district,
  Age_col = Credit_Risk_Dataset$Age,
  Credibilty_score_col = Credit_Risk_Dataset$Credibility_score
)

# Missing values for each column / NA's
missing_values_in_credit_dataset = sapply(numeric_data, function(col) sum(is.na(col) | col == ""))

# Display the results, easier going iteratively than reusing blocks of code
for (column_name in names(missing_values_in_credit_dataset)){
  cat(paste("Column", column_name, "has", missing_values_in_credit_dataset[column_name], "missing values, \n"))
}

head(Credit_Risk_Dataset)
# Non-numeric data / Nominal Data
non_numeric_data = data.frame(
  checking_acc = Credit_Risk_Dataset$Checking.Acct,
  credit_history = Credit_Risk_Dataset$Credit.History,
  loan_reason = Credit_Risk_Dataset$Loan.Reason,
  savings_account = Credit_Risk_Dataset$Savings_Acct,
  employment = Credit_Risk_Dataset$Employment,
  personal_status = Credit_Risk_Dataset$Personal_Status,
  housing_status = Credit_Risk_Dataset$Housing,
  job_type = Credit_Risk_Dataset$Job.Type,
  Foreign = Credit_Risk_Dataset$Foreign.National,
  reg = Credit_Risk_Dataset$Reg.State,
  credit_stand = Credit_Risk_Dataset$Credit.Standing
)

# Checking for missing values in the non_numeric data frame
missing_values_in_non_numeric_data = sapply(non_numeric_data, function(col) sum(is.na(col) | col == ""))



# Summary of looking through data set for missing values

# Numeric:
# No columns containing missing values

# Non-numeric - results:
# Column employment has 11 missing values,
# Column personal_status has 6 missing values, 
# Column housing_status has 5 missing values
# Column credit_stand has 2 missing values,

# Method's I can use are the following:
# 1) Leave the values as they are 
# 2) Impute the missing values using one of the following
# a) Mode Imputation: Replace missing values with the mode / most frequent value
# b) Random Sampling: Randomly assign missing values based on distribution
# c) Prediction: Using machine learning to predict the missing value based on other features

# Will use Imputation to replace the missing values with the mode / most frequent

# Categories to Impute are:
# Employment
# Personal_status
# Housing status
# Credit_stand

# Load the dplyr library (if not already loaded)
library(dplyr)

View(Credit_Risk_Dataset)
# Specify the columns you want to impute as character strings
#columns_to_impute <- c("Employment", "Personal_Status", "Housing" )

# Loop through the columns and impute missing values with the mode
#for (col in columns_to_impute) {
#Credit_Risk_Dataset <- Credit_Risk_Dataset %>% 
#mutate({{col}} := ifelse({{col}} == "", mode({{col}}, na.rm = TRUE), {{col}}))
#}

get_mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Impute missing values with the mode (most frequent value) in the "MyColumn" column
Credit_Risk_Dataset$Employment <- ifelse(Credit_Risk_Dataset$Employment == "", get_mode(Credit_Risk_Dataset$Employment), Credit_Risk_Dataset$Employment)

Credit_Risk_Dataset$Personal_Status <- ifelse(Credit_Risk_Dataset$Personal_Status == "", get_mode(Credit_Risk_Dataset$Personal_Status), Credit_Risk_Dataset$Personal_Status)

Credit_Risk_Dataset$Housing <- ifelse(Credit_Risk_Dataset$Housing == "", get_mode(Credit_Risk_Dataset$Housing), Credit_Risk_Dataset$Housing)

Credit_Risk_Dataset$Credit.Standing <- ifelse(Credit_Risk_Dataset$Credit.Standing == "", get_mode(Credit_Risk_Dataset$Credit.Standing), Credit_Risk_Dataset$Credit.Standing)



View(Credit_Risk_Dataset)

# Checking for any outliers within our numeric data
# Methods I could have used are:
# 1) Boxplots: help visualize the distribution of data
# 2) Histograms and Density Plots:
# 3) Scatterplots: Identify points that are far from the majority of the data
# 4) Z-score's / Standardization: Help identify values that are significantly different from mean
# 5) IQR: Calculate the IQR for a column and identify values x1.5 greater than the mean 
# 6) Using the outlier detection libraries 

# For this I will be using the outliers library to detect any outliers within the numeric data
library(outliers)

# Numeric Data
# ID                                : int  ( will be ignored as its unique)
# Months.since.Checking.Acct.opened : int  
# Residence.Time.In.current.district: int  
# Age                               : num 
# Credibility_score                 : num  
# check                             : int (ignored as its binary)




# Create a box plot of Age with the outlier highlighted
ggplot(Credit_Risk_Dataset, aes(y = Age)) +
  geom_boxplot() +
  geom_point(data = Credit_Risk_Dataset[Credit_Risk_Dataset$Age == 151, ], aes(x = 1, y = Age, color = "Outlier"), size = 3) +
  labs(title = "Box Plot of Age with Outlier Highlighted") +
  scale_color_manual(values = c("Data" = "blue", "Outlier" = "red")) +
  guides(color = guide_legend(title = "Data vs. Outlier"))


# Creating a violin diagram for Age


# Looking through each column
#outlier(Credit_Risk_Dataset$Months.since.Checking.Acct.opened)
#outlier(Credit_Risk_Dataset$Residence.Time.In.current.district)
#outlier(Credit_Risk_Dataset$Age)
#outlier(Credit_Risk_Dataset$Credibility_score)

# Outliers found are the following 
# Months.since.Checking.Acct.opened=> [1] 120
# Residence.Time.In.current.district) => [1] 10
# Age => [1] 151
# Credibility_score) => [1] 214.5519

# For these outliers I can use the following methods:
# 1) Winsorization: truncating or capping extreme values
# 2) Trimming: Removing extreme values from the dataset and exclude from Analysis
# 3) Imputing: Replace outliers with a reasonable value such as mean or median.
# For this I wanted to be consistent so I used imputation like I did with non-numeirc data earlier

# All the outlier values
Age_outlier_value = 151
Months_since_opened_outlier = 120
Credibility_score_outlier = 214.5519

# Replacing the outlier with the median
Credit_Risk_Dataset$Age[Credit_Risk_Dataset$Age == Age_outlier_value] <- median(Credit_Risk_Dataset$Age, na.rm = TRUE)
Credit_Risk_Dataset$Months.since.Checking.Acct.opened[Credit_Risk_Dataset$Months.since.Checking.Acct.opened == Months_since_opened_outlier] <- median(Credit_Risk_Dataset$Months.since.Checking.Acct.opened, na.rm = TRUE)
Credit_Risk_Dataset$Credibility_score[Credit_Risk_Dataset$Credibility_score == Credibility_score_outlier] <- median(Credit_Risk_Dataset$Credibility_score, na.rm = TRUE)

# Checking for the latest outlier
#outlier(Credit_Risk_Dataset$Age)
#outlier(Credit_Risk_Dataset$Months.since.Checking.Acct.opened)
#outlier(Credit_Risk_Dataset$Credibility_score)
# 85.6 is the latest outlier, thats fine as its a realistic age than 151



# I was informed there is no need to implement the below code

#adjust_job_type = function(data) {
#unemployed_indices <- which(data$Employment == "Unemployed")
#data$Job.Type[unemployed_indices] <- "Unemployed"
#return(data)
#}

#Credit_Risk_Dataset = adjust_job_type(Credit_Risk_Dataset)

#view(Credit_Risk_Dataset)



# Correcting the negative value in 'Residence.Time.In.Current.district
Credit_Risk_Dataset$Residence.Time.In.current.district[Credit_Risk_Dataset$Residence.Time.In.current.district == - 2] = 2

# There instances where "Single" is entered as "single"
# Correcting this by targeting instances of this mistake and replacing with "Single"
Credit_Risk_Dataset$Personal_Status = as.character(Credit_Risk_Dataset$Personal_Status)
Credit_Risk_Dataset$Personal_Status[Credit_Risk_Dataset$Personal_Status == "single"] = "Single"

# Convert back to a factor
Credit_Risk_Dataset$Personal_Status = as.factor(Credit_Risk_Dataset$Personal_Status)

#write.csv(Credit_Risk_Dataset, "Corrected_Imputed_Credit_Risk.csv", row.names = FALSE)



# B) Split the dataset into 75% training and 25% test set using set.seed(abc) 
#where abc are the last 3 digits of your student no. 
#(Use this set.seed for all other functions with an element of randomness in this work).
# Student Number is R00257226

# Splitting the data into training and testing data with a 75:25 split
set.seed(226)

split_index = sample(1:nrow(Credit_Risk_Dataset), size = nrow(Credit_Risk_Dataset) * 0.75 )

training_data = Credit_Risk_Dataset[split_index,]

testing_set = Credit_Risk_Dataset[-split_index,]

# Structure of training and test data
str(training_data)

# View training data
view(training_data)

##################################

# C) 

# Define the names of categorical columns in the dataset
categoric_columns = c("Checking.Acct", "Credit.History", "Loan.Reason", "Savings_Acct", "Employment", "Personal_Status", "Housing", "Job.Type", "Foreign.National", "Reg.State")

# Convert each column that has been defined as categorical into a factor for analysis
training_data[categoric_columns] = lapply(training_data[categoric_columns], factor)


# Define a function to calculate entropy, which is a measure of randomness or uncertainty
calculate_entropy = function(data) {
  # Calculate the probabilities for each unique value in the data
  probs = table(data) / length(data)
  # The entropy formula: sum of -p * log2(p) for all probabilities p
  -sum(probs * log2(probs))
}

# Calculate the initial entropy of the target variable 'Credit.Standing'
target_entropy = calculate_entropy(training_data$Credit.Standing)

# Calculate information gain for each categorical variable
# Information gain measures the reduction in entropy or surprise by transforming a dataset
# and is often used in training decision trees
information_gains = sapply(categoric_columns, function(column) {
  unique_values = unique(training_data[[column]])
  
  # Calculate the weighted entropy for each unique value of the column
  weighted_entropy = sapply(unique_values, function(val) {
    # Create a subset of data where the column value equals the unique value
    subset_data = training_data[training_data[[column]] == val,]
    # Proportion of the subset in the whole dataset
    prop = nrow(subset_data) / nrow(training_data)
    # Calculate the entropy of the 'Credit.Standing' column for this subset
    calculate_entropy(subset_data$Credit.Standing) * prop
  })
  
  # The information gain is the difference between the initial entropy and the weighted entropies
  target_entropy - sum(weighted_entropy)
})

# Print the information gains for each variable
print(information_gains)

# Find the variable with the maximum information gain
best_split = names(which.max(information_gains))
print(paste("Best variable to split on:", best_split))


##############################################
# D)
# Now redo part c) but now you are constrained to only binary splits, i.e. a split with only 2 possible 
#################################################


library(combinat)

# Function to calculate information gain for a binary split
calculate_binary_split_gain <- function(data, column, split) {
  # Split data into two groups
  group1 = data[data[[column]] %in% split,]
  group2 = data[!data[[column]] %in% split,]
  
  # Calculate weighted entropy for each group
  prop1 = nrow(group1) / nrow(data)
  prop2 = nrow(group2) / nrow(data)
  entropy1 = calculate_entropy(group1$Credit.Standing) * prop1
  entropy2 = calculate_entropy(group2$Credit.Standing) * prop2
  
  # Information gain
  target_entropy - (entropy1 + entropy2)
}

# Calculate information gains for all possible binary splits
best_splits = lapply(categoric_columns, function(column) {
  unique_values = unique(training_data[[column]])
  if (length(unique_values) > 1) {
    # Generate all possible binary splits
    splits <- unlist(lapply(1:(length(unique_values) - 1), function(n) {
      combn(unique_values, n, simplify = FALSE)
    }), recursive = FALSE)
    
    # Calculate information gain for each split
    gains = sapply(splits, function(split) {
      calculate_binary_split_gain(training_data, column, split)
    })
    
    # Best split for this variable
    best_split = splits[which.max(gains)]
    list(column = column, split = best_split, gain = max(gains))
  } else {
    NULL
  }
})

# Filter out NULL values (for variables with only one unique value)
best_splits = Filter(Negate(is.null), best_splits)

# Find the overall best split
overall_best_split = best_splits[[which.max(sapply(best_splits, function(x) x$gain))]]
print(overall_best_split)


#########################
# E
########################

# All continous columns we will be using
continuous_columns = c('Months.since.Checking.Acct.opened',
                       'Residence.Time.In.current.district',
                       'Age',
                       'Credibility_score')



# Function to calculate information gain for a continuous variable split
calculate_continuous_split_gain = function(data, column, split_point) {
  group1 = data[data[[column]] <= split_point,]
  group2 = data[data[[column]] > split_point,]
  
  prop1 = nrow(group1) / nrow(data)
  prop2 = nrow(group2) / nrow(data)
  entropy1 = calculate_entropy(group1$Credit.Standing) * prop1
  entropy2 = calculate_entropy(group2$Credit.Standing) * prop2
  
  target_entropy - (entropy1 + entropy2)
}

# Find the best split for each continuous variable
best_continuous_splits = lapply(continuous_columns, function(column) {
  sorted_values = sort(unique(training_data[[column]]))
  split_points = (head(sorted_values, -1) + tail(sorted_values, -1)) / 2
  
  gains = sapply(split_points, function(point) {
    calculate_continuous_split_gain(training_data, column, point)
  })
  
  best_point = split_points[which.max(gains)]
  list(column = column, split_point = best_point, gain = max(gains))
})

# Combine with best categorical splits
combined_best_splits = c(best_splits, best_continuous_splits)

# Find the overall best split (categorical or continuous)
overall_best_split = combined_best_splits[[which.max(sapply(combined_best_splits, function(x) x$gain))]]
print(overall_best_split)


###################
# F
##################

# Defining a function to calculate the entropy of a dataset.
# Entropy is a measure of the randomness or disorder within the dataset.
# Calculate the probabilities for each unique value in the data
calculate_entropy = function(data) {
  probs = table(data) / length(data)
  # If any probability is zero, entropy is zero (to prevent log2(0) which is undefined)
  if (any(probs == 0)) {
    return(0)
  } else {
    # Calculate the entropy using the probability values
    return(-sum(probs * log2(probs)))
  }
}

# Defining a function to calculate the information gain from a binary split on categorical variables
calculate_categorical_split_gain = function(subset, column, split) {
  # Divide the dataset into two groups based on the split criteria
  group1 = subset[subset[[column]] %in% split, ]
  group2 = subset[!subset[[column]] %in% split, ]
  
  prop1 = nrow(group1) / nrow(subset)
  prop2 = nrow(group2) / nrow(subset)
  entropy1 = calculate_entropy(group1$Credit.Standing) * prop1
  entropy2 = calculate_entropy(group2$Credit.Standing) * prop2
  
  # Check if entropy values are NA
  if (is.na(entropy1) || is.na(entropy2)) {
    cat("NA entropy detected for column", column, "and split", split, "\n")
    return(NA)
  }
  
  return(target_entropy - (entropy1 + entropy2))
}

calculate_continuous_split_gain = function(subset, column, split_point) {
  group1 = subset[subset[[column]] <= split_point, ]
  group2 = subset[subset[[column]] > split_point, ]
  
  # Here I am Calculating the proportion of each group relative to the entire dataset
  prop1 = nrow(group1) / nrow(subset)
  prop2 = nrow(group2) / nrow(subset)
  # Calculating the weighted entropy for each group
  entropy1 = calculate_entropy(group1$Credit.Standing) * prop1
  entropy2 = calculate_entropy(group2$Credit.Standing) * prop2
  
  # Check if the calculated entropies are not a number (NA) and handle this case
  if (is.na(entropy1) || is.na(entropy2)) {
    cat("NA entropy detected for column", column, "and split point", split_point, "\n")
    return(NA)
  }
  
  # Calculating and returning the information gain from this binary split
  return(target_entropy - (entropy1 + entropy2))
}

calculate_best_split_for_subset = function(subset, categoric_columns, continuous_columns) {
  if (nrow(subset) < 2 || length(unique(subset$Credit.Standing)) < 2) {
    return(NULL)
  }
  
  target_entropy = calculate_entropy(subset$Credit.Standing)
  best_gain = -Inf
  best_split = NULL
  
  # Process categorical variables
  for (column in categoric_columns) {
    unique_values = unique(subset[[column]])
    if (length(unique_values) > 1) {
      splits = lapply(unique_values, function(val) c(val))
      for (split in splits) {
        gain = calculate_categorical_split_gain(subset, column, split)
        # Check if gain is NA before the comparison
        if (is.na(gain)) {
          cat("NA gain detected for column", column, "\n")
        } else if (gain > best_gain) {
          best_gain = gain
          best_split = list(column = column, split = split, gain = gain)
        }
      }
    }
  }
  
  # Process continuous variables
  for (column in continuous_columns) {
    sorted_values = sort(unique(subset[[column]]))
    split_points = (head(sorted_values, -1) + tail(sorted_values, -1)) / 2
    for (split_point in split_points) {
      gain = calculate_continuous_split_gain(subset, column, split_point)
      # Check if gain is NA before the comparison
      if (is.na(gain)) {
        cat("NA gain detected for column", column, "at split point", split_point, "\n")
      } else if (gain > best_gain) {
        best_gain = gain
        best_split = list(column = column, split_point = split_point, gain = gain)
      }
    }
  }
  
  return(best_split)
}

# Predefined categorical and continuous columns used for the binary split
categoric_columns = c("Checking.Acct", "Credit.History", "Loan.Reason", "Savings_Acct",
                       "Employment", "Personal_Status", "Housing", "Job.Type", 
                       "Foreign.National", "Reg.State")
continuous_columns = c("Months.since.Checking.Acct.opened", 
                        "Residence.Time.In.current.district", "Age", "Credibility_score")

# I'm Splitting the training data into subsets based on 'Credit.History' being 'Critical' or not
subset_critical = training_data[training_data$Credit.History == "Critical", ]
subset_not_critical = training_data[training_data$Credit.History != "Critical", ]

# I'm Calculate the best binary split for each subset
best_split_subset_critical = calculate_best_split_for_subset(subset_critical, categoric_columns, continuous_columns)
best_split_subset_not_critical = calculate_best_split_for_subset(subset_not_critical, categoric_columns, continuous_columns)

# I'm Outputting the results of the best binary split
print("Best split for subset where Credit.History is Critical:")
print(best_split_subset_critical)

print("Best split for subset where Credit.History is not Critical:")
print(best_split_subset_not_critical)





# g) Use the tree function from the package tree, or equivalent, build a decision tree and compare the 
# results to those in f) and comment. If you use pruning here you should explain all the methodology 
# you use.

# Build a decision tree using the variables used in part f)
# Categorical and Continuous
# Categorical: "Checking.Acct", "Credit.History", "Loan.Reason", "Savings_Acct", 
#"Employment", "Personal_Status", "Housing", "Job.Type", "Foreign.National", "Reg.State"
# Continuous:"Months.since.Checking.Acct.opened", "Residence.Time.In.current.district", "Age", "Credibility_score"

# Creating Formula
decision_tree_formula_cat_numeric = Credit.Standing ~ Credit.History + Age + Employment +
  Loan.Reason + Months.since.Checking.Acct.opened +
  Housing + Job.Type + Savings_Acct + Personal_Status + Housing + Job.Type + Foreign.National + Reg.State +
  Residence.Time.In.current.district + Credibility_score + Checking.Acct

# Build the decision tree 
decision_tree_vars_from_f = rpart(decision_tree_formula_cat_numeric,
                                  data = training_data,
                                  method = "class")

# Making predictions 
decision_tree_vars_from_f_predictions = predict(decision_tree_vars_from_f,
                                                newdata = training_data,
                                                type = "class")

# Display predictions 
print(decision_tree_vars_from_f_predictions)

# Printing the decision tree
print(decision_tree_vars_from_f)

# plotting the decision tree using prp
prp(decision_tree_vars_from_f)

# Calculating the accuracy of the model 
actual_credit_standing_scores = training_data$Credit.Standing

# Accuracy of the decision tree model
accuracy_of_model = sum(decision_tree_vars_from_f_predictions == actual_credit_standing_scores)
cat("Accuracy on training data:", accuracy_of_model, "\n")

# Confusion Matrix
confusion_matrix = confusionMatrix(factor(decision_tree_vars_from_f_predictions), factor(actual_credit_standing_scores))
print(confusion_matrix)

# Total number of observations
total_observations = nrow(training_data)

# Calculating the accuracy rate
accuracy_rate = accuracy_of_model / total_observations

# Print the accuracy rate
cat("Accuracy rate on training data:", accuracy_rate, "\n")

# Ensure that the factor levels in the test data match those in the training data
testing_set$Credit.History = factor(testing_set$Credit.History, levels = levels(training_data$Credit.History))
# Repeat for all other factor variables used in the model...

# Making predictions on the test set
decision_tree_test_predictions = predict(decision_tree_vars_from_f,
                                          newdata = testing_set,
                                          type = "class")

# Creating a variable for the true values in the test set
true_values_test = testing_set$Credit.Standing

# Calculating the accuracy of the model on the test set
accuracy_of_test_model = sum(decision_tree_test_predictions == true_values_test) / length(true_values_test)

# Printing the accuracy on the test data
cat("Accuracy on test data:", accuracy_of_test_model, "\n")

# Creating a confusion matrix for the test set predictions
confusion_matrix_test = confusionMatrix(factor(decision_tree_test_predictions), factor(true_values_test))

# Printing the confusion matrix for the test set
print(confusion_matrix_test)








#h) Now see if you can improve your results by using a random forest model. Give your results (5 marks) 
#and explain and comment (5 marks).

# Turning credit.standing into a factor
training_data$Credit.Standing = as.factor(training_data$Credit.Standing)

# Turning credit.standing into a factor for test
testing_set$Credit.Standing = as.factor(testing_set$Credit.Standing)

# Create a random forest formula
random_forest_formula = Credit.Standing ~ Credit.History + Age + Employment +
  Loan.Reason + Months.since.Checking.Acct.opened +
  Housing + Job.Type + Savings_Acct + Personal_Status + Housing + Job.Type + Foreign.National + Reg.State +
  Residence.Time.In.current.district + Credibility_score

# Build the random forest model
random_forest_model = randomForest(random_forest_formula,
                                   data = training_data,
                                   ntree = 500)

# Making predictions on random forest model
random_forest_model_predictions = predict(random_forest_model, training_data)

# Creating a variable for the true values
true_values_credit_standing = training_data$Credit.Standing

# Testing Accuracy
random_forest_accuracy = mean(random_forest_predictions == true_values)

# Printing the accuracy 
cat("Accuracy of random Forest Model is:", random_forest_accuracy, "\n")

# Getting a summary of the most important aspects
summary(random_forest_model)

# Getting the most important predictor variables as overfitting is occurring
importance(random_forest_model)

# Summary:
#Credit.History                            77.978815
#Age                                       56.285101
#Employment                                17.972378
#Loan.Reason                               25.475949
#Months.since.Checking.Acct.opened         23.280333
#Housing                                   10.748743
#Job.Type                                  11.459566
#Savings_Acct                              14.496526
#Personal_Status                            7.102075
#Foreign.National                           6.396222
#Reg.State                                  6.261876
#Residence.Time.In.current.district        12.331430
#Credibility_score                         52.950093

# Writing a new formula using the important predictor variables

tuned_random_forest_formula = Credit.Standing ~ Credit.History + 
  Age + Credibility_score + Loan.Reason + Months.since.Checking.Acct.opened

# Putting in the new random forest formula
new_random_forest_model = randomForest(tuned_random_forest_formula,
                                       data = training_data,
                                       ntree = 600)
# Predictions with new model
new_random_forest_model_predictions = predict(random_forest_model, training_data)

# Testing Accuracy
new_random_forest_accuracy = mean(new_random_forest_model_predictions == true_values)

# Printing the accuracy 
cat("Accuracy of random Forest Model is:", new_random_forest_accuracy, "\n")

# Importance of predictor variables in model
importance(new_random_forest_model)

#MeanDecreaseGini
#Credit.History                            93.65519
#Age                                       81.24266
#Credibility_score                         78.71060
#Loan.Reason                               34.04018
#Months.since.Checking.Acct.opened         34.66712

# Evaluating the performance of the model using:
# Cross Validation
# Bootstrap Sampling
# Creating Manual Subsets

# New model from results:
#Credit.History                            93.65519
#Age                                       81.24266
#Credibility_score                         78.71060

tuned_random_forest_formula_3 = Credit.Standing ~ Credit.History + 
  Age + Credibility_score 

# Putting in the new random forest formula
new_random_forest_model_3 = randomForest(tuned_random_forest_formula,
                                         data = training_data,
                                         ntree = 600)

# Cross-validation
set.seed(226)


# Testing our model
index = sample(1:nrow(training_data), 0.7 * nrow(training_data))
new_train_set = training_data[index, ]
new_validation_set = training_data[-index, ]

# Training the model on the new training set
new_random_forest_model_cv = randomForest(tuned_random_forest_formula,
                                          data = new_train_set,
                                          ntree = 500)

# summary of the model
print(new_random_forest_model_cv)

# Bootstrapping with 50 resamples
training_control = trainControl(method = "boot", number = 50)

# training
new_random_forest_model_boot = randomForest(tuned_random_forest_formula,
                                            data = new_train_set,
                                            ntree = 500)

# Summary of the model using bootstrapping
print(new_random_forest_model_boot)



# Manual Subsets

# Training new model on the training sets
new_random_forest_model_man_subset = randomForest(tuned_random_forest_formula,
                                                  data = new_train_set,
                                                  ntree = 500)

# Validating new model on the validation set
predictions = predict(new_random_forest_model_man_subset, new_validation_set)
confusionMatrix(predictions, new_validation_set$Credit.Standing)

# Convert character columns in the test set to factors
factor_columns = c("Checking.Acct", "Credit.History", "Loan.Reason", "Savings_Acct", 
                   "Employment", "Housing", "Job.Type", "Foreign.National", "Reg.State")

for (col in factor_columns) {
  testing_set[[col]] = factor(testing_set[[col]], levels = levels(training_data[[col]]))
}

# After ensuring consistency, try making predictions again
test_set_predictions = predict(new_random_forest_model_3, testing_set)

# Applying the model to the test set to make predictions
test_set_predictions = predict(new_random_forest_model_3, testing_set)

# You can compare these predictions with the actual values in your test set if they are available
# For example, if your test set includes the actual values of Credit.Standing
test_set_accuracy = mean(test_set_predictions == testing_set$Credit.Standing)

# Printing the accuracy on the test set
cat("Accuracy of Random Forest Model on Test Set is:", test_set_accuracy, "\n")
















# i) Due to GDPR you are no longer allowed use the following variables to build your model Age, 
# Personal.Status and Foreign.National. Now redo your working for your best model.

# Variables we can use 
# ID # Checking.Acct  ,Credit.History  ,Loan.Reason  ,Savings_Acct ,Employment  ,Personal_Status
# Housing ,Job.Type ,Foreign.National  ,Months.since.Checking.Acct.opened 
# Residence.Time.In.current.district, Age                               
# Credibility_score ,check                             
# Reg.State,Credit.Standing                  

# Revising the training data to exclude these variables
revised_training_data_GDPR = training_data[, !(names(training_data) %in% c("Age","check","Foreign.National", "Personal_Status"))]

# Rebuilding the random forest model 
set.seed(226)
random_forest_model_GDPR = randomForest(Credit.Standing ~ .,
                                        data = revised_training_data_GDPR,
                                        ntree = 500)

str(revised_training_data_GDPR)

# print the random forest
print(random_forest_model_GDPR)

# Getting the importance 
importance(random_forest_model_GDPR)

# importance(random_forest_model_GDPR)

# High Importance Variables
# Credit.History                            79.628734
# Credibility_score                         69.549314
#Employment                                24.268289

# Moderate Importance
# Loan.Reason                               24.232994
# Months.since.Checking.Acct.opened         21.358502
# Checking.Acct                             12.484563
# Savings_Acct                              13.120680
# Residence.Time.In.current.district        12.502108
# Job.Type                                  10.489516
# Savings_Acct                              13.120680

# Lower importance variables
# Housing                                    9.571953
# Reg.State                                  6.394369
# ID                                        31.328089, not meaningful


# New formula using important variables:

revised_formula_for_GDPR = Credit.Standing ~ Credit.History + Credibility_score +
  Employment + Loan.Reason + Months.since.Checking.Acct.opened

revised_random_forest_model_GDPR = randomForest(revised_formula_for_GDPR,
                                                data = revised_training_data_GDPR,
                                                ntree = 500)

# Printing out the model
print(revised_random_forest_model_GDPR)

# Getting the importance 
importance(revised_random_forest_model_GDPR)

# Testing this new GDPR model, cross fold validation, bootstrapping, manual subsets

# Cross Validation
# setting the seed
set.seed(226) 

# Cross-validation with 10 folds
train_control_cv = trainControl(method = "cv", number = 10)

# Training the model using cross-validation
rf_model_cv = train(as.formula(revised_formula_for_GDPR), data = revised_training_data_GDPR, 
                    method = "rf", trControl = train_control_cv, ntree = 500)

# Print the model summary
print(rf_model_cv)


# Bootstrapping 
training_control_boot = trainControl(method = "boot", number = 50)

# Training the model using bootstrap sampling on the GDPR-compliant dataset
rf_model_boot_gdpr = train(as.formula(revised_formula_for_GDPR), data = revised_training_data_GDPR, 
                           method = "rf", trControl = training_control_boot, ntree = 500)

# printing the model
print(rf_model_boot_gdpr)


# Using manual subsets
# Creating a new training set (70%) and a validation set (30%) from the GDPR-compliant dataset
index = sample(1:nrow(revised_training_data_GDPR), 0.7 * nrow(revised_training_data_GDPR))
new_train_set_gdpr = revised_training_data_GDPR[index, ]
new_validation_set_gdpr = revised_training_data_GDPR[-index, ]

# Training the model on the new GDPR-compliant training set
rf_model_new_gdpr = randomForest(as.formula(revised_formula_for_GDPR), data = new_train_set_gdpr, ntree = 500)

# Validating the model on the new GDPR-compliant validation set
predictions_new_gdpr = predict(rf_model_new_gdpr, new_validation_set_gdpr)
confusionMatrix(predictions_new_gdpr, new_validation_set_gdpr$Credit.Standing)


# importance
importance(rf_model_new_gdpr)

#MeanDecreaseGini
# Credit.History                            68.29687
# Credibility_score                         74.50100
# Employment                                25.80133
# Loan.Reason                               21.64257
# Months.since.Checking.Acct.opened         25.13145




# Testing the model on unseen test set
# Firstly I need to remove the variables foreign national, Age, Personal Status from test data
# Test set must match structure of the training set
test_data_GDPR = testing_set[, !(names(testing_set) %in% c("Age", "check" ,"Personal_Status", "Foreign.National"))]

factor_columns = c("Checking.Acct", "Credit.History", "Loan.Reason", "Savings_Acct", 
                   "Employment", "Housing", "Job.Type", "Reg.State", "Credit.Standing")

test_data_GDPR[factor_columns] = lapply(test_data_GDPR[factor_columns], factor)


# View the new test data to make sure that these variables have been successfully removed
view(test_data_GDPR)


# Aligning the factor levels for 'Loan.Reason'
levels(test_data_GDPR$Loan.Reason) = levels(revised_training_data_GDPR$Loan.Reason)

# Aligning the factor levels for 'Employment'
levels(test_data_GDPR$Employment) = levels(revised_training_data_GDPR$Employment)

# After aligning the levels, try making predictions again
test_set_predictions_GDPR = predict(rf_model_new_gdpr, test_data_GDPR)




# Evaluating the predictions using a confusion matrix
confusion_matrix_test_GDPR = confusionMatrix(test_set_predictions_GDPR, test_data_GDPR$Credit.Standing)

# Print the confusion matrix and model evaluation metrics
print(confusion_matrix_test_GDPR)


# and your test set is named 'test_data_GDPR'
# After aligning the levels, try making predictions again
test_set_predictions_GDPR = predict(rf_model_new_gdpr, test_data_GDPR)

print(test_set_predictions_GDPR)

# Calculate the number of correct predictions
correct_predictions = sum(test_set_predictions_GDPR == test_data_GDPR$Credit.Standing)

# Calculate the total number of predictions
total_predictions = length(test_set_predictions_GDPR)

# Calculate the accuracy
accuracy = correct_predictions / total_predictions

# Print the accuracy
cat("Accuracy on test data:", accuracy, "\n")

# Accuracy on test data: 0.7743363 










########## FEATURE ENGINEERING TO IMPROVE THE MODEL


# To improve this accuracy I wanted to perform interaction / feature engineering

# Grouped Credit History into broader categories 
# Delay and Critical - high risk
# All paid, Current, Bank Paid - low risk
revised_training_data_GDPR$Categorized_Credit_History <- ifelse(revised_training_data_GDPR$Credit.History %in% c("Delay", "Critical"), 
                                                                "High Risk", 
                                                                "Low Risk")

# Check the first few entries of the new variable
head(revised_training_data_GDPR$Categorized_Credit_History)

# Investigating the distribution and outlier's

hist(revised_training_data_GDPR$Credibility_score, main = "Distribution of Credibility Score")

# Boxplot to check for any outliers
boxplot(revised_training_data_GDPR$Credibility_score, main="Credibility Score Box Plot")

# Binning Months.since.Checking.Acct.opened
revised_training_data_GDPR$Checking_Acct_Age_Grouped = cut(revised_training_data_GDPR$Months.since.Checking.Acct.opened, 
                                                           breaks = c(-Inf, 12, 24, Inf), 
                                                           labels = c("New", "Medium", "Old"))

# Grouping Loan.Reason categories
revised_training_data_GDPR$Loan_Reason_Grouped = ifelse(revised_training_data_GDPR$Loan.Reason %in% c("Car New", "Car Used"), 
                                                        "Car Loan", 
                                                        "Other Loan")

# Interaction between Credibility_score and Employment
revised_training_data_GDPR$Credibility_Employment_Interaction = with(revised_training_data_GDPR, 
                                                                     Credibility_score * as.numeric(Employment))

# New formula with engineered features
new_formula_GDPR = Credit.Standing ~ Categorized_Credit_History + Credibility_score +
  Loan_Reason_Grouped + Checking_Acct_Age_Grouped + 
  Credibility_Employment_Interaction

# Rebuilding the model
new_rf_model_GDPR = randomForest(as.formula(new_formula_GDPR), 
                                 data = revised_training_data_GDPR, 
                                 ntree = 500)

# Training the model 
train_predictions_GDPR = predict(new_rf_model_GDPR, revised_training_data_GDPR)
train_confusion_matrix_GDPR = confusionMatrix(train_predictions_GDPR, revised_training_data_GDPR$Credit.Standing)
print(train_confusion_matrix_GDPR)



# Applying the same feature engineering to the test data
test_data_GDPR$Categorized_Credit_History <- ifelse(test_data_GDPR$Credit.History %in% c("Delay", "Critical"), 
                                                    "High Risk", 
                                                    "Low Risk")

# Assuming that Loan.Reason and Employment are already factorized and their levels are aligned with the training data

test_data_GDPR$Checking_Acct_Age_Grouped = cut(test_data_GDPR$Months.since.Checking.Acct.opened, 
                                               breaks = c(-Inf, 12, 24, Inf), 
                                               labels = c("New", "Medium", "Old"))

test_data_GDPR$Credibility_Employment_Interaction = with(test_data_GDPR, 
                                                         Credibility_score * as.numeric(Employment))

# Grouping Loan.Reason categories
test_data_GDPR$Loan_Reason_Grouped = ifelse(test_data_GDPR$Loan.Reason %in% c("Car New", "Car Used"), 
                                            "Car Loan", 
                                            "Other Loan")

# Making predictions on the test data
test_set_predictions_GDPR = predict(new_rf_model_GDPR, test_data_GDPR)

# Evaluate the predictions on test data
test_confusion_matrix_GDPR = confusionMatrix(test_set_predictions_GDPR, test_data_GDPR$Credit.Standing)

# Print the confusion matrix and model evaluation metrics
print(test_confusion_matrix_GDPR)




#j) Siobhán’s company uses a process that is a mixture of a grading system and human input to grade 
# each past loan as good or bad. Siobhán is suspicious that during a particular time that this process 
# performed poorly. The ID numbers can be taken as time stamp values. Develop a strategy to find a
# series of consecutive ID numbers, i.e. where these gradings show a higher than normal pattern of 
# suspiciously incorrect or correct gradings. Detail how you go about your investigation.

# Use the validation set for actuals and ID
actuals = new_validation_set_gdpr$Credit.Standing
ids = new_validation_set_gdpr$ID

# Combine ID, actual grades, and predicted grades
combined_data = data.frame(ID = ids, Actual = actuals, Predicted = predictions)

# Verify the combined data
head(combined_data)

# Define the segment size (e.g., every 50 IDs)
segment_size = 50

# Function to calculate accuracy in a segment
calculate_accuracy = function(segment) {
  sum(segment$Actual == segment$Predicted) / nrow(segment)
}

# Split data into segments based on ID and calculate accuracy for each segment
segment_accuracies = sapply(split(combined_data, (combined_data$ID - 1) %/% segment_size), calculate_accuracy)

# Plot segment accuracies for visual analysis
plot(segment_accuracies, type = "o", xlab = "Segment", ylab = "Accuracy", main = "Grading Accuracy per Segment")


#####

# First, check if the 'Actual' and 'Predicted' columns exist in 'combined_data'
if("Actual" %in% names(combined_data) && "Predicted" %in% names(combined_data)) {
  # If they do, create the 'Accuracy' column
  combined_data$Accuracy = ifelse(combined_data$Actual == combined_data$Predicted, "Correct", "Incorrect")
} else {
  stop("The 'Actual' and/or 'Predicted' columns do not exist in 'combined_data'.")
}

# I created the 'Segment' column if it doesn't exist
if(!"Segment" %in% names(combined_data)) {
  # Defined segment size
  segment_size = 50  # Adjust as needed
  
  # Created the 'Segment' column based on 'ID'
  combined_data$Segment = cut(combined_data$ID, breaks=seq(from=min(combined_data$ID, na.rm=TRUE), to=max(combined_data$ID, na.rm=TRUE), by=segment_size), include.lowest=TRUE, labels=FALSE)
  
  # If the cut function results in NAs because of missing values, handle them
  if(any(is.na(combined_data$Segment))) {
    combined_data = na.omit(combined_data)
  }
} else {
  # If 'Segment' column exists, ensure it's numeric and has no missing values
  combined_data$Segment = as.numeric(as.character(combined_data$Segment))
  combined_data = na.omit(combined_data)
}

# attemptting to create the 'observed' table again
observed = table(combined_data$Segment, combined_data$Accuracy)

# Check the structure of the 'observed' table
if(dim(observed)[1] == 0 || dim(observed)[2] == 0) {
  stop("The 'observed' table is empty. Check the 'Segment' and 'Accuracy' columns.")
} else {
  print(observed)
}

# Assuming 'observed' is no longer empty, you can proceed with the chi-squared test
if(all(observed > 0)) {
  chi_squared_results <- chisq.test(observed)
  print(chi_squared_results)
} else {

}

# Run the chi-squared test to get the expected frequencies
chi_squared_results = chisq.test(observed)

# Print the expected frequencies
print(chi_squared_results$expected)

# If any expected frequencies are less than 5, consider combining adjacent segments
# or using a different approach to assess the significance of the observed distribution


# Look at the standardized residuals
print(chi_squared_results$residuals)





# TRYING ANOTHER APPROACH 

# Trying another approach 
#1. Time-Series Analysis Techniques
#Trend Analysis: Identify overall trends in grading accuracy over time. For this, 
#you could plot the accuracy over time (ID as a proxy for time) to see if there's an increasing or decreasing trend.

#Seasonality Analysis: Check if there's a repeating pattern at regular intervals.
#For instance, certain times of the year might show different grading patterns.

#Autocorrelation Analysis: This involves checking if the grading accuracy at a given point in time is 
#correlated with its past values. It can help identify patterns that are not immediately apparent.

#2. Exploring Consecutive IDs for Patterns
#Moving Averages: Calculate the moving average of grading accuracy over a set number of consecutive IDs. 
#This smoothens short-term fluctuations and highlights longer-term trends or cycles.

#Change Point Detection: Identify points where there's a significant shift in the data. 
#This could be where the grading accuracy suddenly changes, indicating a potential issue in the grading process.

#Segmentation with Variable Window Sizes: Instead of fixed-size segments, 
#use variable lengths based on certain criteria, like a significant change in grading accuracy.

# Check if 'Segment' column exists and is numeric
if(!"Segment" %in% names(combined_data) || !is.numeric(combined_data$Segment)) {
  # Define the segment size
  segment_size = 50  # Adjust as needed
  
  # Create the 'Segment' column based on 'ID'
  combined_data$Segment = cut(combined_data$ID, breaks = seq(from = min(combined_data$ID, na.rm = TRUE), 
                                                              to = max(combined_data$ID, na.rm = TRUE), 
                                                              by = segment_size), 
                               include.lowest = TRUE, 
                               labels = FALSE)
  # Convert 'Segment' to numeric
  combined_data$Segment <- as.numeric(as.character(combined_data$Segment))
}

# Remove rows with NAs
combined_data = na.omit(combined_data)

# Compute accuracy for each segment
combined_data$IsCorrect = combined_data$Actual == combined_data$Predicted
segment_accuracy = aggregate(IsCorrect ~ Segment, data = combined_data, FUN = mean)

# Create the time series object
accuracy_over_time = ts(segment_accuracy$IsCorrect, start = min(segment_accuracy$Segment), frequency = 20)

# Plot the time series
plot(accuracy_over_time, type = "o", xlab = "Segment", ylab = "Accuracy", main = "Grading Accuracy Over Time")



































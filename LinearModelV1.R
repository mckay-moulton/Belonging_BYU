#MODELING

# packages
library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(openxlsx)
library(ggplot2)
library(ggcorrplot)
library(digest)

# Bring in the data
# This is from Renaming_Location_Cleaning Script
file <- "C:\\Users\\McKay Moulton\\Documents\\MISM_2nd\\Belonging\\outputs\\surveys_with_model_prep_recordedV1.csv"
data_to_model <- read.csv(file)

names(data_to_model)


# Load additional packages
library(tidyr)
library(caret)
library(fastDummies)


# only keep the short-list columns:
data_to_model <- data_to_model %>%
  select(duration_in_seconds, where_were_you, who_were_you_with, bipoc, first_gen, low_income, comparison_group, hour, week_num, days_elapsed, while_engaged_in_this_activity_did_you_feel_like_you_belonged, current_mood_rating_for_isolated_socially_connected)

#*********************CHANGE LABEL HERE IF NEEDED*************************
# the idea is to use "label" for each iteration so I don't have to update more of the code later on
data_to_model <- data_to_model %>%
  mutate(label = current_mood_rating_for_isolated_socially_connected) %>%
  select(-c(current_mood_rating_for_isolated_socially_connected))



# Pre-process the data
# Replace NA values in numeric columns with the mean
numeric_columns <- c("duration_in_seconds", "hour", "week_num", "days_elapsed")
data_to_model[numeric_columns] <- lapply(data_to_model[numeric_columns], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Dummify categorical variables
categorical_columns <- c("where_were_you", "who_were_you_with", "comparison_group")
data_to_model <- dummy_cols(data_to_model, select_columns = categorical_columns, remove_selected_columns = TRUE)

glimpse(data_to_model)
# Remove rows with NA values in the remaining categorical columns
remaining_categorical <- c("bipoc", "first_gen", "low_income")
data_to_model <- data_to_model[complete.cases(data_to_model[remaining_categorical]),]

# Identify categorical columns with only one level
single_level_cols <- sapply(data_to_model, function(x) is.factor(x) & length(levels(x)) < 2)

# Remove these columns from the data
data_to_model <- data_to_model[, !single_level_cols]


data_to_model <- data_to_model %>%
  select(-c(comparison_group_0))  # don't need it since we have the three other groups


# Make the label binary 
#Ok, so we are going to recode the label to be binary, 

# 1 and 2 will be 0 (feel like they don't belong') 
data_to_model <- data_to_model %>%
  mutate(label = ifelse(label == 1 | label == 2 | label == 3,0, label))

# 3 and 4 will be 1 (feel like they belong)
data_to_model <- data_to_model %>%
  mutate(label = ifelse(label == 4 | label == 5, 1, label))


# Build the logistic regression model (not linear since its binary)
model <- glm(label ~ ., data = data_to_model, family = "binomial")

# Summary of the model
summary(model)



# test for multicolliniarity
# library(car)
# vif(model)
# 
# 
# library(ggcorrplot)
# multi_view <- cor(data_to_model[, -1])
# 
# 
# library(corrplot)
# multi_view <- cor(data_to_model[, -1])
# corrplot(multi_view, method = "pairwise.complete.obs", type = "upper", order = "hclust", addCoef.col = "blue", tl.col = "black")



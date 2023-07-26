# MODELING
#This model is the same as 'CategoricalModelV2_WITH_EMOTIONS', expect that I made the label binary instead of 1-4
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
library(tidymodels)
tidymodels_prefer()
library(readr)
library(yardstick)
library(tidypredict)
library(yaml)
library(fs)
library(lightgbm)
library(xgboost)
library(bonsai)
library(parsnip)
library(parameters)
library(vip) # to evaluate model
# Register parallel backend
library(doParallel)
library(foreach)

cores <- parallel::detectCores()
cl <- makeCluster((cores)-2)
registerDoParallel(cl)

# Bring in the data
# This is from Renaming_Location_Cleaning Script
file <- "C:\\Users\\McKay Moulton\\Documents\\MISM_2nd\\Belonging\\outputs\\surveys_with_model_prep_recordedV1.csv"
data_to_model <- read.csv(file)

#old version  (to easily tell the difference)
data_to_model <- data_to_model %>%
  select(duration_in_seconds, where_were_you, who_were_you_with, bipoc, first_gen, low_income, comparison_group, hour, week_num, days_elapsed, while_engaged_in_this_activity_did_you_feel_like_you_belonged)

# only keep the short-list columns to start
# data_to_model <- data_to_model %>%
#   select(duration_in_seconds, where_were_you, who_were_you_with, bipoc, first_gen, low_income, 
#          comparison_group, hour, week_num, days_elapsed, 
#          14:45)
# names(data_to_model)   make sure expected columns are there

# Get the count of NA's for the label to help decide strategy for Na values
# na_count <- sum(is.na(data_to_model$while_engaged_in_this_activity_did_you_feel_like_you_belonged))
# na_count = 59
# Conclusion, just drop em
data_to_model <- data_to_model[!is.na(data_to_model$while_engaged_in_this_activity_did_you_feel_like_you_belonged), ]

# also drop this unwanted (and unclean column)
# data_to_model <- data_to_model %>%
#   select(-c(what_other_things_were_you_doing))
# doing these manually before the recipe to make sure I have control over it
# Pre-process the NUMERIC data

# ***********took out after making recipe**********
# Replace NA values in numeric columns with the mean
# numeric_columns <- c("duration_in_seconds", "hour", "week_num", "days_elapsed")
# data_to_model[numeric_columns] <- lapply(data_to_model[numeric_columns], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Dummify the most important categorical variables in case the recipe muddles it up. Every other column should be strictly numeric 
# categorical_columns <- c("where_were_you", "who_were_you_with", "comparison_group")
# data_to_model <- dummy_cols(data_to_model, select_columns = categorical_columns, remove_selected_columns = TRUE)

# Identify categorical columns with only one level
# single_level_cols <- sapply(data_to_model, function(x) is.factor(x) & length(levels(x)) < 2)

# Remove these columns from the data
# data_to_model <- data_to_model[, !single_level_cols]
# remove 0 variance column
#______________________________________________

# Remove rows with NA values in the remaining categorical columns, we don't want to impute with these
remaining_categorical <- c("bipoc", "first_gen", "low_income")
data_to_model <- data_to_model[complete.cases(data_to_model[remaining_categorical]),]




#1 Prepare data for XGBoost

#*********************CHANGE LABEL HERE IF NEEDED*************************

# the idea is to use "label" for each iteration so I don't have to update more of the code later on
data_to_model <- data_to_model %>%
  mutate(label = while_engaged_in_this_activity_did_you_feel_like_you_belonged) %>%
  select(-c(while_engaged_in_this_activity_did_you_feel_like_you_belonged))


#Ok, so we are going to recode the label to be binary, 

# 1 and 2 will be 0 (feel like they don't belong') 
data_to_model <- data_to_model %>%
  mutate(label = ifelse(label == 1 | label == 2, 0, label))

# 3 and 4 will be 1 (feel like they belong)
data_to_model <- data_to_model %>%
  mutate(label = ifelse(label == 3 | label == 4, 1, label))

#convert to factor
data_to_model <- data_to_model %>%
  mutate(label= as.factor(label))

# Train/Test Split________________________________________________
set.seed(12)
data_split <- initial_split(data_to_model, strata = label)

data_training <- data_split %>% training()
data_testing <- data_split %>% testing()

#I'm having some factor or dummify issues, to trying to respolve it
data_training[remaining_categorical] <- lapply(data_training[remaining_categorical], as.factor)
data_testing[remaining_categorical] <- lapply(data_testing[remaining_categorical], as.factor)

# 2. Feature Engineering Recipes________________________________________________
recipe_1 <- recipe(label ~ ., 
                   data = data_training) %>% 
  step_impute_mean(all_numeric_predictors()) %>%   # mean imputations
  step_YeoJohnson(all_numeric_predictors()) %>%    
  step_zv(all_numeric_predictors()) %>% # add this line to remove zero-variance columns
  step_normalize(all_numeric_predictors()) %>%  # normalize w/ z-score
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors()) %>% # remove columns with no variation
  step_corr(all_numeric_predictors()) # removes columns with too much correlation based on default threshold


# 3. Model Algorithm Specifications________________________________________________
model_spec_1 <- boost_tree() %>%
  set_engine("xgboost") %>% 
  set_mode('classification')

#4. Workflow Object(s)________________________________________________
# just one for one
r1_m1_wkfl <- workflow() %>% 
  add_model(model_spec_1) %>% 
  add_recipe(recipe_1)



# 5. Initial Cross-Validated Results and define metrics___________________
set.seed(12)

data_folds <- vfold_cv(data_training, v = 10, strata = label)

custom_metrics <- metric_set(
  roc_auc,
  accuracy,
  precision,
  recall,
  f_meas # F1-score
)

r1_m1_fit <- fit_resamples(
  r1_m1_wkfl,
  resamples = data_folds,
  metrics = custom_metrics
)
show_notes(r1_m1_fit)

r1_m1_metrics <- r1_m1_fit %>% collect_metrics()
workflow_perf_summary <- r1_m1_metrics %>% mutate(workflow_name = "r1_m1_wkfl", algorithm_name = "boost_tree", engine_name = "xgboost")
workflow_perf_summary


# 6 Algorithm (Workflow) Selection________________________________________________
best_initial_workflow_fit <- r1_m1_fit

# 7 Hyperparameter Tuning________________________________________________
set.seed(12)

# basic specs
# model_spec_tunable<- boost_tree(tree_depth=tune(), learn_rate=tune()) %>% 
#   set_engine('xgboost') %>% set_mode('classification')

# more intensive specs V2
model_spec_tunable <- boost_tree(
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Create a tuning grid using parameters::grid_random()
# Create tuning grid
tuning_grid <- grid_random(
  extract_parameter_set_dials(model_spec_tunable),
  size = 1000  # increase size to sample more parameter combinations
)

# basic tuning grid
# tuning_grid <- grid_random(
#   hardhat::extract_parameter_set_dials(model_spec_tunable),
#   size = 50)


# TUNING GRID EXTREME V2
# Create a tuning grid using grid_random()
tuning_grid <- grid_random(
  extract_parameter_set_dials(model_spec_tunable),
  size = 250,  #tried up to 1000 but no difference in the metrics
  max_parallel = 6  
)




tunable_wkfl <-workflow() %>% 
  add_model(model_spec_tunable) %>% 
  add_recipe(recipe_1)

set.seed(12)
tunable_fit <- tunable_wkfl %>%
  tune_grid(resamples = data_folds,
            grid = tuning_grid,
            control = control_grid(verbose = TRUE))  # verbose = TRUE to display progress)



# Stop the cluster
stopCluster(cl)

# 8. Finalize your Model________________________________________________
set.seed(12)
tunable_fit %>% show_best(metric='roc_auc')
best_parameters <- tunable_fit %>% select_best(metric='roc_auc')

finalized_wkfl <- tunable_wkfl %>% 
  finalize_workflow(best_parameters)

final_fit <- finalized_wkfl %>% 
  last_fit(split = data_split)

# 9. Summarize Model Performance
final_fit %>% collect_metrics()
best_initial_workflow_fit %>% collect_metrics()


test_results <- final_fit %>% collect_predictions()


# 10 Evaulate______________________________________
# Calculate TN, FN, TP, and FP
confusion_matrix <- test_results %>%
  group_by(label, .pred_class) %>%
  summarise(count = n()) %>%
  spread(.pred_class, count, fill = 0)

confusion_matrix

TN <- confusion_matrix$`0`[confusion_matrix$label == "0"]
FN <- confusion_matrix$`1`[confusion_matrix$label == "0"]
TP <- confusion_matrix$`1`[confusion_matrix$label == "1"]
FP <- confusion_matrix$`0`[confusion_matrix$label == "1"]

# Calculate TNR (specificity)
TNR <- TN / (TN + FP)
TNR  #The TNR variable is the percentage of predictions for '0' that were actually '0'.

# LOOK DEEPER AT MODEL
# Get the xgboost model (class xgb.Booster) from the final_fit object
xgb_model <- extract_fit_parsnip(final_fit)$fit

# Calculate feature importance
feature_importance <- xgb.importance(model = xgb_model)

# Plot feature importance
xgb.plot.importance(feature_importance)



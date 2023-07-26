#MODELING PREP

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
file <- "C:\\Users\\McKay Moulton\\Documents\\MISM_2nd\\Belonging\\outputs\\surveys_names_cleaned_2.csv"
data_to_prep <- read.csv(file)

# change dates to week #
# we have a start date, and end date, and recorded date, which appears to be the same as end-date
# lets remove the start and end date, and only use the recorded date
data_to_prep <- data_to_prep %>% select(-c(start_date, end_date))

data_to_prep <- data_to_prep %>% 
  mutate(recorded_date = as.POSIXct(recorded_date, format = "%m/%d/%Y %H:%M"))



# Idk if the hours will be useful or not, but it might be so lets have a seperate data and hour column
data_to_prep <- data_to_prep %>% 
  mutate(hour = hour(round_date(as.POSIXct(recorded_date), unit = "hour")),
         date = as.Date(recorded_date))




# decide what to do with this later
# maybe look at a count of values for each unique day
dates <- data_to_prep %>% 
  group_by(date) %>% 
  summarize(count = n())


#In this case, since your end goal is to predict the biggest factors in how a person responds to a quantitative question about their sense of belonging, it is essential to consider how the date information might influence this response. Here are some possible approaches:

# Group by week: Using a week number in the model can help capture any weekly patterns or trends in the data. This approach can help account for fluctuations due to day of the week, weekends, or holidays. However, this method might lose some information if the exact day is important in understanding the response.
# 
# Days_elapsed: This approach maintains the continuous nature of the data, preserving the information about the exact day. This method can be useful if there are specific events or trends related to the date that you want to capture.
# 
# Date features: Instead of grouping by week or using days_elapsed, you can create multiple features based on the date, such as day of the week, day of the month, or month. This approach allows the model to capture different aspects of the date information, which might be relevant to the response.
# 
# Ultimately, the best approach will depend on the nature of the data and the relationships between the date and the target variable. You might consider trying multiple representations and comparing their performance in the model. 

# create both a week and days_elapsed column to give the model
data_to_prep <- data_to_prep %>%
mutate(week_num = as.numeric(floor(as.numeric(difftime(date, min(date), units = "weeks")))) + 1,
       days_elapsed = as.numeric(difftime(date, min(date), units = "days")))



#*****************************************DEAL WITH NA's************************
# using a mean impute seems unecessary given than most rows are completely filled out.
# Let's find out, among our numeric columns, which values are NA

# summarise the count of NAs for each column
na_summary <- data_to_prep %>%
summarise_at(vars(30:46), list(count_NA = ~sum(is.na(.))))  


# there's a lot of NA's for 'competitive_cooperative' so drop it
data_to_prep <- data_to_prep %>%
select(-current_mood_rating_for_competitive_cooperative)

# We also want to change the number system 
# i think in the code it will actually need to be flipped around

# RECODING
# for columns 30 through 46 (current_mood_for___), I want to recode the following values, since the Qualtrics survey outputted them weird 
# NOTE: Confirmed assumptions with Josie after verifiying individual reponse isntances were correct with assumptions below
# NA --gone no response
# 1 should be 1
# 2 should be 2 
# 6 should be 3
# 7 should be 4
# 8 should be 5


data_to_prep <- data_to_prep %>%
  mutate_at(vars(30:46), ~case_when(
    . == 1 ~ 1,
    . == 2 ~ 2,
    . == 6 ~ 3,
    . == 7 ~ 4,
    . == 8 ~ 5,
    TRUE ~ NA_real_
  ))

# also need to recode the who_were_you_with, which is currently numeric (how Qualtrics outputted it)
data_to_prep <- data_to_prep %>%
  mutate(who_were_you_with = case_when(
    who_were_you_with == 1 ~ "Close_Friend",
    who_were_you_with == 2 ~ "Acquaintance",
    who_were_you_with == 3 ~ "Roommate",
    who_were_you_with == 4 ~ "Alone",
    who_were_you_with == 5 ~ "Strangers",
    who_were_you_with == 6 ~ "Immediate_Family",
    who_were_you_with == 7 ~ "Coworkers",
    who_were_you_with == 8 ~ "BYU_Faculty_Member",
    # 9 (TA) I made into 10 since there was only a couple responses
    who_were_you_with == 10 ~ "Other"
  ))


# export that to excel
write.csv(data_to_prep, file = "C:\\Users\\McKay Moulton\\Documents\\MISM_2nd\\Belonging\\outputs\\surveys_with_model_prep_recordedV1.csv", row.names = FALSE)



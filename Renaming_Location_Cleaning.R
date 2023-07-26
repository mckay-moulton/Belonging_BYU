library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(openxlsx)
library(ggplot2)
library(ggcorrplot)
library(digest)

file <- "C:\\Users\\McKay Moulton\\Documents\\MISM_2nd\\Belonging\\outputs\\surveys_names_reversed_.csv"
surveys_reversed <- read.csv(file)

#******************************* anonamize students names*************************
# surveys_reversed$anon_name <- digest(surveys_reversed$student_names)
# # replace student col with new col
# surveys_reversed$student_names <- surveys_reversed$anon_name
# surveys_reversed <- select(surveys_reversed, -anon_name)

# rename column name to be more accurate
surveys_reversed <- surveys_reversed %>% 
  rename(student_id = student_names)


# Where were you clean up
# lowercase eveything
surveys_reversed$where_were_you <- tolower(surveys_reversed$where_were_you)

locations_1 <- surveys_reversed %>% 
  group_by(where_were_you) %>% 
  summarize(count = n())

# create a vector of substrings to shorten long responses to one-two words
replace_strings <- c("home", "church", "class", "bed", "desk", "my apartment", "library", "walking", "word", "bus", "car", "eating", "campus")

# loop through each row of the tibble
for (i in 1:nrow(surveys_reversed)) {
  # loop through the vector of substrings to replace
  for (s in replace_strings) {
    # if the current value contains the substring anywhere, replace the entire string with the substring
    if (grepl(s, surveys_reversed$where_were_you[i], ignore.case = TRUE)) {
      surveys_reversed$where_were_you[i] <- s
      break  # exit the loop once a replacement has been made
    }
  }
}



locations_2 <- surveys_reversed %>% 
  group_by(where_were_you) %>% 
  summarize(count = n())


# create a vector of family-related words
family_words <- c("husband", "wife", "grandma", "grandpa", "aunt", "uncle", "brother", "sister", "mom", "dad", "parents house", "parent", "friend", "boyfriend", "roomate", "fhe activity")

# loop through each row of the tibble
for (i in 1:nrow(surveys_reversed)) {
  # loop through the vector of family-related words
  for (w in family_words) {
    # if the current value contains the word or any variation of it, replace the entire string with "with_family"
    if (grepl(w, surveys_reversed$where_were_you[i], ignore.case = TRUE)) {
      surveys_reversed$where_were_you[i] <- "Family_or_Aquantance"
      break  # exit the loop once a replacement has been made
    }
  }
}

locations_3 <- surveys_reversed %>% 
  group_by(where_were_you) %>% 
  summarize(count = n())


# create a vector of location-related words
location_words <- c("jfsb", "kmbl", "jsb", "rel", "tanner", "jkb", "hfac", "hbll", "hist", "lab", "lsb", "marriott center", "marb", "snell", "testing center", "byu", "rb", "wsc", "wilk", "wlk", "library", "class", "101")

# loop through each row of the tibble
for (i in 1:nrow(surveys_reversed)) {
  # loop through the vector of location-related words
  for (w in location_words) {
    # if the current value contains the word, replace the entire string with "campus"
    if (grepl(w, surveys_reversed$where_were_you[i], ignore.case = TRUE)) {
      surveys_reversed$where_were_you[i] <- "campus/class"
      break  # exit the loop once a replacement has been made
    }
  }
}

locations_4 <- surveys_reversed %>% 
  group_by(where_were_you) %>% 
  summarize(count = n())

# create a vector of room-related words
room_words <- c("my couch", "my room", "my kitchen", "my living room", "my apt", "my house", "living room")

# loop through each row of the tibble
for (i in 1:nrow(surveys_reversed)) {
  # loop through the vector of room-related words
  for (w in room_words) {
    # if the current value contains the word, replace the entire string with "my apartment"
    if (grepl(w, surveys_reversed$where_were_you[i], ignore.case = TRUE)) {
      surveys_reversed$where_were_you[i] <- "my apartment"
      break  # exit the loop once a replacement has been made
    }
  }
}
locations_5 <- surveys_reversed %>% 
  group_by(where_were_you) %>% 
  summarize(count = n())

# change "at work" to "work"
# loop through each row of the tibble
for (i in 1:nrow(surveys_reversed)) {
  # if the current value contains the phrase "at work" anywhere, replace the entire string with "work"
  if (grepl("at work", surveys_reversed$where_were_you[i], ignore.case = TRUE)) {
    surveys_reversed$where_were_you[i] <- "work"
  }
}
# Modify the 'where_were_you' column for bed - my apartment
# since we've already grouped values with friend or family terms, apartment is most likely the same as "my apartment"
surveys_reversed <- surveys_reversed %>%
  mutate(where_were_you = case_when(
    where_were_you == "bed" ~ "my apartment",
    where_were_you == "apartment" ~ "my apartment",
    TRUE ~ where_were_you
  ))
# do the same for "car", "walking", and d"riving", "on the train"
surveys_reversed <- surveys_reversed %>%
  mutate(where_were_you = case_when(
    where_were_you == "car" ~ "in_transit",
    where_were_you == "walking" ~ "in_transit",
    where_were_you == "driving" ~ "in_transit",
    TRUE ~ where_were_you
  ))
locations_6 <- surveys_reversed %>% 
  group_by(where_were_you) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

# change everything else to "other"
#replace every value in the 'count' column below 28 with "Other".
location_keepers <- c(
  "campus/class",
  "my apartment",
  "home",
  "work",
  "Family_or_Aquantance",
  "in_transit",
  "campus",
  "church"
)

surveys_reversed <- surveys_reversed %>%
  mutate(where_were_you = case_when(
    where_were_you %in% location_keepers ~ where_were_you,
    TRUE ~ "other"
  )) %>%
  mutate(where_were_you = tolower(where_were_you))




#_________________________________________________________________________________________
# Clean up 'who_were_you_with'
# first remove the mostly useless text columns that just expand on whats answered in col 52
surveys_reversed <- surveys_reversed %>%
  select(-(53:57))

# also the sentiment scores
surveys_reversed <- surveys_reversed %>%
  select(-(54:61))

#couple stragglers
surveys_reversed <- surveys_reversed %>%
  select(-(52:53))

#should be down to 50 columns

# now lets remove rows that took over 5 hours to finish
surveys_reversed <- surveys_reversed %>%
  filter(duration_in_seconds <= 2000)

#1250 before  #1201 after


# remove surveys that were not finished
surveys_reversed <- surveys_reversed %>%
  filter(finished == 1)
# only removed 1 record


#************************************COMPANY*******************
# Print the column names
column_names <- colnames(surveys_reversed)
print(column_names)
#*That's a nasty column name, so lets change it
# Rename the column
surveys_reversed <- surveys_reversed %>%
  rename(who_were_you_with = who_were_you_with_when_you_received_this_survey_select_all_that_apply_selected_choice)

#*
company1 <- surveys_reversed %>% 
  group_by(who_were_you_with)%>% 
  summarize(count = n())


# just the 1s to start
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = case_when(
    who_were_you_with == "1,2" ~ "1",
    who_were_you_with == "1,5" ~ "1",
    who_were_you_with == "1,7" ~ "1",
    who_were_you_with == "1,2,3" ~ "1",
    who_were_you_with == "1,2,3,5,6" ~ "1",
    who_were_you_with == "1,3" ~ "1",
    TRUE ~ who_were_you_with
  ))



company2 <- surveys_reversed %>% 
  group_by(who_were_you_with) %>% 
  summarize(count = n())

# now the 2's
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = case_when(
    who_were_you_with == "8,9" ~ "8",
    who_were_you_with == "2,8" ~ "8",
    who_were_you_with == "1,7" ~ "7",
    who_were_you_with == "8,10" ~ "8",
    who_were_you_with == "7,8" ~ "8",
    TRUE ~ who_were_you_with
  ))

company3 <- surveys_reversed %>% 
  group_by(who_were_you_with) %>% 
  summarize(count = n())



# if they are with strangers and a teacher (5,8) prioritize the teacher
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = case_when(
    who_were_you_with == "5,8" ~ "8",
    who_were_you_with == "2,6" ~ "6",
    who_were_you_with == "6,10" ~ "6", #if with family and others, then prioritize family
    who_were_you_with == "5,6" ~ "6",  # if with strangers and family, then prioritize family
    who_were_you_with == "2,5" ~ "5", # an acquaintance is a stranger with a name in most campus settings, so prioritize 5
    who_were_you_with == "4,10" ~ "10", # if they are alone and with someone in "other" category, then they are not alone so 10
    who_were_you_with == "4,5" ~ "5", # same as above except with "strangers"
    TRUE ~ who_were_you_with
  ))


# I've decided that the 'friend' column is going to be a key indicator, 
# more than others 'TA' or strangers, when combined with 1. Therefore, I will change every value that contains a '1, ' to be 1. 

surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = ifelse(str_detect(who_were_you_with, "1,"), "1", who_were_you_with))

# next priotize 'family'
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = ifelse(str_detect(who_were_you_with, "6"), "6", who_were_you_with))

# next prioritize 'roommates'
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = ifelse(str_detect(who_were_you_with, "3"), "3", who_were_you_with))

# then coworkers
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = ifelse(str_detect(who_were_you_with, "7"), "7", who_were_you_with))

# then "Other"
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = ifelse(str_detect(who_were_you_with, "10"), "10", who_were_you_with))

# for the small handful who are left (less than 5)
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = ifelse(str_detect(who_were_you_with, "2"), "2", who_were_you_with))

# TA has less than 10, so lets just make that "other" to get rid of the category altogether
surveys_reversed <- surveys_reversed %>%
  mutate(who_were_you_with = ifelse(str_detect(who_were_you_with, "9"), "10", who_were_you_with))



#view
company4 <- surveys_reversed %>% 
  group_by(who_were_you_with) %>% 
  summarize(count = n())

# finally, add a new column that will make it easier to compute and compare survey results across groups

surveys_reversed <- surveys_reversed %>%
  mutate(comparison_group = ifelse(group == "Comparison", 1, 0))


# Export to csv
write.csv(surveys_reversed, file = "C:\\Users\\McKay Moulton\\Documents\\MISM_2nd\\Belonging\\outputs\\surveys_names_cleaned_2.csv", row.names = FALSE)


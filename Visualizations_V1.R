library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(openxlsx)
library(ggplot2)
library(ggcorrplot)


file_path <- "C://Users//McKay Moulton//Documents//MISM_2nd//Belonging//Files_To_Upload//students_with_groups_to_load_V1.xlsx"

# read in data
surveys_with_names_read_in <- read_excel(file_path)

# the names are very long, so I am shortening some of them (move to data prep later I think
surveys_with_names_read_in <- surveys_with_names_read_in %>%
  rename_with(~ gsub("describe_your_mood_when_you_received_this_survey", 
                     "current_mood_rating_for", 
                     .x),
              contains("describe_your_mood_when_you_received_this_survey"))



#*****************VISUALIZATION TIME BABY**********************
# Get counts for "bipoc", "low_income", and "first_gen" columns
counts <- surveys_with_names_read_in %>%
  summarize(bipoc_count = sum(bipoc == 1),
            low_income_count = sum(low_income == 1),
            first_gen_count = sum(first_gen == 1),
            main_count = sum(group == "Main")) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# bipoc_count | low_income_count | first_gen_count | main_count
# <int>             <int>              <int>          <int>
#  466               507                475            817

# Plot the counts (pie chart)
group_total_survey_responses_pie <- ggplot(counts, aes(x = "", y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Distribution of Survey Responses") +
  scale_fill_manual(values = c("#040f5c", "#bc5090", "#ffa600", "#58508d")) +
  labs(fill = "Variable")

counts <- data.frame(variable = c("bipoc", "low_income", "first_gen", "main"),
                     count = c(466, 507, 475, 817))

# Create a bar chart
ggplot(counts, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Survey Responses", x = "Variable", y = "Count") +
  geom_text(aes(label = count), hjust = -0.2, size = 4) +
  coord_flip() +
  theme_minimal()



# Filtered tibble for BIPOC
# bipoc_filtered_surveys <- surveys_with_names_read_in %>%
#   filter(bipoc == 1)


# first an overall comparison between the two main groups and support they did/didn't feel

# ggplot(surveys_with_names_read_in, 
#        aes(x = current_mood_rating_for_supported_unsupported, fill = group)) +
#   geom_bar(position = "dodge", width = 0.8, color = "black") +
#   labs(x = "supported-----unsupported", y = "Count") +
#   scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue"))
# 
# supported_bar <- ggplot(surveys_with_names_read_in, 
#        aes(x = current_mood_rating_for_supported_unsupported, fill = group)) +
#   geom_bar(position = "dodge", width = 0.8, color = "black") +
#   labs(x = "supported-----unsupported", y = "Count") +
#   scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue"))



# Define a function to reverse the order of values
reverse_scale <- function(x) {
  recode(x, `1` = 8, `2` = 7, `3` = 6, `4` = 5, `5` = 4, `6` = 3, `7` = 2, `8` = 1)
}

# Reverse the order of values in specific columns
surveys_with_names_read_in_reversed <- surveys_with_names_read_in %>%
  mutate_at(vars(current_mood_rating_for_confident_anxious, 
                 current_mood_rating_for_strong_weak,
                 current_mood_rating_for_excited_bored,
                 current_mood_rating_for_clear_confused,
                 current_mood_rating_for_engaged_distanced,
                 current_mood_rating_for_included_excluded,
                 current_mood_rating_for_fitting_in_out_of_place,
                 current_mood_rating_for_respected_disrespected,
                 current_mood_rating_for_valued_underappreciated,
                 current_mood_rating_for_supported_unsupported
  ), reverse_scale)

# change question names to reflect changes
surveys_with_names_read_in_reversed <- surveys_with_names_read_in_reversed %>%
  rename(current_mood_rating_for_anxious_confident = current_mood_rating_for_confident_anxious,
         current_mood_rating_for_weak_strong = current_mood_rating_for_strong_weak,
         current_mood_rating_for_bored_excited = current_mood_rating_for_excited_bored,
         current_mood_rating_for_confused_clear = current_mood_rating_for_clear_confused,
         current_mood_rating_for_distanced_engaged = current_mood_rating_for_engaged_distanced,
         current_mood_rating_for_excluded_included = current_mood_rating_for_included_excluded,
         current_mood_rating_for_feeling_out_of_place_fitting_in = current_mood_rating_for_fitting_in_out_of_place,
         current_mood_rating_for_disrespected__respected = current_mood_rating_for_respected_disrespected,
         current_mood_rating_for_underappreciated_valued = current_mood_rating_for_valued_underappreciated,
         current_mood_rating_for_unsupported_supported = current_mood_rating_for_supported_unsupported)

 
# MAKE SURE IT WORKED
names(surveys_with_names_read_in_reversed)

#*************************EXPORT**********************
write.csv(surveys_with_names_read_in_reversed, file = "C:\\Users\\McKay Moulton\\Documents\\MISM_2nd\\Belonging\\outputs\\surveys_names_reversed_.csv", row.names = FALSE)
#_____________________________________________________________
# create correlation matrix to determine redundant columns 
correlations <- surveys_with_names_read_in_reversed %>%
  select(32:48) %>% # select columns 11-70 to avoid weird correlations
  select_if(is.numeric) %>% # some columns aren't dummfified yet so just look at numeric
  cor(use="pairwise.complete.obs") # compute the correlation matrix


# Create a correlation plot using ggcorrplot
ggcorrplot(correlations, type = "lower", hc.order = FALSE, lab = TRUE)



# for 17-31 ...'regarding this activity...'
# Truncate column names to the last 20 characters to fit screen
correlations2 <- surveys_with_names_read_in_reversed %>%
  rename_with(~ str_sub(.x, start = -37), everything())

correlations2 <- correlations2%>%
  select(17:31) %>% # select columns 11-70 to avoid weird correlations
  select_if(is.numeric) %>% # some columns aren't dummfified yet so just look at numeric
  cor(use="pairwise.complete.obs") # compute the correlation matrix

# Create a correlation plot using ggcorrplot
ggcorrplot(correlations2, type = "lower", hc.order = FALSE, lab = TRUE)

#*********************BAR CHARTS***************

# Filter out 'NA' values
# tibble_for_graph1 <- surveys_with_names_read_in_reversed %>%
#   filter(!is.na(current_mood_rating_for_anxious_confident)) 

# Group the data by group and current_mood_rating_for_anxious_confident
# anxious_confident_bar <- tibble_for_graph1 %>%
#   group_by(group, current_mood_rating_for_anxious_confident) %>%
#   # Count the number of responses for each group and response type
#   summarize(count = n()) %>%
#   ungroup() %>%
#   # Calculate the percentage of responses for each group for the entire survey
#   group_by(group) %>%
#   mutate(percent = count / sum(count) * 100) %>%
#   # Create the bar graph
#   ggplot(aes(x = current_mood_rating_for_anxious_confident, y = percent, fill = group)) +
#   geom_bar(position = "dodge", width = 0.8, color = "black", stat = "identity") +
#   labs(x = "anxious---confident", y = "Percentage of Responses") +
#   scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue")) +
#   # Set the y-axis to percentage values
#   scale_y_continuous()


#****************UNSUPORTED_SUPORTED
# Group the data by group and current_mood_rating_for_anxious_confident
# tibble_for_graph2 <- surveys_with_names_read_in_reversed %>%
#   filter(!is.na(current_mood_rating_for_unsupported_supported))
#                 
# supported_bar <- tibble_for_graph2 %>%
#   group_by(group, current_mood_rating_for_unsupported_supported) %>%
#   # Count the number of responses for each group and response type
#   summarize(count = n()) %>%
#   ungroup() %>%
#   # Calculate the percentage of responses for each group for the entire survey
#   group_by(group) %>%
#   mutate(percent = count / sum(count) * 100) %>%
#   # Create the bar graph
#   ggplot(aes(x = current_mood_rating_for_unsupported_supported, y = percent, fill = group)) +
#   geom_bar(position = "dodge", width = 0.8, color = "black", stat = "identity") +
#   labs(x = "unsupported---supported", y = "Percentage of Responses") +
#   scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue")) +
#   # Set the y-axis to percentage values
#   scale_y_continuous()


#****************EXCLUDED_INCLUDED********************
# Group the data by group and current_mood_rating_for_anxious_confident
tibble_for_graph3 <- surveys_with_names_read_in_reversed %>%
  filter(!is.na(current_mood_rating_for_excluded_included))

excluded_bar <- tibble_for_graph3 %>%
  group_by(group, current_mood_rating_for_excluded_included) %>%
  # Count the number of responses for each group and response type
  summarize(count = n()) %>%
  ungroup() %>%
  # Calculate the percentage of responses for each group for the entire survey
  group_by(group) %>%
  mutate(percent = count / sum(count) * 100) %>%
  # Create the bar graph
  ggplot(aes(x = current_mood_rating_for_excluded_included, y = percent, fill = group)) +
  geom_bar(position = "dodge", width = 0.8, color = "black", stat = "identity") +
  labs(x = "EXCLUDED_INCLUDED", y = "Percentage of Responses") +
  scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue")) +
  # Set the y-axis to percentage values
  scale_y_continuous()





#****************did_you_feel_good_about_yourself********************

# Group the data by group and current_mood_rating_for_anxious_confident
tibble_for_graph4 <- surveys_with_names_read_in_reversed %>%
  filter(!is.na(while_engaged_in_this_activity_did_you_feel_good_about_yourself))


did_you_feel_good <- tibble_for_graph4 %>%
  group_by(group, while_engaged_in_this_activity_did_you_feel_good_about_yourself) %>%
  # Count the number of responses for each group and response type
  summarize(count = n()) %>%
  ungroup() %>%
  # Calculate the percentage of responses for each group for the entire survey
  group_by(group) %>%
  mutate(percent = count / sum(count) * 100) %>%
  # Create the bar graph
  ggplot(aes(x = while_engaged_in_this_activity_did_you_feel_good_about_yourself, y = percent, fill = group)) +
  geom_bar(position = "dodge", width = 0.8, color = "black", stat = "identity") +
  labs(x = "Did You Feel Good About Yourself?", y = "Percentage of Responses") +
  scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue")) +
  # Set the y-axis to percentage values
  scale_y_continuous()


#****************while_engaged_in_this_activity_did_you_feel_like_you_belonged********************

# Group the data by group and current_mood_rating_for_anxious_confident
tibble_for_graph5 <- surveys_with_names_read_in_reversed %>%
  filter(!is.na(while_engaged_in_this_activity_did_you_feel_like_you_belonged))


did_you_feel_like_you_belonged <- tibble_for_graph5 %>%
  group_by(group, while_engaged_in_this_activity_did_you_feel_like_you_belonged) %>%
  # Count the number of responses for each group and response type
  summarize(count = n()) %>%
  ungroup() %>%
  # Calculate the percentage of responses for each group for the entire survey
  group_by(group) %>%
  mutate(percent = count / sum(count) * 100) %>%
  # Create the bar graph
  ggplot(aes(x = while_engaged_in_this_activity_did_you_feel_like_you_belonged, y = percent, fill = group)) +
  geom_bar(position = "dodge", width = 0.8, color = "black", stat = "identity") +
  labs(x = "Did You Feel Like You Belonged?", y = "Percentage of Responses") +
  scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue")) +
  # Set the y-axis to percentage values
  scale_y_continuous()


#****************regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you********************

# Group the data by group and current_mood_rating_for_anxious_confident
tibble_for_graph5 <- surveys_with_names_read_in_reversed %>%
  filter(!is.na(regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you))


do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you_bar <- tibble_for_graph5 %>%
  group_by(group, regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you) %>%
  # Count the number of responses for each group and response type
  summarize(count = n()) %>%
  ungroup() %>%
  # Calculate the percentage of responses for each group for the entire survey
  group_by(group) %>%
  mutate(percent = count / sum(count) * 100) %>%
  # Create the bar graph
  ggplot(aes(x = regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you, y = percent, fill = group)) +
  geom_bar(position = "dodge", width = 0.8, color = "black", stat = "identity") +
  labs(x = "Do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you?", y = "Percentage of Responses") +
  scale_fill_manual(values = c("Main" = "red", "Comparison" = "blue")) +
  # Set the y-axis to percentage values
  scale_y_continuous()




#*********************Quick Time Series Plot************************
# library(dplyr)
# library(ggplot2)

# create week column to help visualization look better and be more accurate
# the rest of the code is to place it by the existing time-related columns
#**********************regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you****************

surveys_with_names_read_in_reversed <- surveys_with_names_read_in_reversed %>%
  filter(!is.na(regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you))%>%
  mutate(week = cut(end_date, "week"))

# Convert week column to Date format
surveys_with_names_read_in_reversed$week <- ymd(surveys_with_names_read_in_reversed$week)

group_week_data <- surveys_with_names_read_in_reversed %>%
  group_by(group, week, regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you) %>%
  summarize(count = n(), avg_score = mean(regarding_this_activity_do_you_feel_like_what_you_were_doing_was_accepted_by_those_around_you)) %>%
  ungroup()

# charts will show spikes from only one person, so let's remove those records for now
group_week_data <- group_week_data %>% 
  filter(count >= 3)

# now just get the average score (using the counts from above
group_week_averages <- group_week_data %>%
  group_by(group, week) %>%
  summarize(avg_score = mean(avg_score))

ggplot(group_week_averages, aes(x = week, y = avg_score, color = "Acceptance")) +
  geom_point() +
  facet_wrap(~group) +
  labs(x = "Week", y = "Average Score", color = "Acceptance", 
       title = "Do you feel like what you were doing was accepted by those around you?") +
  scale_y_continuous(limits = c(1, NA)) +
  scale_x_date(date_breaks = "2 week", date_labels = "%b-%d")




break here 

# Attempting to Do A Bar Graph by the three groups
# Create a tibble that just has the three columns, the group column, and the question
tibble_short <- surveys_with_names_read_in_reversed %>%
  filter(!is.na(while_engaged_in_this_activity_did_you_feel_like_you_belonged)) %>%
  select(group, bipoc, low_income, first_gen, while_engaged_in_this_activity_did_you_feel_like_you_belonged)

# Create a new column called 'comparison'
tibble_short <- tibble_short %>%
  mutate(comparison = ifelse(group == "Comparison", 1, 0))

#drop the group column, we don't need it anymore
tibble_short <- tibble_short %>%
  select(-group)
#*****************************try the chart
# glimpse(tibble_short)

# Create a new tibble that contains the proportions of responses for each group and each level of while_engaged_in_this_activity_did_you_feel_like_you_belonged
# proportions <- tibble_short %>%
#   group_by(comparison, bipoc, low_income, first_gen, while_engaged_in_this_activity_did_you_feel_like_you_belonged) %>%
#   summarize(count = n()) %>%
#   ungroup() %>%
#   group_by(comparison, bipoc, low_income, first_gen) %>%
#   mutate(percent = count / sum(count) * 100)
# 
# # Create the bar graph
# ggplot(proportions, aes(x = while_engaged_in_this_activity_did_you_feel_like_you_belonged, y = percent, fill = factor(comparison))) +
#   geom_bar(position = "dodge", stat = "identity", width = 0.7) +
#   labs(x = "Did You Feel Like You Belonged?", y = "Percentage of Responses", fill = "Group") +
#   scale_fill_manual(values = c("red", "blue")) +
#   facet_grid(. ~ bipoc + low_income + first_gen) +
#   theme_bw()







STOP HERE
# NOTES FROM WILSON

# 'recode' for graphs (if-else)
# maps values into groups 'low / 'high' for model'

# propose grouping of similar columns 
# look for correlations and plot it between (correlation matrix)
# look at how they change over time (for given person)
# see range (if its fairly consistence then find an average)


# let's get more specific
STOP HERE
#**************************FOR LOCATION**************************
#*
#*
# for visualizations
library(ggmap)
library(ggplot2)
library(sf)
library(rstudioapi)

#set up credentials
# register_google(key = "AIzaSyDqUWy-9xO1OmHSUu3xVpHq-8DY4loW-4I")

# make a copy of the tibble
surveys_location <- surveys_with_names

# check for NA's  (both returned TRUE)
any(is.na(surveys_location$location_longitude))
any(is.na(surveys_location$location_latitude))

# drop 'em
surveys_location <- drop_na(surveys_location, location_latitude, location_longitude)

surveys_location <- surveys_location %>% select(location_longitude, location_latitude)


#**************************ZIP CODE**********************************

# load in zipcode data to make a variable that determines if they are in provo when they take the survey
# Read in the State-zip-code-GeoJSON file from GitHub, convert it to an sf object


# zip_codes_url <- "https://raw.githubusercontent.com/OpenDataDE/State-zip-code-GeoJSON/master/ut_utah_zip_codes_geo.min.json"
https://www2.census.gov/geo/tiger/TIGER2020/ZCTA5/tl_2020_49_zcta510.zip
zip_codes_UT <- st_read(zip_codes_url)


# Read in the zip code GeoJSON file as an sf object
# zip_codes_sf <- st_read(zip_codes_url)

# Define the target zip code
target_zip <- "84601"

# Create an sf object from the surveys_location tibble
my_sf <- st_as_sf(surveys_location, coords = c("location_longitude", "location_latitude"), crs = 4326)

# Use st_contains to create a logical vector indicating which rows of my_sf are within the target zip code
is_in_zip <- st_contains(zip_codes_UT[zip_codes_UT$zipcode == target_zip,], my_sf)

# Add the is_in_zip vector to the original surveys_location tibble
surveys_location$is_in_zip <- is_in_zip

unique(zip_codes_UT$zipcode)

# Check which rows of my_tibble are in Provo
my_tibble_filtered <- surveys_location[which(st_within(my_sf, zip_codes_UT %>% filter(zipcode %in% target_zips))), ]

# Print the filtered tibble
my_tibble_filtered

if (target_zip %in% zip_codes_UT$zipcode) {
  is_in_zip <- st_contains(zip_codes_UT[zip_codes_UT$zipcode == target_zip,], my_sf)
  surveys_location$is_in_zip <- is_in_zip
} else {
  cat("Target zip code not found in zip codes data")
}


#****************PLOT DATA*****************************
# Let's start by looking at location (according to lat-long)
# find center (through the average) to know how much to zoom in
map <- get_googlemap(center = c(mean(surveys_location$location_longitude), mean(surveys_location$location_latitude)), zoom = 11)
  
ggmap(map)

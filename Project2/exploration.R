library(magick)
library(tidyverse)

# Reading data from published CSV file into a data frame called learning_data
csv_file <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSmUDWsAXTLGBXuf6gxC8vctyKPe9Vr-Qk4sdyNd4bQILq7X69iFs_FwW-M3-dlpYbZ_7-2hV_es6OZ/pub?gid=1145940176&single=true&output=csv"

# Renaming variables of the data frame learning_data
learning_data <- read_csv(csv_file) %>%
  rename(age = 2,
         sex = 3,
         if_exercise = 4,
         exercise_type = 5,
         exercise_days = 6,
         exercise_hours = 7,
         intensity_level = 8,
         affecting_factors = 9,
         exercise_loaction = 10,
         main_reason =11
         ) 

# Summary values based on the data
avg_exercise_days <- mean(learning_data$exercise_days, na.rm = TRUE)
avg_exercise_hours <- mean(learning_data$exercise_hours, na.rm = TRUE)

average_exercise_duration <- paste("The average training days per week of respondents are", 
                        round(avg_exercise_days, 1),
                        "days,",
                        "and average training hours on the exercise are",
                        round(avg_exercise_hours, 1),
                        "hours.") %>% print()

minimum_total_exercise_hours = min(learning_data$exercise_hours*learning_data$exercise_days, na.rm = TRUE)
maximum_total_exercise_hours = max(learning_data$exercise_hours*learning_data$exercise_days, na.rm = TRUE)

extremum_of_total_exercise_hours <- paste("The minimum total exercise hours of respondents per week are",
                                          minimum_total_exercise_hours,
                                          "hours,",
                                          "and the maximum total exercise hours of respondents per week are",
                                          maximum_total_exercise_hours,
                                          "hours.") %>% print()

# Bar chart for types of physical exercise
cleaned_data <- na.omit(learning_data)

cleaned_data %>%
  ggplot() +
  geom_bar(aes(x = exercise_type), fill = "skyblue") +
  labs(title = "Types of physical exercise",
       caption = "Source: My Physical Exercise Behaviors Survey",
       x = "Exercise type",
       y = "Number of people")

# Bar chart for the popularity of different location
learning_data_longer <- na.omit(learning_data) %>%
  separate_rows(exercise_loaction, sep = ", ")

learning_data_longer %>%
  ggplot() +
  geom_bar(aes(x = exercise_loaction,fill = sex)) +
  labs(title = "The popularity of different location",
       caption = "Source: My Physical Exercise Behaviors Survey",
       x = "Exercise type",
       y = "Number of choices")
  

















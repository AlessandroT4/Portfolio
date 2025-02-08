library("tidyverse")
library("janitor")
library("here")
library("skimr")


daily_activity <- read_csv("C:/Users/USER/Desktop/Work/CASE STUDY - BellaBeat/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
hourly_steps <- read_csv("C:/Users/USER/Desktop/Work/CASE STUDY - BellaBeat/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
sleep_day <- read_csv("C:/Users/USER/Desktop/Work/CASE STUDY - BellaBeat/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

#Checking unique value
n_unique(daily_activity$Id)
n_unique(hourly_steps$Id)
n_unique(sleep_day$Id)

sum(duplicated(daily_activity))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_day))

#Remove the duplicate
daily_activity <- drop_na(daily_activity)
hourly_steps <- drop_na(hourly_steps)
sleep_day <- sleep_day %>% 
  distinct() %>% 
  drop_na()

sum(duplicated(sleep_day))


#Adjusting columns
daily_activity <- clean_names(daily_activity)
hourly_steps <- clean_names(hourly_steps)
sleep_day <- clean_names(sleep_day)

daily_activity <- rename_with(daily_activity, tolower)
hourly_steps <- rename_with(hourly_steps, tolower)
sleep_day <- rename_with(sleep_day, tolower)

#Date-time Format
daily_activity <- daily_activity %>% 
  rename(date = activity_date) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

sleep_day <- sleep_day  %>% 
  rename(date = sleep_day) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p"))

hourly_steps <- hourly_steps %>% 
  rename(date_time = activity_hour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p"))

hourly_steps <- hourly_steps %>% 
  separate(date_time, into = c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date))

hourly_steps <- hourly_steps %>% 
  mutate(time =
           ifelse(is.na(time), "00:00:00",
                  time))

#Dataset merging
daily_activity_sleep <- merge(daily_activity, sleep_day, by = c("id", "date"))
glimpse(daily_activity_sleep)

#Feature creation
daily_average <- daily_activity_sleep %>% 
  group_by(id) %>%
  summarise(mean_daily_steps = mean(total_steps), 
            mean_daily_calories = mean(calories), 
            mean_daily_sleep = mean(total_minutes_asleep))

head(daily_average)


user_type <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "Low active",
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "Somewhat active",
    mean_daily_steps >= 10000 ~ "Active"
  ))

user_type <- user_type %>% 
  mutate(mean_daily_sleep_hr = mean_daily_sleep /60)

#Verification
head(daily_activity)
str(daily_activity)

head(sleep_day)
str(sleep_day)

head(hourly_steps)
str(hourly_steps)

head(user_type)
str(user_type)

head(daily_activity_sleep)
str(daily_activity_sleep)


#Analyze
daily_activity %>% 
  select(total_steps,
         total_distance,
         sedentary_minutes,
         calories) %>% 
  summary()

sleep_day %>% 
  select(total_minutes_asleep,
         total_time_in_bed) %>% 
  summary()

# Averages indicate that participants take 7,638 steps per day.
# Sedentary time is significant, with an average of 991 minutes daily (16.5 hours).
# Active participants fall primarily into the "Somewhat Active", meaning they take between 7,500 and 10,000 steps daily.
# Participants sleep an average of 7 hours per night.

#Correlation 
cor(daily_activity$total_steps, daily_activity$calories)
cor(daily_activity_sleep$total_steps, daily_activity_sleep$total_minutes_asleep)
cor(daily_activity_sleep$total_minutes_asleep, daily_activity_sleep$total_time_in_bed)
cor(daily_activity_sleep$sedentary_minutes, daily_activity_sleep$total_minutes_asleep)

#Plotting

#Distribution of Calories Burned by User Type
ggplot(user_type, aes(x = user_type, y = mean_daily_calories, fill = user_type)) +
  geom_boxplot() +
  labs(title = "Distribution of Calories Burned by User Type",
       x = "User Type",
       y = "Average Calories Burned") +
  theme_minimal()


#Average Sleep Hours by User Type
ggplot(user_type, aes(x = user_type, y = mean_daily_sleep_hr, fill = user_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sleep Hours by User Type",
       x = "User Type",
       y = "Average Sleep Hours") +
  theme_minimal()


#Hourly steps throughout the day
hourly_steps %>% 
  group_by(time) %>% 
  summarize(average_steps = mean(step_total)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = time, y = average_steps, fill = average_steps))+
  labs(title = "Hourly Steps throughout the day",
       x = "Time",
       y = "Average Steps") + 
  scale_fill_gradient(low = "green", high = "red") +
  theme(axis.text.x = element_text (angle = 90))


# Steps vs Calories Burned
ggplot(daily_activity, aes(x = total_steps, y = calories)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Relationship Between Steps and Calories Burned",
       x = "Total Steps",
       y = "Calories Burned") +
  theme_minimal()


#Sedentary Minutes vs. Total Minutes Asleep
ggplot(daily_activity_sleep, aes(x = sedentary_minutes, y = total_minutes_asleep)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship Between Sedentary Time and Sleep Duration",
       x = "Sedentary Minutes",
       y = "Total Minutes Asleep") +
  theme_minimal()




# Loading Packeges
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)

# Importing Data

daily_activity <- read_csv("~/Projects/Bellabeat/FitabaseData 2016_12_4-201612_5/dailyActivity_merged.csv")

daily_calories <- read_csv("~/Projects/Bellabeat/FitabaseData 2016_12_4-201612_5/dailyCalories_merged.csv")

daily_intensities <- read_csv("~/Projects/Bellabeat/FitabaseData 2016_12_4-201612_5/dailyIntensities_merged.csv")

daily_steps <- read_csv("~/Projects/Bellabeat/FitabaseData 2016_12_4-201612_5/dailySteps_merged.csv")

sleep <- read_csv("~/Projects/Bellabeat/FitabaseData 2016_12_4-201612_5/sleepDay_merged.csv")

weight <- read_csv("~/Projects/Bellabeat/FitabaseData 2016_12_4-201612_5/weightLogInfo_merged.csv")

# Ensure consistency
# Editing Format of Date(chr to date)

daily_activity$ActivityDate = mdy(daily_activity$ActivityDate)

daily_calories$ActivityDay = mdy(daily_calories$ActivityDay)

daily_intensities$ActivityDay = mdy(daily_intensities$ActivityDay)

daily_steps$ActivityDay = mdy(daily_steps$ActivityDay)

sleep$SleepDay = as.POSIXct(sleep$SleepDay, format = "%m/%d/%Y %H:%M:%S")

weight$Date = as.POSIXct(weight$Date, format = "%m/%d/%Y %H:%M:%S")

# Removing Duplicated data

daily_activity <- distinct(daily_activity)

daily_calories <- distinct(daily_calories)

daily_intensities <- distinct(daily_intensities)

daily_steps <- distinct(daily_steps)

sleep <- distinct(sleep)

weight <- distinct(weight)

# Exploring data

n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_steps$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# Rename column name
clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)
daily_activity <- rename(daily_activity, activityday = activitydate)

clean_names(daily_calories)
daily_calories <- rename_with(daily_calories, tolower)

clean_names(daily_intensities)
daily_intensities <- rename_with(daily_intensities, tolower)

clean_names(sleep)
sleep <- rename_with(sleep, tolower)

# Merging datasets

merge_daily_data <- merge(daily_activity, sleep, all.x = TRUE) %>% 
  drop_na()
merge_daily_data <- merge_daily_data %>% 
  select(-c(trackerdistance, sleepday, totalsleeprecords))
 
# Summarizing data
merge_daily_data %>% 
  select(-c(id, activityday)) %>% 
  summary()

# Transforming Data

merge_daily_data <- merge_daily_data %>% 
  mutate(weekdays = weekdays(activityday))

weekday_data <- merge_daily_data %>% 
  group_by(weekdays) %>% 
  summarize(
    steps = mean(totalsteps),
    sleeptime = mean(totalminutesasleep),
    burnt_calories = mean(calories)
  )
#weekday_data <- weekday_data %>% 
  #pivot_longer(
    #cols = steps:burnt_calories,
    #names_to = "measurement",
    #values_to = "value"
 # )

# Visualizing Data

ggplot(weekday_data, aes(x = weekdays, y = value, fill = measurement)) + 
  geom_bar(stat="identity", position = "dodge", ylim = c(0,600)) +
  scale_fill_brewer(palette = "Set1")

ggplot(weekday_data, aes( x = value, y = weekdays)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ measurement)


sleepgp <- weekday_data %>%
  ggplot(aes(x = factor(weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = sleeptime, fill = weekdays)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Sleep Time Throughout Week") +
  ylab("minutes asleep") +
  scale_x_discrete("Day", guide = guide_axis(angle = 45)) +
  scale_fill_brewer(palette = "Accent")
sleepgp + ylim(0,430)

weekday_data %>%
  ggplot(aes(x = factor(weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = steps, fill = weekdays)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Steps Throughout Week") +
  ylab("steps") +
  scale_x_discrete("Day", guide = guide_axis(angle = 45)) +
  scale_fill_brewer(palette = "Accent")

weekday_data %>%
  ggplot(aes(x = factor(weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = burnt_calories, fill = weekdays)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Calories burnt Throughout Week") +
  ylab("calories") +
  scale_x_discrete("Day", guide = guide_axis(angle = 45)) +
  scale_fill_brewer(palette = "Accent")

ave_daily_data_by_id <- merge_daily_data %>% 
  group_by(id) %>% 
  summarize(
    ave_calories = mean(calories),
    ave_steps = mean(totalsteps),
    ave_distance = mean(totaldistance),
    ave_sleep = mean(totalminutesasleep),
    
  )







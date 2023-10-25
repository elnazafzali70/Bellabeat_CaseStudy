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
weekday_data <- weekday_data %>% 
  pivot_longer(
    cols = steps:burnt_calories,
    names_to = "measurement",
    values_to = "value"
  )
#test
ggplot(weekday_data, aes(x = weekdays, y = value, fill = measurement)) + 
  geom_bar(stat="identity", position = "dodge", ylim = c(0,200)) +
  scale_fill_brewer(palette = "Set1")

ggplot(weekday_data, aes( x = value, y = weekdays)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ measurement)

#ggplot(weekday_data, aes(x = weekdays, fill = c(steps, sleeptime, burnt_calories))) +
  #geom_col(position = "dodge", colour = "black") +
  #scale_fill_brewer(palette = "Pastel1")

weekday_data %>%
  ggplot(aes(x = factor(weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = sleeptime, color = weekdays)) +
  geom_bar() + #(state = "identity", position = "jitter", alpha = 0.3 ) +
  labs(title = "Minutes asleep throughout week") +
  ylab("Minutes asleep") +
  #scale_x_discrete("Day", guide = guide_axis(angle = 90))

ave_daily_data_by_id <- merge_daily_data %>% 
  group_by(id) %>% 
  summarize(
    ave_calories = mean(calories),
    ave_steps = mean(totalsteps),
    ave_distance = mean(totaldistance),
    ave_sleep = mean(totalminutesasleep),
    
  )





#x %>% 
  #ggplot(mapping = aes(x = week_days, y = steps, fill = "#006699")) +
  #geom_bar(stat = "identity")
#x %>% 
  #ggplot(aes(x = burncalories, y = steps, color = week_days)) +
  #geom_point()

#daily_life_data %>% 
  #ggplot(mapping = aes(x = totalminutesasleep, y = calories)) +
  #geom_point()
#daily_life_data %>% 
  #ggplot(mapping = aes(x = totalminutesasleep, y = totalsteps)) +
  #geom_bar()






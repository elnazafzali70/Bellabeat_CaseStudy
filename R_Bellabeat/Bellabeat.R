# Loading Packeges
install.packages("janitor")
#install.packages("ggpubr")
#install.packages("magrittr")
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(ggplot2)
#library(ggpubr)
library(tidyr)
#library(magrittr)

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
sleep <- sleep %>% 
  tidyr::separate_wider_delim(sleepday, " ", names = c("sleepday", "time"))

merge_daily_data <- merge(daily_activity, sleep, by.x = c("id", "activityday"), by.y = c("id", "sleepday")) %>% 
  drop_na()

merge_daily_data <- merge_daily_data %>% 
  select(-c(trackerdistance, time, totalsleeprecords))
  
 
# Summarizing data
merge_daily_data %>% 
    select(-c(id, activityday)) %>% 
    summary()
  

# Visualizing Data
#finding trends base on weekdays

merge_daily_data <- merge_daily_data %>% 
  mutate(weekdays = weekdays(activityday))

#weekday_data <- weekday_data %>% 
#pivot_longer(
#cols = steps:burnt_calories,
#names_to = "measurement",
#values_to = "value"
# )

weekday_data <- merge_daily_data %>% 
  group_by(weekdays) %>% 
  summarize(
    steps = mean(totalsteps),
    sleeptime = mean(totalminutesasleep),
    burnt_calories = mean(calories)
  )
weekday_data %>%
  ggplot(aes(x = factor(weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = sleeptime)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "#154c79") +
  labs(title = "Average Sleep Time Throughout Week") +
  ylab("minutes asleep") +
  scale_x_discrete("Day", guide = guide_axis(angle = 45))
  #scale_fill_brewer(palette = "Accent")
#sleepgp + ylim(0,430)

weekday_data %>%
  ggplot(aes(x = factor(weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = steps)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "#154c79") +
  labs(title = "Average Steps Throughout Week") +
  ylab("steps") +
  scale_x_discrete("Day", guide = guide_axis(angle = 45)) 
  #scale_fill_brewer(palette = "Accent")

weekday_data %>%
  ggplot(aes(x = factor(weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = burnt_calories)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "#154c79") +
  labs(title = "Average Calories burnt Throughout Week") +
  ylab("calories") +
  scale_x_discrete("Day", guide = guide_axis(angle = 45)) 

merge_daily_data %>%
  ggplot(aes( x = totalsteps, y = calories))+
  geom_jitter(alpha = 0.7)+
  geom_rug(position = "jitter", size = 0.03)+
  geom_smooth(method = lm, size = 0.6, color = "red")+
  labs(title = "Daily steps vs. calories", x = "daily steps", y = "calories")+
  theme_minimal()

data_by_id <- merge_daily_data %>% 
  group_by(id) %>% 
  summarize(
    ave_calories = mean(calories),
    ave_steps = mean(totalsteps),
    ave_distance = mean(totaldistance),
    ave_sleep = mean(totalminutesasleep),
    num_day_used = n()
    ) %>% 
 
   mutate(
    user_usage = case_when(
    num_day_used >= 1 & num_day_used <= 10 ~ "low usage",
    num_day_used >= 11 & num_day_used <= 20 ~ "Medium usage", 
    num_day_used >= 21 & num_day_used <= 31 ~ "High usage", 
  ))

data_by_id %>% 
  ggplot(aes(x = user_usage, y = ave_calories, fill = user_usage)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(title = "Calories burned by User usage", x = NULL) +
  theme(legend.position = "none", text = element_text(size = 10), plot.title = element_text(hjust = 0.5))


data_by_usage <- data_by_id %>% 
  group_by(user_usage) %>% 
  summarize(
    num_user = n(),
    type_calories = mean(ave_calories)
    
  ) %>%
  mutate(
    total = sum(num_user),
    percentage = scales::percent(num_user / total)
    
  )


data_by_usage %>%
  ggplot(aes(x = "", y = percentage, fill = user_usage)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  geom_text(aes(label = percentage),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(labels = c("High usage", "Medium usage","Low usage")) +
  scale_fill_brewer(palette="Set1") +
  labs(title = "Percentage of user usage")

#merge_daily_data %>% 
  #ggplot(aes(totalsteps,calories,fill=totalsteps)) +
  #geom_boxplot() +
  #facet_wrap(~distance)+
  #labs(title="Calories burned by Steps",x=NULL) +
  #theme(legend.position="none", text = element_text(size = 20),plot.title = element_text(hjust = 0.5))





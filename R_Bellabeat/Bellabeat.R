# Loading Packeges

library(tidyverse)
library(lubridate)
library(dplyr)

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



# Summarizing data
daily_activity %>% 
  select(TotalSteps, TotalDistance, )
summary()










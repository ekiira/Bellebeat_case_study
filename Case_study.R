#load packages
library(tidyverse)
library(lubridate)

#PREPARE
## Import dataframe
daily_activity <- read_csv('FitBit_Dataset/dailyActivity_merged.csv')
sleep_day <- read_csv('FitBit_Dataset/sleepDay_merged.csv')
weightLog <- read_csv('FitBit_Dataset/weightLogInfo_merged.csv')

## preview the data_activity dataframe
head(daily_activity)
colnames(daily_activity)

## preview the sleep_day dataframe
head(sleep_day)
colnames(sleep_day)

## preview the weightLog dataframe
head(weightLog)
colnames(weightLog)

#PROCESS
## Data cleaning
### check for duplicate data
sum(duplicated(daily_activity))
sum(duplicated(sleep_day))
sum(duplicated(weightLog))

### since sleep_day has duplicate data we remove duplicates.
sleep_day_cleaned <- unique(sleep_day)
sum(duplicated(sleep_day_cleaned))

##Rename `ActivityDate` and `SleepDay` to `Date` for consistency when merging later on. Also change format to Date for the 3 data frames
# rename ActivityDate column and SleepDay and change the format Date
daily_activity_cleaned <- daily_activity %>%
  rename(Date = ActivityDate) %>%
  mutate(Date = mdy(Date))
sleep_day_cleaned <- sleep_day_cleaned %>%
  rename(Date = SleepDay) %>%
  mutate(Date = mdy_hms(Date))

# change weightLog$Date date format
weightLog_cleaned <- weightLog %>%
  mutate(Date = mdy_hms(Date))

# check
str(daily_activity_cleaned)
str(sleep_day_cleaned)
str(weightLog_cleaned)

##Add days of week column to daily_Activity dataframe
daily_activity_cleaned$DayofWeek <-  weekdays(daily_activity_cleaned$Date)
str(daily_activity_cleaned)

### Check for missing values
sum(is.na(daily_activity_cleaned))
sum(is.na(sleep_day_cleaned))
sum(is.na(weightLog_cleaned))

### `weightLog_cleaned` has 65 missing values
colSums(is.na(weightLog_cleaned)) 
nrow(weightLog_cleaned)
# The missing values are in the `Fat` column. The number of rows in the `weightLog_cleaned` df is 67, meaning only 2 values in the column are available.
# Also as this is a third party dataset It is not certain what the missing values could be, 
# Therefore, we delete the `Fat` column. It also won't ulter our analysis because I would not be needing the`Fat` column.  
weightLog_cleaned <- weightLog_cleaned[,-5]
#check
colnames(weightLog_cleaned)
colSums(is.na(weightLog_cleaned))

##Check numbers of users in each data frame
n_distinct(daily_activity_cleaned$Id)
n_distinct(sleep_day_cleaned$Id)
n_distinct(weightLog_cleaned$Id)
### more users logged in daily_activity than sleep_day data and weightLog

##Look for 0 values
## TotalSteps, TotalDistance and Calories have values of 0, which can not be possible, in order to eliminate bias, we delete the rows 
daily_activity_cleaned <- daily_activity_cleaned %>% 
  filter(TotalSteps > 0, TotalDistance > 0, Calories > 0) 

sum(daily_activity_cleaned$TotalSteps <= 0)
sum(daily_activity_cleaned$TotalDistance <= 0)
sum(daily_activity_cleaned$Calories <= 0)

## Fortunately `weightLog_cleaned` and `sleep_day_cleaned` have no observation with 0 values
##check number of rows
nrow(daily_activity_cleaned)
nrow(sleep_day_cleaned)
nrow(weightLog_cleaned)


#ANALYSIS
##Data summary
daily_activity_cleaned %>%
  select(TotalSteps, TotalDistance, Calories, SedentaryMinutes) %>%
  summary()

sleep_day_cleaned %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

weightLog_cleaned %>%
  select(BMI, WeightPounds, WeightKg) %>%
  summary()

#FINDINGS
#1.  Average Total steps = 8329
#2.  Average Total Distance  = 5.986
#3.  Average sedentary minutes = 955.2
#4.  Average calories burned = 2362
#5.  Average Sleep Records = 1.12
#6.  Average Minutes asleep = 419.2 minutes apprrox 7 hours of sleep
#7.  Average Time in bed = 458.5 minutes
#8.  Average BMI = 25.19
#9.  Average weight in kg = 72.04

#All these are subject to bias because it is not specified the gender of the participants and also not enough data

## Merge dataframes
### To discover more relationships and trends in visualizations, I need to merge dataframes.
#daily_activity_cleaned and sleep_day_cleaned
combined_daily_sleep <- merge(daily_activity_cleaned, sleep_day_cleaned, by= c("Id", "Date"))

#daily_activity_cleaned, weightLog_cleaned
combined_daily_weight <- merge(daily_activity_cleaned, weightLog_cleaned, by= c("Id", "Date"))

#check
nrow(combined_daily_sleep)
sum(duplicated(combined_daily_sleep))
sum(is.na(combined_daily_sleep))

nrow(combined_daily_weight)
sum(duplicated(combined_daily_weight))
sum(is.na(combined_daily_weight))

#SHARE
#Create data visualizations to show relationships and trends

##daily activity usage
daily_activity_cleaned$DayofWeek <- factor(
  daily_activity_cleaned$DayofWeek, 
  level = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

ggplot(data = daily_activity_cleaned, mapping = aes(x = DayofWeek, width = 0.6)) +
  geom_bar(fill = "darkblue") +
  labs(title = "Smart Device Activity Usage by Day", x = "Day of the week")
#monday is the least days users use the smart device


## relationship between total steps and calories burned
ggplot(data=daily_activity_cleaned, 
       aes(x=TotalSteps, y=Calories )) + 
  geom_point(aes(color=Calories)) +
  geom_smooth() +
  labs(title="Total steps vs Calories Burned",
       x = "Total Steps",
       y = "Calories burned")

## relationship between sedentary minutes and calories burned
ggplot(data=daily_activity_cleaned, 
     aes(x=SedentaryMinutes, y=Calories)) + 
  geom_point(aes(color=Calories)) +
  geom_smooth() +
  labs(title="Sedentary Minutes vs Calories Burned",
       x = "Sedentary Minutess",
       y = "Calories burned")


## relationship between total steps and BMI
ggplot(data=combined_daily_weight, 
       aes(x=TotalSteps, y=BMI)) + 
  geom_point(aes(color=BMI)) +
  geom_smooth() +
  labs(title="Total Steps vs BMI",
       x = "Total Steps",
       y = "BMI")
# the lower your steps, higher your bmi

## relationship between sedentaryMinutes and BMI
ggplot(data=combined_daily_weight, 
       aes(x=BMI, y=SedentaryMinutes)) + 
  geom_point(aes(color=BMI)) +
  geom_smooth() +
  labs(title="BMI vs Sedentary Minutes ",
       x = "BMI",
       y = "Sedentary Minutes")
# the higher your sedentary minutes, the higher your bmi,
#sedentarry lifestyle contributes to high Bmi, we can encourage users to reach a certian number of steps per day

ggplot(data = sleep_day_cleaned, mapping = aes(x=TotalMinutesAsleep/60)) + 
 geom_histogram( color="white", fill="navyblue",binwidth = 1) +
  labs(title = "Amount of sleep hours per day", x = "Total minutes asleep (hours)")
#have 7 hours of sleep, which can be considered good but for the recommend amount of sleep in a day


## relationship between Sedentary activity and Time asleep
ggplot(data=combined_daily_sleep, 
       aes(x=TotalMinutesAsleep/60, y=SedentaryMinutes/60)) + 
  geom_point() +
  geom_smooth() +
  labs(title="Hours Asleep vs Sedentary Activity Hours",
       x = "Hours Asleep",
       y = "Sedentary Activity Hours")





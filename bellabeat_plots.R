setwd("D:/GDA/bellabeat")
library(tidyverse)
library(ggplot2)
dailyActivity <- read.csv('Data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv')
sleep_vs_activity <- read.csv('Data/Fitabase Data 4.12.16-5.12.16/generated/sleep_vs_activity.csv')

#sleep to more activity and confirms
ggplot(data = sleep_vs_activity) + geom_col(mapping = aes(x = TotalMinutesAsleep, y = LightlyActiveMinutes))
#sleep to more activity
ggplot(data = sleep_vs_activity) + geom_col(mapping = aes(x = TotalMinutesAsleep, y = FairlyActiveMinutes+VeryActiveMinutes))+
  labs (x = 'Sleep Minutes', y = "Active Minutes", Fill = 'Activity Inten')

#in bed before asleep
sleep <- read.csv('Data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv')
sleep [c('Date', 'Time', 'AM/PM')] <- str_split_fixed(sleep$SleepDay, ' ', 3)
sleep$Date <- as.Date(sleep$Date,format='%m/%d/%Y')
ggplot(data = sleep) + geom_point(mapping = aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) + 
  labs(x = "In-bed Minutes", y = 'Asleep Minutes') + 
  annotate('text', x = 700, y = 420, label = 'Average 39.17 minutes', color = 'red')
mean(sleep$TotalTimeInBed) - mean(sleep$TotalMinutesAsleep)

#type of exercise and calories
da_et<-read.csv('Data/Fitabase Data 4.12.16-5.12.16/generated/da_excercise_type.csv')
ggplot(data = da_et) + 
geom_point(mapping = aes(x = FairlyActiveMinutes+LightlyActiveMinutes+VeryActiveMinutes, y = Calories, color = Type))+
  labs(x = 'Excercise Duration')+
  annotate("rect", xmin = 200, xmax = 400, ymin = 3250, ymax = 4500,alpha = .2, color = 'blue')+
  annotate('text', x = 400, y = 4600, label = 'Running = higher calorie burn')

#types of exercise 
da_et$Id <- as.character(da_et$Id)
ggplot(da_et, aes(Type)) + geom_bar()

#week day vs active_minutes
library(lubridate)
da_et$weekday <- wday(da_et$ActivityDate, label=TRUE)
da_wdsum_wide<-da_et %>%
  group_by(weekday) %>%
  summarise(LightlyActiveMinutes_sum = sum(LightlyActiveMinutes),FairlyActiveMinutes_sum = sum(FairlyActiveMinutes),VeryActiveMinutes_sum = sum(VeryActiveMinutes) )
da_wdsum_long <- gather(da_wdsum_wide, Activity_intensity, ActiveMinutes, LightlyActiveMinutes_sum:VeryActiveMinutes_sum, factor_key=TRUE)
ggplot(data = da_wdsum_long) + geom_col(mapping = aes(x = weekday, y = ActiveMinutes, fill = Activity_intensity)) + coord_polar() + 
  labs(x = 'Day of the Week', fill = 'Activity Intensity')

#hour step intensity
hour_step_intensity <- read.csv('Data/Fitabase Data 4.12.16-5.12.16/generated/hour_step_intensity.csv')
hour_step_intensity [c('Date', 'Time_AMPM')] <- str_split_fixed(hour_step_intensity$ActivityHour, ' ', 2)
hour_step_intensity$Time_AMPM<-format(strptime(hour_step_intensity$Time_AMPM, "%I:%M:%S %p"), format="%H:%M:%S")
hour_step_intensity_gbHour <- hour_step_intensity %>% 
  group_by(Time_AMPM) %>% 
  summarise(Step_mean = mean(TotalStep), Intensity_mean = mean(TotalIntensity))
hour_step_intensity_long <- gather(hour_step_intensity_gbHour, type, mean, Step_mean:Intensity_mean, factor_key=TRUE)

ggplot(data = hour_step_intensity_gbHour) + geom_col(mapping = aes(x = Time_AMPM, y = Step_mean)) + coord_polar()
ggplot(data = hour_step_intensity_gbHour) + geom_col(mapping = aes(x = Time_AMPM, y = Intensity_mean)) + coord_polar()

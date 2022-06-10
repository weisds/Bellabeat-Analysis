#useful R 

library(tidyverse)
#split
heartrate_sp <- heartrate [c('Date', 'Time', 'AM/PM')] <- str_split_fixed(heartrate$Time, ' ', 3)
#convert to date
heartrate$Date <- as.Date(heartrate$Date,format='%m/%d/%Y')
#convert ampm time
hour_step_intensity$Time_AMPM_fixed<-format(strptime(hour_step_intensity$Time_AMPM, "%I:%M:%S %p"), format="%H:%M:%S")
#wide to long
da_wdsum_long <- gather(da_wdsum_wide, Activity_intensity, ActiveMinutes, LightlyActiveMinutes_sum:VeryActiveMinutes_sum, factor_key=TRUE)
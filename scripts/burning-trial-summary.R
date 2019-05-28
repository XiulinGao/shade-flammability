###this scrips is excused to clean up recorded trial data
### in burning-trial.csv
###main mission is to do basic summary for each trial

library(dplyr)
library(lubridate)
library(stringr)
#Sys.setenv(TZ = "America/Chicago") 
TZ = "CST6CDT" 

trials <- read.csv("../data/burning-trial.csv", stringsAsFactors=FALSE,
                   na.strings = c("", "N/A", "NA", "na", "n/a")) 

trials <- trials %>% mutate(tfresh.mass = initial.weight-final.weight) %>%
  mutate(start.time = mdy_hm(str_c(trial.date, " ",
                                   start.time),tz=TZ)) %>%
  mutate(end.time = mdy_hm(str_c(trial.date, " ", end.time), tz=TZ),
         label = paste(spcode, light, block, sep="")) 

trials$interval <- interval(trials$start.time, trials$end.time)

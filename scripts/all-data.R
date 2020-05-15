## all-data.R

## script that source all required data for further analysis runs before
## running analysis script. This code is meant to be run only once and loads
## all common packages needed.

## Packages
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(lme4)
library(broom)
library(pcaMethods)
library(car) # for Anova()

## Other packages needed but not loaded via library():
# corrplot ( for) corrplot::corrplot
# ggpubr  # needed?
# sjPlot
# insight # for model_info ? needed. 

## project wide settings:
TZ = "CST6CDT"  # time zone
RESULTS <- "../results/"  # directory for figures and tables

# set the random number seed
set.seed(-372)

# read main data files
source("./read-hobo.R") # need tempsec.sum for temperature summary
source("./read-balance.R")  # read in raw balance data
source("./read-traits.R") # need trait.trial dataset for all plant traits and trial measurements
source("./read-weather.R") # get temperature data for each trial: burn_weather dataset

# join all data to make a complete dataset where all info can be found
alldata <- tempsec.sum %>% left_join(filter(trait.trial,treatment == "b"), by = "label") %>% 
  left_join(flamNLModsCoef_sig, by = c("label", "spcode")) %>%
  select(-peak.time, -num.NA, -treatment, -wind, -interval, -start.time, -end.time,
         -term, -p.value, -end.weight, -final.weight, -f.fuelresid, -tfresh.mass, 
         -initial.weight, -post.fmc, -t.fuelresid)

alldata <- ungroup(alldata)
# covert temp difference to heat
# heat = mass * 0.921 J/g*C * temp_diff
alldata <- alldata %>% mutate(diffb = tbase.flam - tbase.ini, #-0.1
                              diff50 = t50.flam - t50.ini) %>% #-1.4
  mutate(heat50 = (diff50 + 1.5)*52.91*0.921, 
         heatb = (diffb + 0.2)*53.21*0.921) %>% 
  mutate_at(c("heat50", "heatb"), list(~round(.,2)))

colnames(alldata)[colnames(alldata)=="ave.gain"] <- "st"

## data that one trail per row with all needed trait measurements and flammability
## measurements including heat release at 2 locations, integrated temp at 4 locations
## mass loss, loss rate, max flame height, time to ignition, and combustion duration
## easy for modeling

flamdt <- alldata %>% select(-peak.temp, -dur.100, -degsec.100, -dur.60) 
flamdt <- spread(flamdt, location, degsec.60) 
flamdt <- flamdt %>% mutate(heatb_log = log(heatb),
                            heat50_log = log(heat50))




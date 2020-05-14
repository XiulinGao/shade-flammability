# Read hobo data logger temperature data, split into sections by burn trial, and
# summarize for each trial


#TZ = "CST6CDT"
## clean up recorded trial data in burning-trial.csv. The main mission is to do
## basic summary for each trial

## Read trial data first

trials <- read.csv("../data/burning-trial.csv", stringsAsFactors=FALSE,
                   na.strings = c("", "N/A", "NA", "na", "n/a")) 
trials <- trials %>% mutate(tfresh.mass = initial.weight-final.weight) %>%
  mutate(start.time = mdy_hm(str_c(trial.date, " ",
                                   start.time),tz=TZ)) %>%
  mutate(end.time = mdy_hm(str_c(trial.date, " ", end.time), tz=TZ),
         label = paste(spcode, light, block, sep="")) 

trials$interval <- interval(trials$start.time, trials$end.time)



read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip=2, header=FALSE)
  names(hobo)[1:3] <- c("row", "time", "temp")
  hobo <- hobo %>% select(time, temp) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
    mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
  return(hobo) #change timezone
}

concat_hobo_files <- function(filelist, label){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  names(r) <- c("time", label)
  return(r)
}


# get sets for each of four thermocouple locations
base <- concat_hobo_files(list.files("../data/hobo",
                                      full.names=TRUE, pattern = "0CM*"), "base")
height10 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "10CM*"), "height.10")
height50 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "50CM*"), "height.50")
height100 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "100CM*"), "height.100")

# Now merge all of these together to get one continuous time series (wide
# data). Do we even need this? Really only necessary if we ever compare temps
# across thermocouples.
thermocouples.wide <- base %>% full_join(height10, by="time") %>% 
  full_join(height50, by="time") %>% 
  full_join(height100, by="time") 

thermocouples.wide <- thermocouples.wide %>% mutate_at(c("base", "height.10",
                                   "height.50", "height.100"),list(~ round(.,2)))

## get label from a time point
get_trial_label <- function(time) {
  matches <- time %within% trials$interval
  if(! any(matches)) return(NA)
  return(trials$label[which.max(matches)])
}


# assign labels
thermocouples.wide$label <- unlist(sapply(thermocouples.wide$time, get_trial_label))

# throw away data outside of a trial
thermocouples.wide <- thermocouples.wide %>% filter(! is.na(label))

# Long form data porbably more useful for per thermcouple summaries.
thermocouples.long <- thermocouples.wide %>% gather(location, temperature, -time, -label)

## then do the summary
threshold.a=60 # temperature threshold in degrees C
threshold.b=100

tempsec.sum <- thermocouples.long %>% group_by(label, location) %>%
  summarise(dur.60 = sum(temperature > threshold.a),
            dur.100 = sum(temperature > threshold.b),
            degsec.60 = sum(temperature[temperature > threshold.a]),
            degsec.100 = sum(temperature[temperature > threshold.b]),
            peak.temp = max(temperature, na.rm=TRUE),
            peak.time = time[which(peak.temp == temperature)[1]],
            num.NA = sum(is.na(temperature)))

## plot out temp summary to see how species burned in terms of flame temp

# grab spcode from trials
## plotdf <- trials %>% select(c("spcode", "label", "light")) %>% 
##   right_join(tempsec.sum, by ="label")

## ## degsec above 60
## ggplot(plotdf, aes(spcode, degsec.a, color = spcode)) + 
##   geom_jitter() + facet_grid(. ~ location) # by species and location

## ggplot(plotdf, aes(spcode, degsec.a, color = light)) + geom_jitter() +
##   facet_grid(. ~ location) # by species, location and light treatment

## ## degsec above 100

## ggplot(plotdf, aes(spcode, degsec.b, color = spcode)) + 
##   geom_jitter() + facet_grid(. ~ location) # by species and location

## ggplot(plotdf, aes(spcode, degsec.b, color = light)) + geom_jitter() +
##   facet_grid(. ~ location) # by species, location and light treatment


#clean up env
rm("concat_hobo_files", "get_trial_label", "read_hobo_file", "threshold.a",
   "threshold.b",  "thermocouples.wide", "thermocouples.long",
   "height10", "height50", "height100", 
   "base")

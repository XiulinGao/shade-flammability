## script to read anc clean up hobo and i-button (20190329 burn) data 
## for temperature and relative humidity data measured on each burning day

## read weather data from files produced by hobo logger
read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip=3, header=FALSE)
  names(hobo)[1:4] <- c("datet", "temp", "rh", "dewpt")
  hobo <- hobo %>% select(datet, temp, rh, dewpt) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
    mutate(datet = floor_date(ymd_hms(datet, tz=TZ), "second"))
  return(hobo) #change timezone
}

concat_hobo_files <- function(filelist){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  return(r)
}

hobo_weadata <- concat_hobo_files(list.files("../data/t_h",
                                     full.names=TRUE, pattern="*T_H"))
#convert temp into celcius 
hobo_weadata <- hobo_weadata %>% mutate(temp = (temp-32) * 5/9) %>% 
  mutate(temp = round(temp, 2))

## read data from file produced by ibutton
ib_weadata <- read.csv("../data/t_h/0329T.csv", skip=19)
names(ib_weadata) <- c("datet", "unit", "temp")
ib_weadata <- ib_weadata %>% select(datet, temp) %>%
  mutate(datet = floor_date(mdy_hms(datet, tz=TZ), "second"),
         temp = round(temp, 2)) #round date time

## bind two data files
weadata <- bind_rows(ib_weadata, hobo_weadata) %>%
  filter(!is.na(temp)) %>% # filter out row where there is no logged data 
  mutate(trial.date = as.Date(datet, tz=TZ, format = "%Y-%m-%d"))

## extrac temperatures that measured during each burning trial

get_trial_label <- function(time) {
  matches <- time %within% trials$interval
  if(! any(matches)) return(NA)
  return(trials$label[which.max(matches)])
}

# assign labels
weadata$label <- unlist(sapply(weadata$datet, get_trial_label))
burn_weather <- filter (weadata, !is.na(label))

#average temperature measurements for each trial
burn_weather <- burn_weather %>% group_by(label) %>% summarize(weatemp = round(mean(temp), 2))




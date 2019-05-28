## Read and clean biomass loss data

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
options(digits.secs=3)

#get data per trial

source("./burning-trial-summary.R")

# read and clean a single balance file produced by serial-balance.py. See
# https://github.com/schwilklab/serial-balance

## 1st way to do this
#read_balance_file <- function(filename) {
  #bdf <- read.csv(filename, sep="\t", skip = 2, stringsAsFactors=FALSE)
  # in hydraulic format, the first 2 lines are only text print, and only
  # 1 or 2 columns if sep="\t", which fails the data reading, so need to skip
  #n <- ncol(bdf) # use number of cols to separate the two formats
  #if(n == 4) {
  #names (bdf) <- c("datet", "label", "nsec", "mass")  
  #} else{ 
    #names (bdf) <- c("datet", "mass", "diff", "unk1", "unk2") 
    #bdf$label <- ""
  #}
  
  #bname <- basename(filename) 
  
  #bdf$label <- str_sub(bname, 1 , -5)
  #return(bdf)
#}

# read mutiple .csv files at once and rbind them as a file
#concat_csv_dir <- function(path) { 
  #files <- dir(path, pattern = '.csv', full.names = TRUE)
  #tables <- lapply(files, read_balance_file)
  #return(bind_rows(tables))
#}

#balance_data_skip2all <- concat_csv_dir('../data/balance') 
## still need to manually add time when balance starts to receive data
## in order to calculate accumulative seconds

#2nd way to do this
# read different formats separately

read_balance_raw <- function(filename_raw) {
  bdf_raw <- read.csv(filename_raw, sep="\t", skip = 1, stringsAsFactors=FALSE)
  names(bdf_raw) <- c("datet", "label", "nsec", "mass")
  bname_raw <- basename(filename_raw)
  
  bdf_raw$label <- str_sub(bname_raw, 1, -5)
  return(bdf_raw)
}


read_balance_hydraulic <- function(filename_hyd) {
 bdf_hyd <- read.csv(filename_hyd, sep="\t", skip = 2, stringsAsFactors=FALSE)
 bdf_time <- read.csv(filename_hyd, sep="\t", header=FALSE,stringsAsFactors=FALSE)
 # read it twice in different way, bdf_time is used to extract strat time when
 # laptop starts receiving balance data for cumulative seconds calculation
 names(bdf_hyd) <- c("datet", "mass", "diff", "unk1", "unk2")
 bdf_hyd$label <- ""
 bname_hyd <- basename(filename_hyd)
 
 bdf_hyd$label <- str_sub(bname_hyd, 1, -5)
 bdf_time$start <- bdf_time$V1[1] #extrat start time 
 ## from here I don't know how to calculate diff.time (between each current time and
 ## start time to obtain accumulative seconds in milliseconds, or being more specifically,
 ## i don't know how to convert character to time with milliseconds remained)
 #bdf_time <- bdf_time %>% lapply(as.POSIXct, format = "%d-%m-%y %H:%M:%S", tz = TZ )
 #bdf$nsec < - bdf %>% mutate_at()
 return(bdf_hyd)
}

# read mutiple .csv files at once and rbind them as a file based
# on files. balance data from 20190329 burn has different formats

# file names that contain all burns from 20190329 burning dates
# which are produced by raw-balance reading mode

sep_balance_file <- c("bogr2fsb4.csv", "agsc5fsb4.csv", "ercu2fsb4.csv", "elvi3fsb4.csv", 
                      "navi4fsb4.csv", "sesc2fsb4.csv", "arpu9fsb4.csv", "pava2fsb4.csv", 
                      "bogr2sb4.csv", "agsc5sb4.csv", "ercu2sb4.csv", "elvi3sb4.csv", 
                      "navi4sb4.csv", "sesc2sb4.csv", "arpu9sb4.csv", "pava2sb4.csv")

concat_csv_trial <- function(path) { 
  files <- dir(path, pattern = '.csv', full.names = TRUE)
  df <- data.frame(datet = "", label = "", mass ="", nsec = "", diff = "",
                   unk1 = "", unk2 = "") #define empty data frame to store returned 
  # balance data in ifelse loop
  
  for (i in 1:length(files)) {
  if (basename(files[i]) %in% sep_balance_file) {
    table_raw <- read_balance_raw(files[i])
    df <- bind_rows(table_raw, df)
  } else{
    table_hyd <- read_balance_hydraulic(files[i])
    df <- bind_rows(table_hyd, df)
  }
  }
  return (df)
}

balance_data <- concat_csv_trial("../data/balance") 
# strip off line with NA
balance_data <- filter(balance_data, !is.na(label))

#calculate cumulative nsec for balance data 


# join balance data with burning-trials.csv
balance_data <- left_join(balance_data, trials, by = "label")
balance_data <- balance_data %>%
  mutate(mass = mass * 0.001) %>% # mass to g
  group_by(label) %>% 
  mutate(
    is.flaming = nsec > 50+ignition & nsec < 50+ignition+combustion,
    is.smoldering = nsec > 50+ignition+combustion & nsec < 50+ignition+combustion+smoldering,
    septime = 50+ignition+combustion)


# graph biomass loss based on time in seconds
ID <- unique(balance_data$label)
for (i in 1: length(unique(ID))) {  
  onelabel <- filter(balance_data, label==ID[i]) # subset
  flamburn <- filter(onelabel, is.flaming) # only flaming stage
  pdf(file.path("../results", file=paste(ID[i], ".pdf", sep=""))) # plot in pdf
  print(qplot(nsec, mass , data=flamburn, geom="point") + 
          geom_vline(xintercept = onelabel$septime[1]))
  dev.off()
}

#clean up env
rm("df", "concat_csv_dir", "read_balance_file", "contact_csv_trial", "read_balance_raw",
   "read_balance_hydraulic")

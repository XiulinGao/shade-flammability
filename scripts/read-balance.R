## Read and clean biomass loss data

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
#options(digits.secs=3) 

BALANCE_DATA_DIR <- "../data/balance"

## get data per trial
source("./burning-trial-summary.R")

# read and clean a single balance file produced by serial-balance.py. See
# https://github.com/schwilklab/serial-balance

## balance data was read under two different modes. Data from 20190329 burning date 
## was under raw balance data reading mode and the data has mainly 3 outputs:
## time in ymd:hmos, weight in mg, and accumulative number of seconds in millionseconds 
## since the first reading. Data after that were are read under hydraulic mode, 
## of which the outputs contain time, weight, and other 3 columns that are 
## not necessary for loss rate calculation.because data are stored in 
## two different formats, we have to write code to deal with it
## and mainly extract the time and weight from each file for loss rate calculation later. 

# file names that contain all burns from 20190329 burning dates
# which are produced by raw-balance reading mode

## raw_format_files <- ("bogr2fsb4.csv", "agsc5fsb4.csv", "ercu2fsb4.csv",
##                       "elvi3fsb4.csv", "navi4fsb4.csv", "sesc2fsb4.csv",
##                       "arpu9fsb4.csv", "pava2fsb4.csv", "bogr2sb4.csv",
##                       "agsc5sb4.csv", "ercu2sb4.csv", "elvi3sb4.csv",
##                       "navi4sb4.csv", "sesc2sb4.csv", "arpu9sb4.csv",
##                       "pava2sb4.csv")

read_balance_file <- function(filen) {
  label <- tools::file_path_sans_ext(basename(filen))
  file_string <- readr::read_file(filen)

  if(str_detect(file_string, "Enter a label:")) {
    # print("We have a raw format file")
    file_string <- str_sub(file_string, 16)
    needed_cols <- c(1,4) # "label" col are all "" empty so weight is 4th col
  } else {
    # print("We have a hydro format file")
    pos <- str_locate(file_string, "[^\n]+\n[^\n]+\n")[2]
    file_string <- str_sub(file_string, pos+1)
    needed_cols <- c(1,2)
  }

 #  print("Reading data")
  # Now read the file from the string and extract the two columns we need, time
  # and weight.
  file_dat <- read.csv(text=file_string, header=FALSE, sep="\t",
                         stringsAsFactors=FALSE)

  file_dat <- file_dat[,needed_cols]
  
  names(file_dat) <- c("time", "weight")
  file_dat <- mutate(file_dat, time = gsub(",", ".", time, fixed=TRUE),
                     time = ymd_hms(time),
                     diff_time = as.numeric(time - time[1]),
                     label = label)
  
  return(file_dat)
}
    
  
read_all_balance_files <-function() {
  file_names <- list.files(BALANCE_DATA_DIR, ".csv", full.names=TRUE)
  file_dat <- lapply(file_names, read_balance_file)
  return(bind_rows(file_dat))
}

  
# main script:
balance_data <- read_all_balance_files()

# join trial data and convert mg to g for weight

balance_data <- left_join(balance_data, trials, by = "label")
balance_data <- balance_data %>%
  mutate (weight = weight*0.001)



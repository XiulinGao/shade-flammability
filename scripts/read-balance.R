## Read and clean biomass loss data

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
#options(digits.secs=3)  ## DWS: ooh, I do not like this setting of global
                        ## options. Why is this necessary?

BALANCE_DATA_DIR <- "../data/balance"

## get data per trial
source("./burning-trial-summary.R")

# read and clean a single balance file produced by serial-balance.py. See
# https://github.com/schwilklab/serial-balance

## DWS: add comment explaining why the raw data is in two different formats so
## that the complicated code below makes sense.
# read mutiple .csv files at once and rbind them as a file based
# on files. balance data from 20190329 burn has different formats

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
    needed_cols <- c(1,3)
  } else {
  #  print("We have a hydro format file")
    pos <- str_locate(file_string, "[^\n]+\n[^\n]+\n")[2]
    file_string <- str_sub(file_string, pos+1)
    needed_cols <- c(1,2)
  }

 #  print("Reading data")
  # Now read the file from the string and extract the two columns we need, time
  # and weight.
  file_dat <- read.csv(text=file_string, header=FALSE, sep="\t",
                       stringsAsFactors=FALSE) %>%
    dplyr::select(needed_cols)
  
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

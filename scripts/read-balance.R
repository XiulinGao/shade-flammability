## Read and clean biomass loss data
#options(digits.secs=3) 

BALANCE_DATA_DIR <- "../data/balance"

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


## After reading in the raw data, calculate maximum biomass loss rate with

###############################################################################
## Read balance data
###############################################################################

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


###############################################################################
## Calculate loss rate
###############################################################################


## there are character inputs for ignition as failed ignition, which 
## make the entire column as character variable, convert it to numeric
## and mark time period when it's flaming(is.flaming) or smoldering stage (is.smoldering)
## as well mark when flaming combustion ends (septime)

balance_data <- balance_data %>% group_by (label) %>%
  mutate(ignition = as.numeric(ignition)) %>%
  mutate(is.flaming = diff_time>50+ignition & diff_time<50+ignition+combustion,
  is.smoldering = diff_time>50+ignition+combustion & diff_time<50+ignition+combustion+smoldering,
         septime = 50+ignition+combustion)

# plot weight against diff_time for each trial
# complete trial and flaming combustion only
#label <- unique(balance_data$label)

#for (i in 1:length(label)) {
  #plabel <- label[i]
  #subdata <- filter(balance_data, label == plabel)
  #flaming <- filter(subdata, is.flaming)
  #pdf(file.path("../results", file=paste(plabel, ".pdf", sep=""))) # plot in pdf
  #pdf(file.path("../results", file=paste(plabel, "flaming", ".pdf", sep="")))
 # print(qplot(diff_time, weight, data=subdata, geom="point") + 
          #geom_vline(xintercept = subdata$septime[1]))
  #print(qplot(diff_time, weight, data = flaming, geom="point"))
  #dev.off() 
#}

## fit a mass decay model to entire balance and flaming only data (both linear and 
## negative exponential models) to see which works best
## the fitted coefficient from the best model will be extracted as maximum 
## biomass loss rate when p<0.05

# some basic summary to calculate biomass lost during burn based on 
# balance data
#balance_sum <- balance_data %>% group_by(label) %>% 
# summarize(balance.initial = mean(weight[diff_time<50]),
# balance.final = mean(weight[diff_time > (max(diff_time) - 20)]),
#            balance.burned = balance.initial - balance.final) %>%
#  left_join(trials, by="label") %>% 
#  mutate(fconsum = initial.weight - end.weight) %>%
#  select(-interval)

# see how balace-data-based biomass loss match up with recorded biomass loss in 
# burning-trials.csv

# ggplot(filter(balance_sum, balance.burned<100), 
#       aes(fconsum, balance.burned)) + geom_point() +
# geom_abline(intercept = 0, slope = 1) #fits perfectly, except there
# are two trials had very large balance.burned data, probably due to 
# the windy environment or disturbance during burns

## Approach 1: fit entire balance data to a negative exponential curve

bytrial <- balance_data %>% group_by (label) %>%
  mutate(decaymass = weight - min(weight))

flam.bytrial <-  balance_data %>% group_by (label) %>%
  filter(is.flaming) %>%  mutate(decaym = weight - min(weight),
                                 decayt = diff_time - min(diff_time))

decayID <- unique(bytrial$label)
flamID <- unique(flam.bytrial$label)
nlaics <- numeric(length(decayID))
flam.nlaics <- numeric(length(decayID))

decayNLModsCoef <- data.frame(label=character(),spcode=character())
flamNLModsCoef <- data.frame(label=character(), spcode=character())

for (i in 1:length(decayID)){
  
  subdecay <- filter(bytrial, label==decayID[i])
    decayNLMod <- nls(decaymass ~ a*exp(b*diff_time), data=subdecay, 
                        start=list(a=subdecay$decaymass[1],b=0))
    nlaics[i] <- AIC(decayNLMod) #calculate AIC for each fit
    mod_coef <- tidy(decayNLMod) #get model coef as data frame
    mod_coef$label <- subdecay$label[1]
    mod_coef$spcode <- subdecay$spcode[1]
    mod_coef <- mod_coef[, c(1:2, 5:7)]
    decayNLModsCoef<- bind_rows(mod_coef, decayNLModsCoef)
    flammods <- tryCatch({
      if (decayID[i] %in% flamID){
        subflam <- filter(flam.bytrial, label==decayID[i])
        flamNLMod <- nls(decaym ~ c*exp(d*decayt), data=subflam, 
                         start=list(c=mean(subflam$decaym[1:3]), d=0))
        flam.nlaics[i] <- AIC(flamNLMod)
        flam_coef <- tidy(flamNLMod)
        flam_coef$label <- subflam$label[1]
        flam_coef$spcode <- subflam$spcode[1]
        flam_coef <- flam_coef[, c(1:2, 5:7)]
        flamNLModsCoef <- bind_rows(flam_coef, flamNLModsCoef)
      } else {
        flam.nlaics[i] <- NA}
       }, error = function(e){
        message("caught error")
        print(e) #print original error message
        print(paste(decayID[i], "can't fit exponential model", sep=" "))
      }
    )
}
    
decayNLModsCoef_sig <- decayNLModsCoef %>% filter(term=="b", estimate<0) %>%
  filter(p.value < 0.05) 
flamNLModsCoef_sig <- flamNLModsCoef %>% filter(term=="d", estimate<0) 
#%>%filter(p.value < 0.05)

ggplot(decayNLModsCoef_sig, aes(spcode, estimate)) + geom_point()
ggplot(flamNLModsCoef_sig, aes(spcode, estimate)) + geom_point()
# seems maximum biomass loss rate didn't differ among species
#qplot(flam.nlaics, nlaics) 
# only fit negative exponential to flaming stage works better


## Approach 2: fit flamming stage balance data to linear model

flamingLMods <- flam.bytrial %>% nest(data = -label) %>% 
  mutate(
    fit = map(data, ~ lm(decaym ~ decayt, data = .x)),
                           tidied = map(fit, tidy)
  ) %>% unnest(tidied)

flamingLMcoef <- flamingLMods %>% filter(term=='decayt') %>% 
  filter(estimate<0) %>% select(-data, -fit)

flamingLMcoef_sig <- flamingLMcoef %>%
  filter(p.value <0.05)
laics <- sapply(flamingLMods$fit, AIC)

#flam.nlaics <- as.numeric(flam.nlaics)
#flam.nlaics <- as.data.frame(flam.nlaics)
#flam.nlaics <- filter(flam.nlaics, !is.na(flam.nlaics))
#laics <- as.data.frame(laics)
#plot(flam.nlaics$flam.nlaics, laics$laics)
# negative exponential model works better. In conclusion, negative exponential model
# fit for only flaming stage works the best

#rename column 'term' to 'lossrate' in flamingNLModsCoef_sig
colnames(flamNLModsCoef_sig)[colnames(flamNLModsCoef_sig)=="estimate"] <- "lossrate"
## clean env
rm("decayID", "i", "subdecay", "mod_coef", "subflam",
   "flam_coef", "flamNLMod", "flamID", "flammods",
   "flamingLMods", "decayNLMod", "decayNLModsCoef", "flamingLMcoef",
   "flamNLModsCoef", "flam.bytrial", "bytrial")

## this script is executed to calculate maximum
## biomass loss rate with using balance_data
## from read-balance.R

library(dplyr)
library(ggplot2)
library(broom)

source("./read-balance.R")

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
#  plabel <- label[i]
# subdata <- filter(balance_data, label == plabel)
#  flaming <- filter(subdata, is.flaming)
#  pdf(file.path("../results", file=paste(plabel, ".pdf", sep=""))) # plot in pdf
#  pdf(file.path("../results", file=paste(plabel, "flaming", ".pdf", sep="")))
#  print(qplot(diff_time, weight, data=subdata, geom="point") + 
#          geom_vline(xintercept = subdata$septime[1]))
#  print(qplot(diff_time, weight, data = flaming, geom="point"))
#  dev.off() 
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
flamNLModsCoef_sig <- flamNLModsCoef %>% filter(term=="d", estimate<0) %>%
  filter(p.value < 0.05)

ggplot(decayNLModsCoef_sig, aes(spcode, estimate)) + geom_point()
ggplot(flamNLModsCoef_sig, aes(spcode, estimate)) + geom_point()
# seems maximum biomass loss rate didn't differ among species
#qplot(flam.nlaics, nlaics) 
# only fit negative exponential to flaming stage works better


## Approach 2: fit flamming stage balance data to linear model

flamingLMods <- flam.bytrial %>% group_by(label) %>%
  do(flamingmod = lm(decaym ~ decayt, data = .)) 

flamingLMcoef <- tidy(flamingLMods, flamingmod) %>%
  filter(estimate < 0)

flamingLMcoef_sig <- tidy(flamingLMods, flamingmod) %>%
  filter(estimate < 0) %>%
  filter(term == "decayt", p.value <0.05)
laics <- sapply(flamingLMods$flamingmod, AIC)

#flam.nlaics <- as.numeric(flam.nlaics)
#flam.nlaics <- as.data.frame(flam.nlaics)
#flam.nlaics <- filter(flam.nlaics, !is.na(flam.nlaics))
#laics <- as.data.frame(laics)
#plot(flam.nlaics$flam.nlaics, laics$laics)
# negative exponential model works better. In conclusion, negative exponential model
# fit for only flaming stage works the best
## clean env
rm("decayID", "i", "subdecay", "mod_coef", "subflam",
   "flam_coef", "flamNLMod", "flamID", "flammods",
   "flamingLMods", "decayNLMod")
  


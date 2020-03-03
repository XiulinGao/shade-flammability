## all-data.R
## script that source all required data for further analysis
## runs before running analysis script

source("./hobo-temp-summary.R") #need tempsec.sum for temperature summary
source("./loss-rate-cal.R") #need flamNLModsCoef_sig for mass loss rate
source("./plant-traits.R") #need trait.trial dataset for all plant traits and trial measurements

# join all data to make a complete dataset where all info can be found
alldata <- tempsec.sum %>% left_join(filter(trait.trial,treatment == "b"), by = "label") %>% 
  left_join(flamNLModsCoef_sig, by = c("label", "spcode")) %>%
  select(-peak.time, -num.NA, -treatment, -wind, -interval, -start.time, -end.time,
         -term, -p.value, -end.weight, -final.weight, -f.fuelresid, -tfresh.mass, 
         -initial.weight, -post.fmc, -t.fuelresid,  -above.drym3,
         -combust.mass3, -pre.fmc3)

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
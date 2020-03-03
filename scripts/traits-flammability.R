## traits-flammability.R
## determine how flammability varies in response to trait variations 
## traits: biomass, fmc, specific leaf area, and canopy trait
library(lme4)
library(car)
library(dplyr)

source("./shade-flammability.R") ## flamdt is the dataset we will use
source("./read-weather.R")## get temperature data for each trial: burn_weather dataset

flamdt <- flamdt %>% left_join(burn_weather, by="label")
flamdt <- flamdt %>% mutate(weatemp_s = zscore(weatemp))

## mod 1: trait effects on soil heating ##
traitsoil_mod <- lmer(heatb_log ~ above.drym_s*pre.fmc_s*ave.sla_s*bulkden_s +
                              weatemp_s - above.drym_s:pre.fmc_s:ave.sla_s:bulkden_s +
                              (1|spcode), flamdt, REML=TRUE)
summary(traitsoil_mod)
traitsoilaov <- Anova(traitsoil_mod, type="3", test.statistic = "F")
traitsoilaov

##mod 2: trait effects on mid-camopy heating ##
trait50_mod <- lmer(heat50_log~ above.drym_s*pre.fmc_s*ave.sla_s*bulkden_s +
                            weatemp_s - above.drym_s:pre.fmc_s:ave.sla_s:bulkden_s +
                            (1|spcode), flamdt, REML=TRUE)
summary(trait50_mod)
trait50aov <- Anova(trait50_mod, type="3", test.statistic = "F") 
trait50aov

# biomass positively influenced mid-canopy and soil heating, fmc negatively influenced 
# mid-canopy and soil heating. 
# there is a positive interaction between fmc and sla on soil heating

## how fuel moisture content is related to live:dead biomass ratio, light, and 
## specific leaf area

#filter out one observation where live:dead ratio is infinite number
fmcdt <- filter(flamdt, !ldratio=="Inf")
#fmcdt <- fmcdt %>% mutate (ave.sla, ave.sla_s = zscore(ave.sla))
fmcmod <- lmer(pre.fmc ~ ave.sla_s*light*ldratio + (1|spcode), fmcdt, REML = TRUE)
summary(fmcmod)
fmcaov <- Anova(fmcmod, type = 3, test.statistic = "F")
fmcaov
#plot(fmcmod)

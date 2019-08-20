## rank shade tolerance of 17 different grasses based on relative above 
## ground biomass gained 4 months after shade treatment by plants growing under
## 50% shade in comparison with plants growing under full sun light

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
#library(afex)
library(xtable)

## final biomass and species data
fmass <- read.csv("../data/shade-flam-final-biomass.csv", stringsAsFactors=FALSE,
                  na.strings = c("", "NA", "na"))
imass <- read.csv("../data/shade-flam-initial-biomass.csv", stringsAsFactors=FALSE,
                  na.strings = c("", "NA", "na"))
species <- read.csv("../data/species-info.csv", stringsAsFactors = FALSE)

# average initial mass for each species
ave.imass <- imass %>% group_by (spcode) %>% 
  summarize (imass.ave = mean (biomass, na.rm=TRUE)) 

# round imass.ave to 4 digits
ave.imass$imass.ave <- round(ave.imass$imass.ave, digits = 4)

# subtract average biomass from final biomass for all plants to obtain
# biomass gained 4 months after treatment
mass.gain <- left_join (fmass, ave.imass, by = "spcode")
mass.gain <- mass.gain %>% group_by (spcode, treatment, block) %>%
  summarise (gmass = biomass - imass.ave)

ggplot(mass.gain, aes(treatment, gmass)) + geom_boxplot() + facet_wrap(~spcode) 
#ggsave("../results/final-biomass.jpeg", width=col1, height= 0.9*col1)

## how do species response to shade differently in terms of biomass gain?

## average percentage biomass gained by shaded plants compare to sun plants
gmass.wide <- mass.gain %>% group_by(spcode) %>% spread(treatment, gmass)
gmass.wide <- gmass.wide %>% group_by(spcode) %>% mutate(percnt.gain = s/fs) 
gmass.wide$percnt.gain <- round(gmass.wide$percnt.gain,4)

# average % biomass gain by species
massgain.sp <- gmass.wide %>% group_by(spcode) %>% 
  summarize(ave.gain = mean(percnt.gain, na.rm=TRUE)) 
massgain.sp$ave.gain <- round(massgain.sp$ave.gain, digits = 4)

## rank averaged percentage biomass gain for all species
require(data.table)
massgain.sp <- data.table(massgain.sp, key="ave.gain")

## attach species name
massgain.sp <- left_join(species, massgain.sp, by="spcode")

## use model coefficient to rank
rankshade.mod <- lmer(percnt.gain ~ spcode + (1|block),gmass.wide, REML=FALSE)
summary(rankshade.mod)
print(xtable(summary(rankshade.mod)$coefficients), type = "html", 
      file = "../results/rank-shade-mod.html")
## very similar ranking

#save mod outputs as data frame
modrank <- tidy(rankshade.mod)
#extrac the spcode out of term 
modrank$spcode <- substring(modrank$term, 7)
#assign agsc5 to first spcode element and strip off the last 2 rows and 1st, 5th column
modrank$spcode[1] <- "agsc5"
modrank <- modrank[-c(18:19), c(2, 6)]
colnames(modrank)[colnames(modrank)=="estimate"] <- "shade.rank"
## clean environment
rm(fmass, imass, ave.imass)


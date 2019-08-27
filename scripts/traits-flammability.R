##traits-flammbility.R
## to explore how species vary in flammability as response to variation in traits
## including shade tolerance, leaf traits and architecture. 

library(lme4)
library(afex)
library(pcaMethods)
library(dplyr)

source("./hobo-temp-summary.R") #need tempsec.sum for temperature summary
source("./loss-rate-cal.R") #need flamNLModsCoef_sig for mass loss rate
source("./plant-traits.R") #need trait.trial dataset for all plant traits and trial measurements
# join all data to make a complete dataset where all info can be found
alldata <- tempsec.sum %>% left_join(filter(trait.trial,treatment == "b"), by = "label") %>% 
  left_join(flamNLModsCoef_sig, by = c("label", "spcode")) %>%
  select(-peak.time, -num.NA, -treatment, -wind, -interval, -start.time, -end.time,
         -term, -p.value, -end.weight, -final.weight, -f.fuelresid, -tfresh.mass, 
         -initial.weight, -post.fmc, -t.fuelresid)
colnames(alldata)[colnames(alldata)=="ave.gain"] <- "shade.rank"
###1. PCA of flammability 
flamPCA <- filter(alldata, location == "height.10") %>% 
  select ( dur.100, degsec.100, lossrate, combust.mass2,
           max.flam) %>% pca(nPcs=3, method="nipals",
                           center=TRUE,scale="uv")
summary(flamPCA)
biplot(flamPCA) #two dimention: rate of heat release and total heat release
flampca.loads <- as.data.frame(loadings(flamPCA))
# 1st axis: dur and degsec; 2nd axis: lossrate
flampca.scores <- as.data.frame(scores(flamPCA))

##2. whetehr how species vary in flammability in terms of temp, heating duration, 
## max. flame height, and max. loss rate. is variation related to shade tolerance?

# full mixed effects model that includes all possible effects
flam10 <- filter(alldata, location=="height.10")
#rescale variables
zscore <- function(x) (x - mean(x)) / sd(x) 
flam10_s <- flam10 %>% 
  dplyr::mutate_at(c("shade.rank", "pre.fmc", "curate", "above.drym2", "photo.ratio", "massden"), 
  zscore)

                             
dur100ten.fullmod <- lmer(dur.100 ~ shade.rank*light*pre.fmc*curate*above.drym2*photo.ratio*massden + 
                           (1|spcode) + (1|block), data = flam10,
                          REML = FALSE)

dur100ten.mod1 <- lmer(dur.100 ~ shade.rank*light*curate + 
                            (1|spcode) + (1|block), data = flam10,
                          REML = FALSE)
anova(dur100ten.fullmod, dur100ten.mod1)
#full mod 
dur100ten.mod2 <- lmer(dur.100 ~ shade.rank*light*above.drym2 + 
                            (1|spcode) + (1|block), data = flam10,
                          REML = FALSE)
anova(dur100ten.fullmod, dur100ten.mod2)
#full mod is not necessarily better

dur100ten.mod3 <- lmer(dur.100 ~ shade.rank*above.drym2 + 
                            (1|spcode) + (1|block), data = flam10,
                          REML = FALSE)
anova(dur100ten.mod2, dur100ten.mod3)
#mod3
dur100ten.mod4 <- lmer(dur.100 ~ above.drym2 + 
                            (1|spcode) + (1|block), data = flam10,
                          REML = FALSE)
anova(dur100ten.mod4, dur100ten.mod3)
#mod 3 
anova(dur100ten.mod3)
ggplot(flam10, aes(shade.rank, dur.100)) + geom_point() +
  geom_smooth(method = lm, se=FALSE)



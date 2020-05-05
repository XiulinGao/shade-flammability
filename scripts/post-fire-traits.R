## post-fire-traits.R
## determine how post-fire survival rate varied among species with 
## different photosythetic pathway, shade tolerance and flammability (soil heating)

library(dplyr)
library(lme4)
library(car)
source("./all-data.R") #need pre-burn tiller number from catiller and catiller.open
options(contrasts = c("contr.sum", "contr.poly"))

catiller$label <- paste(catiller$spcode, catiller$light, catiller$block, sep="")
catiller.open$label <- paste(catiller.open$spcode, catiller.open$light, 
                             catiller.open$block, sep="")
pre_tinum1 <- catiller %>%  group_by(label) %>% summarize(pre_tinum = tiller.num[1]) 
                                                                          
pre_tinum2 <- catiller.open %>% group_by(label) %>% summarize(pre_tinum = tiller.num[1]) 
                                                                               
pre_tinum <- rbind(pre_tinum1, pre_tinum2)

post_traits <- read.csv("../data/post-fire-traits.csv", stringsAsFactors=FALSE,
                        na.strings = c(""))
#soilmc <- read.csv("../data/soil-moisture.csv", stringsAsFactors = FALSE, na.strings = c(""))

post_traits$label <- paste(post_traits$spcode, post_traits$light, post_traits$block, 
                           sep="")
#soilmc$label <- paste(soilmc$spcode, soilmc$light, soilmc$block, 
                      #sep="")
survi_dt <- post_traits %>% #left_join(soilmc, by = c("spcode", "light", "block", "label")) %>% 
  left_join(flamdt, by = c("spcode", "light", "block", "label")) %>% left_join(pre_tinum,
                                                                 by = "label") %>% 
  select("spcode", "light", "block", "label",  "heatb", "phototype", "st", "above.drym",
         "curate", "pre_tinum", "tiller.num",  "max.leaflen", "tmass.dry", "survival") 

colnames(survi_dt)[which(colnames(survi_dt)=="tiller.num")] <- "post_tinum"

### H: shade intolerant grasses and C4 grasses are better fire-adapted (higher survival rate###
###Grasses produce less heat at soil surface during fire also have higher survival ratee ###

#rescale varibales
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 
survi_dt <- survi_dt %>% mutate_at(c('pre_tinum', 'heatb', 'st', 'above.drym'), 
                                   funs(s = zscore(.)))

##model survival and resprouting strength separately using logistic and liner
## mixed effect model

#survival rate logistic mod

survi_mod <- glm(survival ~ heatb_s*pre_tinum_s*st_s, 
                  survi_dt, family = binomial)
summary(survi_mod)
survi.aov <- car::Anova(survi_mod, type = 3)
survi.aov

##resprout strength linear mixed effect mod
respdt <- filter(survi_dt, tmass.dry != 0) %>% # excluded individuals didn't resprout
  mutate(recover_pcnt = tmass.dry/above.drym) %>% 
  mutate(log_pcntm = log(recover_pcnt))

resp_lmmod <- lmer(log_pcntm ~ heatb_s*pre_tinum_s*st_s + (1|spcode),
                    respdt, REML = TRUE)
#plot(resp_lmmod) #check resid normality 
summary(resp_lmmod)
resp.aov <- Anova(resp_lmmod, type = 3, test.statistic = "F")
resp.aov
#AIC(resp_lmmod)

#check prediction
#respdt$pred_resp <- predict(resp_lmmod, newdata = respdt)
#ggplot(respdt, aes(pre_tinum, postinum_log)) + geom_point() + 
  #geom_point(data = respdt, aes(pre_tinum, pred_resp), color = "red")

#ggplot(respdt, aes(heatb, postinum_log)) + geom_point() +
  #geom_point(data = respdt, aes(heatb, pred_resp), color = "red")

#clean env

rm("pre_tinum1", "pre_tinum2", "pre_tinum", "post_traits")

## analyses.R

source("./all-data.R") # run this once only

## former shade-flammbility.R
## to explore how species vary in flammability as response to variation in
## shade tolerance. 

options(contrasts = c("contr.sum", "contr.poly")) #type III anova table, set treatment contrast
## DWS: danger setting global options! Better to pass to the function.

zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

## mutate_cond Create a simple function for data frames or data tables that can be
## incorporated into pipelines. This function is like mutate but only acts on the
## rows satisfying the condition. See  https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}



#################### correlation matrix of flam&trait measurements ####################

## because experiment methods influenced heat release at 50cm (see supplementary-
## material-figs.R), need to throw away obs for heat50 from 3/29/19 trial date
## for any analysis for heat50
#flam50 <- flamdt %>% filter(trial.date!="3/29/19")

# Just replace those values with NA

flamdt <- mutate_cond(flamdt, trial.date=="3/29/19", t50.ini = NA, t50.flam = NA, t50.smol = NA,
                      diff50=NA, heat50=NA, heat50_log=NA)

cor.df <- flamdt %>% select(heatb,heat50, 
                            ave.sla, ave.sav,bulkden, 
                            above.drym, pre.fmc)
varcors <- cor(cor.df, method = "kendall", use = "pairwise")
corrplot::corrplot(varcors)
#scater plot to see if it is linear relationship
pairs(cor.df)

###### H: as shade tolerance increases, species flammability decrases ###### 

#rescale all independent variables and log transform dependent variable
flamdt <- flamdt %>% mutate_at(c("st", "above.drym", "pre.fmc", "ave.sla", "bulkden"),
                               list(s = zscore)) #funs() is depressed, use list()


#shade tolerance and soil heating mod
baseheat_mod <- lmer(heatb_log ~ above.drym_s*st_s*light*phototype -
                       above.drym_s:st_s:light:phototype + (1|spcode), 
                       flamdt, REML=TRUE)
#plot(baseheat_mod)
summary(baseheat_mod)
baseanova <- Anova(baseheat_mod, type ="3", test.statistic = "F")
baseanova

#shade tolrance and heat50 mod

heat50_mod <- lmer(heat50_log ~ above.drym_s*st_s*light*phototype -
                     above.drym_s:st_s:light:phototype + (1|spcode), 
                         flamdt, REML=TRUE)
#plot(heat50_mod)
summary(heat50_mod)
heat50anova <- Anova(heat50_mod, type = "3", test.statistic = "F")
heat50anova

## There are sig. negative effects of shade tolerance on 
## mid-canopy heating. 

## traits-flammability
## determine how flammability varies in response to trait variations 
## traits: biomass, fmc, specific leaf area, and canopy trait

#bind weather measurements to each trial
flamdt <- flamdt %>% left_join(burn_weather, by="label")
#z-score air temperature measurements
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
# mid-canopy and soil heating.biomass density negatively influenced mid-canopy heating
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



## former post-fire-traits.R
## determine how post-fire survival rate varied among species with 
## different shade tolerance, tiller, and flammability (soil heating)

## post-fire survival dataset
survi_dt <- post_traits %>% #left_join(soilmc, by = c("spcode", "light", "block", "label")) %>% 
  left_join(flamdt, by = c("spcode", "light", "block", "label")) %>% left_join(pre_tinum,
                                                                               by = "label") %>% 
  select("spcode", "light", "block", "label",  "heatb", "phototype", "st", "above.drym",
         "curate", "pre_tinum", "tiller.num",  "max.leaflen", "tmass.dry", "survival") 

colnames(survi_dt)[which(colnames(survi_dt)=="tiller.num")] <- "post_tinum"
#rescale varibales
survi_dt <- survi_dt %>% mutate_at(c('pre_tinum', 'heatb', 'st', 'above.drym'), 
                                   list(s = zscore)) 

##model survival and resprouting strength separately using logistic and liner
## mixed effect model

#survival rate logistic mod

survi_mod <- glm(survival ~ heatb_s*pre_tinum_s*st_s*phototype -
                   heatb_s:pre_tinum_s:st_s:phototype, 
                  survi_dt, family = binomial)
summary(survi_mod)
survi.aov <- car::Anova(survi_mod, type = 3)
survi.aov

##resprout strength linear mixed effect mod
respdt <- filter(survi_dt, tmass.dry != 0) %>% # excluded individuals didn't resprout
  mutate(recover_pcnt = tmass.dry/above.drym) %>% 
  mutate(log_pcntm = log10(recover_pcnt)) #calculate percentage biomass recovered 
                                        # and do log transformation
resp_lmmod <- lmer(log_pcntm ~ heatb_s*pre_tinum_s*st_s*phototype -
                     heatb_s:pre_tinum_s:st_s:phototype + (1|spcode),
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

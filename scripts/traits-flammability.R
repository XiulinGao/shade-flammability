##traits-flammbility.R
## to explore how species vary in flammability as response to variation in traits
## including shade tolerance, leaf traits and architecture. 

library(lme4)
#library(afex)
library(pcaMethods)
library(dplyr)

source("./hobo-temp-summary.R") #need tempsec.sum for temperature summary
source("./loss-rate-cal.R") #need flamNLModsCoef_sig for mass loss rate
source("./plant-traits.R") #need trait.trial dataset for all plant traits and trial measurements
source("./ggplot-theme.R")
# join all data to make a complete dataset where all info can be found
alldata <- tempsec.sum %>% left_join(filter(trait.trial,treatment == "b"), by = "label") %>% 
  left_join(flamNLModsCoef_sig, by = c("label", "spcode")) %>%
  select(-peak.time, -num.NA, -treatment, -wind, -interval, -start.time, -end.time,
         -term, -p.value, -end.weight, -final.weight, -f.fuelresid, -tfresh.mass, 
         -initial.weight, -post.fmc, -t.fuelresid, -above.drym, -above.drym3,
         -combust.mass, -combust.mass3, -pre.fmc, -pre.fmc3)

alldata <- ungroup(alldata)
alldata <- alldata %>% mutate(heat50 = t50.flam-t50.ini, heatb = tbase.flam - tbase.ini)
colnames(alldata)[colnames(alldata)=="ave.gain"] <- "shade.rank"

############## PCA of flammability and trait measurements ##############

pcadt <- alldata %>% select(-peak.temp, -dur.100, -degsec.100, -dur.60) 
pcadt <- spread(pcadt, location, degsec.60)

flamPCA <- pcadt %>% 
  select ( base, height.10, height.50, height.100, lossrate, combust.mass2,
           max.flam, ignition, combustion, heat50, heatb) %>% pca(nPcs=8, method="ppca",
                         center=TRUE,scale="uv")
summary(flamPCA)
biplot(flamPCA) 
flampca.loads <- as.data.frame(loadings(flamPCA))
flampca.loads
# 1st axis: base temp and combustbiomass; 2nd axis: height.100 temp , 3rd axis: combustion
# first 3 axes account for 83.5% total variance in the dataset. 

flampca.scores <- as.data.frame(scores(flamPCA))


traitPCA <- pcadt %>% 
   select(ave.sla, ave.sav, pre.fmc2, curate, above.drym2, leafmass, bulkden) %>% 
  pca(nPcs = 7, method = "svd", center=TRUE, scale="uv")
summary(traitPCA)
biplot(traitPCA)
traitpca.loads <- as.data.frame(loadings(traitPCA))
traitpca.loads
## measurements have large loadings on the first 4  axes: leafmass, above.drym2, 
## bulkden pre.fmc2, ave.sav and ave.sla, account for 85.3% total variance in the 
## data

#################### correlation matrix of flam&trait measurements ####################
flam10 <- pcadt
cor.df <- flam10 %>% select(base, heatb, height.100, heat50,combustion,combust.mass2,  
                            ave.sla, ave.sav, bulkden, leafmass,
                            above.drym2, pre.fmc2)
varcors <- cor(cor.df, method = "kendall", use = "pairwise")
corrplot::corrplot(varcors)
#scater plot to see if it is linear relationship
pairs(cor.df)

###### H1: as shade tolerance increases, species flammability decrases ###### 
##such flammability variation is not simply caused by physical environment variation or
## other species specific variation ##
flam10 <- pcadt
spmean <- flam10 %>% group_by(spcode, display.name, light) %>% 
  summarise_at(c("shade.rank", "combust.mass2", "base", "height.100",
                        "heat50", "heatb", "combustion", "ave.sla", "ave.sav",
                 "pre.fmc2", "above.drym2", "bulkden"), mean, na.rm=TRUE)
#graphic explor
ggplot(flam10, aes(shade.rank, combust.mass2)) + geom_point() + facet_grid(.~light) +
geom_point(data = spmean, aes(color=display.name)) + geom_smooth(method="lm")
#mod
combustmod <- lmer(combust.mass2~ shade.rank*light + (1|spcode), 
                   flam10, REML=FALSE)
summary(combustmod)

combustmod <- afex::lmer(combust.mass2~ shade.rank*light + (1|spcode), 
                   flam10, REML=FALSE)

#graphic explor
ggplot(flam10, aes(shade.rank, base)) + geom_point() + facet_grid(.~light) +
  geom_point(data = spmean, aes(color=display.name)) + geom_smooth(method="lm") + 
  scale_color_manual(values=gcolor) +pubtheme.nogridlines
#mod
durmod <- afex::lmer(base ~ shade.rank*light + (1|spcode), flam10, REML=FALSE)
summary(durmod)

#graph
ggplot(flam10, aes(shade.rank, height.100)) + geom_point() + facet_grid(.~light) +
  geom_point(data = spmean, aes(color=display.name)) + geom_smooth(method="lm") +
  scale_color_manual(values=gcolor) +pubtheme.nogridlines

t100mod <- afex::lmer(height.100 ~ shade.rank*light + (1|spcode), flam10, REML=FALSE)
summary(t100mod)

#graph
ggplot(flam10, aes(shade.rank, combustion)) + geom_point() + facet_grid(.~light) +
  geom_point(data = spmean, aes(color=display.name)) + geom_smooth(method="lm") +
  scale_color_manual(values=gcolor) +pubtheme.nogridlines

combustionmod <- afex:: lmer(combustion ~ shade.rank*light + (1|spcode), flam10, REML=FALSE)
summary(combustionmod)

## conclusion:
## physical environment had significant effect on flammability with plants grew under
## shade always showed reduced flammability, unsurprisingly. 
## however, there is a weak (p<0.05) negetiave effect of shade tolerance 
## on heating duration at soil base and sig. interaction between light and shade 
## tolerance. 


###### H2:flammability measurements representing heat release (base, height.100, combust.mass2 ######
###### are directly affected  by biomass, fmc, and biomass allocation; ######
###### may also be indirectly influenced by specific leaf area, ######
###### which can influence water absorbtion of live fuel######

#rescale all independent variables
zscore <- function(x) (x - mean(x)) / sd(x) 
flam10_s <- flam10 %>% 
  mutate_at(c("pre.fmc2", "above.drym2", "bulkden",
              "ave.sla", "ave.sav"), zscore)

 #the real full mod is singular fit (too complexed random effect structure, i did
# a summary of mod and see which random effect variance is close to 0 and gradually dropped
#those to fix the singular fit issue, that's how it comes to this mod)
combus.fullmod <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*bulkden*ave.sla +
                         (1 + above.drym2 + pre.fmc2 + bulkden + ave.sla |spcode) + 
                         (1|trial.date) + (1|block) + (1|light), flam10_s,  REML=FALSE)
#singular fit, random effect structure is too complex
summary(combus.fullmod)
combus.mod <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*bulkden*ave.sla +
                        (0 + above.drym2 |spcode)  + (1|spcode) +
                         (1|trial.date) , flam10_s,  REML=FALSE)
# i firstly dropped random intercept that has variance close to 0, and the drop random
# correlation, then gradually added slope to see if it can fix singular fitting issue
# where few random effect structure all work, once slope is chosen, i added correlation
# between slope and intercept to see if it significantly increases goodnees of fit
# if not, correlation of random slop and intercept is not included. 

# for singular fit 
#see ref below
#https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#singular-models-random-effect-variances-estimated-as-zero-or-correlations-estimated-as---1

#drop 4 way interactions
combus.4inter <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden -
                    above.drym2:pre.fmc2:ave.sla:bulkden + 
                      (0+above.drym2|spcode) + 
                      (1|spcode) + (1|trial.date), flam10_s, REML=FALSE)

anova(combus.mod, combus.4inter) #combus.4inter

#drop 3-way interaction terms 
combus.3intermod1 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                      above.drym2:pre.fmc2:ave.sla:bulkden -
                      above.drym2:pre.fmc2:ave.sla +
                      (1|spcode) + (1|trial.date)+
                        (0+above.drym2|spcode), 
                      flam10_s, REML=FALSE)
anova(combus.4inter, combus.3intermod1) #3intermod1

combus.3intermod2 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - above.drym2:pre.fmc2:bulkden +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE)
anova(combus.3intermod1, combus.3intermod2) #combus.3intermod1

combus.3intermod3 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE)
anova(combus.3intermod1, combus.3intermod3)#3intermod3

#drop 2 way interactions
combus.2intermod1 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2 +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE)
anova(combus.2intermod1, combus.3intermod3) #3intermod3

combus.2intermod2 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                            above.drym2:ave.sla +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE)
anova(combus.2intermod2, combus.3intermod3)#3intermod3

combus.2intermod3 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                            above.drym2:bulkden +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE)
anova(combus.3intermod3, combus.2intermod3)#3intermod3

combus.2intermod4 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                            pre.fmc2:ave.sla +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE)
anova(combus.3intermod3, combus.2intermod4) #combus.3intermod3

combus.2intermod5 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                            pre.fmc2:bulkden +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE)
anova(combus.2intermod5, combus.3intermod3) #2intermod5

combus.2intermod6 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                            above.drym2:pre.fmc2:ave.sla:bulkden -
                            above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                           pre.fmc2:bulkden - ave.sla:bulkden +
                            (1|spcode) + (1|trial.date)+
                            (0+above.drym2|spcode), 
                          flam10_s, REML=FALSE) #failed

##drop main effects
combus.mainmod1 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden- bulkden-
                          above.drym2:pre.fmc2:ave.sla:bulkden -
                          above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                          pre.fmc2:bulkden +
                          (1|spcode) + (1|trial.date)+
                          (0+above.drym2|spcode), 
                        flam10_s, REML=FALSE)
anova(combus.2intermod5, combus.mainmod1)#mainmod1

combus.mainmod2 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden- ave.sla-
                          bulkden-
                          above.drym2:pre.fmc2:ave.sla:bulkden -
                          above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                          pre.fmc2:bulkden+
                          (1|spcode) + (1|trial.date)+
                          (0+above.drym2|spcode), 
                        flam10_s, REML=FALSE)
anova(combus.mainmod1, combus.mainmod2) #mainmod1

combus.mainmod3 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden-
                          bulkden- pre.fmc2-
                          above.drym2:pre.fmc2:ave.sla:bulkden -
                          above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                          pre.fmc2:bulkden+
                          (1|spcode) + (1|trial.date)+
                          (0+above.drym2|spcode), 
                        flam10_s, REML=FALSE)
anova(combus.mainmod1, combus.mainmod3) #mainmod1


combus.mainmod4 <- lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden- 
                          bulkden- above.drym2-
                          above.drym2:pre.fmc2:ave.sla:bulkden -
                          above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                          pre.fmc2:bulkden  +
                          (1|spcode) + (1|trial.date)+
                          (0+above.drym2|spcode), 
                        flam10_s, REML=FALSE)
anova(combus.mainmod1, combus.mainmod4) #mainmod1
summary(combus.mainmod1)
combus.mainmod1 <- afex::lmer(combust.mass2 ~ above.drym2*pre.fmc2*ave.sla*bulkden- 
                          bulkden-
                          above.drym2:pre.fmc2:ave.sla:bulkden -
                          above.drym2:pre.fmc2:ave.sla - pre.fmc2:ave.sla:bulkden -
                          pre.fmc2:bulkden +
                          (1|spcode) + (1|trial.date)+
                          (0+above.drym2|spcode), 
                        flam10_s, REML=FALSE)
ggplot(flam10_s, aes(above.drym2, combust.mass2, color = spcode))
####base####
base.fullmod <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla + 
                       (1+above.drym2+pre.fmc2+bulkden+ave.sla|spcode) +
                       (1|trial.date) + (1|block) + (1|light), flam10_s, REML=FALSE)
summary(base.fullmod)
base.mod <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla + 
                   (0+above.drym2|spcode) + (1|block),
                  flam10_s, REML=FALSE)

##drop 4-way interaction

base.4inter <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                      above.drym2:pre.fmc2:bulkden:ave.sla + 
                      (0+above.drym2|spcode) + (1|block),
                    flam10_s, REML=FALSE)
anova(base.mod, base.4inter) #4inter

##drop 3-way interactions
base.3intermod1 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - above.drym2:pre.fmc2:bulkden + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.4inter, base.3intermod1) #4inter

base.3intermod2 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - above.drym2:pre.fmc2:ave.sla + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.4inter, base.3intermod2) #4inter

base.3intermod3 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.4inter, base.3intermod3) #3intermod3

## drop 2-way interactions

base.2intermod1 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                          above.drym2:pre.fmc2 + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.3intermod3, base.2intermod1) #3intermod3

base.2intermod2 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                          above.drym2:bulkden + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.3intermod3, base.2intermod2)#3intermod3

base.2intermod3 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                          above.drym2:ave.sla + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.3intermod3, base.2intermod3) #3intermod3

base.2intermod4 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                          pre.fmc2:bulkden + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.3intermod3, base.2intermod4) #3intermod3

base.2intermod5 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                          pre.fmc2:ave.sla + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.3intermod3, base.2intermod5) #3intermod3

base.2intermod6 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                          above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                          bulkden:ave.sla + 
                          (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.3intermod3, base.2intermod6) #2intermod6

##drop mian effects
base.mainmod1 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - ave.sla -
                        above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                        bulkden:ave.sla + 
                        (0+above.drym2|spcode) + (1|block),
                      flam10_s, REML=FALSE)
anova(base.mainmod1, base.2intermod6) #2intermod6

base.mainmod2 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - bulkden -
                        above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                        bulkden:ave.sla + 
                        (0+above.drym2|spcode) + (1|block),
                      flam10_s, REML=FALSE)
anova(base.mainmod2, base.2intermod6) #mainmod2

base.mainmod3 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - pre.fmc2 - bulkden - 
                        above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                        bulkden:ave.sla + 
                        (0+above.drym2|spcode) + (1|block),
                        flam10_s, REML=FALSE)
anova(base.mainmod2, base.mainmod3) #mainmod2

base.mainmod4 <- lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - bulkden - above.drym2-
                        above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                        bulkden:ave.sla + 
                        (0+above.drym2|spcode) + (1|block),
                      flam10_s, REML=FALSE)    
anova(base.mainmod4, base.mainmod2)#mainmod2

base.finalmod <- afex::lmer(base ~ above.drym2*pre.fmc2*bulkden*ave.sla - bulkden -
                        above.drym2:pre.fmc2:bulkden:ave.sla - pre.fmc2:ave.sla:bulkden -
                        bulkden:ave.sla + 
                        (0+above.drym2|spcode) + (1|block),
                      flam10_s, REML=FALSE)
summary(base.finalmod)

##height.100
can.fullmod <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla + 
                      (1+above.drym2+pre.fmc2+bulkden+ave.sla|spcode) + 
                      (1|block) + (1|trial.date) + (1|light), flam10_s, REML=FALSE)
summary(can.fullmod)

can.mod <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla + 
                  (0+above.drym2|spcode) + (1|block),
                   flam10_s, REML=FALSE)

#drop 4-way interaction
can.4inter <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                     above.drym2:pre.fmc2:bulkden:ave.sla + 
                     (0+above.drym2|spcode) + (1|block),
                   flam10_s, REML=FALSE) 
anova(can.mod, can.4inter) #4inter

#drop 3-way interactions
can.3intermod1 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE) 
anova(can.4inter, can.3intermod1) #3intermod1

can.3intermod2 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.3intermod1, can.3intermod2) #3intermod2

can.3intermod3 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.3intermod2, can.3intermod3) #3intermod3

#drop 2-way interactions
can.2intermod1 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla - above.drym2:pre.fmc2 + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.3intermod3, can.2intermod1) #3intermod3

can.2intermod2 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla - above.drym2:bulkden + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.3intermod3, can.2intermod2) #2intermod2

can.2intermod3 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                         above.drym2:ave.sla + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.2intermod2, can.2intermod3) #2intermod3

can.2intermod4 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                         above.drym2:ave.sla - pre.fmc2:bulkden + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.2intermod3, can.2intermod4) #2intermod4

can.2intermod5 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                         above.drym2:ave.sla - pre.fmc2:bulkden - pre.fmc2:ave.sla + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.2intermod5, can.2intermod4) #2intermod5

can.2intermod6 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                         above.drym2:ave.sla - pre.fmc2:bulkden - pre.fmc2:ave.sla -
                         bulkden:ave.sla + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.2intermod5, can.2intermod6) #2intermod6

#drop main effects
can.mainmod1<- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - ave.sla-
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                         pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                         above.drym2:ave.sla - pre.fmc2:bulkden - pre.fmc2:ave.sla -
                         bulkden:ave.sla + 
                         (0+above.drym2|spcode) + (1|block),
                       flam10_s, REML=FALSE)
anova(can.2intermod6, can.mainmod1) #mainmod1

can.mainmod2 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - ave.sla-
                       bulkden -
                      above.drym2:pre.fmc2:bulkden:ave.sla -
                      above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                      pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                      above.drym2:ave.sla - pre.fmc2:bulkden - pre.fmc2:ave.sla -
                      bulkden:ave.sla + 
                      (0+above.drym2|spcode) + (1|block),
                    flam10_s, REML=FALSE)
anova(can.mainmod1, can.mainmod2) #mianmod2

can.mainmod3 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - ave.sla-
                       bulkden - pre.fmc2-
                       above.drym2:pre.fmc2:bulkden:ave.sla -
                       above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                       pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                       above.drym2:ave.sla - pre.fmc2:bulkden - pre.fmc2:ave.sla -
                       bulkden:ave.sla + 
                       (0+above.drym2|spcode) + (1|block),
                     flam10_s, REML=FALSE)
anova(can.mainmod2, can.mainmod3) #mainmod2

can.mainmod4 <- lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - ave.sla-
                       bulkden - above.drym2-
                       above.drym2:pre.fmc2:bulkden:ave.sla -
                       above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                       pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                       above.drym2:ave.sla - pre.fmc2:bulkden - pre.fmc2:ave.sla -
                       bulkden:ave.sla + 
                       (0+above.drym2|spcode) + (1|block),
                     flam10_s, REML=FALSE)
anova(can.mainmod2, can.mainmod4) #mainmod2

can.finalmod <- afex::lmer(height.100 ~ above.drym2*pre.fmc2*bulkden*ave.sla - ave.sla-
                       bulkden -
                       above.drym2:pre.fmc2:bulkden:ave.sla -
                       above.drym2:pre.fmc2:bulkden - above.drym2:pre.fmc2:ave.sla -
                       pre.fmc2:bulkden:ave.sla - above.drym2:bulkden  -
                       above.drym2:ave.sla - pre.fmc2:bulkden - pre.fmc2:ave.sla -
                       bulkden:ave.sla + 
                       (0+above.drym2|spcode) + (1|block),
                     flam10_s, REML=FALSE)
summary(can.finalmod)

#heat50
t50.finalmod <- afex::lmer(heat50 ~ above.drym2*pre.fmc2*bulkden*ave.sla - ave.sla-
                       above.drym2:pre.fmc2:bulkden:ave.sla -
                       above.drym2:pre.fmc2 - above.drym2:bulkden -
                       pre.fmc2:bulkden - pre.fmc2:ave.sla - 
                       bulkden:ave.sla+ 
                       (0+above.drym2+pre.fmc2|spcode)+
                       (1|spcode) + (1|trial.date), flam10_s, REML=FALSE)
summary(t50.finalmod)

## how to do model selection when there's too much terms to drop gradually?###

#heat base
tbase.fullmod <- lmer(heatb ~ above.drym2*pre.fmc2*bulkden*ave.sla + 
                        (1+above.drym2+pre.fmc2+bulkden+ave.sla|spcode) + (1|block)+ 
                        (1|trial.date) + (1|light), flam10_s,
                      REML=FALSE) #singular
summary(tbase.fullmod)
tbase.mod <- lmer(heatb ~ above.drym2*pre.fmc2*bulkden*ave.sla +
                    (0+above.drym2+pre.fmc2|spcode) + (1|spcode), flam10_s, REML=FALSE)

tbase.finalmod <- afex::lmer(heatb ~ above.drym2*pre.fmc2*bulkden*ave.sla- bulkden - above.drym2- 
                         above.drym2:pre.fmc2:bulkden:ave.sla -
                         above.drym2:pre.fmc2:bulkden - 
                         above.drym2:pre.fmc2 + 
                         (0+above.drym2+pre.fmc2|spcode) + (1|spcode), 
                       flam10_s, REML=FALSE)
summary(tbase.finalmod) #is not influenced by biomass density



###### H3: physical environment, plant phenology and leaf trait######
           ###### all have effects on live fuel moisture content ######

fmcdf_s <- flam10 %>% 
  mutate_at(c("curate", "ave.sla", "ave.sav"), zscore)

fmc.mod <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) + 
                      (0+ave.sla|spcode) + (1|spcode),
                      fmcdf_s, REML=FALSE)

#drop 4 way interaction
fmc.4inter <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                     light:curate:ave.sla:ave.sav +
                     (0+ave.sla|spcode) + (1|spcode),
                   fmcdf_s, REML=FALSE)

anova(fmc.mod, fmc.4inter) #4inter
#drop 3 way interactions
fmc.3intermod1 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         light:curate:ave.sla +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE)
anova(fmc.4inter, fmc.3intermod1) #4inter

fmc.3intermod2 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         light:curate:ave.sav +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE)
anova(fmc.3intermod2, fmc.4inter) #4inter

fmc.3intermod3 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         curate:ave.sav:ave.sla +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE)

anova(fmc.3intermod3, fmc.4inter)#3inter3

#drop 2 way interactions
fmc.2intermod1 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         curate:ave.sav:ave.sla - 
                         light:curate +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE)
anova(fmc.3intermod3, fmc.2intermod1)#3inter3

fmc.2intermod2 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         curate:ave.sav:ave.sla - 
                         light:ave.sla +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE) 
anova(fmc.3intermod3, fmc.2intermod2) #2inter2

fmc.2intermod3 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         curate:ave.sav:ave.sla - 
                         light:ave.sla - light:ave.sav +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE) 
anova(fmc.2intermod2, fmc.2intermod3) #2inter3

fmc.2intermod4 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         curate:ave.sav:ave.sla - 
                         light:ave.sla - light:ave.sav - curate:ave.sla +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE) 
anova(fmc.2intermod3, fmc.2intermod4) #2inter3

fmc.2intermod5 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         curate:ave.sav:ave.sla - 
                         light:ave.sla - light:ave.sav - curate:ave.sav +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE) 
anova(fmc.2intermod3, fmc.2intermod5) #2inter5

fmc.2intermod6 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) -
                         light:curate:ave.sla:ave.sav - 
                         curate:ave.sav:ave.sla - 
                         light:ave.sla - light:ave.sav - curate:ave.sav -
                         ave.sla:ave.sav +
                         (0+ave.sla|spcode) + (1|spcode),
                       fmcdf_s, REML=FALSE) 
anova(fmc.2intermod5, fmc.2intermod6) #2intermod5

#drop main effect
fmc.main1 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) - 
                    light-
                    light:curate:ave.sla:ave.sav - 
                    curate:ave.sav:ave.sla - 
                    light:ave.sla - light:ave.sav - curate:ave.sav +
                    (0+ave.sla|spcode) + (1|spcode),
                  fmcdf_s, REML=FALSE) 
                    
anova(fmc.main1, fmc.2intermod5) #main1

fmc.main2 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) - 
                    light-curate-
                    light:curate:ave.sla:ave.sav - 
                    curate:ave.sav:ave.sla - 
                    light:ave.sla - light:ave.sav - curate:ave.sav +
                    (0+ave.sla|spcode) + (1|spcode),
                  fmcdf_s, REML=FALSE) 
                
anova(fmc.main1, fmc.main2) #main1

fmc.main3 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) - 
                    light-ave.sla-
                    light:curate:ave.sla:ave.sav - 
                    curate:ave.sav:ave.sla - 
                    light:ave.sla - light:ave.sav - curate:ave.sav +
                    (0+ave.sla|spcode) + (1|spcode),
                  fmcdf_s, REML=FALSE) 
anova(fmc.main1, fmc.main3) #main1

fmc.main4 <- lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) - 
                    light-ave.sav-
                    light:curate:ave.sla:ave.sav - 
                    curate:ave.sav:ave.sla - 
                    light:ave.sla - light:ave.sav - curate:ave.sav +
                    (0+ave.sla|spcode) + (1|spcode),
                  fmcdf_s, REML=FALSE) 
anova(fmc.main1, fmc.main4) #mainmod1

fmc.finalmod <- afex::lmer(pre.fmc2 ~ light*curate*ave.sla*ave.sav + (1|block) - 
                    light-
                    light:curate:ave.sla:ave.sav - 
                    curate:ave.sav:ave.sla - 
                    light:ave.sla - light:ave.sav - curate:ave.sav +
                    (0+ave.sla|spcode) + (1|spcode),
                  fmcdf_s, REML=FALSE)
summary(fmc.finalmod)
#plant phenology and leaf traits significantlly influenced live fuel moisture content
# although there is no physical environment effect detected, environment interacted with 
#curate to influence fmc. there's also sig. 3-way interactions between light, curate and sav,
# and between light, ave.sla and ave.sav





##shade-flammbility.R
## to explore how species vary in flammability as response to variation in
## shade tolerance. 

library(lme4)
library(car)
library(dplyr)
#library(ggeffects)
set.seed(100)
options(contrasts = c("contr.sum", "contr.poly")) #type III anova table, set treatment contrast
source("./all-data.R")

#################### correlation matrix of flam&trait measurements ####################

cor.df <- flamdt %>% select(heatb,heat50, 
                            ave.sla, ave.sav,bulkden, 
                            above.drym, pre.fmc)
varcors <- cor(cor.df, method = "kendall", use = "pairwise")
corrplot::corrplot(varcors)
#scater plot to see if it is linear relationship
pairs(cor.df)

###### H: as shade tolerance increases, species flammability decrases ###### 

#rescale all independent variables and log transform dependent variable
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 
flamdt <- flamdt %>% mutate_at(c("st", "above.drym", "pre.fmc", "ave.sla", "bulkden"),
                               funs(s = zscore))

#shade tolerance and soil heating mod
baseheat_mod <- lmer(heatb_log ~ above.drym_s*st_s*light + (1|spcode), 
                       flamdt, REML=TRUE)
#plot(baseheat_mod)
summary(baseheat_mod)
baseanova <- Anova(baseheat_mod, type ="3", test.statistic = "F")
baseanova

#shade tolrance and heat50 mod
heat50_mod <- lmer(heat50_log ~ above.drym_s*st_s*light + (1|spcode), 
                         flamdt, REML=TRUE)
#plot(heat50_mod)
summary(heat50_mod)
heat50anova <- Anova(heat50_mod, type = "3", test.statistic = "F")
heat50anova

## There are sig. negative effects of shade tolerance on 
## mid-canopy heating. In addition

  
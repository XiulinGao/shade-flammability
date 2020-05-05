##fig-table.R
## make all figures and tables for shade-flammability manuscript
library(ggpubr)
library(pcaMethods)
library(xtable)

source("./ggplot-theme.R")
source("./post-fire-traits.R")
source("./traits-flammability.R")
set.seed(100)

#ptshape <- c(0, 1, 2, 4, 5, 6, 7,8, 10, 12, 13, 14,15, 16, 17, 18, 20)
col1 = 13.2
col2 = 7
ptsize = 3.5
RESULTS <- "../results/"
#textsize <- 20
#axissz <- textsize-2

spmean <- flamdt %>% group_by(spcode, short.name, light) %>% 
  summarise_at(c("st", "combust.mass", "base", "height.100",
                 "heat50", "heat50_log", "heatb", "heatb_log", "combustion", "ave.sla", "ave.sav",
                 "pre.fmc", "above.drym", "bulkden"), 
               list(~mean(., na.rm=TRUE), ~sd(., na.rm=TRUE)))

#rename columns 
colnames(spmean)[4:17] <- c("st", "combust.mass", "base", "height.100", 
                            "heat50", "heat50_log", "heatb", "heatb_log", "combustion", 
                            "ave.sla", "ave.sav", "pre.fmc", "above.drym", "bulkden")

#Table S1: species mean for measurements
spmean_tab <- xtable(spmean)
print(spmean_tab, type = "html", 
      file=file.path(RESULTS, "va_mean.html"), include.rownames=FALSE)


#Fig2. shade tolerance-soil heating 
lightlable <- c("0% shade", "50% shade")
names(lightlable) <- c("fs", "s")

#break variable into several groups by giving cut-off values
vabreak <- function(variable, value){
  if (variable < value[which.min(value)]) return("Low")
  else {
    if (variable > value[which.max(value)]) return ("High")
    return ("Moderate")
  }
}

#break shade tolerance into 3 groups (-0.7, 0.3)
flamdt$stgroup <- sapply(flamdt$st_s, vabreak, value = c(-0.7, 0.3))

#add grouped shade tolerance to species mean
spst <- flamdt %>% group_by(spcode) %>% summarise(stgroup = stgroup[1]) 

spmean <- left_join(spmean, spst, by = "spcode")


#specify the order of shade tolerance group
flamdt$stgroup <- factor(flamdt$stgroup, levels = c("High", "Moderate", "Low"))

ggplot(flamdt, aes(above.drym, heatb_log, color = stgroup)) +
  geom_point(size = ptsize-1.5, alpha = 0.5) + 
  geom_point(data = spmean, aes(above.drym, heatb_log, color = stgroup), size = ptsize) + 
  facet_grid(.~light, labeller = labeller(light = lightlable)) +
  geom_smooth(data = spmean, method = "lm", se=FALSE, size = 0.8, color = 'black',) +
  #geom_abline(data = ref, aes(slope = slope, intercept = intcpt)) +
  #geom_errorbarh(aes(xmin=above.drym-above.drym_sd, xmax=above.drym+above.drym_sd), 
  #position = "identity", linetype = 1) + 
  scale_color_manual(values = schwilkcolors) +
  xlab("Aboveground biomass (g)") +
  ylab("Log-transformed heat release at soil surface") +
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain")))+
  pubtheme.nogridlines + theme(legend.title = element_blank(),
                               legend.position = c(0.3, 0.2))

ggsave("../results/fig2.pdf", width = col1, height= 0.7*col1, 
       units="cm")

#Table S2: mixed effect model coeffecient and anova table 
#for soil heating and shade tolerance
tab1base.aov <- xtable(baseanova)
print(tab1base.aov, type = "html", file = file.path(RESULTS, "base-st-aov.html"))
basest.coef <- summary(baseheat_mod)$coefficients 
tab1base.coef <- xtable(basest.coef, digits = 4)
print(tab1base.coef, type = "html", file = file.path(RESULTS, "base-st-coef.html"))

#Fig.3 shade tolerance-50cm heating 

ggplot(flamdt, aes(above.drym, heat50_log, color = stgroup)) +
  geom_point(alpha = 0.5, size = ptsize-1.5) + 
  facet_grid(.~light, labeller = labeller(light = lightlable)) + #scales = "free_x") +
  geom_point(data = spmean, aes(above.drym, heat50_log, color = stgroup), size = ptsize) + 
  geom_smooth(data = spmean, method = "lm", se=FALSE, size = 0.8)  +
  #geom_errorbar(data = spmean, aes(ymin = heat50_log-heat50_log_sd, 
  #ymax = heat50_log+heat50_log_sd), width = 0.1)+
  #scale_shape_manual(values = ptshape) + 
  scale_color_manual(values = schwilkcolors) + 
  xlab("Aboveground biomass (g)") + ylab("Log-transformed heat release at 50cm") + 
  pubtheme.nogridlines + 
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain")))+
  theme(legend.title = element_blank(), 
        legend.position = c(0.3, 0.2))
ggsave("../results/fig3.pdf", width = col1, height= 0.7*col1, 
       units="cm")

#Table S3: coeffecient and anova table for shade tolerance-50cm heating model
tab150.aov <- xtable(heat50anova)
print(tab150.aov, type = "html", file = file.path(RESULTS, "50-st-aov.html"))
st50.coef <- summary(heat50_mod)$coefficients
tab150.coef <- xtable(st50.coef, digits = 4)
print(tab150.coef, type = "html", file = file.path(RESULTS, "50-st-coef.html"))

## Fig.4 Predicted post-fire survival rate 
sjPlot::plot_model(survi_mod, type="pred", terms = c("heatb_s [all]"), se= FALSE,
                   colors = schwilkcolors, 
                   legend.title = "Standadized tiller num.")+
  pubtheme.nogridlines +
  #guides(color = guide_legend(title.position = "bottom",
    #label.theme = element_text(family=fontfamily, 
                    #size=smsize,face = "plain"))) + 
  pubtheme.nogridlines + theme(plot.title = element_blank()) +
                               #legend.title = element_blank(),
                               #legend.position = "right") +
  xlab("Standardized heat release at soil surface") + 
  ylab("Predicted survival rate")


ggsave("../results/fig4.pdf", width = col1 , height= 0.7*col1, 
       units="cm") 
#Table S4: post fire rsyrvival rate model coefficients and anova table
tab2survi.aov <- xtable(survi.aov) 
print(tab2survi.aov, type = "html", file = file.path(RESULTS, "survival-aov.html"))
survi.coef <- summary(survi_mod)$coefficients
tab2survi.coef <- xtable(survi.coef, digits = 4)



#Fig.5 Predicted post-fire percentage biomass recovery 
sjPlot::plot_model(resp_lmmod, type="pred", 
                   terms = c("heatb_s", "pre_tinum_s"),
                   colors = schwilkcolors,
                   legend.title = "Standardized tiller num.") +
  pubtheme.nogridlines + 
  xlab("Standardized heat release at soil surface") + 
  ylab("Predicted post-fire biomass recovery(%)") +
  guides(color = guide_legend(title.position = "bottom",
    label.theme = element_text(family=fontfamily, 
                  size=smsize,face = "plain"))) + 
  pubtheme.nogridlines + theme(plot.title = element_blank(),
                               #legend.title = element_blank(),
                               legend.position = "right") 

ggsave("../results/fig5.pdf", width = col1, height= 0.7*col1, 
       units="cm")

#Table S5: post fire biomass recovery model coefficients and anova table
print(tab2survi.coef, type = "html", file = file.path(RESULTS, "survival-coef.html"))
tab2resp.aov <- xtable(resp.aov)
print(tab2resp.aov, type = "html", file = file.path(RESULTS, "resprout-aov.html"))
resp.coef <- summary(resp_lmmod)$coefficients
tab2resp.coef <- xtable(resp.coef, digits = 4)
print(tab2resp.coef, type = "html", file = file.path(RESULTS, "resprout-coef.html"))



#Fig.6 plant traits-soil heating

#sjPlot::plot_model(traitsoil_mod, type = "pred", terms = c( "pre.fmc_s", "ave.sla_s")) +
  #pubtheme.nogridlines 
# break ave.sla into 2 groups: sla<300, and sla>300
flamdt$slagroup <- sapply(flamdt$ave.sla, vabreak, value = c(300))
flamdt$slagroup <- as.factor(flamdt$slagroup)
## break pre.fmc into 2 groups: fmc<0.4,and fmc > 0.4
flamdt$fmcgroup <- sapply(flamdt$pre.fmc, vabreak, value = c(0.4))
flamdt$fmcgroup <- as.factor(flamdt$fmcgroup)
fmclabel <- c("FMC < 40%", "FMC > 40%")
names(fmclabel) <- c("Low", "High")
slalabel <- c("SLA > 300cm^2/g",  "SLA < 300cm^2/g")
names(slalabel) <- c("High", "Low")
#predict value of soil heating from mixed effect model
predheat <- flamdt %>% select(label, spcode, light, block, ave.sla, pre.fmc, bulkden,
                              above.drym, weatemp, above.drym_s, pre.fmc_s, ave.sla_s, 
                              bulkden_s, weatemp_s, stgroup)
predheat$heatb_log <- predict(traitsoil_mod, newdata = predheat)

#attach fmc and sla group to predict value
spfmc <- flamdt %>% group_by(spcode) %>% summarise(fmcgroup = fmcgroup[1]) 
spsla <- flamdt %>% group_by(spcode) %>% summarise(slagroup = slagroup[1]) 
predheat <- predheat %>% left_join(spfmc, by = "spcode") %>% left_join(spsla, by = "spcode")

#fig.a: biomass and fmc main effects
#ggplot(flamdt, aes(above.drym, heatb_log, color = fmcgroup)) + 
  #geom_point(aes(shape = short.name), size = 2.5, stroke = 0.8) + 
  #geom_smooth(method = "lm", se=FALSE, size = 0.8) + 
  #scale_shape_manual(values = ptshape) +
  #scale_color_manual(values = schwilkcolors, breaks = c("Low", "High"), labels = fmclabel) +
  #xlab("Aboveground biomass (g)") + ylab("Heat release at soil surface") +
  #guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         #size=smsize,face = "plain"))) + 
  #pubtheme.nogridlines + theme(legend.title = element_blank(),
                               #legend.position = "bottom") 
#ggsave("../results/base-trait.pdf", width = col1, height= 0.7*col1, 
       #units="cm") 

#fig.b: fmc and sla interaction effect
#ggplot(flamdt, aes(pre.fmc, heatb_log)) + 
  #geom_point(aes(shape = short.name), size = 2.5, stroke = 0.8) +
  #geom_smooth(method = "lm", se=FALSE, size = 0.8, color = "black") +
  #scale_shape_manual(values = ptshape) +
  #facet_grid(.~slagroup, scales = "free_x", labeller = labeller(slagroup = slalabel)) +
  #xlab("Fuel moisture content") + ylab("Heat release at soil surface") +
  #pubtheme.nogridlines + theme(legend.title = element_blank(),
                               #legend.position = "bottom") 
#ggsave("../results/base-slafmc.pdf", width = col1, height= 0.7*col1, 
       #units="cm") 

#or combine into one figure
ggplot(flamdt, aes(above.drym, heatb_log, color = fmcgroup)) + 
  geom_point(size = ptsize) + 
  geom_blank(data = predheat, aes(above.drym, heatb_log, color = fmcgroup)) +
  geom_smooth(data = predheat, method = "lm", se=FALSE, size = 0.8) + 
  scale_color_manual(values = schwilkcolors[c(1, 3)], breaks = c("Low", "High"), labels = fmclabel) +
  facet_grid(.~slagroup, labeller = labeller(slagroup = slalabel))+
  xlab("Aboveground biomass (g)") + ylab("Log-transformed heat release at soil surface") +
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain"))) + 
  pubtheme.nogridlines + theme(legend.title = element_blank(),
                               legend.position = c(0.3, 0.2)) 

ggsave("../results/fig6.pdf", width = col1, height= 0.7*col1, 
       units="cm") 

#Table S6: traits-soil heating model coeeficients and anova table
traitb.aovtab <- xtable(traitsoilaov)
print(traitb.aovtab, type = "html", file = file.path(RESULTS, "trait-soil-aov.html"))
traitb.coef <- summary(traitsoil_mod)$coefficients
traitb.coeftab <- xtable(traitb.coef, digits = 4)
print(traitb.coeftab, type = "html", file = file.path(RESULTS, "trait-soil-coef.html"))



#Fig.7 plant traits-50cm heating 

#predict value for heat at 50cm from model
predheat$heat50_log <- predict(trait50_mod, newdata = predheat)

ggplot(flamdt, aes(above.drym, heat50_log)) + #, color = stgroup)) + 
  geom_point(size = ptsize)+
  geom_smooth(data = predheat, method = "lm", se=FALSE, size = 0.8, color = 'black') +
  facet_grid(.~fmcgroup, labeller = labeller(fmcgroup = fmclabel)) +
  xlab("Aboveground biomass (g)") + 
  ylab("Log-transformed heat release at 50cm") +
  #scale_color_manual(values = schwilkcolors) +
  #guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         #size=smsize,face = "plain"))) + 
  pubtheme.nogridlines + theme(legend.title = element_blank(),
                               legend.position = c(0.3, 0.2)) 

ggsave("../results/fig7.pdf", width = col1, height= 0.7*col1, 
       units="cm")  

#Table S7: traits-50cm heating model coefficients and anova table
trait50.aovtab <- xtable(trait50aov)
print(trait50.aovtab, type = "html", file = file.path(RESULTS, "trait-50-aov.html"))
trait50.coef <- summary(trait50_mod)$coefficients
trait50.coeftab <- xtable(trait50.coef, digits = 4)
print(trait50.coeftab, type = "html", file = file.path(RESULTS, "trait-50-coef.html"))



#Fig.8 plant traits, light environments, and live fuel moisture
fmc.mean <- fmcdt %>% group_by(spcode, short.name, light) %>% 
  summarise(pre.fmc = mean(pre.fmc), ldratio = mean(ldratio))

fmc.mean <- left_join(fmc.mean, spst, by = "spcode")

#group ave.sla to >300 and <300
fmcdt$slagroup <- sapply(fmcdt$ave.sla, vabreak, value = c(300))
fmcdt$slagroup <- as.factor(fmcdt$slagroup)


ggplot(fmcdt, aes(ldratio, pre.fmc, color = slagroup)) +
  geom_point(size = ptsize) +
  facet_grid(.~light,labeller = labeller(light = lightlable)) + 
  geom_smooth(data = fmc.mean, method = "lm", se = FALSE, color = "black", size = 0.8)+
  xlab("Live to dead mass ratio") + 
  ylab("Pre-burn fuel moisture content(%)")+
  scale_color_manual(values = schwilkcolors, breaks = c("Low", "High"), labels = slalabel) +
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain"))) + 
  pubtheme.nogridlines + theme(legend.title = element_blank(),
                               legend.position = c(0.3, 0.2)) 

ggsave("../results/fig8.pdf", width = col1, height= 0.7*col1, 
       units="cm") 

#Table S8: plant phenology, sla and light effects on live fuel moisture
tab5.fmcaov <-xtable(fmcaov) 
print(tab5.fmcaov, type = "html", file = file.path(RESULTS, "fmc-aov.html"))
fmccoef <- summary(fmcmod)$coefficients
tab5.fmccoef <- xtable(fmccoef, digits = 4)
print(tab5.fmccoef, type = "html", file = file.path(RESULTS, "fmc-coef.html"))

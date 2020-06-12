##fig-table.R
## make all figures and tables for shade-flammability manuscript

source("./analyses.R")
source("./ggplot-theme.R")
#library(patchwork)
#ptshape <- c(0, 1, 2, 4, 5, 6, 7,8, 10, 12, 13, 14,15, 16, 17, 18, 20)
## col1 = 13.2
## col2 = 7  # This makes no sense! a 2 column figure needs to be at least 2x as wide as a 1 col figure. Check submission requirements.
ptsize = 3.5

## DWS: why the below ?
y0_breaks = c(exp(3), exp(5), exp(7))
y50_breaks = c(exp(2.5), exp(5), exp(7.5))

# textsize <- 20
# axissz <- textsize-2

# calculate mean of each measurement, mean of heat50 need to be calculated by
# excluding measurements from 03/29/19
spmean <- flamdt %>% group_by(spcode, short.name, light) %>% 
  summarise_at(c("st", "combust.mass", "base", "height.100",
                 "heat50", "heatb", "combustion", "ave.sla", "ave.sav",
                 "pre.fmc", "above.drym", "bulkden"), 
               list(~mean(., na.rm=TRUE), ~sd(., na.rm=TRUE)))

## mean50 <- flam50 %>% group_by(spcode, short.name, light) %>% 
## summarise(heat50_mean2 = mean(heat50, na.rm=TRUE),
##           heat50_sd2 = sd(heat50, na.rm = TRUE))

# Use heat50_mean calculated without obs from first trial
## spmean <- spmean %>% left_join(mean50, by = c("spcode", "short.name", "light")) %>% 
##   select(-heat50_mean, -heat50_sd)

## DWS: Check that this works now. If there are numbers that are bad (eg one
## trial date) then take those out FIRST before anything else is done.

# Rename columns
## DWS: DANGEROUS don't hard code column positions. Certinaly not this late in
## the analyses

## colnames(spmean)[c(4:14, 26:27)] <- c("st", "combust.mass", "base", "height.100", 
##                             "heatb",  "combustion", 
##                             "ave.sla", "ave.sav", "pre.fmc", "above.drym", "bulkden",
##                             "heat50", "heat50_sd")

names(spmean) <- str_replace(names(spmean), "_mean", "")

###############################################################################
## Table S1: Species means 
###############################################################################
spmean_tab <- xtable(spmean)
print(spmean_tab, type = "html", 
      file=file.path(RESULTS, "va_mean.html"), include.rownames=FALSE)

###############################################################################
## Fig. 2: heat release by biomass by shade treatment and shade tolerance
###############################################################################

lightlabel <- c("0% shade", "50% shade")
names(lightlabel) <- c("fs", "s")

heightlabel <- c("At soil surface", "At 50 cm height")
names(heightlabel) <- c("b", "50")


# break shade tolerance into 3 groups. Xiulin wants orange to be "low" and that
# makes sense for "shade tolerance", so we should make a specaial color set for
# the shade tolerance factor.
shade_factor_colors <- c("#D68D18",  "#836B43", "#A0AE6A")
shadelabels <- c("Low", "Moderate", "High")
flamdt <- flamdt %>% mutate(stgroup = cut(st_s,
                                          breaks = c(-Inf, -0.7, 0.3, Inf),
                                          labels = shadelabels))
## flam50 <- flam50 %>% mutate(stgroup = cut(st_s,
##                                           breaks = c(-Inf, -0.7, 0.3, Inf),
##                                           labels = shadelabels))
## DWS: why are the different heights in different files? I disagree with that
## design choice. Keep data together and unduplicated.

# Add grouped shade tolerance to species mean
spst <- flamdt %>% group_by(spcode) %>% summarise(stgroup = stgroup[1]) 
spmean <- left_join(spmean, spst)# , by = "spcode")


## DWS: TODO the lines need to be from the mixed model.

## first pivot the data longer
fig2.flamdt <- flamdt %>% select(spcode, short.name, light, stgroup, above.drym, heatb, heat50) %>%
  tidyr::pivot_longer(cols = starts_with("heat"),
                      names_to = "height",
                      names_prefix = "heat",
                      values_to = "heat" )

fig2.spmean <- spmean %>% select(spcode, short.name,light, stgroup, above.drym, heatb, heat50) %>%
    tidyr::pivot_longer(cols = starts_with("heat"),
                      names_to = "height",
                      names_prefix = "heat",
                      values_to = "heat" )


## Fig 2 approach 1, as facets)

fig2 <- ggplot(fig2.flamdt, aes(above.drym, heat, color = stgroup)) +
  geom_point(alpha = 0.5, size = ptsize-1.5) + 
  facet_grid(height ~ light, labeller = labeller(light = lightlabel, height=heightlabel)) + #scales = "free_x") +
  geom_point(data = fig2.spmean, aes(above.drym, heat, color = stgroup), size = ptsize) + 
  geom_smooth(data = fig2.spmean, method = "lm", se=FALSE, size = 0.8)  +
  #geom_errorbar(data = spmean, aes(ymin = heat50_log-heat50_log_sd, 
  #ymax = heat50_log+heat50_log_sd), width = 0.1)+
  #scale_shape_manual(values = ptshape) + 
  scale_color_manual(values = shade_factor_colors) + 
  scale_y_continuous("Heat release at (J)",
                     trans = "log10") +
                     #breaks = y50_breaks,
                     #labels = label_number()) +
  #labels = trans_format("log", math_format(e^.x)))+
  scale_x_continuous("Above ground biomass (g)", limits=c(0,40)) +
  pubtheme.nogridlines + 
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain"))) +
  theme(legend.title = element_blank(),
        legend.position = c(0.89, 0.2))
fig2
ggsave(file.path(RESULTS, "fig2_approach1.pdf"), plot=fig2,
       width=col2, height=col1*2, unit="cm")
## Approach 2 using patchwork

fig2a <- ggplot(subset(fig2.flamdt, height=="50"), aes(above.drym, heat, color = stgroup)) +
  geom_point(alpha = 0.5, size = ptsize-1.5) + 
  facet_grid(.~light, labeller = labeller(light = lightlabel)) + #scales = "free_x") +
  geom_point(data = subset(fig2.spmean, height=="50"), size = ptsize) + 
  geom_smooth(data = subset(fig2.spmean, height=="50"), method = "lm", se=FALSE, size = 0.8)  +
  #geom_errorbar(data = spmean, aes(ymin = heat50_log-heat50_log_sd, 
  #ymax = heat50_log+heat50_log_sd), width = 0.1)+
  #scale_shape_manual(values = ptshape) + 
  scale_color_manual(values = shade_factor_colors) + 
  scale_y_continuous("Heat release at 50cm (J)",
                     trans = "log10") +
                     #breaks = y50_breaks,
                     #labels = label_number()) +
  #labels = trans_format("log", math_format(e^.x)))+
  scale_x_continuous(limits=c(0,40)) +
  pubtheme.nogridlines + 
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain"))) +
  theme(legend.title = element_blank(),
        #legend.position = c(0.89, 0.2),
        axis.title.x = element_blank())
fig2a

fig2b <- ggplot(subset(fig2.flamdt, height=="b"), aes(above.drym, heat, color = stgroup)) +
  geom_point(size = ptsize-1.5, alpha = 0.5) + 
  geom_point(data = subset(fig2.spmean, height=="b"), size = ptsize) + 
  geom_smooth(data = subset(fig2.spmean, height=="b"), method = "lm", se=FALSE, size = 0.8, color = 'black') +
  facet_grid(.~light, labeller = labeller(light = lightlabel)) +
  #geom_abline(data = ref, aes(slope = slope, intercept = intcpt)) +
  #geom_errorbarh(aes(xmin=above.drym-above.drym_sd, xmax=above.drym+above.drym_sd), 
  #position = "identity", linetype = 1) + 
  scale_color_manual(values = shade_factor_colors) +
  scale_y_continuous("Heat release at soil surface (J)",
                     trans = "log10") +
#                     breaks = y0_breaks,
                     #labels = label_number()) +
  #                     labels = trans_format("log", math_format(e^.x))) +
  scale_x_continuous("Aboveground biomass (g)", limits=c(0,40)) +
#  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
#                                                         size=smsize,face = "plain")))+
  pubtheme.nogridlines +
theme(legend.title = element_blank(),
      strip.text.x=element_blank())
  #      legend.position = c(0.89, 0.2))
fig2b


fig2_approach2 <- fig2a + fig2b +
plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = unit(c(4, 4, 4, 4), "pt"),
        plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -0.5, vjust = 0.3))

fig2_approach2
ggsave(file.path(RESULTS, "fig2_approach2.pdf"), plot=fig2_approach2,
       width=col2, height=col1*2, unit="cm")

###############################################################################
## Table S2: mixed effect model coefficient and anova table
###############################################################################

# for soil heating and shade tolerance
tab1base.aov <- xtable(baseanova)
print(tab1base.aov, type = "html", file = file.path(RESULTS, "base-st-aov.html"))
basest.coef <- summary(baseheat_mod)$coefficients 
tab1base.coef <- xtable(basest.coef, digits = 4)
print(tab1base.coef, type = "html", file = file.path(RESULTS, "base-st-coef.html"))



###############################################################################
## Table S3: coeffecient and anova table for shade tolerance-50cm heating model
###############################################################################
tab150.aov <- xtable(heat50anova)
print(tab150.aov, type = "html", file = file.path(RESULTS, "50-st-aov.html"))
st50.coef <- summary(heat50_mod)$coefficients
tab150.coef <- xtable(st50.coef, digits = 4)
print(tab150.coef, type = "html", file = file.path(RESULTS, "50-st-coef.html"))

###############################################################################
## Fig. 4: Predicted post-fire survival rate 
###############################################################################

fig4 <- sjPlot::plot_model(survi_mod, type="pred", terms = c("heatb_s [all]"), se= FALSE)+
  pubtheme.nogridlines +
  #guides(color = guide_legend(title.position = "bottom",
    #label.theme = element_text(family=fontfamily, 
                    #size=smsize,face = "plain"))) + 
  pubtheme.nogridlines + theme(plot.title = element_blank()) +
                               #legend.title = element_blank(),
                               #legend.position = "right") +
  xlab("Standardized heat release at soil surface") + 
  ylab("Predicted survival rate")
fig4

ggsave(fig4, file = file.path(RESULTS, "fig4.pdf"), width = col1 , height= 0.7*col1, 
       units="cm")

###############################################################################
## Table S4: post fire rsyrvival rate model coefficients and anova table
###############################################################################

tab2survi.aov <- xtable(survi.aov) 
print(tab2survi.aov, type = "html", file = file.path(RESULTS, "survival-aov.html"))
survi.coef <- summary(survi_mod)$coefficients
tab2survi.coef <- xtable(survi.coef, digits = 4)

###############################################################################
# Fig. 5: Predicted post-fire percentage biomass recovery
###############################################################################

fig5 <- sjPlot::plot_model(resp_lmmod, type="pred",
                   terms = c("heatb_s", "pre_tinum_s"),
                   colors = schwilkcolors,
                   legend.title = "Standardized\ntiller #")+
  pubtheme.nogridlines +
  scale_y_continuous(labels = math_format(e^.x))+
  xlab("Standardized heat release at soil surface") + 
  ylab("Predicted post-fire biomass recovery (%)") +
  pubtheme.nogridlines + theme(plot.title = element_blank(),
                               legend.position = "right") +
  theme(legend.position = c(0.2, 0.32))
fig5
ggsave(fig5, file = file.path(RESULTS, "fig5.pdf"), width = col1, height= 0.7*col1,
       units="cm")

###############################################################################
## Table S5: post fire biomass recovery model coefficients and anova table
###############################################################################

print(tab2survi.coef, type = "html", file = file.path(RESULTS, "survival-coef.html"))
tab2resp.aov <- xtable(resp.aov)
print(tab2resp.aov, type = "html", file = file.path(RESULTS, "resprout-aov.html"))
resp.coef <- summary(resp_lmmod)$coefficients
tab2resp.coef <- xtable(resp.coef, digits = 4)
print(tab2resp.coef, type = "html", file = file.path(RESULTS, "resprout-coef.html"))

###############################################################################
## Fig. 6: plant traits-soil heating
###############################################################################

flamdt <- flamdt %>% mutate(slagroup = cut(ave.sla,
                                           breaks = c(-Inf, 300, Inf),
                                           labels =  c("SLA > 300~cm^2~g^{-1}",  "SLA < 300~cm^2~g^{-1}")),
                            fmcgroup = cut(pre.fmc, breaks=c(-Inf, 0.4, Inf),
                                           labels = c("FMC < 40%", "FMC > 40%"))
                            )
# Predict value of soil heating from mixed effect model
predheat <- flamdt %>% select(label, spcode, light, block, ave.sla, pre.fmc, bulkden,
                              above.drym, weatemp, above.drym_s, pre.fmc_s, ave.sla_s, 
                              bulkden_s, weatemp_s, stgroup)
predheat$heatb_log <- predict(traitsoil_mod, newdata = predheat)
predheat <- predheat %>% mutate(heatb = exp(heatb_log)) %>% 
  mutate(heatb = round(heatb, 2))

## DWS: Why are you using natural log above? Is there are good reason? UNless there
## is a strong reason to use natural log, I recommend log10.

# attach fmc and sla group to predict value
spfmc <- flamdt %>% group_by(spcode) %>% summarise(fmcgroup = fmcgroup[1]) 
spsla <- flamdt %>% group_by(spcode) %>% summarise(slagroup = slagroup[1]) 
predheat <- predheat %>% left_join(spfmc, by = "spcode") %>% left_join(spsla, by = "spcode")

fig6 <- ggplot(flamdt, aes(above.drym, heatb, color = fmcgroup)) + 
  geom_point(size = ptsize) + 
  geom_blank(data = predheat, aes(above.drym, heatb, color = fmcgroup)) +
  geom_smooth(data = predheat, method = "lm", se=FALSE, size = 0.8) + 
  scale_y_continuous(trans = "log",
                     breaks = y0_breaks,
                     labels = trans_format("log", math_format(e^.x)))+
  scale_color_manual(values = schwilkcolors[c(1, 3)]) +
  facet_grid(.~slagroup, labeller = label_parsed) + # use label_parsed to allow expressions in legends
  xlab("Aboveground biomass (g)") + 
  ylab("Heat release at soil surface (J)") +
  pubtheme.nogridlines + theme(legend.title = element_blank(),
                               legend.position = c(0.88, 0.2)) 
fig6
ggsave(fig6, file = file.path(RESULTS, "fig6.pdf"), width = col2, height= col1, 
       units="cm")


###############################################################################
## Table S6: traits-soil heating model coefficients and anova table
###############################################################################

traitb.aovtab <- xtable(traitsoilaov)
print(traitb.aovtab, type = "html", file = file.path(RESULTS, "trait-soil-aov.html"))
traitb.coef <- summary(traitsoil_mod)$coefficients
traitb.coeftab <- xtable(traitb.coef, digits = 4)
print(traitb.coeftab, type = "html", file = file.path(RESULTS, "trait-soil-coef.html"))

###############################################################################
## Fig.7 plant traits-50 cm heating
###############################################################################

# Break bulkden into 2 groups: bulkden<0.0013, and bulkden>0.0013

## DWS: why is there this repeated code? all the heat data should be in a
## single data frame. That is what your all_data.R script should be doing.
flam50 <- flam50 %>% mutate(dengroup = cut(bulkden, c(-Inf, 0.0013, Inf),
                                             labels =  c("Bulk~density < 0.0013~g~cm^{-3}", "Bulk~density > 0.0013~g~cm^{-3}")),
                              fmcgroup = cut(pre.fmc, breaks=c(-Inf, 0.4, Inf),
                                             labels = c("FMC < 40%", "FMC > 40%")))
# Predict value for heat at 50cm from model
predheat50 <- flam50 %>% select(label, spcode, light, block, ave.sla, pre.fmc, bulkden,
                              above.drym, weatemp, above.drym_s, pre.fmc_s, ave.sla_s, 
                              bulkden_s, weatemp_s, stgroup)
predheat50$heat50_log <- predict(trait50_mod, newdata = predheat50)
predheat50 <- predheat50 %>% mutate(heat50 = exp(heat50_log)) %>% 
  mutate(heat50 = round(heat50, 2))

# Attach fmc and bulkden group to predict value
fmc50 <- flam50 %>% group_by(spcode) %>% summarise(fmcgroup = fmcgroup[1]) 
den50 <- flam50 %>% group_by(spcode) %>% summarise(dengroup = dengroup[1]) 
predheat50 <- predheat50 %>% left_join(fmc50, by = "spcode") %>% 
  left_join(den50, by = "spcode")

fig7 <- ggplot(flam50, aes(above.drym, heat50, color = dengroup)) + 
  geom_point(size = ptsize)+
  geom_smooth(data = predheat50, method = "lm", se=FALSE, size = 0.8, color = "black") +
  facet_grid(. ~ fmcgroup) +
  scale_y_continuous(trans = "log",  # log10!
                     breaks = y50_breaks,
                     labels = trans_format("log", math_format(e^.x)))+
  xlab("Aboveground biomass (g)") + 
  ylab("Heat release at 50cm (J)") +
  scale_color_manual(values = schwilkcolors[c(1, 3)], labels=parse_format() ) +
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain"))) + 
  pubtheme.nogridlines +
  theme(legend.title = element_blank(),
        legend.position = c(0.35, 0.2),
        legend.background = element_rect(color="black"))

fig7
ggsave(file = file.path(RESULTS, "fig7.pdf"), width = col2, height= col1, 
       units="cm")  

###############################################################################
## Table S7: traits-50cm heating model coefficients and anova table
###############################################################################

trait50.aovtab <- xtable(trait50aov)
print(trait50.aovtab, type = "html", file = file.path(RESULTS, "trait-50-aov.html"))
trait50.coef <- summary(trait50_mod)$coefficients
trait50.coeftab <- xtable(trait50.coef, digits = 4)
print(trait50.coeftab, type = "html", file = file.path(RESULTS, "trait-50-coef.html"))

###############################################################################
## Fig. 8: plant traits, light environments, and live fuel moisture
###############################################################################

fmc.mean <- fmcdt %>% group_by(spcode, short.name, light) %>% 
  summarise(pre.fmc = mean(pre.fmc), ldratio = mean(ldratio))
fmc.mean <- left_join(fmc.mean, spst, by = "spcode")

fig8 <- ggplot(fmcdt, aes(ldratio, pre.fmc)) +
  geom_point(size = ptsize) +
  facet_grid(.~light,labeller = labeller(light = lightlable)) + 
  geom_smooth(data = fmc.mean, method = "lm", se = FALSE, color = "black", size = 0.8)+
  scale_y_continuous(labels = percent)+
  xlab("Live to dead mass ratio") + 
  ylab("Pre-burn fuel moisture content") +
  pubtheme.nogridlines 

ggsave(fig8, file = file.path(RESULTS, "fig8.pdf"), width = col1, height= 0.7*col1, 
       units="cm") 

###############################################################################
## Table S8: plant phenology, sla and light effects on live fuel moisture
###############################################################################

tab5.fmcaov <-xtable(fmcaov) 
print(tab5.fmcaov, type = "html", file = file.path(RESULTS, "fmc-aov.html"))
fmccoef <- summary(fmcmod)$coefficients
tab5.fmccoef <- xtable(fmccoef, digits = 4)
print(tab5.fmccoef, type = "html", file = file.path(RESULTS, "fmc-coef.html"))

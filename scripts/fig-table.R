##fig-table.R
## make all figures and tables for shade-flammability manuscript

source("./analyses.R")
source("./ggplot-theme.R")
library(patchwork)
library(sjPlot)
#ptshape <- c(0, 1, 2, 4, 5, 6, 7,8, 10, 12, 13, 14,15, 16, 17, 18, 20)
## col1 = 13.2
## col2 = 7  # This makes no sense! a 2 column figure needs to be at least 2x as wide as a 1 col figure. Check submission requirements.
ptsize = 3.5

# textsize <- 20
# axissz <- textsize-2

# calculate mean of each measurement, mean of heat50 need to be calculated by
# excluding measurements from 03/29/19
spmean <- flamdt %>% group_by(spcode, short.name, light) %>% 
  summarise_at(c("st", "combust.mass", "heat50", "heatb", "combustion", "lossrate", "max.flam", 
                 "ignition", "ave.sla", "pre.fmc", "above.drym", "bulkden"), 
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
spmean_tab <- xtable(spmean, digits = 4)
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

## get predictions of heat release for 2 data points (max. and min. of 
## standardized biomass) for each stgroup and light using mixed effect model

fig2.fit <- flamdt %>% select(spcode, phototype, light, above.drym_s, st_s, above.drym, st, stgroup) %>% 
  filter(above.drym <=40) %>% #try to match the range of x of the main layer 
  group_by(light, stgroup) %>% 
  arrange(above.drym_s) %>% filter(above.drym_s %in% range(above.drym_s))
## max. and min. of aboveground biomass for each group

fig2.fit$heat50_log <- predict(heat50_mod, newdata = fig2.fit)
                                      #re.form = NA) #does not account for random effects
fig2.fit$heatb_log <- predict(baseheat_mod, newdata = fig2.fit) #, re.form = NA)
fig2.fit <- fig2.fit %>% mutate(heat50 = 10^(heat50_log),
                                  heatb = 10^(heatb_log)) %>% 
  select(-heat50_log, -heatb_log, -above.drym_s) %>% 
  tidyr::pivot_longer(cols = starts_with("heat"),
                      names_to = "height",
                      names_prefix = "heat",
                      values_to = "heat" )

fig2a <- ggplot(subset(fig2.flamdt, height=="50"), aes(above.drym, heat, color = stgroup)) +
  geom_point(size = ptsize, shape = 16) + 
  facet_grid(.~light, labeller = labeller(light = lightlabel)) + #scales = "free_x") +
  geom_line(aes(above.drym, heat, color = stgroup), subset(fig2.fit, height == "50"),
            size = 0.8) +
  #geom_point(data = subset(fig2.spmean, height=="50"), size = ptsize) + 
  #geom_smooth(data = subset(fig2.spmean, height=="50"), method = "lm", se=FALSE, size = 0.8)  +
  scale_color_manual(values = shade_factor_colors) + 
  scale_y_continuous("Heat release at 50cm (J)",
                     trans = "log10") +
  scale_x_continuous(limits=c(0,40)) +
  pubtheme.nogridlines + 
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=smsize,face = "plain"))) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
fig2a

fig2b <- ggplot(subset(fig2.flamdt, height=="b"), aes(above.drym, heat, color = stgroup)) +
  geom_point(size = ptsize, shape = 16) + 
  geom_line(aes(above.drym, heat, color = stgroup), subset(fig2.fit, height =="b"), size = 0.8)+
  #geom_point(data = subset(fig2.spmean, height=="b"), size = ptsize) + 
  #geom_smooth(data = subset(fig2.spmean, height=="b"), method = "lm", se=FALSE, size = 0.8, color = "black") +
  facet_grid(.~light, labeller = labeller(light = lightlabel)) +
  #geom_abline(data = ref, aes(slope = slope, intercept = intcpt)) +
  #geom_errorbarh(aes(xmin=above.drym-above.drym_sd, xmax=above.drym+above.drym_sd), 
  #position = "identity", linetype = 1) + 
  scale_color_manual(values = shade_factor_colors)+
                     #name = "Shade tolerance group") +
  scale_y_continuous("Heat release at 0cm (J)",
                     trans = "log10") +
#                     breaks = y0_breaks,
                     #labels = label_number()) +
  #                     labels = trans_format("log", math_format(e^.x))) +
  scale_x_continuous("Aboveground biomass (g)", limits=c(0,40)) +
#  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
#                                                         size=smsize,face = "plain")))+
  pubtheme.nogridlines +
theme(legend.title = element_blank(),
      strip.text.x=element_blank(),
   legend.position = "right")

fig2b

fig2_approach2 <- fig2a + fig2b +
plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = 'a') &
  theme(plot.margin = unit(c(4, 4, 4, 4), "pt"),
        plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -0.5, vjust = 0.35))

fig2_approach2
ggsave(file.path(RESULTS, "fig2_approach2.jpeg"), plot=fig2_approach2,
       width=col2, height=0.7*col2, dpi = 600, unit="cm")

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
## Fig. 3: Predicted post-fire survival rate 
###############################################################################

set_theme(pubtheme.nogridlines,
          axis.title.color = "black",
          axis.textcolor = "black") #set global plot theme for sjPlot function
#use the mean (+/-) one stand deviation for shade tolerance and tiller number
# to show interaction effects. corresponding raw obs in shade tolerance are:
# 0.22, 0.43, 0.63
fig3a <- plot_model(survi_mod, type ="pred", terms = c("heatb_s [all]",
                    "pre_tinum_s [meansd]", "st_s [-1, 0, 1]", "phototype [C3]"),
                    colors = schwilkcolors[3:1], auto.label = TRUE,
                   axis.title = c("", "Survival"), title = "C3") + 
                   scale_x_continuous(breaks = c(0, 2, 4),
                   labels = c("0" = "488", "2" = "2044", "4"="3599"))+
  #convert standardized values to raw observations
  theme(legend.position = "none",
        title = element_text(size = smsize, color = "black"),
        axis.text = element_text(size = smsize))
#rewrite the facet levels so I can use nice labels for shade tolerance
levels(fig3a$data$facet) <- c("Shade.tol = 0.22", "Shade.tol = 0.43", 
                              "Shade.tol = 0.63")
fig3a


fig3b <- plot_model(survi_mod, type ="pred", terms = c("heatb_s [all]",
                   "pre_tinum_s [meansd]", "st_s [-1, 0, 1]", "phototype [C4]"),
                    axis.title = c("Heat release at soil surface (J)", "Survival"), 
                   title = "C4", legend.title = "Tiller number", 
                   colors = schwilkcolors[3:1]) +
  scale_color_manual(labels = c("14", "63", "112"), values = schwilkcolors[3:1])+
  scale_x_continuous(breaks = c(0, 2, 4),
    labels = c("0" = "488", "2" = "2044", "4"="3599"))+
  #convert standardized values to raw observations
  theme(legend.position = "right",
        legend.text = element_text(size = smsize),
        title = element_text(size = smsize),
        axis.text = element_text(size = smsize))

levels(fig3b$data$facet) <- c("Shade.tol = 0.22", "Shade.tol = 0.43", 
                              "Shade.tol = 0.63")
fig3b

fig3 <- fig3a + fig3b  +
  plot_layout(ncol = 1, nrow = 2, guides = "collect") +
  theme(plot.margin = unit(c(4, 4, 4, 4), "pt"),
        plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -0.5, vjust = 0.35))
fig3

ggsave(fig3, file = file.path(RESULTS, "fig3.jpeg"), width = col2 , height= 0.7*col2, 
       units="cm", dpi = 600)



###############################################################################
## Table S4: post fire survival rate model coefficients and anova table
###############################################################################

tab2survi.aov <- xtable(survi.aov) 
print(tab2survi.aov, type = "html", file = file.path(RESULTS, "survival-aov.html"))
survi.coef <- summary(survi_mod)$coefficients
tab2survi.coef <- xtable(survi.coef, digits = 4)

###############################################################################
# Fig. 4: Predicted post-fire percentage biomass recovery
###############################################################################

fig4 <- plot_model(resp_lmmod, type="pred", 
                   terms = c("heatb_s [all]"), 
                   axis.title = c("Heat release at soil surface (J)",
                                  "Predicted relative mass recovery")) +
  scale_x_continuous(breaks = c(0, 2, 4),
                     labels = c("0" = "488", "2" = "2044", "4"="3599"))+
  scale_y_continuous(labels = math_format(10^.x))+
  geom_line(size = 0.4) +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 0.5*textsize),
        axis.title.x = element_text(size = 0.5*textsize),
        axis.text.y = element_text(size = 0.5*smsize),
        axis.text.x = element_text(size = 0.5*smsize),
        strip.text.x = element_text(family=fontfamily, size = 0.5*textsize),
        panel.border = element_rect(size = 0.8)) 

fig4

ggsave(fig4, file = file.path(RESULTS, "fig4.jpeg"), width = col1, height= 0.7*col1,
       units="cm", dpi = 600)

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
## Fig. 5: plant traits-soil heating
###############################################################################

flamdt <- flamdt %>% mutate(slagroup = cut(ave.sla,
                                           breaks = c(-Inf, 300, Inf),
                                           labels =  c("SLA > 300~cm^2~g^{-1}",  "SLA < 300~cm^2~g^{-1}")),
                            fmcgroup = cut(pre.fmc, breaks=c(-Inf, 0.4, Inf),
                                           labels = c("FMC < 40%", "FMC > 40%")),
                            dengroup = cut(bulkden, c(-Inf, 0.0013, Inf),
                                           labels =  c("Bulk~density < 0.0013~g~cm^{-3}", "Bulk~density > 0.0013~g~cm^{-3}"))
                            )
# Predict value of soil heating from mixed effect model
fig5.fit <- flamdt %>% select(spcode, light, slagroup, fmcgroup, above.drym, 
                              pre.fmc, bulkden, ave.sla, weatemp, above.drym_s, 
                              pre.fmc_s, bulkden_s, ave.sla_s, weatemp_s) %>% 
  group_by(slagroup, fmcgroup) %>% 
  arrange(above.drym_s) %>% filter(above.drym_s %in% range(above.drym_s)) %>% 
  filter(spcode !="pava2") #within fmc<40% group ended up with two obs having the
                           # same biomass, selecte the one having higher fmc

fig5.fit$heatb_log <- predict(traitsoil_mod, newdata = fig5.fit) #random effect considered
fig5.fit <- fig5.fit %>% mutate(heatb = 10^(heatb_log)) %>% 
  select(-heatb_log, -above.drym_s, -pre.fmc_s, -bulkden_s, -ave.sla_s, -weatemp_s)

fig5 <- ggplot(flamdt, aes(above.drym, heatb, color = fmcgroup)) + 
  geom_point(size = 0.5*ptsize, alpha = 0.8, shape = 16) + 
  facet_grid(.~slagroup, labeller = label_parsed) +
  geom_line(aes(above.drym, heatb, color = fmcgroup), fig5.fit, size = 0.4) +
  scale_color_manual(values = schwilkcolors[c(3, 1)]) + 
  scale_y_continuous("Heat release at soil surface (J)",
                     trans = "log10") +
  xlab("Aboveground biomass (g)") +
  pubtheme.nogridlines + 
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size= 0.5*smsize,face = "plain"))) +
  theme(axis.title.y = element_text(size = 0.5*textsize),
        axis.title.x = element_text(size = 0.5*textsize),
        axis.text.y = element_text(size = 0.5*smsize),
        axis.text.x = element_text(size = 0.5*smsize),
        strip.text.x = element_text(family=fontfamily, size = 0.5*textsize),
        panel.border = element_rect(size = 0.8),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.2))
fig5
ggsave(fig5, file = file.path(RESULTS, "fig5.jpeg"), width = col1, height= 0.7*col1, 
      dpi = 600,  units="cm")


###############################################################################
## Table S6: traits-soil heating model coefficients and anova table
###############################################################################

traitb.aovtab <- xtable(traitsoilaov)
print(traitb.aovtab, type = "html", file = file.path(RESULTS, "trait-soil-aov.html"))
traitb.coef <- summary(traitsoil_mod)$coefficients
traitb.coeftab <- xtable(traitb.coef, digits = 4)
print(traitb.coeftab, type = "html", file = file.path(RESULTS, "trait-soil-coef.html"))

###############################################################################
## Fig.6 plant traits-50 cm heating
###############################################################################
fig6.fit <- flamdt %>% select(spcode, light, dengroup, fmcgroup, above.drym, 
                              pre.fmc, bulkden, ave.sla, weatemp, above.drym_s, 
                              pre.fmc_s, bulkden_s, ave.sla_s, weatemp_s) %>% 
  filter(above.drym <= 40) %>% #the only ob had mass >40 is from first trial and excluded 
  group_by(dengroup, fmcgroup) %>% 
  arrange(above.drym_s) %>% filter(above.drym_s %in% range(above.drym_s))

fig6.fit$heat50_log <- predict(trait50_mod, newdata = fig6.fit) #random effect considered
fig6.fit <- fig6.fit %>% mutate(heat50 = 10^(heat50_log)) %>% 
  select(-heat50_log, -above.drym_s, -pre.fmc_s, -bulkden_s, -ave.sla_s, -weatemp_s)

fig6 <- ggplot(flamdt, aes(above.drym, heat50, color = dengroup)) + 
  geom_point(size = 0.5*ptsize, alpha = 0.8, shape = 16) + 
  facet_grid(.~fmcgroup) +
  geom_line(aes(above.drym, heat50, color = dengroup), fig6.fit, size = 0.4) +
  scale_color_manual(values = schwilkcolors[c(3, 1)], labels=parse_format()) + 
  scale_y_continuous("Heat release at 50cm (J)",
                     trans = "log10") +
  xlab("Aboveground biomass (g)") +
  scale_x_continuous(limits=c(0,40)) +
  guides(color = guide_legend(label.theme = element_text(family=fontfamily, 
                                                         size=0.5*smsize,face = "plain"))) +
  pubtheme.nogridlines + 
  theme(axis.title.y = element_text(size = 0.5*textsize),
        axis.title.x = element_text(size = 0.5*textsize),
        axis.text.y = element_text(size = 0.5*smsize),
        axis.text.x = element_text(size = 0.5*smsize),
        strip.text.x = element_text(family=fontfamily, size = 0.5*textsize),
        panel.border = element_rect(size = 0.8),
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.margin = margin(t = -12))
        #legend.background = element_rect(color="black"))
fig6
ggsave(file = file.path(RESULTS, "fig6.jpeg"), width = col1, height= 0.7*col1, 
       units="cm", dpi = 600)  

###############################################################################
## Table S7: traits-50cm heating model coefficients and anova table
###############################################################################

trait50.aovtab <- xtable(trait50aov)
print(trait50.aovtab, type = "html", file = file.path(RESULTS, "trait-50-aov.html"))
trait50.coef <- summary(trait50_mod)$coefficients
trait50.coeftab <- xtable(trait50.coef, digits = 4)
print(trait50.coeftab, type = "html", file = file.path(RESULTS, "trait-50-coef.html"))

###############################################################################
## Fig. 7: plant traits, light environments, and live fuel moisture
###############################################################################

#fmc.mean <- fmcdt %>% group_by(spcode, short.name, light) %>% 
  #summarise(pre.fmc = mean(pre.fmc), ldratio = mean(ldratio))

fig7.fit <- fmcdt %>% select(spcode, light, ldratio, ave.sla, ave.sla_s) %>% 
  filter(ldratio > 0) %>% 
  group_by(light) %>% 
  arrange(ldratio) %>% filter(ldratio %in% range(ldratio))
  
fig7.fit$pre.fmc <- predict(fmcmod, newdata = fig7.fit)

fig7 <- ggplot(fmcdt, aes(ldratio, pre.fmc)) +
  geom_point(size = 0.5*ptsize, alpha = 0.5, shape = 16) +
  facet_grid(.~light, labeller = labeller(light = lightlabel)) + 
  geom_line(aes(ldratio, pre.fmc), fig7.fit, size = 0.4)+
  scale_y_continuous(labels = percent)+
  xlab("Live to dead mass ratio") + 
  ylab("Pre-burn fuel moisture content") +
  pubtheme.nogridlines +
  theme(axis.title.y = element_text(size = 0.5*textsize),
        axis.title.x = element_text(size = 0.5*textsize),
        axis.text.y = element_text(size = 0.5*smsize),
        axis.text.x = element_text(size = 0.5*smsize),
        strip.text.x = element_text(family=fontfamily, size = 0.5*textsize),
        panel.border = element_rect(size = 0.8))

fig7
ggsave(fig7, file = file.path(RESULTS, "fig7.jpeg"), width = col1, height= 0.7*col1, 
       units="cm", dpi = 600) 

###############################################################################
## Table S8: plant phenology, sla and light effects on live fuel moisture
###############################################################################

tab5.fmcaov <-xtable(fmcaov) 
print(tab5.fmcaov, type = "html", file = file.path(RESULTS, "fmc-aov.html"))
fmccoef <- summary(fmcmod)$coefficients
tab5.fmccoef <- xtable(fmccoef, digits = 4)
print(tab5.fmccoef, type = "html", file = file.path(RESULTS, "fmc-coef.html"))

#supplementary-material-figs.R
# r script for making figures for supplementary materials

source("./flam-traits-PCA.R")
source("./ggplot-theme.R")

#Figure S2
s1d <- alldata %>% select(spcode, label, short.name, light, trial.date, 
                          heat50, heatb, ignition, combustion, lossrate) %>% 
  mutate(lossrate = abs(lossrate))
s1d$ignition <- as.numeric(s1d$ignition)

s1d <- s1d[!duplicated(s1d), ] #delete duplicated rows
s1d <- s1d %>%  mutate_at(c("heat50", "heatb", "ignition", "combustion",
                            "lossrate"), ~log(.))
trial1 <- s1d %>% filter(trial.date == "3/29/19") #data from 1st trial 
trial1.sp <- unique(trial1$spcode) #species included in 1st trial
s1d <- s1d %>% filter(spcode %in% trial1.sp) #only observations from those species 

#group observations by different flammability experiment method
group_method <- function(trial.date){
  if (trial.date == "3/29/19") return("method1")
  else return("method2")
}
s1d$method <- sapply(s1d$trial.date, group_method)

#model examine if observations were influenced by methods
mod1 <- lme4::lmer(heat50 ~ method*light + (1|spcode), s1d, REML= TRUE)
Anova(mod1, test.statistic = "F") #yes, method influenced heta50
mod2 <- lme4::lmer(heatb ~ method*light + (1|spcode), s1d, REML = TRUE)
Anova(mod2, test.statistic = "F") #no, method did not influence heat release at soil surface
mod3 <- lme4::lmer(ignition ~  method*light + (1|spcode), s1d, REML= TRUE)
Anova(mod3, test.statistic = "F")
mod4 <- lme4::lmer(combustion ~  method*light + (1|spcode), s1d, REML= TRUE)
Anova(mod4, test.statistic = "F")
mod5 <- lme4::lmer(lossrate ~  method*light + (1|spcode), s1d, REML= TRUE)
Anova(mod5, test.statistic = "F")

##experiment methods only had influence on heat release at 50cm
## so have to throw away measurements for heat50 from the first trial date
#visualize the method effects
#lightlable <- c("0% shade", "50% shade")
#names(lightlable) <- c("fs", "s")

#ggplot(s1d, aes(spcode, heat50)) + geom_point() +
  #geom_point(data = trial1, aes(spcode, heat50), color = "red") +
  #facet_grid(light ~., labeller = labeller(light = lightlable)) + 
  #xlab("Species") +
  #ylab("Heat release at 50cm in log-scale (J)") +
  #pubtheme.nogridlines
#ggsave("../results/S2-1.pdf", width = col1, height= 0.7*col1, 
       #units="cm")

#ggplot(s1d, aes(spcode, heatb))+ geom_point() +
#  geom_point(data = trial1, aes(spcode, heatb), color = "red") +
  #facet_grid(light ~., labeller = labeller(light = lightlable)) + 
  #xlab("Species") +
  #ylab("Heat release at soil surface in log-scale (J)") +
  #pubtheme.nogridlines
#ggsave("../results/S2-2.pdf", width = col1, height= 0.7*col1, 
       #units="cm")

#Figure S3
sample <- balance_data %>% filter(label == "ante2fsb1") %>% 
  filter(is.flaming) %>% 
  mutate(diff_time = diff_time - 50) # example data from one of the trials

ggplot(sample, aes(diff_time, weight)) + geom_point() +
  xlab("Time since ignition (s)") +
  ylab("Remained mass (g)") + 
  pubtheme.nogridlines
ggsave("../results/S3.pdf", width = col1, height= 0.7*col1, 
       units="cm")

# Figure S4

summary(fppca)
flampca.loads <- as.data.frame(loadings(fppca))
flampca.loads
flampca.scores <- as.data.frame(scores(fppca))
var.name <- c("combustion", "lossrate", "mass loss", 
              "heat50", "heat0", "ignition")

get_ratio <- function(pcscores, pcloads) {
  # get the ratio for PC1-PC2 biplot
  mult <- min(
    (max(pcscores$PC2) - min(pcscores$PC2)/(max(pcloads$PC2)-min(pcloads$PC2))),
    (max(pcscores$PC1) - min(pcscores$PC1)/(max(pcloads$PC1)-min(pcloads$PC1)))
  )
  return(mult)
}

mult <- get_ratio(flampca.scores,flampca.loads)

## produce new loading coordinates by the score/loading ratio 
## to propotionally extend loading segments on biplot

flampca.loads <- transform(flampca.loads,
                           v1 = 0.8 * mult * PC1,
                           v2 = 0.8 * mult * PC2)

## adjust text coordinate for each variable name to avoid overlap of
## text
text.cor <- flampca.loads %>% select(v1, v2)

text.cor$v2[1] <- text.cor$v2[1] + 0.3
text.cor$v2[2] <- text.cor$v2[2] + 0.4
text.cor$v2[3] <- text.cor$v2[3] - 0.2
text.cor$v1[3] <- text.cor$v1[3] + 0.5
text.cor$v2[4] <- text.cor$v2[4] - 0.3
text.cor$v2[5] <- text.cor$v2[5] + 0.4
text.cor$v1[6] <- text.cor$v1[6] + 0.1


ggplot() +
  geom_point(data = flampca.scores, aes(x=PC1, y=PC2), alpha = 0.5) +
  geom_segment(data = flampca.loads, aes(x = 0, y = 0, xend = v1, yend = v2),
               arrow = arrow(length= unit(0.2, "cm")), alpha = 0.75)+
  geom_text(data = text.cor, aes(x=v1, y=v2, label=var.name),
            size = 3, vjust=0.5, hjust="inward", color="black") +
  xlab("Principal component 1 (48.1%)") +
  ylab("Principal component 2 (18.1%)") +
  pubtheme.nogridlines + theme(legend.position = "none",
                               axis.title = element_blank())

ggsave("../results/S4.pdf", width = col1, height= 0.7*col1, 
       units="cm")

  
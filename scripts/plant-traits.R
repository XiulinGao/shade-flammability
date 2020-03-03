## read plant traits data and do specific leaf area(SLA), surface to volume ratio (SA:V), 
## biomass density,fuel moisture content, biomass height ratio calculation
library(dplyr)
source("./shade-tolerance-ranking.R")
source("./burning-trial-summary.R")


## read leaf dimension and dry mass data for wide leaf and leaf resembles needle
## for SLA and surface area to volume ratio calculation
nleaf <- read.csv("../data/SLA-needle.csv", stringsAsFactors = FALSE,  
                  na.strings = c("", "NA", "na"))
wleaf <- read.csv("../data/SLA-wide.csv", stringsAsFactors = FALSE, 
                  na.strings = c("", "NA", "na"))

## read canopy dimension and tiller thickness data for measurements with and
## without wire cloth, for canopy architecture traits calculation
catiller <- read.csv("../data/canopy-tiller.csv", stringsAsFactors = FALSE,
                     na.strings = c("", "NA", "na")) # without wire cloth
catiller.open <- read.csv("../data/canopy-tiller-no-cylinder.csv", 
                    stringsAsFactors = FALSE, na.strings = c("", "NA", "na")) #with wire cloth

## read fuel moisture content, leaf and non-leaf dry mass data, for fuel moisture content
## leaf:non-leaf mass ratio, and probably surface area to volume ratio calculation for 
## entire plant
fmc.mratio <- read.csv("../data/fmc-leaf-culm-ratio.csv", stringsAsFactors = FALSE,
                       na.strings = c("", "NA", "na"))
fmc.mratio <- fmc.mratio %>% left_join(trials, by = c("spcode", "light", "block")) %>%
  group_by(spcode, light, block, label) %>%
  summarise(pre.fmc = (t.fresh - m.leaf - m.nleaf)/(m.leaf + m.nleaf), 
            post.fmc = (f.fuelresid - d.fuelresid)/d.fuelresid,
            leftfuel = d.fuelresid,
            char = end.weight-final.weight-f.fuelresid,#2 negative
            # value both are -0.1, due to inaccurate measurement at burning exp
            char = round(char, 1), 
            curate = d.fresh/t.fresh, #curing rate
            ldratio = (t.fresh-d.fresh)/d.fresh, # live to dead fuel ratio
            leafpcnt = m.leaf/(m.leaf+m.nleaf)) #leaf mass percentage

## pocofsb3 and arpu9sb3 had negative char value, which likely
## is due to inaccurate measurements of (end.weight, final weight or f.fuelresid) 
## at the burn scene. Hoever, the value is small, both are -0.1, so I'll assign
## 0 to char for pocofsb3 and arpu9sb3

#assign 0 to char for the two -0.1 values 
fmc.mratio$char[which(fmc.mratio$label=="pocofsb3")] <- 0
fmc.mratio$char[which(fmc.mratio$label=="arpu9sb3")] <- 0

fmc.mratio <- fmc.mratio %>% mutate(t.fuelresid = leftfuel + char) %>% 
  select(-char, -leftfuel)
## SLA and SA:V ratio calculation
wleaf_sum <- wleaf %>% group_by (spcode, light, block, treatment, rep) %>%
  mutate(sla = round(area/mass, 2)) %>%
  mutate (mean.dia = mean(thick.a, thick.b, thick.c, trim = 0, na.rm = TRUE),
    mean.dia = mean.dia/10, #covert mm to cm
                          sav = round(1/mean.dia, 2))

nleaf <- nleaf %>% mutate(dia.a = dia.a/10, dia.b = dia.b/10) #convert thickness to cm
nleaf <- nleaf %>%
    mutate(area.a = pi*dia.a*(length + dia.a/2),
    area.b = pi*dia.b*(length + dia.b/2)) %>% 
    mutate(ave.sa = (area.a + area.b)/2) 
nleaf_sum <- nleaf %>%  group_by(spcode, light, block, treatment, rep) %>%
  mutate (sa = mean(ave.sa, trim = 0, na.rm = TRUE), sa.sd = sd(ave.sa, na.rm = TRUE),
  mean.dia= mean(dia.a, dia.b, trim = 0, na.rm = TRUE)) %>%
  mutate(sla = round(sa/mass, 2), sav = round (1/mean.dia,2 ))

## combine two summary datasets and only keep calculated specific leaf area 
## and surface area to volume ratio for each plant with 3 replicates per plant
leaftrait <- bind_rows (nleaf_sum, wleaf_sum) %>%
select(spcode, light, block, treatment, rep, sla, sav) %>%
  filter(!is.na(sla))
leaftrait.sum <- leaftrait %>% group_by(spcode, light, block, treatment) %>%
  summarise(ave.sla = mean(sla, trim = 0, na.rm = TRUE),
            sd.sla = sd(sla, na.rm = TRUE),
            ave.sav = mean(sav, trim = 0, na.rm = TRUE),
            sd.sav = sd(sav, na.rm = TRUE))

## biomass density: assume plant enclosed is made up by 3 sections: one
## truncated cone at the bottom before it touches the cylinder (12.6cm in diameter), one
## cylinder section from where it starts touching the cylinder to 
## the top end of the cylinder, and another truncated cone that extends out of the cylinder.
## height of wire cylinder is 30cm, so the cylinder section of the plant parts
## enclosed is (30-h1), height of truncated cone at bottom is h1, and height of
## truncated cone at top is (h2-30)
catiller <- catiller %>%  #calculate height for sections
mutate(ave.baseW = round((baseW.a + baseW.b)/2, 1),
       ave.canW = round((canW.a + canW.b)/2, 1)) %>% # average width at each sampling direction of same point
mutate(ave.tiller1tck = round((tiller1tck.a + tiller1tck.b)/(2*10), 2),
       ave.tiller2tck = round((tiller2tck.a + tiller2tck.b)/(2*10), 2),
       ave.tiller3tck = round((tiller3tck.a + tiller3tck.b)/(2*10), 2)) 
# calculate average tiller thickness and convert mm to cm 

## as there are cases where the plant never touched the cylinder and no part extended out
## I'll calculate volume for each case differently

case1 <- catiller %>% filter(is.na(h1)) %>% 
     mutate(total.vol = pi/3 * h2 * (ave.baseW/2*ave.baseW/2 + ave.canW/2*ave.canW/2 + 
                                      ave.baseW/2 * ave.canW/2))
#plant enclosed never touched the cylinder and no parts extended out 

case2 <- catiller %>% filter(!is.na(h1) & h2>30) %>%
  mutate(cyl.h = 30-h1, tcone2.h = h2-30) %>%
        mutate(bcone.vol = pi/3 * h1 * (6.3*6.3 + ave.baseW/2*ave.baseW/2 + 6.3*ave.baseW/2),
               cyl.vol = pi*6.3*6.3*cyl.h, 
               tcone.vol = pi/3 * tcone2.h * (6.3*6.3 + ave.canW/2*ave.canW/2 + 6.3*ave.canW/2))
#plant enclosed touched the cylinder and also entended out 

case3 <- catiller %>% filter(!is.na(h1) & h2<=30) %>%
  mutate(cyl.h = h2 - h1) %>%
  mutate(bcone.vol = pi/3 * h1 * (6.3*6.3 + ave.baseW/2*ave.baseW/2 + 6.3*ave.baseW/2),
         cyl.vol = pi*6.3*6.3*cyl.h)
#plant enclosed did touch the cylinder, however, no portion extended out

## calculate average total volume and sd of total volume for each case
sum1 <- case1 %>% group_by ( spcode, light, block) %>%
  summarise(ave.tvol = mean(total.vol, trim = 0), 
            sd.tvol = sd(total.vol)) 

sum2 <- case2 %>% mutate(total.vol = bcone.vol + cyl.vol + tcone.vol) %>%
  group_by(spcode, light, block) %>% summarise (ave.tvol = mean(total.vol, trim = 0),
                                                sd.tvol = sd(total.vol))
sum3 <- case3 %>% mutate(total.vol = bcone.vol + cyl.vol) %>%
  group_by(spcode, light, block) %>% summarise(ave.tvol = mean(total.vol, trim = 0),
                                               sd.tvol = sd(total.vol))
## volume for plants without wire cylinder 
catiller.open <- catiller.open %>% mutate(ave.baseW = round((baseW.a + baseW.b)/2, 1),
                                          ave.midW = round((midW.a + midW.b)/2, 1),
                                          canW = 2*ave.midW - ave.baseW) %>% # average width at each sampling direction of same point
  mutate(ave.tiller1tck = round((tiller1tck.a + tiller1tck.b)/(2*10), 2),
         ave.tiller2tck = round((tiller2tck.a + tiller2tck.b)/(2*10), 2),
         ave.tiller3tck = round((tiller3tck.a + tiller3tck.b)/(2*10), 2)) 

sum4 <- catiller.open %>% mutate(total.vol = pi/3 * h2 * (ave.baseW/2*ave.baseW/2 + 
                                            canW/2* canW/2 + ave.baseW/2*canW/2)) %>%
                          group_by(spcode, light, block) %>%
                          summarise(ave.tvol = mean(total.vol, trim = 0), 
                                    sd.tvol = sd(total.vol))

## combine all and calculate total volume (except for the special case)
vol.sum <- bind_rows(sum1, sum2, sum3, sum4)

## combine leaf trait, volume, fmc-related traits, and shade tolerance
## to make a complete traits dataset
grasstraits <- leaftrait.sum %>% left_join(vol.sum, by = c("spcode", "light",
                                                           "block")) %>% 
  left_join(fmc.mratio, by = c("spcode", "light", "block")) %>%
  left_join(massgain.sp, by = "spcode")

#join trials
trait.trial <- grasstraits %>% left_join(trials, by = c("label", "spcode", "light","block")) %>% 
  mutate(above.drym = round(tfresh.mass/(pre.fmc + 1), 1), 
         combust.mass = above.drym - t.fuelresid) #18 negative values
## trying 2 different solutions to reduce negative combust.mass value
## assumption: post.fmc is sampled with larger sample size, especially for 
## samples didn't burn well, should be more accurate than pre.fmc given
## difference in sample size

trait.trial$pre.fmc2 <- NA
trait.trial$pre.fmc3 <- NA
 
#pre.fmc2: replace pre.fmc with post.fmc where combust biomass is negative

  for (i in 1:length(trait.trial$combust.mass)){
    if(!is.na(trait.trial$combust.mass[i])){
      if(trait.trial$combust.mass[i]<0) {
        trait.trial$pre.fmc2[i] <- trait.trial$post.fmc[i]
      }
      else{trait.trial$pre.fmc2[i] <- trait.trial$pre.fmc[i]}
    }
    else{trait.trial$pre.fmc2[i] <- trait.trial$pre.fmc[i]}
  }

#calculate combust biomass using the replaced pre.fmc
trait.trial <- trait.trial %>% mutate(above.drym2 = round(tfresh.mass/(pre.fmc + 1), 1),
                                      combust.mass2 = above.drym2 - t.fuelresid)

#pre.fmc3: replace pre.fmc with post.fmc when it is lower than post.fmc

for (i in 1:length(trait.trial$pre.fmc)){
  if(!is.na(trait.trial$pre.fmc[i])){
    if (!is.na(trait.trial$post.fmc[i])){
      if(trait.trial$pre.fmc[i]<trait.trial$post.fmc[i]) {
        trait.trial$pre.fmc3[i] <- trait.trial$post.fmc[i]
      }
      else{(trait.trial$pre.fmc3[i] <- trait.trial$pre.fmc[i])}
    }
   else{trait.trial$pre.fmc3[i] <- trait.trial$pre.fmc[i]}
  }
}

#calculate combust biomass using the replaced pre.fmc
trait.trial <- trait.trial %>% mutate(above.drym2 = round(tfresh.mass/(pre.fmc2 + 1), 1),
                                      combust.mass2 = above.drym2 - t.fuelresid,
                                      above.drym3 = round(tfresh.mass/(pre.fmc3 + 1), 1),
                                      combust.mass3 = above.drym3 - t.fuelresid)
## replace pre.fmc with post.fmc when combust.mass is negative is better
## gonna use above.drym2 and combust.mass2 for later analysis

#use above.drym2 to calculate mass density and leaf mass (total mass*leafpcnt)
trait.trial <- trait.trial %>% mutate(bulkden = round(above.drym/ave.tvol, 6),
                                      bulkden2 = round(above.drym2/ave.tvol, 6),
                                      leafmass = above.drym2*leafpcnt)

#plot to see how these estimated combust biomass differ
plot(trait.trial$combust.mass, type="p", col="blue")
par(new=TRUE)
plot(trait.trial$combust.mass2, type="p", col="red", ylab="", yaxt = "n")
#par(new=TRUE)
#plot(trait.trial$combust.mass3, type="p", col="black", ylab="", yaxt="n")


## clean env
rm("case1", "case2", "case3", "sum1", "sum2", "sum3", "sum4", "nleaf_sum",
   "wleaf_sum", "grasstraits", "vol.sum",
   "leaftrait", "leaftrait.sum", "fmc.mratio", "nleaf", "wleaf")






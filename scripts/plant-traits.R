## read plant traits data and do specific leaf area(SLA), surface to volume ratio (SA:V), 
## biomass density,fuel moisture content, biomass height ratio calculation
library(dplyr)

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
## SLA and SA:V ratio calculation
wleaf_sum <- wleaf %>% group_by (spcode, light, block, treatment, rep) %>%
  mutate(sla = round(area/mass, 2)) %>%
  mutate (mean.dia = mean(thick.a, thick.b, thick.c, trim = 0, na.rm = TRUE),
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

## combine two 
leaftrait <- bind_rows (nleaf_sum, wleaf_sum) %>%
select(spcode, light, block, treatment, rep, sla, sav) %>%
  filter(!is.na(sla))

## biomass density: assume plant enclosed is made up by 3 sections: one
## truncated cone at the bottom before it touches the cylinder (12.6cm in diameter), one
## cylinder section and another truncated cone at the top
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
## I'll split the data and calculate volume for each case 

case1 <- catiller %>% filter(is.na(h1)) %>% 
     mutate(total.vol = pi/3 * h2 * (ave.baseW/2*ave.baseW/2 + ave.canW/2*ave.canW/2 + 
                                      ave.baseW/2 * ave.canW/2))

case2 <- catiller %>% filter(!is.na(h1) & h2>30) %>%
  mutate(cyl.h = 30-h1, tcone2.h = h2-30) %>%
        mutate(bcone.vol = pi/3 * h1 * (6.3*6.3 + ave.baseW/2*ave.baseW/2 + 6.3*ave.baseW/2),
               cyl.vol = pi*6.3*6.3*cyl.h, 
               tcone.vol = pi/3 * tcone2.h * (6.3*6.3 + ave.canW/2*ave.canW/2 + 6.3*ave.canW/2))

case3 <- catiller %>% filter(!is.na(h1) & h2<=30) %>%
  mutate(cyl.h = h2 - h1) %>%
  mutate(bcone.vol = pi/3 * h1 * (6.3*6.3 + ave.baseW/2*ave.baseW/2 + 6.3*ave.baseW/2),
         cyl.vol = pi*6.3*6.3*cyl.h)

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

  






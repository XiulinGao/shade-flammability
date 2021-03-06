## flam-traits-PCA.R
##script to do PCA on both plant traits and flammability measurements
source('./all-data.R')

############## PCA of flammability and trait measurements ##############
d <- flamdt      
len <- length(d$heat50)
#exclude heat50 from the first trial
# as the observations are biased
for (i in 1:len){
  if (d$trial.date[i] =="3/29/19"){
    d$heat50[i] <- NA}
}

#character entry in ignition converted the entire column to character
# need to convert to numeric 
d$ignition <- as.numeric(d$ignition)

d1 <- d %>% filter(!is.na(ignition)) %>%  #remove two rows where all obs are NA due to 
  select(combustion, lossrate, combust.mass,    #failed ignition
  heat50, heatb, ignition) 

d1 <- prep(d1, scale = "uv", center = TRUE) #z-score as obs at very diff scales

#correlation matrix
varcors <- cor(d1, method = "kendall", use = "pairwise")
corrplot::corrplot(varcors)
pairs(d1) #loss rate isn't linear related to any?? should it still be included?

#PCA with imputation
fppca <- d1 %>% 
  pca(nPcs=6, method="ppca") #impute with probabilistic model, which can handle 10-15%
                             # data missing

summary(fppca)
biplot(fppca) 
fppca.loads <- as.data.frame(loadings(fppca))
fppca.loads

#PCA without imputation and test of dimensionality
d2 <- d %>% filter(!is.na(lossrate)) %>% filter(!is.na(heat50)) %>% 
  filter(!is.na(combust.mass)) %>% 
  select(combustion, lossrate, combust.mass,      #remove all NAs
         heat50, heatb, ignition) 

#d2 <- prep(d2, scale = "uv", center = TRUE)
#fpca <- d2 %>% pca(nPcs = 6, method = "svd")
#summary(fpca)
#fpca.loads <- as.data.frame(loadings(fpca))
#fpca.loads

#test of dimensionality
dpca <- ade4::dudi.pca(d2, scannf = FALSE)
dtest <- ade4::testdim(dpca)
dtest$nb #number of axis to keep


# 1st axis: base temp and combustbiomass, heatb and heat50; 2nd axis: combustion;
# 3rd axis: lossrate
# first 3 axes account for 89% total variance in the dataset. 

## measurements have large loadings on the first 4  axes: leafmass, above.drym2, 
## bulkden pre.fmc2, ave.sav and ave.sla, account for 85.3% total variance in the 
## data

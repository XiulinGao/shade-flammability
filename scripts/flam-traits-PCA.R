## flam-traits-PCA.R
##script to do PCA on both plant traits and flammability measurements
library(pcaMethods)
source('./all-data.R')

############## PCA of flammability and trait measurements ##############

flamPCA <- flamdt %>% 
  select (lossrate, combust.mass, max.flam, ignition, combustion, heat50, heatb) %>% 
  pca(nPcs=5, method="ppca", center=TRUE,scale="uv")
summary(flamPCA)
biplot(flamPCA) 
flampca.loads <- as.data.frame(loadings(flamPCA))
flampca.loads
# 1st axis: base temp and combustbiomass, heatb and heat50; 2nd axis: combustion;
# 3rd axis: lossrate
# first 3 axes account for 89% total variance in the dataset. 

flampca.scores <- as.data.frame(scores(flamPCA))

traitPCA <- flamdt %>% 
  select(pre.fmc, above.drym, ave.sla, ave.sav, bulkden) %>% 
  pca(nPcs = 5, method = "svd", center=TRUE, scale="uv")
summary(traitPCA)
biplot(traitPCA)
traitpca.loads <- as.data.frame(loadings(traitPCA))
traitpca.loads
## measurements have large loadings on the first 4  axes: leafmass, above.drym2, 
## bulkden pre.fmc2, ave.sav and ave.sla, account for 85.3% total variance in the 
## data
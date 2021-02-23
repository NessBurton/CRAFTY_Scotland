
# date: 23/02/21
# author: VB
# descriptions: adapt all scottish data/model structure to new CRAFTY template (CoBRA_Trunk)

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")

### production files -----------------------------------------------------------

# order = Service, capitals, Production

agentFiles <- list.files(paste0(dirData,"/templateBasic_csv/"), pattern = "AT", full.names = T)

for (i in agentFiles){
  
  #i <- agentFiles[1]
  name <- strsplit(i, "[_]")[[1]][5]
  name <- strsplit(name, "[.]")[[1]][1]
  AFT <- read.csv(paste0(dirData,"/templateBasic_csv/AT1_prodnnconifer.csv"))
  AFT <- AFT[,c(1,3:26,2)]
  colnames(AFT)[1] <- "Service"
  write.csv(AFT, paste0(dirOut,"/production/Baseline/",name,".csv"), row.names=F)
  
}

### behavioural parameters -----------------------------------------------------

# V1
# woodland agents, marginal, water&urban, estates stubborn/don't give in once established
aftParamId <- 0
givingInDistributionMean <- 1 # woodland agents don't give in once established
givingInDistributionSD <- 0
givingUpDistributionMean <- 0
givingUpDistributionSD <- 0
serviceLevelNoiseMin <- 1
serviceLevelNoiseMax <- 1
givingUpProb <- 0

stubbornAgents <- c("prodnnconifer","prodnconifer","prodnnbroad","prodnbroad",
                    "multinnc","multinc","multinnb","multinb","multimixed",
                    "consvnative", 
                    "waterurban","marginal",
                    "estateconsv","estatemulti","estatesport")

for (i in stubbornAgents){
  
  productionCsvFile <- paste0(".//production/%s/",i,".csv")
  params <- tibble(aftParamId,givingInDistributionMean,givingInDistributionSD,givingUpDistributionMean,givingUpDistributionSD,
                   serviceLevelNoiseMin,serviceLevelNoiseMax,givingUpProb,productionCsvFile)
  write.csv(params, paste0(dirOut,"/agents/V1/AftParams_",i,".csv"), row.names=F)
  
}

# agroforestry semi-stubborn
givingInDistributionMean <- 0.5 
productionCsvFile <- paste0(".//production/%s/agroforestry.csv")
params <- tibble(aftParamId,givingInDistributionMean,givingInDistributionSD,givingUpDistributionMean,givingUpDistributionSD,
                 serviceLevelNoiseMin,serviceLevelNoiseMax,givingUpProb,productionCsvFile)
write.csv(params, paste0(dirOut,"/agents/V1/AftParams_agroforestry.csv"), row.names=F)

# rest, no thresholds in V1
givingInDistributionMean <- 0
otherAgents <- c("intarable","extarable","intpastoral","extpastoral")
for (i in otherAgents){
  
  productionCsvFile <- paste0(".//production/%s/",i,".csv")
  params <- tibble(aftParamId,givingInDistributionMean,givingInDistributionSD,givingUpDistributionMean,givingUpDistributionSD,
                   serviceLevelNoiseMin,serviceLevelNoiseMax,givingUpProb,productionCsvFile)
  write.csv(params, paste0(dirOut,"/agents/V1/AftParams_",i,".csv"), row.names=F)
  
}

# V2 - take away giving-in threshold from estates

### capital & service index files ----------------------------------------------

# Capitals
capitals <- read.csv(paste0(dirData,"/templateBasic_csv/Capitals.csv"))
capitals # ok as is
write.csv(capitals, paste0(dirOut,"/csv/Capitals.csv"), row.names = F)


# Services
services <- read.csv(paste0(dirData,"/templateBasic_csv/Services.csv"))
services # ok as is
write.csv(services, paste0(dirOut,"/csv/Services.csv"), row.names = F)


### demand ---------------------------------------------------------------------

Year <- c(1,2,3,4,5,6,7,8,9,10)
bio <- 10490.667 #12000 # inital supply V4 = 11,412
biodiversity <- seq(11000, 11900, 100)
rec <- 6304 #8000 # inital supply V4 = 7,224
recreation <- seq(7000, 7900, 100)
# increase initial demand to see if it drives change
#management <- 12000 # set so that it is higher than bio, taking priority
Demand <- tibble(Year,biodiversity,recreation)
write.csv(Demand, paste0(wd,"worlds/baseline/Demand.csv"), row.names=F)

bio/10000
rec/10000
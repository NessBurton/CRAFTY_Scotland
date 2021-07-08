
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

#agentFiles <- list.files(paste0(dirData,"/templateBasic_csv/"), pattern = "AT", full.names = T)

#for (i in agentFiles){
  
  #i <- agentFiles[1]
  #name <- strsplit(i, "[_]")[[1]][5]
  #name <- strsplit(name, "[.]")[[1]][1]
  #AFT <- read.csv(i)
  #AFT <- AFT[,c(1,3:26,2)]
  #colnames(AFT)[1] <- "Service"
  #write.csv(AFT, paste0(dirOut,"/production/Baseline/",name,".csv"), row.names=F)
  
#}

### behavioural parameters -----------------------------------------------------

# Thesis version
# woodland agents, marginal, water&urban, estates stubborn/don't give in once established
# aftParamId <- 0
# givingInDistributionMean <- 1 # woodland agents don't give in once established
# givingInDistributionSD <- 0
# givingUpDistributionMean <- 0
# givingUpDistributionSD <- 0
# serviceLevelNoiseMin <- 1
# serviceLevelNoiseMax <- 1
# givingUpProb <- 0
# 
# stubbornAgents <- c("prodnnconifer","prodnconifer","prodnnbroad","prodnbroad",
#                     "multinnc","multinc","multinnb","multinb","multimixed",
#                     "consvnative", 
#                     "waterurban","marginal",
#                     "estateconsv","estatemulti","estatesport")
# 
# for (i in stubbornAgents){
#   
#   productionCsvFile <- paste0(".//production/%v/%s/",i,".csv")
#   params <- tibble(aftParamId,givingInDistributionMean,givingInDistributionSD,givingUpDistributionMean,givingUpDistributionSD,
#                    serviceLevelNoiseMin,serviceLevelNoiseMax,givingUpProb,productionCsvFile)
#   write.csv(params, paste0(dirOut,"/agents/V1/AftParams_",i,".csv"), row.names=F)
#   
# }
# 
# # agroforestry semi-stubborn
# givingInDistributionMean <- 0.5 
# productionCsvFile <- paste0(".//production/%v/%s/agroforestry.csv")
# params <- tibble(aftParamId,givingInDistributionMean,givingInDistributionSD,givingUpDistributionMean,givingUpDistributionSD,
#                  serviceLevelNoiseMin,serviceLevelNoiseMax,givingUpProb,productionCsvFile)
# write.csv(params, paste0(dirOut,"/agents/V1/AftParams_agroforestry.csv"), row.names=F)
# 
# # rest, no thresholds in V1
# givingInDistributionMean <- 0
# otherAgents <- c("intarable","extarable","intpastoral","extpastoral")
# for (i in otherAgents){
#   
#   productionCsvFile <- paste0(".//production/%v/%s/",i,".csv")
#   params <- tibble(aftParamId,givingInDistributionMean,givingInDistributionSD,givingUpDistributionMean,givingUpDistributionSD,
#                    serviceLevelNoiseMin,serviceLevelNoiseMax,givingUpProb,productionCsvFile)
#   write.csv(params, paste0(dirOut,"/agents/V1/AftParams_",i,".csv"), row.names=F)
#   
# }

# Two behavioural models

dfBehaviour <- read.csv(paste0(dirData,"/BehaviourMaster.csv"))
colnames(dfBehaviour)[1] <- "Agent"
colnames(dfBehaviour)[4] <- "givingInDistributionSD"
dfBehaviour <- dfBehaviour %>% mutate(productionCsvFile = paste0(".//production/%s/",Agent,".csv"))
#dfBehaviour$Agent <- NULL
dfBehaviour$aftParamId <- 0
dfBehaviour <- dfBehaviour[,c(1:2,11,3:10)]

# Behavioural baseline
dfBaseline <- dfBehaviour %>% filter(Paramset == "BehaviouralBaseline")

for (i in 1:nrow(dfBaseline)){
  
  #i <- 1
  r1 <- dfBaseline[i,]
  r1$Paramset <- NULL
  Agent <- r1$Agent
  r1$Agent <- NULL
  write.csv(r1, paste0(dirOut,"/agents/BehaviouralBaseline/AftParams_",Agent,".csv"),row.names = FALSE)
    
}


# Thresholds
dfThresholds <- dfBehaviour %>% filter(Paramset == "Thresholds")

for (i in 1:nrow(dfThresholds)){
  
  #i <- 1
  r1 <- dfThresholds[i,]
  r1$Paramset <- NULL
  Agent <- r1$Agent
  r1$Agent <- NULL
  write.csv(r1, paste0(dirOut,"/agents/Thresholds/AftParams_",Agent,".csv"),row.names = FALSE)
  
}

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

demandInitial <- read.csv(paste0(dirData,"/templateBasic_csv/Demand_initial.csv"))
Year<-c(2020:2100)
softwood.timber<-rep(demandInitial$softwood.timber,length(Year)) 
hardwood.timber<-rep(demandInitial$hardwood.timber,length(Year)) 
biodiversity<-rep(demandInitial$biodiversity,length(Year)) 
carbon<-rep(demandInitial$carbon,length(Year)) 
flood.regulation<-rep(demandInitial$flood.regulation,length(Year)) 
recreation<-rep(demandInitial$recreation,length(Year)) 
livestock<-rep(demandInitial$livestock,length(Year)) 
crop.service<-rep(demandInitial$crop.service,length(Year)) 
employment<-rep(demandInitial$employment,length(Year)) 
bdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(bdemand, paste0(dirOut,"/worlds/Scotland/Baseline/Demand_Baseline.csv"),row.names = F)

demandGG <- read.csv(paste0(dirData,"/templateBasic_csv/Demand_GG.csv"))
demandMB <- read.csv(paste0(dirData,"/templateBasic_csv/Demand_MB.csv"))
demandNN <- read.csv(paste0(dirData,"/templateBasic_csv/Demand_NN.csv"))
demandWC <- read.csv(paste0(dirData,"/templateBasic_csv/Demand_WC.csv"))
demandWW <- read.csv(paste0(dirData,"/templateBasic_csv/Demand_WW.csv"))

write.csv(demandGG, paste0(dirOut,"/worlds/Scotland/Green_Gold/Demand_Green_Gold.csv"),row.names = F)
write.csv(demandMB, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits/Demand_Multiple_Benefits.csv"),row.names = F)
write.csv(demandNN, paste0(dirOut,"/worlds/Scotland/Native_Networks/Demand_Native_Networks.csv"),row.names = F)
write.csv(demandWC, paste0(dirOut,"/worlds/Scotland/Woodland_Culture/Demand_Woodland_Culture.csv"),row.names = F)
write.csv(demandWW, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Demand_Wild_Woodlands.csv"),row.names = F)


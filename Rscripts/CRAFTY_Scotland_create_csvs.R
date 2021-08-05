
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

dfProduction <- read.csv(paste0(dirData,"/AgentMaster.csv"))
head(dfProduction)
names(dfProduction)[1] <- "Agent"

# lower sensitivity to attitude capitals for woodland agents
# dfProduction[,19:24]
# dfProduction$moreNW[which(dfProduction$moreNW == 0.8)] <- 0.05
# dfProduction$lessNW[which(dfProduction$lessNW == 0.8)] <- 0.05
# dfProduction$moreF[which(dfProduction$moreF == 0.8)] <- 0.05
# dfProduction$lessF[which(dfProduction$lessF == 0.8)] <- 0.05
# dfProduction$moreNAT[which(dfProduction$moreNAT == 0.8)] <- 0.05
# dfProduction$lessNAT[which(dfProduction$lessNAT == 0.8)] <- 0.05

lstAgents <- unique(dfProduction$Agent) # remove to remove water.urban from list!
lstAgents <- lstAgents[-20]
#lstAgents <- str_remove(lstAgents,"[.]")
#lstAgents <- str_remove(lstAgents,"[.]")

lstVisions <- c( "Baseline", "Green_Gold", "Multiple_Benefits", "Native_Networks", "Wild_Woodlands", "Woodland_Culture")

for (AFT in lstAgents){
  
  #AFT <- lstAgents[2]
  dfAFT <- filter(dfProduction, Agent == AFT)
  dfAFT$Agent <- NULL
  
  print(AFT)
  
  for (vision in lstVisions){
    
    #vision <- lstVisions[6]
    
    # per vision, increase sensitivity of key agents to financial capital (PES)
    
    # if (vision == "Green_Gold" & AFT %in% c("prod.n.broad","prod.n.conifer","prod.nn.broad","prod.nn.conifer")){
    #   
    #   dfAFT$financial[which(dfAFT$financial > 0)] <- 0.08
    #   
    #   print(paste0(AFT,"  altered for vision ", vision))
    #   
    # }
    # if (vision == "Multiple_Benefits" & AFT %in% c("agroforestry", "multi.mixed", "multi.nb", "multi.nc", "multi.nnc", "multi.nnb")){
    #   
    #   dfAFT$financial[which(dfAFT$financial==0.02)] <- 0.05
    #   dfAFT$financial[which(dfAFT$financial==0.05)] <- 0.08
    #   
    #   print(paste0(AFT,"  altered for vision ", vision))
    #   
    #   
    # } 
    # if (vision == "Native_Networks" & AFT %in% c("consv.native","multi.nc","multi.nb")){
    #   
    #   dfAFT$financial[which(dfAFT$financial > 0)] <- 0.05
    #   
    #   print(paste0(AFT,"  altered for vision ", vision))
    #   
    #   
    # }
    # if (vision == "Wild_Woodlands" & AFT %in% c("consv.native","multi.nc")){
    #   
    #   dfAFT$financial[which(dfAFT$financial == 0.02)] <- 0.05
    #   dfAFT$financial[which(dfAFT$financial == 0.05)] <- 0.08
    #   
    #   print(paste0(AFT,"  altered for vision ", vision))
    #   
    # }
    # if (vision == "Woodland_Culture" & AFT %in% c("prod.n.broad","prod.n.conifer","prod.nn.broad","prod.nn.conifer",
    #                                               "agroforestry", "multi.mixed", "multi.nb", "multi.nc", "multi.nnc", "multi.nnb",
    #                                               "consv.native")){
    #   
    #   dfAFT$financial[which(dfAFT$financial == 0.02)] <- 0.05
    #   dfAFT$financial[which(dfAFT$financial == 0.05)] <- 0.08
    #   dfAFT$human[which(dfAFT$human > 0)] <- dfAFT$human[which(dfAFT$human > 0)] + 0.01
    #   dfAFT$social[which(dfAFT$social > 0)] <- dfAFT$social[which(dfAFT$social > 0)] + 0.01
    #   dfAFT$manufactured[which(dfAFT$manufactured > 0)] <- dfAFT$manufactured[which(dfAFT$manufactured > 0)] + 0.01
    #   
    #   print(paste0(AFT,"  altered for vision ", vision))
    #   
    # }
  
    print(dfAFT)
    AFT_nodot <- str_remove(AFT,"[.]") %>% str_remove(., "[.]")
    print(AFT_nodot)
    write.csv(dfAFT, paste0(dirOut,"/production/",vision,"/",AFT_nodot,".csv"), row.names = F)
      
  }
  
}




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
#options(scipen = 999)

dfBehaviour <- read.csv(paste0(dirData,"/BehaviourMaster.csv"))
colnames(dfBehaviour)[1] <- "Agent"
dfBehaviour <- dfBehaviour %>%  filter(Agent != "waterurban")

#colnames(dfBehaviour)[4] <- "givingInDistributionSD"
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



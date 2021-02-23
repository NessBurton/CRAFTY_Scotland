
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

### behavioural parameters -----------------------------------------------------

aftParamId <- 0
givingInDistributionMean <- 0
givingInDistributionSD <- 0
givingUpDistributionMean <- 0
givingUpDistributionSD <- 0
serviceLevelNoiseMin <- 1
serviceLevelNoiseMax <- 1
givingUpProb <- 0
productionCsvFile <- ".//production/%s/no_mgmt.csv"
params0 <- tibble(aftParamId,givingInDistributionMean,givingInDistributionSD,givingUpDistributionMean,givingUpDistributionSD,
                  serviceLevelNoiseMin,serviceLevelNoiseMax,givingUpProb,productionCsvFile)
write.csv(params0, paste0(agentBehavFilepath,"AftParams_no_mgmt.csv"), row.names=F)

### production files -----------------------------------------------------------

# no management
Service <- c("biodiversity","recreation")
Production <- c(1,1) # if no OPM, assume maximum amount of Services can be produced
#OPMpresence <- c(0,0) # not relianct on presence
OPMinverted <- c(0.9,0.9) # but if OPM present and unable to manage, bio and recreation provision compromised maximum amount
riskPerc <- c(0,0) # no reliance on risk perception
budget <- c(0,0) # no reliance on budget
knowledge <- c(0,0) # no reliance on knowledge
nature <- c(1,0) # Production of biodiversity fully reliant on nature capital level, 1:1
access <- c(0,1) # Production of recreation fully reliant on access capital level, 1:1

no.mgmt <- tibble(Service,OPMinverted,riskPerc,budget,knowledge,nature,access,Production)
view(no.mgmt)
write.csv(no.mgmt, paste0(agentProdFilepath,"no_mgmt.csv"), row.names=F)

### capital & service index files ----------------------------------------------

# Capitals
Name <- c("OPMinverted","riskPerc","budget","knowledge","nature","access")
Index <- c(0,1,2,3,4,5)
Capitals <- tibble(Name,Index)
write.csv(Capitals, paste0(wd,"csv/Capitals.csv"), row.names=F)

# Services
Name <- c("biodiversity","recreation")
Index <- c(0,1)
Services <- tibble(Name,Index)
write.csv(Services, paste0(wd,"csv/Services.csv"), row.names=F)

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
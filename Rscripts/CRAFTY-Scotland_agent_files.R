
# date: 12/03/2021
# author: VB
# purpose: create csv files for each agent

library(tidyverse)

#wd <- "~/CRAFTY-opm" # sandbox VM
wd <- "~/eclipse-workspace/CRAFTY_Scotland/data_Scotland/"

agentProdFilepath <- paste0(wd,"production/Baseline_V2/")
agentBehavFilepath <- paste0(wd,"agents/V4/")

# define Production levels for Services (0-1, low-high)
# and sensitivity of Production to each capital (0-1, low-high)

# V2 - rethink sensitivities, and tweak productivities based on mean top50 from normalised data

# agroforestry
Service <- c("softwood.timber","hardwood.timber","biodiversity","carbon","flood.regulation","recreation","livestock","crop.service","employment")
Production <- c(0,0.95,0.71,0.45,0.5,0.1,0.1,0,0.28) 
nn.conifer.yc <- c(0.9,0.9) 
nn.broad.yc <- c(0,0) 
n.conifer.yc <- c(0,0) 
n.broad.yc <- c(0,0) 
n.broad.consv <- c(1,0) 
agro.yc <- c(0,1) 
mixed.yc <- c()
crop.productivity <- c()
grassland <- c()
water.runoff <- c()
human <- c()
social <- c()
manufactured <- c()
financial <- c()
moreNW <- c()
lessNW <- c()
moreF <- c()
lessF <- c()
moreNAT <- c()
lessNAT <- c()
deep.peat <- c()
deer.density <- c()
wild.land <- c()


agro <- tibble(Service,Production)
view(agro)
write.csv(agro, paste0(agentProdFilepath,"no_mgmt.csv"), row.names=F)


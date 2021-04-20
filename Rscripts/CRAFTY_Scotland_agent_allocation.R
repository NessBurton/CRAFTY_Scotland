
# date: 20/04/21
# author: VB
# purpose: edit initial allocation of agent types


### working dirs --------------------------------------------------------

#wd <- "~/CRAFTY-opm" # sandbox VM
#wd <- "~/eclipse-workspace/CRAFTY_Scotland/data_Scotland/"
wd <- "~/R/CRAFTY_Scotland/" # FR laptop
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")


### libs ----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(viridis)

### read in landcover & woodland data -----------------------------------

# details of landcover are in GitCRAFTYr/CRAFTY_Scotland_setup.Rmd
# essentially proportions of either landuse/woodland type/ngo owner/designation/wild land
# deer density also included

landcover <- read.csv(paste0(dirData,'/output/landcover_landownership4.csv'))
head(landcover)
summary(landcover)


### plot ----------------------------------------------------------------

(p1 <- landcover %>% 
  pivot_longer(cols = broadleaf:marginal,
               names_to = "type",
               values_to = "proportion") %>% 
  ggplot()+
  geom_tile(aes(x=X,y=Y,fill=proportion))+
  scale_fill_viridis()+
  facet_wrap(~type)+
  theme_bw())

(p2 <- landcover %>% 
  pivot_longer(cols = nfi.broadleaf:nfi.marginal,
               names_to = "type",
               values_to = "proportion") %>% 
  ggplot()+
  geom_tile(aes(x=X,y=Y,fill=proportion))+
  scale_fill_viridis()+
  facet_wrap(~type)+
  theme_bw())

(p3 <- landcover %>% 
    pivot_longer(cols = nwss.upland.birch:nwss.marginal,
                 names_to = "type",
                 values_to = "proportion") %>% 
    ggplot()+
    geom_tile(aes(x=X,y=Y,fill=proportion))+
    scale_fill_viridis()+
    facet_wrap(~type)+
    theme_bw())

(p4 <- landcover %>% 
    pivot_longer(cols = arable.int:wild.land,
                 names_to = "type",
                 values_to = "proportion") %>% 
    ggplot()+
    geom_tile(aes(x=X,y=Y,fill=proportion))+
    scale_fill_viridis()+
    facet_wrap(~type)+
    theme_bw())


### allocate AFT type ---------------------------------------------------

landcover[is.na(landcover)] <- 0
nrows <- length(landcover[,1])
AFT <- rep("N_A",nrows)


for (i in c(1:nrows)) { # Every condition subject to the one before it
  
  # Mask out waterbodies and dense artifical (urban) areas
  if (landcover$artificial[i]>0.7) {
    AFT[i]<-'water.urban'
  } 
  if (landcover$waterbodies[i]>0.7) {
    AFT[i]<-'water.urban'
  } 
  
  # woodland
  if (landcover$broadleaf[i]+landcover$conifer[i]>=0.5){ # if half or more classified woodland in lcm
    AFT[i]<-'multi.mixed'
    if (landcover$broadleaf[i]>=0.5){
      AFT[i]<-'multi.nb'
    }
    if (landcover$conifer[i]>=0.5){
      AFT[i]<-'prod.nn.conifer'
    }
    if(landcover$nfi.conifer[i]>=0.5){
      if(landcover$nwss.non.native[i]>=0.5){
        AFT[i]<-'prod.nn.conifer'
      }
      if(landcover$nwss.native.pine[i]>=0.5){
        AFT[i]<-'prod.n.conifer'
      }
    }
    if(landcover$nfi.broadleaf[i]|landcover$nfi.coppice[i]>=0.5){
      if(landcover$nwss.non.native[i]>=0.5){
        AFT[i]<-'prod.nn.broad'
      }
      if(landcover$nwss.lowland.deciduous[i]
         +landcover$nwss.upland.ash[i]
         +landcover$nwss.upland.birch[i]
         +landcover$nwss.upland.oak[i]>=0.5){
        AFT[i]<-'prod.n.broad'
      }
    }
    if(landcover$nwss.lowland.deciduous[i]
       +landcover$nwss.native.pine[i]
       +landcover$nwss.native.regen[i]
       +landcover$nwss.scrub[i]
       +landcover$nwss.upland.ash[i]
       +landcover$nwss.upland.birch[i]
       +landcover$nwss.upland.oak[i]
       +landcover$nwss.wet.woodland[i]>=0.5
       &landcover$sssi[i]
       |landcover$spa[i]
       |landcover$sac[i]
       |landcover$nnr[i]
       |landcover$ramsar[i]>=0.5){
      AFT[i]<-'consv.native'
    }
    if(landcover$nfi.mixed.broadleaf[i]+landcover$nfi.mixed.conifer[i]>=0.5){
      if(landcover$nwss.lowland.deciduous[i]<0.5
         &landcover$nwss.native.pine[i]<0.5
         &landcover$nwss.native.regen[i]<0.5
         &landcover$nwss.scrub[i]<0.5
         &landcover$nwss.upland.ash[i]<0.5
         &landcover$nwss.upland.birch[i]<0.5
         &landcover$nwss.upland.oak[i]<0.5
         &landcover$nwss.wet.woodland[i]<0.5
         &landcover$nwss.non.native[i]<0.5){
        AFT[i]<-'multi.mixed'
      }
    }
    if(landcover$nfi.mixed.conifer[i]>0.5){
      if(landcover$nwss.non.native[i]>0.5){
        AFT[i]<-'multi.nnc'
      }
      if(landcover$nwss.native.pine[i]>0.5){
        AFT[i]<-'multi.nc'
      }
    }
    if(landcover$nfi.mixed.broadleaf[i]>0.5){
      if(landcover$nwss.lowland.deciduous[i]
         +landcover$nwss.native.regen[i]
         +landcover$nwss.scrub[i]
         +landcover$nwss.upland.ash[i]
         +landcover$nwss.upland.birch[i]
         +landcover$nwss.upland.oak[i]
         +landcover$nwss.wet.woodland[i]>=0.5){
        AFT[i]<-'multi.nb'
      }
      if(landcover$nwss.non.native[i]>0.5){
        AFT[i]<-'multi.nnb'
      }
    }
  }
  
  # arable
  if (AFT[i]=="N_A") {
    if (landcover$arable[i]>=0.7) {
      AFT[i]<-"ext.arable"
      if (landcover$arable.int[i]>50) { # 50 not an error! keep because data added direct from IAP, don't change to 0.5
        AFT[i]<-"int.arable"
      }
    }
  }
  
  # intensive pastoral
  if (AFT[i]=="N_A") {
    if (landcover$intensive.grassland[i]>=0.7){
      AFT[i]<-"int.pastoral"
    }
  }
  
  # extensive pastoral
  if (AFT[i]=="N_A"){
    if (landcover$extensive.grassland[i]>=0.7){
      AFT[i]<-"ext.pastoral"
    }
  }
  
  # traditional multifunctional estate (estate.multi)
  if(AFT[i] == "N_A"|AFT[i] == "ext.pastoral"){
    if (landcover$dmu[i]>1
        &landcover$broadleaf[i]>0.01
        &landcover$broadleaf[i]<=0.07 # based on SRUC survey (estate multi has average woodland 3.2% native 3.6% amenity )
        &landcover$conifer[i]>0.01
        &landcover$conifer[i]<= 0.1 # based on SRUC survey (estate multi average coverage of 8.8% commercial)
        &landcover$arable[i]|landcover$intensive.grassland[i]<0.5 # took out extensive grassland from here to try and switch some extensive grassland agents to estate.multi
        &landcover$extensive.grassland[i]>0.01
        &landcover$heather[i]<0.8
        &landcover$heather[i]>0.01){
      AFT[i] <- "estate.multi"
    }
  }
  
  # sporting management (estate.sport) 
  if(AFT[i]=="N_A"|AFT[i] == "ext.pastoral"){
    if(landcover$dmu[i]>1
       &landcover$heather[i]>=0.8
       |landcover$deer.density[i]>=40){
      AFT[i]<-"estate.sport"
    }
  }
  
  # conservationist management (estate.consv)
  if(AFT[i]=="N_A"){
    if(landcover$dmu[i]>1
       &landcover$spa[i]
       |landcover$sac[i]
       |landcover$sssi[i]
       |landcover$nnr[i]
       |landcover$ramsar[i]>=0.5){
      AFT[i]<-'estate.consv'
    }
  }
  
  # agroforestry
  if(AFT[i]=="N_A"){
    if ((landcover$broadleaf[i]+landcover$conifer[i])>=0.45
        &(landcover$broadleaf[i]+landcover$conifer[i])<=0.55
        &(landcover$arable[i]+landcover$intensive.grassland[i]+landcover$extensive.grassland[i])>=0.45
        &(landcover$arable[i]+landcover$intensive.grassland[i]+landcover$extensive.grassland[i])<=0.55){
      AFT[i]<-"agroforestry"
    }
  }
  
  # check for missing forest areas
  # i.e. if there are nfi and nwss areas which are outside of lcm broadleaf or conifer
  if (AFT[i]=="N_A"){
    #if (landcover$broadleaf[i]+landcover$conifer[i]>=0.2){
      if(landcover$nfi.conifer[i]>=0.5){
        if(landcover$nwss.non.native[i]>=0.5){
          AFT[i]<-'prod.nn.conifer'
        }
        if(landcover$nwss.native.pine[i]>=0.5){
          AFT[i]<-'prod.n.conifer'
        }
      }
      if(landcover$nfi.broadleaf[i]+landcover$nfi.coppice[i]>=0.5){
        if(landcover$nwss.non.native[i]>=0.5){
          AFT[i]<-'prod.nn.broad'
        }
        if(landcover$nwss.lowland.deciduous[i]
           +landcover$nwss.upland.ash[i]
           +landcover$nwss.upland.birch[i]
           +landcover$nwss.upland.oak[i]>=0.5){
          AFT[i]<-'prod.n.broad'
        }
      }
      if(landcover$nwss.lowland.deciduous[i]
         +landcover$nwss.native.pine[i]
         +landcover$nwss.native.regen[i]
         +landcover$nwss.scrub[i]
         +landcover$nwss.upland.ash[i]
         +landcover$nwss.upland.birch[i]
         +landcover$nwss.upland.oak[i]
         +landcover$nwss.wet.woodland[i]>=0.5
         &landcover$sssi[i]
         |landcover$spa[i]
         |landcover$sac[i]
         |landcover$nnr[i]
         |landcover$ramsar[i]>=0.5){
        AFT[i]<-'consv.native'
      }
      if(landcover$nfi.mixed.broadleaf[i]+landcover$nfi.mixed.conifer[i]>=0.5){
        if(landcover$nwss.lowland.deciduous[i]<0.5
           &landcover$nwss.native.pine[i]<0.5
           &landcover$nwss.native.regen[i]<0.5
           &landcover$nwss.scrub[i]<0.5
           &landcover$nwss.upland.ash[i]<0.5
           &landcover$nwss.upland.birch[i]<0.5
           &landcover$nwss.upland.oak[i]<0.5
           &landcover$nwss.wet.woodland[i]<0.5
           &landcover$nwss.non.native[i]<0.5){
          AFT[i]<-'multi.mixed'
        }
      }
      if(landcover$nfi.mixed.conifer[i]>0.5){
        if(landcover$nwss.non.native[i]>0.5){
          AFT[i]<-'multi.nnc'
        }
        if(landcover$nwss.native.pine[i]>0.5){
          AFT[i]<-'multi.nc'
        }
      }
      if(landcover$nfi.mixed.broadleaf[i]>0.5){
        if(landcover$nwss.lowland.deciduous[i]
           +landcover$nwss.native.regen[i]
           +landcover$nwss.scrub[i]
           +landcover$nwss.upland.ash[i]
           +landcover$nwss.upland.birch[i]
           +landcover$nwss.upland.oak[i]
           +landcover$nwss.wet.woodland[i]>=0.5){
          AFT[i]<-'multi.nb'
        }
        if(landcover$nwss.non.native[i]>0.5){
          AFT[i]<-'multi.nnb'
        }
      }      
    }
  #}  
  
  # Check for substantial farming areas missed so far
  # This is important to note in the methodology
  # It means that extensive arable/pastoral can either be extensive across the whole cell...
  # ...or it can be intensive management over only a fraction of the cell
  # this caught areas (esp. Aberdeenshire) where land was dropping out into marginal...
  # ...because it didn't meet the threshold for intensive arable/pastoral
  if (AFT[i]=="N_A"){
    if (landcover$arable[i]>=0.4){
      AFT[i]<-'ext.arable'
    }
    if((landcover$intensive.grassland[i]+landcover$extensive.grassland[i])>=0.4){
      AFT[i]<-'ext.pastoral'
    }
  }
  
  # marginal management
  if (AFT[i]=="N_A"){
    AFT[i]<-"marginal"
  }
  
  #setTxtProgressBar(pb, i)
  #close(pb)
  
}

landcover$AFT <- AFT
landcover$AFT <- as.factor(landcover$AFT)

# count number of AFTs
summary(landcover$AFT)

# summary table with percentages

AFT_summary <- landcover %>% 
  group_by(AFT) %>% 
  summarise(count = n(),
            percentage = count/nrows*100)
AFT_summary

# woodland cover
sum(AFT_summary$percentage[which(AFT_summary$AFT == "multi.mixed" |
                                   AFT_summary$AFT == "multi.nb" |
                                   AFT_summary$AFT == "multi.nc" |
                                   AFT_summary$AFT == "prod.n.broad" |
                                   AFT_summary$AFT == "prod.n.conifer" |
                                   AFT_summary$AFT == "prod.nn.broad" |
                                   AFT_summary$AFT == "prod.nn.conifer" |
                                   AFT_summary$AFT == "prod.n.broad" |
                                   AFT_summary$AFT == "consv.native")])

# 17.9% - pretty bang on for data used (2015 LCM - Forestry Stats estimate 18% cover)

head(landcover)
write.csv(landcover, paste0(dirData,"/output/AFT_allocation_Apr2021.csv"))

### plot -----------------------------------------------------------

aftColours <- c("prod.nn.conifer" = "#005A32",
                "prod.n.conifer" = "#005A32",
                "multi.nc" = "#005A32",
                "multi.nnc"= "#005A32",
                "multi.mixed" = "#238B45",
                "prod.nn.broad" = "#41AB5D", 
                "prod.n.broad" = "#41AB5D",
                "multi.nb" = "#41AB5D",
                "multi.nnb" = "#41AB5D",
                "consv.native" = "#1B9E77",
                "agroforestry" = "#D8B70A",
                "int.arable" = "darkkhaki",#9C964A", 
                "ext.arable" = "khaki4", #C3B091", 
                "int.pastoral" = "#A2A475", 
                "ext.pastoral" = "#A6DBA0", #"grey65", 
                "estate.multi" = "#C2A5CF", 
                "estate.sport" = "#9970AB",
                "estate.consv" = "#762A83", 
                "marginal" = "lightgrey",
                "water.urban" = "white")


landcover %>% 
  ggplot()+
  geom_tile(aes(x=X,y=Y,fill=AFT))+
  scale_fill_manual(values = aftColours)+
  theme_bw()


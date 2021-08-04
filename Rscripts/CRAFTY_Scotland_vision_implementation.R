
# date: 15/03/21
# author: VB
# descriptions: edit baseline capitals to represent desired changes in each vision

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(viridis)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")

# new agent allocations
AFT <- read.csv(paste0(dirData,"/output/AFT_allocation_Apr2021.csv"))
head(AFT)

yrList <- seq(2020,2095,by=5)


### baseline capitals ----------------------------------------------------------

#baseline <- read.csv(paste0(dirData,"/templateBasic_csv/TestRegion_edit.csv"))
baseline <- read.csv((paste0(dirData,"/output/capitals_normalised_Aug21.csv")))
head(baseline)
summary(baseline)

baseline[is.na(baseline)] <- 0

baseline$id <- NULL
#baseline$FR <- baseline$Agent
baseline$FR <- AFT$AFT
baseline$BT <- 0
baseline$Agent <- NULL
colnames(baseline)[1:2] <- c("x","y")
#baseline <- baseline[,-c(27:35)]

baseline$FR
baseline$FR <- str_replace_all(baseline$FR, "[[.]]", "")


write.csv(baseline, paste0(dirOut,"/worlds/Scotland_natural/Baseline/Baseline_capitals.csv"), row.names = F)
# write.csv(baseline, paste0(dirOut,"/worlds/Scotland_natural/Multiple_Benefits/Multiple_Benefits_capitals.csv"), row.names = F)
# write.csv(baseline, paste0(dirOut,"/worlds/Scotland_natural/Green_Gold/Green_Gold_capitals.csv"), row.names = F)
# write.csv(baseline, paste0(dirOut,"/worlds/Scotland_natural/Native_Networks/Native_Networks_capitals.csv"), row.names = F)
# write.csv(baseline, paste0(dirOut,"/worlds/Scotland_natural/Wild_Woodlands/Wild_Woodlands_capitals.csv"), row.names = F)
# write.csv(baseline, paste0(dirOut,"/worlds/Scotland_natural/Woodland_Culture/Woodland_Culture_capitals.csv"), row.names = F)


# baseline updater files
#baseline <- read.csv(paste0(dirOut,"/worlds/Scotland_natural/Baseline/Baseline_capitals.csv"))
#head(baseline)

updater <- baseline[,-c(27:28)]

for (yr in yrList){
  
  write.csv(updater,paste0(dirOut,"/worlds/Scotland_natural/Baseline/Baseline_",yr,".csv"),row.names = F)
  
}

### spin-up test capitals

head(baseline)
spin_up <- baseline
spin_up$FR <- NA

write.csv(spin_up,paste0(dirOut,"/worlds/Scotland_natural/Spin-up/Spin-up_capitals.csv"),row.names = F)

### raw capitals & normalise function ------------------------------------------

capitalsRAW <- read.csv(paste0(dirData,"/output/capitals_raw_Aug21.csv"))

normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}


### datasets to be used for vision changes -------------------------------------

# WEAG phase 3 areas
weag <- read.csv(paste0(dirData,'/input/weag3.csv')) 
head(weag)
weag$RASTERVALU[which(weag$RASTERVALU == -9999)] <- 0
weag$Id <- NULL
weag$ORIG_FID <- NULL
colnames(weag) <- c('id', 'x', 'y', 'phase3')
ggplot(weag)+
  geom_raster(mapping = aes(x=x, y=y, fill = phase3))
weag<-weag[,c(1,4)]

# LFA areas
lfa <- read.csv(paste0(dirData,'/input/LFA.csv'))
head(lfa)
lfa$Id<-NULL
lfa$X<-NULL
lfa$Y<-NULL
lfa$RASTERVALU<-NULL
lfa$FID <- NULL
colnames(lfa)<-c('id','status')

# Scottish Forest Habitat Network
connect <- read.csv(paste0(dirData,'/input/connectivity2.csv'))
head(connect)
connect$connectivity <- NA
connect$connectivity[which(connect$ZONE == 'Core Native Woodland')] <- 1
connect$connectivity[which(connect$ZONE == 'Primary Zone')] <- 0.75
connect$connectivity[which(connect$ZONE == 'Secondary Zone')] <- 0.5

# check
ggplot(connect)+
  geom_tile(mapping = aes(x=X, y=Y, fill = connectivity))

connect<-connect[,c(1,8)]
colnames(connect)<-c('id','connect')
connect[is.na(connect)]<-0

# Simulated land reform (attitude capitals broken into smaller sizes over 3 timesteps)
landreform <- read.csv(paste0(dirData,'/output/TestRegion_LR1.csv'))
head(landreform)
landreform$X.1<-NULL
landreform<-landreform[,c(1,19:24)]

landreform2 <- read.csv(paste0(dirData,'/output/TestRegion_LR2.csv'))
head(landreform2)
landreform2$X.1 <- NULL
landreform2<-landreform2[,c(1,19:24)]

landreform3 <- read.csv(paste0(dirData,'/output/TestRegion_LR3.csv'))
head(landreform3)
landreform3$X.1 <- NULL
landreform3<-landreform3[,c(1,19:24)]


### SCOTLAND_FINANCIAL WORLD - changes to social capitals (primarily financial to represent PES) -----

# Multiple Benefits ----

baseline <- capitalsRAW
MB <- capitalsRAW
ggplot(MB)+
  geom_tile(aes(x,y,fill=financial))

# increase financial capital where mixed yc capital > 0.5
summary(MB$mixed.yc)
MB$financial[which(MB$mixed.yc>=8)] <- MB$financial[which(MB$mixed.yc>=8)]*1.2
# increase financial capital where agroforestry capital > 0.5
summary(MB$agro.yc)
MB$financial[which(MB$agro.yc>=7)] <- MB$financial[which(MB$agro.yc>=7)]*1.2

head(MB)
# for cell updater specs
MB$FR<-NULL
MB$BT<-NULL
MB$X <- NULL
MB$id <- NULL
MB$Agent <- NULL
head(baseline)
baseline$FR<-NULL
baseline$BT<-NULL
baseline$X <- NULL
baseline$id <- NULL
baseline$Agent <- NULL

dfMB <- baseline
dfMB$year <- 2015
head(dfMB)

for (yr in yrList){
  
  MB$year <- yr
  dfMB <- rbind(dfMB,MB)
  
}


summary(dfMB)
dfMB$crop.productivity[which(dfMB$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfMB)
dfMB <- data.frame(dfMB[,c(1:3,27:28)], lapply(dfMB[4:26], normalise))
summary(dfMB)
dfMB[is.na(dfMB)] <- 0

#remove filter column
dfMB$agri.filter <- NULL

# invert deer density
invert <- dfMB$deer.density - 1
z <- abs(invert)
dfMB$deer.density <- z

yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  MB <- filter(dfMB, year == yr)
  
  if (yr == 2015){
    
    MB$FR <- AFT$AFT
    MB$FR <- str_replace_all(MB$FR, "[[.]]", "")
    MB$year <- NULL
    write.csv(MB, paste0(dirOut,"/worlds/Scotland_financial/Multiple_Benefits/Multiple_Benefits_capitals.csv"), row.names = F)
    
    
  }else{
    
    MB$year <- NULL
    write.csv(MB, paste0(dirOut,"/worlds/Scotland_financial/Multiple_Benefits/Multiple_Benefits_",yr,".csv"), row.names = F)
    
  }
  
}

# Wild Woodlands ----

WW <- capitalsRAW

ggplot(WW)+
  geom_tile(aes(x,y,fill=n.broad.consv))
ggplot(WW)+
  geom_tile(aes(x,y,fill=mixed.yc))
ggplot(WW)+
  geom_tile(aes(x,y,fill=n.conifer.yc))

# use WEAG to  target productive woodland capitals
WW<-merge(WW,weag,by='id')
ggplot(WW)+
  geom_tile(aes(x,y,fill=phase3))

# increase financial capital where n.broad.consv capital is high & WEAG phase 3 areas exist by 50%
summary(WW$n.broad.consv)
WW$financial[which(WW$n.broad.consv >= 3.5 & WW$phase3 > 0)] <- WW$financial[which(WW$n.broad.consv >= 3.5 & WW$phase3 > 0)] * 1.2
# increase financial capital where mixed.yc is high & in WEAG phase 3 areas exist by 50%
summary(WW$mixed.yc)
WW$financial[which(WW$mixed.yc >= 8 & WW$phase3 > 0)] <- WW$financial[which(WW$mixed.yc >= 8 & WW$phase3 > 0)] * 1.2
# increase n.conifer.yc in WEAG phase 3 areas by 50%
summary(WW$n.conifer.yc)
WW$financial[which(WW$n.conifer.yc >= 6 & WW$phase3 > 0)] <- WW$n.conifer.yc[which(WW$n.conifer.yc >= 6 & WW$phase3 > 0)] * 1.2

ggplot(WW)+
  geom_tile(aes(x,y,fill=financial))

baseline$X <- NULL
baseline$id <- NULL
baseline$Agent <- NULL
dfWW <- baseline
dfWW$year <- 2015
head(dfWW)

WW1 <- WW
head(WW1)

# for cell updater specs
WW1$id<-NULL
WW1$X <- NULL
WW1$Agent <- NULL
WW1$phase3 <- NULL

WW1$year <- 2020

dfWW <- rbind(dfWW,WW1)

# introduce first stage of land reform and reduce grassland capital by a 1/4
WW2 <- WW
summary(WW2)

ggplot(WW2)+
  geom_tile(aes(x,y,fill=moreNW))

WW2$moreNW <- landreform$moreNW
WW2$lessNW <- landreform$lessNW
WW2$moreF <- landreform$moreF
WW2$lessF <- landreform$lessF
WW2$moreNAT <- landreform$moreNAT
WW2$lessNAT <- landreform$lessNAT

# reduce grassland capital in LFA areas
WW2 <- merge(WW2, lfa, by = 'id', all.x = TRUE)

ggplot(WW2)+
  geom_raster(mapping=aes(x=x,y=y,fill=status))
ggplot(WW2)+
  geom_raster(mapping=aes(x=x,y=y,fill=grassland))

# nrows <- length(WW2[,1])
# 
# for (i in c(1:nrows)) {
#   if (WW2$status[i] == "Severely Disadvantaged") {
#     WW2$grassland[i]<-WW2$grassland[i] - (WW2$grassland[i]/4)
#   }
#   if (WW2$status[i] == "Disadvantaged") {
#     WW2$grassland[i]<-WW2$grassland[i] - (WW2$grassland[i]/4)
#   }
# }

head(WW2)
# for cell updater specs
WW2$id<-NULL
WW2$Agent<-NULL
WW2$X <- NULL
WW2$phase3 <- NULL
WW2$status <- NULL

head(dfWW)

for (yr in c(2025,2030,2035)){
  
  WW2$year <- yr
  dfWW <- rbind(dfWW, WW2)
  
}

# reduce deer density capital by 20%
WW3 <- WW2

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*20)
#deer <- normalise(deer)

head(WW3)
WW3$deer.density <- deer

for (yr in c(2040,2045)){
  
  WW3$year <- yr
  dfWW <- rbind(dfWW, WW3)
  
}

# introduce second stage of land reform and reduce grassland capital by half
WW4 <- WW

ggplot(WW4)+
  geom_tile(aes(x,y,fill=moreNW))

WW4$moreNW <- landreform2$moreNW
WW4$lessNW <- landreform2$lessNW
WW4$moreF <- landreform2$moreF
WW4$lessF <- landreform2$lessF
WW4$moreNAT <- landreform2$moreNAT
WW4$lessNAT <- landreform2$lessNAT

# reduce grassland capital in LFA areas
# WW4 <- merge(WW4, lfa, by = 'id', all.x = TRUE)
# 
# ggplot(WW4)+
#   geom_raster(mapping=aes(x=x,y=y,fill=status))
# ggplot(WW4)+
#   geom_raster(mapping=aes(x=x,y=y,fill=grassland))
# 
# nrows <- length(WW4[,1])
# 
# for (i in c(1:nrows)) {
#   if (WW4$status[i] == "Severely Disadvantaged") {
#     WW4$grassland[i]<-WW4$grassland[i] - (WW4$grassland[i]/2)
#   }
#   if (WW4$status[i] == "Disadvantaged") {
#     WW4$grassland[i]<-WW4$grassland[i] - (WW4$grassland[i]/2)
#   }
# }


# deer density
WW4$deer.density <- WW4$deer.density - (WW4$deer.density/100*20)

# normalise and write updaters
head(WW4)
# for cell updater specs
WW4$id<-NULL
WW4$Agent<-NULL
WW4$X <- NULL
WW4$phase3 <- NULL
WW4$status <- NULL

for (yr in c(2050,2055)){
  
  WW4$year <- yr
  dfWW <- rbind(dfWW, WW4)
  
}

# reduce deer density by 40% (another 20% on what it's been reduced already)
WW5 <- WW4

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*40)
#deer <- normalise(deer)

WW5$deer.density <- deer

for (yr in c(2060,2065)){
  
  WW5$year <- yr
  dfWW <- rbind(dfWW, WW5)
  
}

# introduce land reform 3, reduce deer density by 50%
WW6 <- WW

ggplot(WW6)+
  geom_tile(aes(x,y,fill=moreNW))

WW6$moreNW <- landreform3$moreNW
WW6$lessNW <- landreform3$lessNW
WW6$moreF <- landreform3$moreF
WW6$lessF <- landreform3$lessF
WW6$moreNAT <- landreform3$moreNAT
WW6$lessNAT <- landreform3$lessNAT

# reduce grassland capital in LFA areas
# WW6 <- merge(WW6, lfa, by = 'id', all.x = TRUE)
# 
# ggplot(WW6)+
#   geom_raster(mapping=aes(x=x,y=y,fill=status))
# ggplot(WW6)+
#   geom_raster(mapping=aes(x=x,y=y,fill=grassland))
# 
# nrows <- length(WW6[,1])
# 
# for (i in c(1:nrows)) {
#   if (WW6$status[i] == "Severely Disadvantaged") {
#     WW6$grassland[i]<-WW6$grassland[i] - (WW6$grassland[i]/2)
#   }
#   if (WW6$status[i] == "Disadvantaged") {
#     WW6$grassland[i]<-WW6$grassland[i] - (WW6$grassland[i]/2)
#   }
# }


# deer density
WW6$deer.density <- WW6$deer.density - (WW6$deer.density/100*50)

head(WW6)
# for cell updater specs
WW6$id<-NULL
WW6$Agent<-NULL
WW6$X <- NULL
WW6$phase3 <- NULL
WW6$status <- NULL

for (yr in c(2070,2075)){
  
  WW6$year <- yr
  dfWW <- rbind(dfWW, WW6)
  
}

WW7 <- WW6

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*80)
#deer <- normalise(deer)
WW7$deer.density <- deer

for (yr in c(2080,2085,2090,2095)){
  
  WW7$year <- yr
  dfWW <- rbind(dfWW, WW7)
  
}

unique(dfWW$year)
summary(dfWW)

# use agri-filter
dfWW$crop.productivity[which(dfWW$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfWW)

# normalise
dfWW <- data.frame(dfWW[,c(1:3,27:28)], lapply(dfWW[4:26], normalise))
summary(dfWW)
dfWW[is.na(dfWW)] <- 0

#remove filter column
dfWW$agri.filter <- NULL

# invert deer density
invert <- dfWW$deer.density - 1
z <- abs(invert)
dfWW$deer.density <- z

# write to files
yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  WW <- filter(dfWW, year == yr)
  
  if (yr == 2015){
    
    WW$FR <- AFT$AFT
    WW$FR <- str_replace_all(WW$FR, "[[.]]", "")
    WW$year <- NULL
    write.csv(WW, paste0(dirOut,"/worlds/Scotland_financial/Wild_Woodlands/Wild_Woodlands_capitals.csv"), row.names = F)
    
    
  }else{
    
    WW$year <- NULL
    write.csv(WW, paste0(dirOut,"/worlds/Scotland_financial/Wild_Woodlands/Wild_Woodlands_",yr,".csv"), row.names = F)
    
  }
}


# Native Networks ----

NN <- capitalsRAW

NN<-merge(NN,connect,by='id')
ggplot(NN)+
  geom_raster(mapping = aes(x=x, y=y, fill = connect))+
  scale_fill_viridis()+
  theme_bw()
ggplot(NN)+
  geom_raster(mapping = aes(x=x, y=y, fill = financial))

# increase financial capital in high native woodland capital and connectivity areas
summary(NN$n.conifer.yc)
NN$financial[which(NN$n.conifer.yc >= 6 & NN$connect >=0.5 )] <- NN$financial[which(NN$n.conifer.yc >= 6 & NN$connect >= 0.5 )] * 1.2
summary(NN$n.broad.yc)
NN$financial[which(NN$n.broad.yc >= 4 & NN$connect >= 0.5)] <- NN$financial[which(NN$n.broad.yc >= 4 & NN$connect >= 0.5)] * 1.2
summary(NN$n.broad.consv)
NN$financial[which(NN$n.broad.consv >= 3.5 & NN$connect >= 0.5)] <- NN$financial[which(NN$n.broad.consv >= 3.5 & NN$connect >= 0.5)] * 1.2
summary(NN$mixed.yc)
NN$financial[which(NN$mixed.yc >= 8 & NN$connect >= 0.5)] <- NN$financial[which(NN$mixed.yc >= 8 & NN$connect >= 0.5)] * 1.2

dfNN <- baseline
dfNN$year <- 2015
head(dfNN)

head(NN)
# for cell updater specs
NN$id<-NULL
NN$Agent<-NULL
NN$X <- NULL
NN$connect <- NULL

for (yr in yrList[-1]){
  
  NN$year <- yr
  dfNN <- rbind(dfNN,NN)
  
}

summary(dfNN);unique(dfNN$year)

dfNN$crop.productivity[which(dfNN$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfNN)
dfNN <- data.frame(dfNN[,c(1:3,27:28)], lapply(dfNN[4:26], normalise))
summary(dfNN)
dfNN[is.na(dfNN)] <- 0

#remove filter column
dfNN$agri.filter <- NULL

# invert deer density
invert <- dfNN$deer.density - 1
z <- abs(invert)
dfNN$deer.density <- z

yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  NN <- filter(dfNN, year == yr)
  
  if (yr == 2015){
    
    NN$FR <- AFT$AFT
    NN$FR <- str_replace_all(NN$FR, "[[.]]", "")
    NN$year <- NULL
    write.csv(NN, paste0(dirOut,"/worlds/Scotland_financial/Native_Networks/Native_Networks_capitals.csv"), row.names = F)
    
    
  }else{
    
    NN$year <- NULL
    write.csv(NN, paste0(dirOut,"/worlds/Scotland_financial/Native_Networks/Native_Networks_",yr,".csv"), row.names = F)
    
  }
  
}


# Green Gold ----


GG <- capitalsRAW

# increase financial capital where productive woodland capital are high in WEAG areas
GG<-merge(GG,weag,by='id')
ggplot(GG)+
  geom_tile(aes(x,y,fill=phase3))+
  scale_fill_viridis()+
  theme_bw()

ggplot(GG)+
  geom_tile(aes(x,y,fill=financial))+
  scale_fill_viridis()+
  theme_bw()

summary(GG$n.conifer.yc)
GG$financial[which(GG$n.conifer.yc >= 9 & GG$phase3 == 1)] <- GG$financial[which(GG$n.conifer.yc >= 9 & GG$phase3 == 1)] * 1.2
summary(GG$nn.conifer.yc)
GG$financial[which(GG$nn.conifer.yc >= 20 & GG$phase3 == 1)] <- GG$financial[which(GG$nn.conifer.yc >= 20 & GG$phase3 == 1)] * 1.2
summary(GG$n.broad.yc)
GG$financial[which(GG$n.broad.yc >= 4 & GG$phase3 == 1)] <- GG$financial[which(GG$n.broad.yc >= 4 & GG$phase3 == 1)] * 1.2
summary(GG$nn.broad.yc)
GG$financial[which(GG$nn.broad.yc >= 4 & GG$phase3 == 1)] <- GG$financial[which(GG$nn.broad.yc >= 4 & GG$phase3 == 1)] * 1.2

# reduce grassland capital in LFA by half
# GG <- merge(GG, lfa, by = 'id', all.x = TRUE)
# 
# ggplot(GG)+
#   geom_raster(mapping=aes(x=x,y=y,fill=status))
# ggplot(GG)+
#   geom_raster(mapping=aes(x=x,y=y,fill=grassland))
# 
# nrows <- length(GG[,1])
# 
# for (i in c(1:nrows)) {
#   if (GG$status[i] == "Severely Disadvantaged") {
#     GG$grassland[i]<-GG$grassland[i] - (GG$grassland[i]/2)
#   }
#   if (GG$status[i] == "Disadvantaged") {
#     GG$grassland[i]<-GG$grassland[i] - (GG$grassland[i]/2)
#   }
# }

dfGG <- baseline
dfGG$year <- 2015
head(dfGG)

head(GG)
# for cell updater specs
GG$id<-NULL
GG$Agent<-NULL
GG$X <- NULL
GG$phase3 <- NULL
GG$status <- NULL

for (yr in yrList[-1]){
  
  GG$year <- yr
  dfGG <- rbind(dfGG,GG)
  
}

summary(dfGG);unique(dfGG$year)

dfGG$crop.productivity[which(dfGG$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfGG)
dfGG <- data.frame(dfGG[,c(1:3,27:28)], lapply(dfGG[4:26], normalise))
summary(dfGG)
dfGG[is.na(dfGG)] <- 0

#remove filter column
dfGG$agri.filter <- NULL

# invert deer density
invert <- dfGG$deer.density - 1
z <- abs(invert)
dfGG$deer.density <- z

yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  GG <- filter(dfGG, year == yr)
  
  if (yr == 2015){
    
    GG$FR <- AFT$AFT
    GG$FR <- str_replace_all(GG$FR, "[[.]]", "")
    GG$year <- NULL
    write.csv(GG, paste0(dirOut,"/worlds/Scotland_financial/Green_Gold/Green_Gold_capitals.csv"), row.names = F)
    
    
  }else{
    
    GG$year <- NULL
    write.csv(GG, paste0(dirOut,"/worlds/Scotland_financial/Green_Gold/Green_Gold_",yr,".csv"), row.names = F)
    
  }
  
}


# Woodland Culture ----

WC <- capitalsRAW

# increase human capital - initial growth in knowledge and motivation
summary(WC$human) # take mean (if less than this)
nrows <- length(WC[,1])
for (i in c(1:nrows)) {
  if (WC$human[i] <= 0.63) {
    WC$human[i]<-WC$human[i] * 1.2
  }
}
# increase social capital - new networks and relationships for communities
summary(WC$social)
for (i in c(1:nrows)) {
  if (WC$social[i] <= 0.60) {
    WC$social[i]<-WC$social[i] * 1.2
  }
}
# even out finacial capital - productive power
summary(WC$financial)
for (i in c(1:nrows)) {
  if (WC$financial[i] <= 0.67) {
    WC$financial[i]<-WC$financial[i] * 1.2
  }}

# reduce grassland capital in LFA by 1/4
# WC <- merge(WC, lfa, by = 'id', all.x = TRUE)
# 
# ggplot(WC)+
#   geom_raster(mapping=aes(x=x,y=y,fill=status))
# ggplot(WC)+
#   geom_raster(mapping=aes(x=x,y=y,fill=grassland))
# 
WC1 <- WC
# 
# nrows <- length(WC1[,1])
# 
# for (i in c(1:nrows)) {
#   if (WC1$status[i] == "Severely Disadvantaged") {
#     WC1$grassland[i]<-WC1$grassland[i] - (WC1$grassland[i]/4)
#   }
#   if (WC1$status[i] == "Disadvantaged") {
#     WC1$grassland[i]<-WC1$grassland[i] - (WC1$grassland[i]/4)
#   }
# }

head(WC1)
# for cell updater specs
WC1$id<-NULL
WC1$Agent<-NULL
WC1$X <- NULL
WC1$status <- NULL

dfWC <- baseline
dfWC$year <- 2015
head(dfWC)

for (yr in c(2020,2025,2030)){
  
  WC1$year <- yr
  dfWC <- rbind(dfWC, WC1)
  
}

# first stage land reform
WC2 <- WC

ggplot(WC2)+
  geom_tile(aes(x,y,fill=moreNW))

WC2$moreNW <- landreform$moreNW
WC2$lessNW <- landreform$lessNW
WC2$moreF <- landreform$moreF
WC2$lessF <- landreform$lessF
WC2$moreNAT <- landreform$moreNAT
WC2$lessNAT <- landreform$lessNAT

# for (i in c(1:nrows)) {
#   if (WC2$status[i] == "Severely Disadvantaged") {
#     WC2$grassland[i]<-WC2$grassland[i] - (WC2$grassland[i]/4)
#   }
#   if (WC2$status[i] == "Disadvantaged") {
#     WC2$grassland[i]<-WC2$grassland[i] - (WC2$grassland[i]/4)
#   }
# }

head(WC2)

# for cell updater specs
WC2$id<-NULL
WC2$Agent<-NULL
WC2$X <- NULL
WC2$status <- NULL


for (yr in c(2035,2040)){
  
  WC2$year <- yr
  dfWC <- rbind(dfWC, WC2)
  
}

# reduce grassland capital by 1/2
WC3 <- WC
# 
# for (i in c(1:nrows)) {
#   if (WC3$status[i] == "Severely Disadvantaged") {
#     WC3$grassland[i]<-WC3$grassland[i] - (WC3$grassland[i]/2)
#   }
#   if (WC3$status[i] == "Disadvantaged") {
#     WC3$grassland[i]<-WC3$grassland[i] - (WC3$grassland[i]/2)
#   }
# }

WC3$moreNW <- landreform$moreNW
WC3$lessNW <- landreform$lessNW
WC3$moreF <- landreform$moreF
WC3$lessF <- landreform$lessF
WC3$moreNAT <- landreform$moreNAT
WC3$lessNAT <- landreform$lessNAT

head(WC3)
# for cell updater specs
WC3$id<-NULL
WC3$Agent<-NULL
WC3$X <- NULL
WC3$status <- NULL

for (yr in c(2045,2050)){
  
  WC3$year <- yr
  dfWC <- rbind(dfWC, WC3)
  
}

# second stage land reform

WC4 <- WC

WC4$moreNW <- landreform2$moreNW
WC4$lessNW <- landreform2$lessNW
WC4$moreF <- landreform2$moreF
WC4$lessF <- landreform2$lessF
WC4$moreNAT <- landreform2$moreNAT
WC4$lessNAT <- landreform2$lessNAT

# for (i in c(1:nrows)) {
#   if (WC4$status[i] == "Severely Disadvantaged") {
#     WC4$grassland[i]<-WC4$grassland[i] - (WC4$grassland[i]/2)
#   }
#   if (WC4$status[i] == "Disadvantaged") {
#     WC4$grassland[i]<-WC4$grassland[i] - (WC4$grassland[i]/2)
#   }
# }

head(WC4)
# for cell updater specs
WC4$id<-NULL
WC4$Agent<-NULL
WC4$X <- NULL
WC4$status <- NULL

for (yr in c(2055,2060)){
  
  WC4$year <- yr
  dfWC <- rbind(dfWC, WC4)
  
}


# reduce grassland capital by 3/4
WC5 <- WC

WC5$moreNW <- landreform2$moreNW
WC5$lessNW <- landreform2$lessNW
WC5$moreF <- landreform2$moreF
WC5$lessF <- landreform2$lessF
WC5$moreNAT <- landreform2$moreNAT
WC5$lessNAT <- landreform2$lessNAT

# for (i in c(1:nrows)) {
#   if (WC5$status[i] == "Severely Disadvantaged") {
#     WC5$grassland[i]<-WC5$grassland[i] - (WC5$grassland[i]/100*75)
#   }
#   if (WC5$status[i] == "Disadvantaged") {
#     WC5$grassland[i]<-WC5$grassland[i] - (WC5$grassland[i]/100*75)
#   }
# }

head(WC5)
# for cell updater specs
WC5$id<-NULL
WC5$Agent<-NULL
WC5$X <- NULL
WC5$status <- NULL

for (yr in c(2065,2070)){
  
  WC5$year <- yr
  dfWC <- rbind(dfWC, WC5)
  
}

# third stage land reform
WC6 <- WC

WC6$moreNW <- landreform3$moreNW
WC6$lessNW <- landreform3$lessNW
WC6$moreF <- landreform3$moreF
WC6$lessF <- landreform3$lessF
WC6$moreNAT <- landreform3$moreNAT
WC6$lessNAT <- landreform3$lessNAT

# for (i in c(1:nrows)) {
#   if (WC6$status[i] == "Severely Disadvantaged") {
#     WC6$grassland[i]<-WC6$grassland[i] - (WC6$grassland[i]/100*75)
#   }
#   if (WC6$status[i] == "Disadvantaged") {
#     WC6$grassland[i]<-WC6$grassland[i] - (WC6$grassland[i]/100*75)
#   }
# }

head(WC6)
# for cell updater specs
WC6$id<-NULL
WC6$Agent<-NULL
WC6$X <- NULL
WC6$status <- NULL

for (yr in c(2075,2080,2085,2090,2095)){
  
  WC6$year <- yr
  dfWC <- rbind(dfWC, WC6)
  
}

unique(dfWC$year)
summary(dfWC)

# use agri-filter
dfWC$crop.productivity[which(dfWC$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfWC)

# normalise
dfWC <- data.frame(dfWC[,c(1:3,27:28)], lapply(dfWC[4:26], normalise))
summary(dfWC)
dfWC[is.na(dfWC)] <- 0

#remove filter column
dfWC$agri.filter <- NULL

# invert deer density
invert <- dfWC$deer.density - 1
z <- abs(invert)
dfWC$deer.density <- z


# write to files
yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  WC <- filter(dfWC, year == yr)
  
  if (yr == 2015){
    
    WC$FR <- AFT$AFT
    WC$FR <- str_replace_all(WC$FR, "[[.]]", "")
    WC$year <- NULL
    write.csv(WC, paste0(dirOut,"/worlds/Scotland_financial/Woodland_Culture/Woodland_Culture_capitals.csv"), row.names = F)
    
    
  }else{
    
    WC$year <- NULL
    write.csv(WC, paste0(dirOut,"/worlds/Scotland_financial/Woodland_Culture/Woodland_Culture_",yr,".csv"), row.names = F)
    
  }
  
}



### SCOTLAND_NATURAL world - changes directly to natural capitals ---------------------------------

# Multiple Benefits ----

baseline <- capitalsRAW
MB <- capitalsRAW
summary(MB$mixed.yc)

ggplot(MB)+
  geom_tile(aes(x,y,fill=mixed.yc))
ggplot(MB)+
  geom_tile(aes(x,y,fill=agro.yc))
ggplot(MB)+
  geom_tile(aes(x,y,fill=grassland))

# increase actual natural capitals
summary(MB$mixed.yc)
MB$mixed.yc[which(MB$mixed.yc>0)] <- MB$mixed.yc[which(MB$mixed.yc>0)] * 1.2 # increase by 20% if above 0
# increase agroforestry capital
summary(MB$agro.yc)
MB$agro.yc[which(MB$agro.yc>0)] <- MB$agro.yc[which(MB$agro.yc>0)] * 1.2

head(MB)
# for cell updater specs
MB$FR<-NULL
MB$BT<-NULL
MB$X <- NULL
MB$id <- NULL
MB$Agent <- NULL
head(baseline)
baseline$FR<-NULL
baseline$BT<-NULL
baseline$X <- NULL
baseline$id <- NULL
baseline$Agent <- NULL

dfMB <- baseline
dfMB$year <- 2015
head(dfMB)

for (yr in yrList){
  
  MB$year <- yr
  dfMB <- rbind(dfMB,MB)
  
}


summary(dfMB)
#dfMB$crop.productivity[which(dfMB$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfMB)
dfMB <- data.frame(dfMB[,c(1:3,27)], lapply(dfMB[4:26], normalise))
summary(dfMB)
dfMB[is.na(dfMB)] <- 0

#remove filter column
#dfMB$agri.filter <- NULL

# invert deer density
invert <- dfMB$deer.density - 1
z <- abs(invert)
dfMB$deer.density <- z

yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  MB <- filter(dfMB, year == yr)
  
  if (yr == 2015){
    
    MB$FR <- AFT$AFT
    MB$FR <- str_replace_all(MB$FR, "[[.]]", "")
    MB$year <- NULL
    write.csv(MB, paste0(dirOut,"/worlds/Scotland_natural/Multiple_Benefits/Multiple_Benefits_capitals.csv"), row.names = F)
    
    
  }else{
    
    MB$year <- NULL
    write.csv(MB, paste0(dirOut,"/worlds/Scotland_natural/Multiple_Benefits/Multiple_Benefits_",yr,".csv"), row.names = F)
    
  }
  
}


# Wild Woodlands ----

WW <- capitalsRAW

ggplot(WW)+
  geom_tile(aes(x,y,fill=n.broad.consv))
ggplot(WW)+
  geom_tile(aes(x,y,fill=mixed.yc))
ggplot(WW)+
  geom_tile(aes(x,y,fill=n.conifer.yc))

# use WEAG to  target productive woodland capitals
WW<-merge(WW,weag,by='id')
ggplot(WW)+
  geom_tile(aes(x,y,fill=phase3))

# increase n.broad.consv in WEAG phase 3 areas by 50%
WW$n.broad.consv[which(WW$n.broad.consv > 0 & WW$phase3 > 0)] <- WW$n.broad.consv[which(WW$n.broad.consv > 0 & WW$phase3 > 0)] * 1.2
# increase mixed.yc in WEAG phase 3 areas by 50%
WW$mixed.yc[which(WW$mixed.yc > 0 & WW$phase3 > 0)] <- WW$mixed.yc[which(WW$mixed.yc > 0 & WW$phase3 > 0)] * 1.2
# increase n.conifer.yc in WEAG phase 3 areas by 50%
WW$n.conifer.yc[which(WW$n.conifer.yc > 0 & WW$phase3 > 0)] <- WW$n.conifer.yc[which(WW$n.conifer.yc > 0 & WW$phase3 > 0)] * 1.2

baseline$X <- NULL
baseline$id <- NULL
baseline$Agent <- NULL
dfWW <- baseline
dfWW$year <- 2015
head(dfWW)

WW1 <- WW
head(WW1)

# for cell updater specs
WW1$id<-NULL
WW1$X <- NULL
WW1$Agent <- NULL
WW1$phase3 <- NULL

WW1$year <- 2020

dfWW <- rbind(dfWW,WW1)

# introduce first stage of land reform and reduce grassland capital by a 1/4
WW2 <- WW
summary(WW2)

ggplot(WW2)+
  geom_tile(aes(x,y,fill=moreNW))

WW2$moreNW <- landreform$moreNW
WW2$lessNW <- landreform$lessNW
WW2$moreF <- landreform$moreF
WW2$lessF <- landreform$lessF
WW2$moreNAT <- landreform$moreNAT
WW2$lessNAT <- landreform$lessNAT

# reduce grassland capital in LFA areas
WW2 <- merge(WW2, lfa, by = 'id', all.x = TRUE)

ggplot(WW2)+
  geom_raster(mapping=aes(x=x,y=y,fill=status))
ggplot(WW2)+
  geom_raster(mapping=aes(x=x,y=y,fill=grassland))

nrows <- length(WW2[,1])

for (i in c(1:nrows)) {
  if (WW2$status[i] == "Severely Disadvantaged") {
    WW2$grassland[i]<-WW2$grassland[i] - (WW2$grassland[i]/4)
  }
  if (WW2$status[i] == "Disadvantaged") {
    WW2$grassland[i]<-WW2$grassland[i] - (WW2$grassland[i]/4)
  }
}

head(WW2)
# for cell updater specs
WW2$id<-NULL
WW2$Agent<-NULL
WW2$X <- NULL
WW2$phase3 <- NULL
WW2$status <- NULL

head(dfWW)

for (yr in c(2025,2030,2035)){
  
  WW2$year <- yr
  dfWW <- rbind(dfWW, WW2)
  
}

# reduce deer density capital by 20%
WW3 <- WW2

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*20)
#deer <- normalise(deer)

head(WW3)
WW3$deer.density <- deer

for (yr in c(2040,2045)){
  
  WW3$year <- yr
  dfWW <- rbind(dfWW, WW3)
  
}

# introduce second stage of land reform and reduce grassland capital by half
WW4 <- WW

ggplot(WW4)+
  geom_tile(aes(x,y,fill=moreNW))

WW4$moreNW <- landreform2$moreNW
WW4$lessNW <- landreform2$lessNW
WW4$moreF <- landreform2$moreF
WW4$lessF <- landreform2$lessF
WW4$moreNAT <- landreform2$moreNAT
WW4$lessNAT <- landreform2$lessNAT

# reduce grassland capital in LFA areas
WW4 <- merge(WW4, lfa, by = 'id', all.x = TRUE)

ggplot(WW4)+
  geom_raster(mapping=aes(x=x,y=y,fill=status))
ggplot(WW4)+
  geom_raster(mapping=aes(x=x,y=y,fill=grassland))

nrows <- length(WW4[,1])

for (i in c(1:nrows)) {
  if (WW4$status[i] == "Severely Disadvantaged") {
    WW4$grassland[i]<-WW4$grassland[i] - (WW4$grassland[i]/2)
  }
  if (WW4$status[i] == "Disadvantaged") {
    WW4$grassland[i]<-WW4$grassland[i] - (WW4$grassland[i]/2)
  }
}


# deer density
WW4$deer.density <- WW4$deer.density - (WW4$deer.density/100*20)

# normalise and write updaters
head(WW4)
# for cell updater specs
WW4$id<-NULL
WW4$Agent<-NULL
WW4$X <- NULL
WW4$phase3 <- NULL
WW4$status <- NULL

for (yr in c(2050,2055)){
  
  WW4$year <- yr
  dfWW <- rbind(dfWW, WW4)
  
}

# reduce deer density by 40% (another 20% on what it's been reduced already)
WW5 <- WW4

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*40)
#deer <- normalise(deer)

WW5$deer.density <- deer

for (yr in c(2060,2065)){
  
  WW5$year <- yr
  dfWW <- rbind(dfWW, WW5)
  
}

# introduce land reform 3, reduce deer density by 50%
WW6 <- WW

ggplot(WW6)+
  geom_tile(aes(x,y,fill=moreNW))

WW6$moreNW <- landreform3$moreNW
WW6$lessNW <- landreform3$lessNW
WW6$moreF <- landreform3$moreF
WW6$lessF <- landreform3$lessF
WW6$moreNAT <- landreform3$moreNAT
WW6$lessNAT <- landreform3$lessNAT

# reduce grassland capital in LFA areas
WW6 <- merge(WW6, lfa, by = 'id', all.x = TRUE)

ggplot(WW6)+
  geom_raster(mapping=aes(x=x,y=y,fill=status))
ggplot(WW6)+
  geom_raster(mapping=aes(x=x,y=y,fill=grassland))

nrows <- length(WW6[,1])

for (i in c(1:nrows)) {
  if (WW6$status[i] == "Severely Disadvantaged") {
    WW6$grassland[i]<-WW6$grassland[i] - (WW6$grassland[i]/2)
  }
  if (WW6$status[i] == "Disadvantaged") {
    WW6$grassland[i]<-WW6$grassland[i] - (WW6$grassland[i]/2)
  }
}


# deer density
WW6$deer.density <- WW6$deer.density - (WW6$deer.density/100*50)

head(WW6)
# for cell updater specs
WW6$id<-NULL
WW6$Agent<-NULL
WW6$X <- NULL
WW6$phase3 <- NULL
WW6$status <- NULL

for (yr in c(2070,2075)){
  
  WW6$year <- yr
  dfWW <- rbind(dfWW, WW6)
  
}

WW7 <- WW6

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*80)
#deer <- normalise(deer)
WW7$deer.density <- deer

for (yr in c(2080,2085,2090,2095)){
  
  WW7$year <- yr
  dfWW <- rbind(dfWW, WW7)
  
}


unique(dfWW$year)
summary(dfWW)

# use agri-filter
#dfWW$crop.productivity[which(dfWW$agri.filter==1)]<-0 # remove cap where not suitable for crops
#head(dfWW)

# normalise
dfWW <- data.frame(dfWW[,c(1:3,27)], lapply(dfWW[4:26], normalise))
summary(dfWW)
dfWW[is.na(dfWW)] <- 0

#remove filter column
#dfWW$agri.filter <- NULL

# invert deer density
invert <- dfWW$deer.density - 1
z <- abs(invert)
dfWW$deer.density <- z


# write to files
yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  WW <- filter(dfWW, year == yr)
  
  if (yr == 2015){
    
    WW$FR <- AFT$AFT
    WW$FR <- str_replace_all(WW$FR, "[[.]]", "")
    WW$year <- NULL
    write.csv(WW, paste0(dirOut,"/worlds/Scotland_natural/Wild_Woodlands/Wild_Woodlands_capitals.csv"), row.names = F)
    
    
  }else{
    
    WW$year <- NULL
    write.csv(WW, paste0(dirOut,"/worlds/Scotland_natural/Wild_Woodlands/Wild_Woodlands_",yr,".csv"), row.names = F)
    
  }
  
}

# Native Networks  ----

NN <- capitalsRAW

NN<-merge(NN,connect,by='id')
ggplot(NN)+
  geom_raster(mapping = aes(x=x, y=y, fill = connect))+
  scale_fill_viridis()+
  theme_bw()
ggplot(NN)+
  geom_raster(mapping = aes(x=x, y=y, fill = n.conifer.yc))

# increase native woodland capitals in connectivity areas
NN$n.conifer.yc[which(NN$n.conifer.yc > 0 & NN$connect >=0.5 )] <- NN$n.conifer.yc[which(NN$n.conifer.yc > 0 & NN$connect >= 0.5 )] * 1.2

NN$n.broad.yc[which(NN$n.broad.yc > 0 & NN$connect >= 0.5)] <- NN$n.broad.yc[which(NN$n.broad.yc > 0 & NN$connect >= 0.5)] * 1.2

NN$n.broad.consv[which(NN$n.broad.consv > 0 & NN$connect >= 0.5)] <- NN$n.broad.consv[which(NN$n.broad.consv > 0 & NN$connect >= 0.5)] * 1.2

NN$mixed.yc[which(NN$mixed.yc > 0 & NN$connect >= 0.5)] <- NN$mixed.yc[which(NN$mixed.yc > 0 & NN$connect >= 0.5)] * 1.2

dfNN <- baseline
dfNN$year <- 2015
head(dfNN)

head(NN)
# for cell updater specs
NN$id<-NULL
NN$Agent<-NULL
NN$X <- NULL
NN$connect <- NULL

for (yr in yrList[-1]){
  
  NN$year <- yr
  dfNN <- rbind(dfNN,NN)
  
}

summary(dfNN);unique(dfNN$year)

#dfNN$crop.productivity[which(dfNN$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfNN)
dfNN <- data.frame(dfNN[,c(1:3,27)], lapply(dfNN[4:26], normalise))
summary(dfNN)
dfNN[is.na(dfNN)] <- 0

#remove filter column
#dfNN$agri.filter <- NULL

# invert deer density
invert <- dfNN$deer.density - 1
z <- abs(invert)
dfNN$deer.density <- z

yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  NN <- filter(dfNN, year == yr)
  
  if (yr == 2015){
    
    NN$FR <- AFT$AFT
    NN$FR <- str_replace_all(NN$FR, "[[.]]", "")
    NN$year <- NULL
    write.csv(NN, paste0(dirOut,"/worlds/Scotland_natural/Native_Networks/Native_Networks_capitals.csv"), row.names = F)
    
    
  }else{
    
    NN$year <- NULL
    write.csv(NN, paste0(dirOut,"/worlds/Scotland_natural/Native_Networks/Native_Networks_",yr,".csv"), row.names = F)
    
  }
  
}


# Green Gold ----

GG <- capitalsRAW

# increase productive woodland capitals in WEAG areas
GG<-merge(GG,weag,by='id')
ggplot(GG)+
  geom_tile(aes(x,y,fill=phase3))+
  scale_fill_viridis()+
  theme_bw()

ggplot(GG)+
  geom_tile(aes(x,y,fill=n.conifer.yc))+
  scale_fill_viridis()+
  theme_bw()

GG %>% 
  filter(n.conifer.yc>0 & phase3== 1) %>% 
  ggplot()+
  geom_tile(aes(x,y,fill=n.conifer.yc))+
  scale_fill_viridis()+
  theme_bw()

GG$n.conifer.yc[which(GG$n.conifer.yc > 0 & GG$phase3 == 1)] <- GG$n.conifer.yc[which(GG$n.conifer.yc > 0 & GG$phase3 == 1)] * 1.2
GG$nn.conifer.yc[which(GG$nn.conifer.yc > 0 & GG$phase3 == 1)] <- GG$nn.conifer.yc[which(GG$nn.conifer.yc > 0 & GG$phase3 == 1)] * 1.2
GG$n.broad.yc[which(GG$n.broad.yc > 0 & GG$phase3 == 1)] <- GG$n.broad.yc[which(GG$n.broad.yc > 0 & GG$phase3 == 1)] * 1.2
GG$nn.broad.yc[which(GG$nn.broad.yc > 0 & GG$phase3 == 1)] <- GG$nn.broad.yc[which(GG$nn.broad.yc > 0 & GG$phase3 == 1)] * 1.2

# reduce grassland capital in LFA by half
GG <- merge(GG, lfa, by = 'id', all.x = TRUE)

ggplot(GG)+
  geom_raster(mapping=aes(x=x,y=y,fill=status))
ggplot(GG)+
  geom_raster(mapping=aes(x=x,y=y,fill=grassland))

nrows <- length(GG[,1])

for (i in c(1:nrows)) {
  if (GG$status[i] == "Severely Disadvantaged") {
    GG$grassland[i]<-GG$grassland[i] - (GG$grassland[i]/2)
  }
  if (GG$status[i] == "Disadvantaged") {
    GG$grassland[i]<-GG$grassland[i] - (GG$grassland[i]/2)
  }
}

dfGG <- baseline
dfGG$year <- 2015
head(dfGG)

head(GG)
# for cell updater specs
GG$id<-NULL
GG$Agent<-NULL
GG$X <- NULL
GG$phase3 <- NULL
GG$status <- NULL

for (yr in yrList[-1]){
  
  GG$year <- yr
  dfGG <- rbind(dfGG,GG)
  
}

summary(dfGG);unique(dfGG$year)

#dfGG$crop.productivity[which(dfGG$agri.filter==1)]<-0 # remove cap where not suitable for crops
head(dfGG)
dfGG <- data.frame(dfGG[,c(1:3,27)], lapply(dfGG[4:26], normalise))
summary(dfGG)
dfGG[is.na(dfGG)] <- 0

#remove filter column
#dfGG$agri.filter <- NULL

# invert deer density
invert <- dfGG$deer.density - 1
z <- abs(invert)
dfGG$deer.density <- z

yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  GG <- filter(dfGG, year == yr)
  
  if (yr == 2015){
    
    GG$FR <- AFT$AFT
    GG$FR <- str_replace_all(GG$FR, "[[.]]", "")
    GG$year <- NULL
    write.csv(GG, paste0(dirOut,"/worlds/Scotland_natural/Green_Gold/Green_Gold_capitals.csv"), row.names = F)
    
    
  }else{
    
    GG$year <- NULL
    write.csv(GG, paste0(dirOut,"/worlds/Scotland_natural/Green_Gold/Green_Gold_",yr,".csv"), row.names = F)
    
  }
  
}

# Woodland Culture ----

WC <- capitalsRAW

# increase human capital - initial growth in knowledge and motivation
summary(WC$human) # take mean (if less than this)
nrows <- length(WC[,1])
for (i in c(1:nrows)) {
  if (WC$human[i] <= 0.63) {
    WC$human[i]<-WC$human[i] * 1.2
  }
}
# increase social capital - new networks and relationships for communities
summary(WC$social)
for (i in c(1:nrows)) {
  if (WC$social[i] <= 0.60) {
    WC$social[i]<-WC$social[i] * 1.2
  }
}
# even out finacial capital - productive power
summary(WC$financial)
for (i in c(1:nrows)) {
  if (WC$financial[i] <= 0.67) {
    WC$financial[i]<-WC$financial[i] * 1.2
  }}

# reduce grassland capital in LFA by 1/4
WC <- merge(WC, lfa, by = 'id', all.x = TRUE)

ggplot(WC)+
  geom_raster(mapping=aes(x=x,y=y,fill=status))
ggplot(WC)+
  geom_raster(mapping=aes(x=x,y=y,fill=grassland))

WC1 <- WC

nrows <- length(WC1[,1])

for (i in c(1:nrows)) {
  if (WC1$status[i] == "Severely Disadvantaged") {
    WC1$grassland[i]<-WC1$grassland[i] - (WC1$grassland[i]/4)
  }
  if (WC1$status[i] == "Disadvantaged") {
    WC1$grassland[i]<-WC1$grassland[i] - (WC1$grassland[i]/4)
  }
}

head(WC1)
# for cell updater specs
WC1$id<-NULL
WC1$Agent<-NULL
WC1$X <- NULL
WC1$status <- NULL

dfWC <- baseline
dfWC$year <- 2015
head(dfWC)

for (yr in c(2020,2025,2030)){
  
  WC1$year <- yr
  dfWC <- rbind(dfWC, WC1)
  
}


# first stage land reform
WC2 <- WC

ggplot(WC2)+
  geom_tile(aes(x,y,fill=moreNW))

WC2$moreNW <- landreform$moreNW
WC2$lessNW <- landreform$lessNW
WC2$moreF <- landreform$moreF
WC2$lessF <- landreform$lessF
WC2$moreNAT <- landreform$moreNAT
WC2$lessNAT <- landreform$lessNAT

for (i in c(1:nrows)) {
  if (WC2$status[i] == "Severely Disadvantaged") {
    WC2$grassland[i]<-WC2$grassland[i] - (WC2$grassland[i]/4)
  }
  if (WC2$status[i] == "Disadvantaged") {
    WC2$grassland[i]<-WC2$grassland[i] - (WC2$grassland[i]/4)
  }
}

head(WC2)

# for cell updater specs
WC2$id<-NULL
WC2$Agent<-NULL
WC2$X <- NULL
WC2$status <- NULL


for (yr in c(2035,2040)){
  
  WC2$year <- yr
  dfWC <- rbind(dfWC, WC2)
  
}


# reduce grassland capital by 1/2
WC3 <- WC

for (i in c(1:nrows)) {
  if (WC3$status[i] == "Severely Disadvantaged") {
    WC3$grassland[i]<-WC3$grassland[i] - (WC3$grassland[i]/2)
  }
  if (WC3$status[i] == "Disadvantaged") {
    WC3$grassland[i]<-WC3$grassland[i] - (WC3$grassland[i]/2)
  }
}

WC3$moreNW <- landreform$moreNW
WC3$lessNW <- landreform$lessNW
WC3$moreF <- landreform$moreF
WC3$lessF <- landreform$lessF
WC3$moreNAT <- landreform$moreNAT
WC3$lessNAT <- landreform$lessNAT

head(WC3)
# for cell updater specs
WC3$id<-NULL
WC3$Agent<-NULL
WC3$X <- NULL
WC3$status <- NULL

for (yr in c(2045,2050)){
  
  WC3$year <- yr
  dfWC <- rbind(dfWC, WC3)
  
}


# second stage land reform

WC4 <- WC

WC4$moreNW <- landreform2$moreNW
WC4$lessNW <- landreform2$lessNW
WC4$moreF <- landreform2$moreF
WC4$lessF <- landreform2$lessF
WC4$moreNAT <- landreform2$moreNAT
WC4$lessNAT <- landreform2$lessNAT

for (i in c(1:nrows)) {
  if (WC4$status[i] == "Severely Disadvantaged") {
    WC4$grassland[i]<-WC4$grassland[i] - (WC4$grassland[i]/2)
  }
  if (WC4$status[i] == "Disadvantaged") {
    WC4$grassland[i]<-WC4$grassland[i] - (WC4$grassland[i]/2)
  }
}

head(WC4)
# for cell updater specs
WC4$id<-NULL
WC4$Agent<-NULL
WC4$X <- NULL
WC4$status <- NULL

for (yr in c(2055,2060)){
  
  WC4$year <- yr
  dfWC <- rbind(dfWC, WC4)
  
}


# reduce grassland capital by 3/4
WC5 <- WC

WC5$moreNW <- landreform2$moreNW
WC5$lessNW <- landreform2$lessNW
WC5$moreF <- landreform2$moreF
WC5$lessF <- landreform2$lessF
WC5$moreNAT <- landreform2$moreNAT
WC5$lessNAT <- landreform2$lessNAT

for (i in c(1:nrows)) {
  if (WC5$status[i] == "Severely Disadvantaged") {
    WC5$grassland[i]<-WC5$grassland[i] - (WC5$grassland[i]*.75)
  }
  if (WC5$status[i] == "Disadvantaged") {
    WC5$grassland[i]<-WC5$grassland[i] - (WC5$grassland[i]*.75)
  }
}

head(WC5)
# for cell updater specs
WC5$id<-NULL
WC5$Agent<-NULL
WC5$X <- NULL
WC5$status <- NULL

for (yr in c(2065,2070)){
  
  WC5$year <- yr
  dfWC <- rbind(dfWC, WC5)
  
}

# third stage land reform
WC6 <- WC

WC6$moreNW <- landreform3$moreNW
WC6$lessNW <- landreform3$lessNW
WC6$moreF <- landreform3$moreF
WC6$lessF <- landreform3$lessF
WC6$moreNAT <- landreform3$moreNAT
WC6$lessNAT <- landreform3$lessNAT

for (i in c(1:nrows)) {
  if (WC6$status[i] == "Severely Disadvantaged") {
    WC6$grassland[i]<-WC6$grassland[i] - (WC6$grassland[i]*.75)
  }
  if (WC6$status[i] == "Disadvantaged") {
    WC6$grassland[i]<-WC6$grassland[i] - (WC6$grassland[i]*.75)
  }
}

head(WC6)
# for cell updater specs
WC6$id<-NULL
WC6$Agent<-NULL
WC6$X <- NULL
WC6$status <- NULL

for (yr in c(2075,2080,2085,2090,2095)){
  
  WC6$year <- yr
  dfWC <- rbind(dfWC, WC6)
  
}

unique(dfWC$year)
summary(dfWC)

# use agri-filter
#dfWC$crop.productivity[which(dfWC$agri.filter==1)]<-0 # remove cap where not suitable for crops
#head(dfWC)

# normalise
dfWC <- data.frame(dfWC[,c(1:3,27)], lapply(dfWC[4:26], normalise))
summary(dfWC)
dfWC[is.na(dfWC)] <- 0

#remove filter column
#dfWC$agri.filter <- NULL

# invert deer density
invert <- dfWC$deer.density - 1
z <- abs(invert)
dfWC$deer.density <- z


# write to files
yrList <- seq(2015,2095,by=5)

for (yr in yrList){
  
  #yr <- yrList[1]
  
  WC <- filter(dfWC, year == yr)
  
  if (yr == 2015){
    
    WC$FR <- AFT$AFT
    WC$FR <- str_replace_all(WC$FR, "[[.]]", "")
    WC$year <- NULL
    write.csv(WC, paste0(dirOut,"/worlds/Scotland_natural/Woodland_Culture/Woodland_Culture_capitals.csv"), row.names = F)
    
    
  }else{
    
    WC$year <- NULL
    write.csv(WC, paste0(dirOut,"/worlds/Scotland_natural/Woodland_Culture/Woodland_Culture_",yr,".csv"), row.names = F)
    
  }
  
}


### remove water/urban agents from the model -----------------------------------

lstVisions <- c("Baseline","Green_Gold","Multiple_Benefits","Native_Networks","Wild_Woodlands","Woodland_Culture")

#AFT <- AFT %>% filter(AFT != "water.urban")

world <- "natural"

for (vision in lstVisions){
  
  #vision <- "Baseline"
  
  dfCaps <- read.csv(paste0(dirOut,"/worlds/Scotland_",world,"/",vision,"/",vision,"_capitals.csv"))
  
  unique(dfCaps$FR)
  
  dfCaps <- dfCaps %>% filter(FR != "waterurban")
  
  write.csv(dfCaps, paste0(dirOut,"/worlds/Scotland_",world,"/",vision,"/",vision,"_capitals.csv"),row.names = F)
  
}

for (vision in lstVisions){
  
  vision <- "Wild_Woodlands"
  print(vision)
  
  for (yr in yrList[-1]){
    
    #yr <- 2020
    print(yr)
    
    dfCaps <- read.csv(paste0(dirOut,"/worlds/Scotland_",world,"/",vision,"/",vision,"_",yr,".csv"))
    
    # temp join AFT allocation
    dfCaps$FR <- AFT$AFT
    
    dfCaps <- dfCaps %>% filter(FR != "water.urban")
    
    dfCaps$FR <- NULL
    
    write.csv(dfCaps, paste0(dirOut,"/worlds/Scotland_",world,"/",vision,"/",vision,"_",yr,".csv"),row.names = F)
    
  }
  
  
}

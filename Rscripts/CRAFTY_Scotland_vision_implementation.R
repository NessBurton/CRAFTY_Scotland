
# date: 24/02/21
# author: VB
# descriptions: edit baseline capitals to represent desired changes in each vision

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")

### baseline capitals ----------------------------------------------------------

#baseline <- read.csv(paste0(dirData,"/templateBasic_csv/TestRegion_edit.csv"))
baseline <- read.csv((paste0(dirData,"/output/capitals_normalised_Feb21.csv")))
head(baseline)
summary(baseline)

baseline[is.na(baseline)] <- 0

baseline$id <- NULL
baseline$FR <- baseline$Agent
baseline$BT <- 0
baseline$Agent <- NULL
colnames(baseline)[1:2] <- c("x","y")
#baseline <- baseline[,-c(27:35)]

baseline$FR
baseline$FR <- str_replace_all(baseline$FR, "[[.]]", "")

write.csv(baseline, paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_capitals.csv"), row.names = F)
write.csv(baseline, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits/Multiple_Benefits_capitals.csv"), row.names = F)
write.csv(baseline, paste0(dirOut,"/worlds/Scotland/Green_Gold/Green_Gold_capitals.csv"), row.names = F)
write.csv(baseline, paste0(dirOut,"/worlds/Scotland/Native_Networks/Native_Networks_capitals.csv"), row.names = F)
write.csv(baseline, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_capitals.csv"), row.names = F)
write.csv(baseline, paste0(dirOut,"/worlds/Scotland/Woodland_Culture/Woodland_Culture_capitals.csv"), row.names = F)


# baseline updater files
baseline <- read.csv(paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_capitals.csv"))
head(baseline)

updater <- baseline[,-c(27:28)]

yrList <- seq(2021,2100,by=10)

for (yr in yrList){
  
  write.csv(updater,paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_",yr,".csv"),row.names = F)
  
}

### raw capitals & normalise function ------------------------------------------

capitalsRAW <- read.csv(paste0(dirData,"/output/capitals_raw_Feb21.csv"))

normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}


### multiple benefits ----------------------------------------------------------

MB <- capitalsRAW
summary(MB$mixed.yc)

ggplot(MB)+
  geom_tile(aes(x,y,fill=mixed.yc))
ggplot(MB)+
  geom_tile(aes(x,y,fill=agro.yc))


# V1 (thesis method)
# increase actual natural capitals
MB$mixed.yc <- MB$mixed.yc * 1.5
# increase agroforestry capital
MB$agro.yc <- MB$agro.yc * 1.5

head(MB)
# for cell updater specs
MB$FR<-NULL
MB$BT<-NULL
MB$X <- NULL

summary(MB)
MB <- data.frame(MB[,c(1:5,29)], lapply(MB[6:28], normalise))
summary(MB)
MB[is.na(MB)] <- 0
MB$crop.productivity[which(MB$agri.filter==1)]<-0 # remove cap where not suitable for crops
#remove filter column
MB$agri.filter <- NULL

# invert deer density
invert <- MB$deer.density - 1
z <- abs(invert)
MB$deer.density <- z

MB <- MB[-c(1,4)]

yrList <- seq(2021,2100,by=10)

for (yr in yrList){
  
  write.csv(MB, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits/Multiple_Benefits_",yr,".csv"), row.names = F)
  
}


# V2

MB <- capitalsRAW
ggplot(MB)+
  geom_tile(aes(x,y,fill=financial))

# increase financial capital where mixed yc capital >0.5
MB$financial[which(MB$mixed.yc>0.5)] <- MB$financial[which(MB$mixed.yc>0.5)]+0.2
# increase financial capital where agroforestry capital >0.5
MB$financial[which(MB$agro.yc>0.5)] <- MB$financial[which(MB$agro.yc>0.5)]+0.2

head(MB)
# for cell updater specs
MB$FR<-NULL
MB$BT<-NULL
MB$X <- NULL

summary(MB)
MB <- data.frame(MB[,c(1:5,29)], lapply(MB[6:28], normalise))
summary(MB)
MB[is.na(MB)] <- 0
MB$crop.productivity[which(MB$agri.filter==1)]<-0 # remove cap where not suitable for crops
#remove filter column
MB$agri.filter <- NULL

# invert deer density
invert <- MB$deer.density - 1
z <- abs(invert)
MB$deer.density <- z

MB <- MB[-c(1,4)]

yrList <- seq(2021,2100,by=10)

for (yr in yrList){
  
  write.csv(MB, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits_V2/Multiple_Benefits_",yr,".csv"), row.names = F)
  
}


### wild woodlands -------------------------------------------------------------

WW <- capitalsRAW

ggplot(WW)+
  geom_tile(aes(x,y,fill=n.broad.consv))
ggplot(WW)+
  geom_tile(aes(x,y,fill=mixed.yc))
ggplot(WW)+
  geom_tile(aes(x,y,fill=n.conifer.yc))

# read in WEAG data
# add WEAG to productive woodland capitals
weag <- read.csv(paste0(dirData,'/input/weag3.csv')) 
head(weag)
weag$RASTERVALU[which(weag$RASTERVALU == -9999)] <- 0
weag$Id <- NULL
weag$ORIG_FID <- NULL
colnames(weag) <- c('id', 'x', 'y', 'phase3')
ggplot(weag)+
  geom_raster(mapping = aes(x=x, y=y, fill = phase3))
weag<-weag[,c(1,4)]
WW<-merge(WW,weag,by='id')
ggplot(WW)+
  geom_tile(aes(x,y,fill=phase3))

# 2021
# increase n.broad.consv in WEAG phase 3 areas by 50%
WW$n.broad.consv[which(WW$n.broad.consv > 0 & WW$phase3 > 0)] <- WW$n.broad.consv[which(WW$n.broad.consv > 0 & WW$phase3 > 0)] * 1.5
# increase mixed.yc in WEAG phase 3 areas by 50%
WW$mixed.yc[which(WW$mixed.yc > 0 & WW$phase3 > 0)] <- WW$mixed.yc[which(WW$mixed.yc > 0 & WW$phase3 > 0)] * 1.5
# increase n.conifer.yc in WEAG phase 3 areas by 50%
WW$n.conifer.yc[which(WW$n.conifer.yc > 0 & WW$phase3 > 0)] <- WW$n.conifer.yc[which(WW$n.conifer.yc > 0 & WW$phase3 > 0)] * 1.5

# normalise and write updaters
WW1 <- WW
head(WW1)
# for cell updater specs
WW1$FR<-NULL
WW1$BT<-NULL
WW1$X <- NULL

summary(WW1)
WW1 <- data.frame(WW1[,c(1:5,29)], lapply(WW1[6:28], normalise))
summary(WW1)
WW1[is.na(WW1)] <- 0
WW1$crop.productivity[which(WW1$agri.filter==1)]<-0 # remove cap where not suitable for crops
# remove filter column
WW1$agri.filter <- NULL
# remove WEAG column
WW1$phase3 <- NULL
# invert deer density
invert <- WW1$deer.density - 1
z <- abs(invert)
WW1$deer.density <- z

WW1 <- WW1[-c(1,4)]

yrList <- seq(2021,2100,by=10)

write.csv(WW1, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2021.csv"), row.names = F)

# 2031
# introduce first stage of land reform and reduce grassland capital by a 1/4
WW2 <- WW
summary(WW2)

landreform <- read.csv(paste0(dirData,'/output/TestRegion_LR1.csv'))
head(landreform)
landreform$X.1<-NULL
landreform<-landreform[,c(1,19:24)]

ggplot(WW2)+
  geom_tile(aes(x,y,fill=moreNW))

WW2$moreNW <- landreform$moreNW
WW2$lessNW <- landreform$lessNW
WW2$moreF <- landreform$moreF
WW2$lessF <- landreform$lessF
WW2$moreNAT <- landreform$moreNAT
WW2$lessNAT <- landreform$lessNAT

# reduce grassland capital in LFA areas
lfa <- read.csv(paste0(dirData,'/input/LFA.csv'))
head(lfa)
lfa$Id<-NULL
lfa$X<-NULL
lfa$Y<-NULL
lfa$RASTERVALU<-NULL
lfa$FID <- NULL
colnames(lfa)<-c('id','status')
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

# increase n.broad.consv in WEAG phase 3 areas by 50%
WW2$n.broad.consv[which(WW2$n.broad.consv > 0 & WW2$phase3 > 0)] <- WW2$n.broad.consv[which(WW2$n.broad.consv > 0 & WW2$phase3 > 0)] * 1.5
# increase mixed.yc in WEAG phase 3 areas by 50%
WW2$mixed.yc[which(WW2$mixed.yc > 0 & WW2$phase3 > 0)] <- WW2$mixed.yc[which(WW2$mixed.yc > 0 & WW2$phase3 > 0)] * 1.5
# increase n.conifer.yc in WEAG phase 3 areas by 50%
WW2$n.conifer.yc[which(WW2$n.conifer.yc > 0 & WW2$phase3 > 0)] <- WW2$n.conifer.yc[which(WW2$n.conifer.yc > 0 & WW2$phase3 > 0)] * 1.5

# normalise and write updaters
head(WW2)
# for cell updater specs
WW2$FR<-NULL
WW2$BT<-NULL
WW2$X <- NULL

summary(WW2)
WW2 <- data.frame(WW2[,c(1:5,29)], lapply(WW2[6:28], normalise))
summary(WW2)
WW2[is.na(WW2)] <- 0
WW2$crop.productivity[which(WW2$agri.filter==1)]<-0 # remove cap where not suitable for crops

# remove filter column
WW2$agri.filter <- NULL

# invert deer density
invert <- WW2$deer.density - 1
z <- abs(invert)
WW2$deer.density <- z

WW2 <- WW2[-c(1,4)]

write.csv(WW2, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2031.csv"), row.names = F)

# 2041
# reduce deer density capital by 20%
WW3 <- WW2

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*20)
deer <- normalise(deer)

head(WW3)
WW3$deer.density <- deer

# invert deer density
invert <- WW3$deer.density - 1
z <- abs(invert)
WW3$deer.density <- z

write.csv(WW3, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2041.csv"), row.names = F)

# 2051
# introduce second stage of land reform and reduce grassland capital by half
WW4 <- WW

landreform2 <- read.csv(paste0(dirData,'/output/TestRegion_LR2.csv'))
head(landreform2)
landreform2$X.1 <- NULL
landreform2<-landreform2[,c(1,19:24)]

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

# increase n.broad.consv in WEAG phase 3 areas by 50%
WW4$n.broad.consv[which(WW4$n.broad.consv > 0 & WW4$phase3 > 0)] <- WW4$n.broad.consv[which(WW4$n.broad.consv > 0 & WW4$phase3 > 0)] * 1.5
# increase mixed.yc in WEAG phase 3 areas by 50%
WW4$mixed.yc[which(WW4$mixed.yc > 0 & WW4$phase3 > 0)] <- WW4$mixed.yc[which(WW4$mixed.yc > 0 & WW4$phase3 > 0)] * 1.5
# increase n.conifer.yc in WEAG phase 3 areas by 50%
WW4$n.conifer.yc[which(WW4$n.conifer.yc > 0 & WW4$phase3 > 0)] <- WW4$n.conifer.yc[which(WW4$n.conifer.yc > 0 & WW4$phase3 > 0)] * 1.5

# deer density
WW4$deer.density <- WW4$deer.density - (WW4$deer.density/100*20)

# normalise and write updaters
head(WW4)
# for cell updater specs
WW4$FR<-NULL
WW4$BT<-NULL
WW4$X <- NULL

summary(WW4)
WW4 <- data.frame(WW4[,c(1:5,29)], lapply(WW4[6:28], normalise))
summary(WW4)
WW4[is.na(WW4)] <- 0
WW4$crop.productivity[which(WW4$agri.filter==1)]<-0 # remove cap where not suitable for crops

# remove filter column
WW4$agri.filter <- NULL

# invert deer density
invert <- WW4$deer.density - 1
z <- abs(invert)
WW4$deer.density <- z

WW4 <- WW4[-c(1,4)]

write.csv(WW4, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2051.csv"), row.names = F)

# 2061
# reduce deer density by 40% (another 20% on what it's been reduced already)
WW5 <- WW4

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*40)
deer <- normalise(deer)

head(WW5)
WW5$deer.density <- deer

# invert deer density
invert <- WW5$deer.density - 1
z <- abs(invert)
WW5$deer.density <- z

write.csv(WW5, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2061.csv"), row.names = F)

# 2071
# introduce land reform 3, reduce deer density by 50%
WW6 <- WW

landreform3 <- read.csv(paste0(dirData,'/output/TestRegion_LR3.csv'))
head(landreform3)
landreform3$X.1 <- NULL
landreform3<-landreform3[,c(1,19:24)]

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

# increase n.broad.consv in WEAG phase 3 areas by 50%
WW6$n.broad.consv[which(WW6$n.broad.consv > 0 & WW6$phase3 > 0)] <- WW6$n.broad.consv[which(WW6$n.broad.consv > 0 & WW6$phase3 > 0)] * 1.5
# increase mixed.yc in WEAG phase 3 areas by 50%
WW6$mixed.yc[which(WW6$mixed.yc > 0 & WW6$phase3 > 0)] <- WW6$mixed.yc[which(WW6$mixed.yc > 0 & WW6$phase3 > 0)] * 1.5
# increase n.conifer.yc in WEAG phase 3 areas by 50%
WW6$n.conifer.yc[which(WW6$n.conifer.yc > 0 & WW6$phase3 > 0)] <- WW6$n.conifer.yc[which(WW6$n.conifer.yc > 0 & WW6$phase3 > 0)] * 1.5

# deer density
WW6$deer.density <- WW6$deer.density - (WW6$deer.density/100*50)

# normalise and write updaters
head(WW6)
# for cell updater specs
WW6$FR<-NULL
WW6$BT<-NULL
WW6$X <- NULL

summary(WW6)
WW6 <- data.frame(WW6[,c(1:5,29)], lapply(WW6[6:28], normalise))
summary(WW6)
WW6[is.na(WW6)] <- 0
WW6$crop.productivity[which(WW6$agri.filter==1)]<-0 # remove cap where not suitable for crops

# remove filter column
WW6$agri.filter <- NULL

# invert deer density
invert <- WW6$deer.density - 1
z <- abs(invert)
WW6$deer.density <- z

WW6 <- WW6[-c(1,4)]

write.csv(WW6, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2071.csv"), row.names = F)


# 2081
WW7 <- WW6

ggplot(WW)+
  geom_raster(mapping = aes(x=x, y=y, fill = deer.density))

deer <- WW$deer.density - (WW$deer.density/100*80)
deer <- normalise(deer)

head(WW5)
WW7$deer.density <- deer

# invert deer density
invert <- WW7$deer.density - 1
z <- abs(invert)
WW7$deer.density <- z

write.csv(WW7, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2081.csv"), row.names = F)
write.csv(WW7, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Wild_Woodlands_2091.csv"), row.names = F)


### native networks  -----------------------------------------------------------

NN <- capitalsRAW

connect <- read.csv(paste0(dirData,'/input/connectivity2.csv'))
head(connect)
connect$connectivity <- NA
connect$connectivity[which(connect$ZONE == 'Core Native Woodland')] <- 1
connect$connectivity[which(connect$ZONE == 'Primary Zone')] <- 0.75
connect$connectivity[which(connect$ZONE == 'Secondary Zone')] <- 0.5

# check
ggplot(connect)+
  geom_raster(mapping = aes(x=X, y=Y, fill = connectivity))

connect<-connect[,c(1,8)]
colnames(connect)<-c('id','connect')
connect[is.na(connect)]<-0

NN<-merge(NN,connect,by='id')
ggplot(NN)+
  geom_raster(mapping = aes(x=x, y=y, fill = connect))
ggplot(NN)+
  geom_raster(mapping = aes(x=x, y=y, fill = n.conifer.yc))

# increase native woodland capitals in connectivity areas
NN$n.conifer.yc[which(NN$n.conifer.yc > 0 & NN$connect >=0.5 )] <- NN$n.conifer.yc[which(NN$n.conifer.yc > 0 & NN$connect >= 0.5 )] * 1.5

NN$n.broad.yc[which(NN$n.broad.yc > 0 & NN$connect >= 0.5)] <- NN$n.broad.yc[which(NN$n.broad.yc > 0 & NN$connect >= 0.5)] * 1.5

NN$n.broad.consv[which(NN$n.broad.consv > 0 & NN$connect >= 0.5)] <- NN$n.broad.consv[which(NN$n.broad.consv > 0 & NN$connect >= 0.5)] * 1.5

NN$mixed.yc[which(NN$mixed.yc > 0 & NN$connect >= 0.5)] <- NN$mixed.yc[which(NN$mixed.yc > 0 & NN$connect >= 0.5)] * 1.5

head(NN)
# for cell updater specs
NN$FR<-NULL
NN$BT<-NULL
NN$X <- NULL

summary(NN)
NN <- data.frame(NN[,c(1:5,29)], lapply(NN[6:28], normalise))
summary(NN)
NN[is.na(NN)] <- 0
NN$crop.productivity[which(NN$agri.filter==1)]<-0 # remove cap where not suitable for crops
#remove filter column
NN$agri.filter <- NULL

# invert deer density
invert <- NN$deer.density - 1
z <- abs(invert)
NN$deer.density <- z

NN <- NN[-c(1,4)]

yrList <- seq(2021,2100,by=10)

for (yr in yrList){
  
  write.csv(NN, paste0(dirOut,"/worlds/Scotland/Native_Networks/Native_Networks_",yr,".csv"), row.names = F)
  
}


### Green Gold -----------------------------------------------------------------

GG <- capitalsRAW

# increase productive woodland capitals in WEAG areas
GG<-merge(GG,weag,by='id')
ggplot(GG)+
  geom_tile(aes(x,y,fill=phase3))

GG$n.conifer.yc[which(GG$n.conifer.yc > 0 & GG$phase3 > 0)] <- GG$n.conifer.yc[which(GG$n.conifer.yc > 0 & GG$phase3 > 0)] * 1.5
GG$nn.conifer.yc[which(GG$nn.conifer.yc > 0 & GG$phase3 > 0)] <- GG$nn.conifer.yc[which(GG$nn.conifer.yc > 0 & GG$phase3 > 0)] * 1.5
GG$n.broad.yc[which(GG$n.broad.yc > 0 & GG$phase3 > 0)] <- GG$n.broad.yc[which(GG$n.broad.yc > 0 & GG$phase3 > 0)] * 1.5
GG$nn.broad.yc[which(GG$nn.broad.yc > 0 & GG$phase3 > 0)] <- GG$nn.broad.yc[which(GG$nn.broad.yc > 0 & GG$phase3 > 0)] * 1.5

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

head(GG)
# for cell updater specs
GG$FR<-NULL
GG$BT<-NULL
GG$X <- NULL

summary(GG)
GG <- data.frame(GG[,c(1:5,29)], lapply(GG[6:28], normalise))
summary(GG)
GG[is.na(GG)] <- 0
GG$crop.productivity[which(GG$agri.filter==1)]<-0 # remove cap where not suitable for crops
#remove filter column
GG$agri.filter <- NULL

# invert deer density
invert <- GG$deer.density - 1
z <- abs(invert)
GG$deer.density <- z

GG <- GG[-c(1,4)]

yrList <- seq(2021,2100,by=10)

for (yr in yrList){
  
  write.csv(GG, paste0(dirOut,"/worlds/Scotland/Green_Gold/Green_Gold_",yr,".csv"), row.names = F)
  
}

### Woodland Culture -----------------------------------------------------------


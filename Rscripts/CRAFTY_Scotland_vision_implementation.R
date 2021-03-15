
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

yrList <- seq(2021,2100,by=1)

for (yr in yrList){
  
  write.csv(updater,paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_",yr,".csv"),row.names = F)
  
}

### multiple benefits ----------------------------------------------------------

capitalsRAW <- read.csv(paste0(dirData,"/output/capitals_raw_Feb21.csv"))

normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

MB <- capitalsRAW
summary(MB$mixed.yc)

ggplot(MB)+
  geom_tile(aes(x,y,fill=mixed.yc))
ggplot(MB)+
  geom_tile(aes(x,y,fill=agro.yc))


# V1 (thesis method)
# increase actual natural capitals
MB$mixed.yc <- MB$mixed.yc + MB$mixed.yc # doubled
# increase agroforestry capital
MB$agro.yc <- MB$agro.yc + MB$agro.yc # doubled

head(MB)
# for cell updater specs
MB$FR<-NULL
MB$BT<-NULL
MB$X <- NULL

summary(MB)
MB <- data.frame(MB[,c(1:5)], lapply(MB[6:28], normalise))
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
MB <- data.frame(MB[,c(1:5)], lapply(MB[6:28], normalise))
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



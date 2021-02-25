
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

baseline <- read.csv(paste0(dirData,"/templateBasic_csv/TestRegion_edit.csv"))
head(baseline)
summary(baseline)

baseline$id <- NULL
baseline$FR <- baseline$Agent
baseline$BT <- 0
baseline$Agent <- NULL
colnames(baseline)[1:2] <- c("x","y")
baseline <- baseline[,-c(27:35)]

baseline$FR
baseline$FR <- str_replace_all(baseline$FR, "[[.]]", "")

write.csv(baseline, paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_capitals.csv"), row.names = F)

# baseline updater files
baseline <- read.csv(paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_capitals.csv"))
head(baseline)

updater <- baseline[,-c(27:28)]

yrList <- seq(2021,2100,by=1)

for (yr in yrList){
  
  write.csv(updater,paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_",yr,".csv"),row.names = F)
  
}

### multiple benefits ----------------------------------------------------------

baseline <- read.csv(paste0(dirOut,"/worlds/Scotland/Baseline/Baseline_capitals.csv"))

MB <- baseline
summary(MB$mixed.yc)

ggplot(MB)+
  geom_tile(aes(x,y,fill=agro.yc))
ggplot(MB)+
  geom_tile(aes(x,y,fill=financial))

# V1 (reproduce thesis results)
# increase actual natural capitals
MB$mixed.yc <- MB$mixed.yc + MB$mixed.yc # doubled
MB$mixed.yc[which(MB$mixed.yc>1)]<-1
# increase agroforestry capital
MB$agro.yc <- MB$agro.yc + MB$agro.yc # doubled
MB$agro.yc[which(MB$agro.yc>1)]<-1

head(MB)
# for cell updater specs
MB$FR<-NULL
MB$BT<-NULL

yrList <- seq(2021,2100,by=1)

for (yr in yrList){
  
  write.csv(MB, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits/Multiple_Benefits_",yr,".csv"), row.names = F)
  
}


# V2
# increase financial capital where mixed yc capital >0.5
MB$financial[which(MB$mixed.yc>0.5)] <- MB$financial[which(MB$mixed.yc>0.5)]+0.2
# increase financial capital where agroforestry capital >0.5
MB$financial[which(MB$agro.yc>0.5)] <- MB$financial[which(MB$agro.yc>0.5)]+0.2
MB$financial[which(MB$financial>1)] <- 1

head(MB)
# for cell updater specs
MB$FR<-NULL
MB$BT<-NULL

yrList <- seq(2021,2100,by=1)

for (yr in yrList){
  
  write.csv(MB, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits/Multiple_Benefits_",yr,".csv"), row.names = F)
  
}



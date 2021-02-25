
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

### multiple benefits ----------------------------------------------------------

MB <- baseline
summary(MB$mixed.yc)

ggplot(MB)+
  geom_tile(aes(X,Y,fill=agro.yc))
ggplot(MB)+
  geom_tile(aes(X,Y,fill=financial))

# increase financial capital where mixed yc capital >0.5
MB$financial[which(MB$mixed.yc>0.5)] <- MB$financial[which(MB$mixed.yc>0.5)]+0.2
# increase financial capital where agroforestry capital >0.5
MB$financial[which(MB$agro.yc>0.5)] <- MB$financial[which(MB$agro.yc>0.5)]+0.2
MB$financial[which(MB$financial>1)] <- 1

head(MB)
# for cell updater specs
MB$id<-NULL
MB$Agent<-NULL
colnames(MB)[1]<-'x'
colnames(MB)[2]<-'y'
MB<-MB[,-c(27:35)] # remove services

write.csv(MB, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits/2021.csv"), row.names = F)

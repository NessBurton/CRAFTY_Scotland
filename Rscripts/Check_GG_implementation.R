
# date: 26/05/21
# author: VB
# descriptions: find out why GG woodland cover diving

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(vroom)
library(viridis)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_Scotland")
dataDisk <- "D:/CRAFTY_Scotland"
GGpath <- paste0(dataDisk,"/output/V1/Green_Gold")


### data -----------------------------------------------------------------------

lstFiles <- list.files(GGpath, full.names = T)
lstFiles <- grep("Cell", lstFiles, value = T)

dfGG <- vroom(lstFiles, id="path")
head(dfGG)

dfGG %>%  
  filter(Tick<2027) %>% 
  ggplot()+
  geom_tile(aes(x=X,y=Y,fill=`Capital:grassland`))+
  scale_fill_viridis()+
  facet_wrap(~Tick)+
  theme_bw()

dfGG %>%  
  filter(Tick<2027) %>% 
  ggplot()+
  geom_tile(aes(x=X,y=Y,fill=`Capital:n.conifer.yc`))+
  scale_fill_viridis()+
  facet_wrap(~Tick)+
  theme_bw()

summary(dfGG$`Capital:n.conifer.yc`[which(dfGG$Tick==2015)])
summary(dfGG$`Capital:n.conifer.yc`[which(dfGG$Tick==2016)])

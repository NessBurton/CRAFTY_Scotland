
library(tidyverse)
library(ggplot2)

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")

# proportions of each class per 1km
lca.props <- read.csv(paste0(dirData,'/intermediate/lca_props.csv'))

# detail on LCA here: # https://www.hutton.ac.uk/sites/default/files/files/soils/lca_map_hutton.pdf
# assign crop and grassland capitals from LCA data

lca.props$X<-NULL
nrows<-length(lca.props[,1])
crop.capital<-rep(NA,nrows)
grass.capital<-rep(NA,nrows)

for (i in c(1:nrows)) { 
  if (lca.props$LCA1[i]>=0.5) { # class 1 land capable of providing a very wide range of crops
    crop.capital[i] <- 9
    grass.capital[i] <- 1} 
  if (lca.props$LCA2[i]>=0.5) { # class 2 capable of producing a wide range of crops, minor physical limitations
    crop.capital[i] <- 8
    grass.capital[i] <- 2}
  if (lca.props$LCA31[i]>=0.5) { # class 3, range of crops with high yields
    crop.capital[i] <-7 
    grass.capital[i] <- 3}
  if (lca.props$LCA32[i]>=0.5) { # class 3.2 moderate range of crops, increasing trend towards grass
    crop.capital[i] <- 6
    grass.capital[i] <- 5}
  if (lca.props$LCA41[i]>=0.5) { # class 4.1 narrow range of crops, enterprises are primarily grass
    crop.capital[i] <- 5
    grass.capital[i] <- 8}
  if (lca.props$LCA42[i]>=0.5) { # class 4.2 primarily suited to grassland with limited potential for crops
    crop.capital[i] <- 3
    grass.capital[i] <- 9}
  if (lca.props$LCA51[i]>=0.5) { # class 5.1 capable of use as improved grassland
    crop.capital[i] <- 1
    grass.capital[i] <- 9}
  if (lca.props$LCA52[i]>=0.5) { # class 5.2 capable of use as improved grassland, some physical limitations
    crop.capital[i] <- 1
    grass.capital[i] <- 8}
  if (lca.props$LCA53[i]>=0.5) { # class 5.3 capable of use as improved grassland, deterioration can be rapid
    crop.capital[i] <- 1
    grass.capital[i] <- 7}
  if (lca.props$LCA61[i]>=0.5) { # rough grazing, high value grazing
    crop.capital[i] <- 0.5
    grass.capital[i] <- 7}
  if (lca.props$LCA62[i]>=0.5) { # rough grazing, moderate value
    crop.capital[i]<-0.5
    grass.capital[i] <- 5}
  if (lca.props$LCA63[i]>=0.5) { # rough grazing, low value
    crop.capital[i] <- 0.5
    grass.capital[i] <- 4}
  if (lca.props$LCA7[i]>=0.5) { # limited agricultural value,  very poor rough grazing
    crop.capital[i] <- 0
    grass.capital[i] <- 1}
  if (lca.props$LCA888[i]>=0.5) {
    crop.capital[i] <- 0
    grass.capital[i] <- 0}
  if (lca.props$LCA9500[i]>=0.5) {
    crop.capital[i] <- 0
    grass.capital[i] <- 0}
  if (lca.props$LCA999[i]>=0.5) {
    crop.capital[i] <- 0
    grass.capital[i] <- 0}
  }

lca.props$crop.capital <- crop.capital
lca.props$grass.capital <- grass.capital
lca.props[is.na(lca.props)] <- 0
colnames(lca.props)<-c("id",'lca1','lca2', 'lca31', 'lca32', 
                       'lca41', 'lca42', 'lca51', 'lcs52', 'lca53',
                       'lca61', 'lca62', 'lca63', 'lca7', 'lca8', 'lca95', 'lca99',
                       'area', 'crop.capital','grass.capital')

coords <- read.csv(paste0(dirData,'/output/lcm_iap_coords.csv'))
dfAgriCaps <- merge.data.frame(coords, lca.props, by = "id", all.x=TRUE)

# check to see it looks right
ggplot(dfAgriCaps, aes(x = X, y = Y, fill = crop.capital)) +
  geom_raster()
ggplot(dfAgriCaps, aes(x = X, y = Y, fill = grass.capital)) +
  geom_raster()

write.csv(dfAgriCaps, paste0(dirData,'/output/agri_capitals.csv'),row.names = F)

# write.csv(crop.cap, './outputs/lca_capital.csv')
# write.csv(grassland.cap, './outputs/lca_grassland_capital.csv')
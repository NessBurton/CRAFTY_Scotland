
# date: 24/02/21
# author: VB
# descriptions: check input data and create correct baseline capitals

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(viridis)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")

# input data -------------------------------------------------------------------

capitalsRaw <- read.csv(paste0(dirData,"/output/capitals_data.csv"))
summary(capitalsRaw)

afts<-read.csv(paste0(dirData,"/output/landcover_afts.csv"))
afts$X.2<-NULL
afts$X.1<-NULL
afts<-afts[,c(1,2,3,66)]

capitalsRaw<-merge(afts,capitalsRaw,by='id',all=TRUE)
colnames(capitalsRaw)[2:3]<-c('x',"y")
capitalsRaw$X.y<-NULL
capitalsRaw$X.1<-NULL

# process coords so CRAFTY can deal with them
xmin <- min(capitalsRaw$x)
ymin <- min(capitalsRaw$y)
capitalsRaw$x <- capitalsRaw$x - xmin
capitalsRaw$y <- capitalsRaw$y - ymin
capitalsRaw$x <- capitalsRaw$x / 1000
capitalsRaw$y <- capitalsRaw$y / 1000
colnames(capitalsRaw)[4]<-'Agent'

# edit crop capital
# lca.props <- read.csv(paste0(dirData,"/intermediate/lca_props.csv"))
# lca.props$X<-NULL
# nrows<-length(lca.props[,1])
# agri.capital<-rep(NA,nrows)
# for (i in c(1:nrows)) { 
#   if (lca.props$LCA1[i]>=0.7) {
#     agri.capital[i]<-9} 
#   if (lca.props$LCA2[i]>=0.7) {
#     agri.capital[i]<-8}
#   if (lca.props$LCA31[i]>=0.7) {
#     agri.capital[i]<-7}
#   if (lca.props$LCA32[i]>=0.7) {
#     agri.capital[i]<-7}
#   if (lca.props$LCA41[i]>=0.7) {
#     agri.capital[i]<-6}
#   if (lca.props$LCA42[i]>=0.7) {
#     agri.capital[i]<-6}
#   if (lca.props$LCA51[i]>=0.7) {
#     agri.capital[i]<-5}
#   if (lca.props$LCA52[i]>=0.7) {
#     agri.capital[i]<-5}
#   if (lca.props$LCA53[i]>=0.7) {
#     agri.capital[i]<-5}
#   if (lca.props$LCA61[i]>=0.7) {
#     agri.capital[i]<-4}
#   if (lca.props$LCA62[i]>=0.7) {
#     agri.capital[i]<-4}
#   if (lca.props$LCA63[i]>=0.7) {
#     agri.capital[i]<-4}
#   if (lca.props$LCA7[i]>=0.7) {
#     agri.capital[i]<-3}
#   if (lca.props$LCA888[i]>=0.7) {
#     agri.capital[i]<-2}
#   if (lca.props$LCA9500[i]>=0.7) {
#     agri.capital[i]<-1}
#   if (lca.props$LCA999[i]>=0.7) {
#     agri.capital[i]<-1}
# }
# lca.props$agri.capital <- agri.capital
# lca.props[is.na(lca.props)] <- 0
# colnames(lca.props)<-c("id",'lca1','lca2', 'lca31', 'lca32', 
#                        'lca41', 'lca42', 'lca51', 'lcs52', 'lca53',
#                        'lca61', 'lca62', 'lca63', 'lca7', 'lca8', 'lca95', 'lca99',
#                        'area', 'agri.capital')
# coords <- read.csv(paste0(dirData,'/output/lcm_iap_coords.csv'))
# crops <- merge.data.frame(coords, lca.props, by = "id", all.x=TRUE)
# crops$agri.capital<-as.numeric(crops$agri.capital)
# # filter so that it only includes classes which support crops
# # these are (according to LCA documentation) - 1,2,3.1,3.2,4.1,4.2 (6-9 in my reclass)
# # https://www.hutton.ac.uk/sites/default/files/files/soils/lca_map_hutton.pdf
# crops$agri.filter <- NA
# crops$agri.filter[which(crops$agri.capital<6)]<-1 # marker for classes unsuitable for crops
# capitalsRaw$crop.productivity <- crops$agri.capital
# capitalsRaw$agri.filter <- crops$agri.filter

dfAgriCaps <- read.csv(paste0(dirData,'/output/agri_capitals.csv'))
capitalsRaw$crop.productivity <- dfAgriCaps$crop.capital
capitalsRaw$grassland <- dfAgriCaps$grass.capital

write.csv(capitalsRaw, paste0(dirData,"/output/capitals_raw_Aug21.csv"),row.names = F)

### normalise ------------------------------------------------------------------

normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

capitalsNRM <- capitalsRaw
summary(capitalsNRM)
capitalsNRM$X <- NULL

#capitalsNRM[,c(6:28)] <- normalise(capitalsNRM[,c(6:28)])
capitalsNRM <- data.frame(capitalsNRM[,c(1:5)], lapply(capitalsNRM[6:28], normalise))
summary(capitalsNRM)
capitalsNRM[is.na(capitalsNRM)] <- 0
#capitalsNRM$crop.productivity[which(capitalsNRM$agri.filter==1)]<-0 # remove cap where not suitable for crops
#remove filter column
#capitalsNRM$agri.filter <- NULL

# invert deer density
invert <- capitalsNRM$deer.density - 1
z <- abs(invert)
capitalsNRM$deer.density <- z

summary(capitalsNRM)
capitalsNRM_long <- pivot_longer(capitalsNRM,
                                cols = nn.conifer.yc:wild.land,
                                names_to = "capital",
                                values_to = "value")
ggplot(capitalsNRM_long)+
  geom_tile(aes(x,y,fill=value))+
  facet_wrap(~capital)+
  scale_fill_viridis()+
  theme_bw()

write.csv(capitalsNRM, paste0(dirData,"/output/capitals_normalised_Aug21.csv"),row.names = F)

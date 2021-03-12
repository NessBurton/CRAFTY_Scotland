
# date: 28/02/21
# author: VB
# descriptions: check input data and create correct baseline services

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(viridis)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")

# input data -------------------------------------------------------------------

servicesRaw <- read.csv(paste0(dirData,"/output/service_data.csv"))
summary(servicesRaw)
servicesRaw$X <- NULL

# softwood & hardwood timber from FR growth model look-up table ("/inputs/lookupTable2.csv")
# biodiversity, recreation & carbon (soil) from JHI, already normalised 0-1
# flood reg from look-up ("/inputs/waterServices_lookup.csv")
# crops & livestock from IAP
# employment from look-up ("/inputs/employment_lookup.csv")

# check livestock data ---------------------------------------------------------

#crops_livestock<-read.csv(paste0(dirData,'/output/crops_livestock_data.csv'))
#summary(crops_livestock)
# looks odd
jhi <- read.csv(paste0(dirData,'/output/jhi_es.csv'))
head(jhi[,3:4])
summary(jhi[,3:4])

# calculate total of cattle and sheep density - for meat service
jhi$livestock.total<-0
for (i in c(1:length(jhi$livestock.total))){
  jhi$livestock.total[i]<-sum(c(jhi[i,3],
                                jhi[i,4]),na.rm = TRUE)
}

summary(jhi$livestock.total)

# woodland carbon --------------------------------------------------------------

# use average carbon for NFE (FR data) to calculate average C value for woodland agents
# then add this value to JHI soil carbon data
# (make sure in same units)
# yes both in tonnes of carbon 
# (JHI already normalised to 0-1, so normalise woodland carbon before adding together) - then re-normalise so all 0-1

wood.carbon <- read.csv(paste0(dirData, '/input/C_raster_1k_to_points.csv'))
wood.carbon <- data.frame(wood.carbon$id, wood.carbon$X, wood.carbon$Y,
                          wood.carbon$AFT, wood.carbon$RASTERVALU)
colnames(wood.carbon) <- c('id', 'x', 'y',
                           'AFT', 'carbon')
# make -9999 NA
wood.carbon$carbon[wood.carbon$carbon == -9999] <- NA

# summarise mean carbon per woodland agent
by_AFT <- group_by(wood.carbon, AFT)
wood.carbon.by.AFT <- summarise(by_AFT,
                                avg.carbon = mean(carbon, na.rm = TRUE))
wood.carbon.by.AFT <- arrange(wood.carbon.by.AFT, desc(avg.carbon))

# now assign average carbon to other AFTs
wood.carbon$carbon2 <- NA
wood.carbon$carbon2[which(wood.carbon$AFT=='prod.nn.broad')] <- 6300
wood.carbon$carbon2[which(wood.carbon$AFT=='multi.nc')] <- 6200
wood.carbon$carbon2[which(wood.carbon$AFT=='prod.n.conifer')] <- 5600
wood.carbon$carbon2[which(wood.carbon$AFT=='prod.nn.conifer')] <- 5600
wood.carbon$carbon2[which(wood.carbon$AFT=='consv.native')] <- 5300
wood.carbon$carbon2[which(wood.carbon$AFT=='prod.n.broad')] <- 4500
wood.carbon$carbon2[which(wood.carbon$AFT=='multi.mixed')] <- 4200
wood.carbon$carbon2[which(wood.carbon$AFT=='agroforestry')] <- 3300
wood.carbon$carbon2[which(wood.carbon$AFT=='multi.nb')] <- 3200
wood.carbon$carbon2[which(wood.carbon$AFT=='estate.consv')] <- 2300
wood.carbon$carbon2[which(wood.carbon$AFT=='estate.multi')] <- 2000

summary(wood.carbon)

servicesRaw$wood.carbon <- wood.carbon$carbon2
servicesRaw$livestock <- jhi$livestock.total

write.csv(servicesRaw, paste0(dirData,"/output/services_raw_Feb21.csv"), row.names = F)


### normalise ------------------------------------------------------------------


normalise <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

servicesNRM <- servicesRaw
summary(servicesNRM)
servicesNRM[is.na(servicesNRM)] <- 0

servicesNRM <- data.frame(servicesNRM[,c("id","biodiversity","carbon","recreation")], 
                          lapply(servicesNRM[c("softwood.timber",
                                               "hardwood.timber",
                                               "flood.regulation",
                                               "livestock",
                                               "crop.service",
                                               "employment",
                                               "wood.carbon")], normalise))
summary(servicesNRM)

# add woodland carbon to soil carbon & re-normalise
summary(servicesNRM$carbon)
for (i in c(1:length(servicesNRM$carbon))){
  servicesNRM$carbon[i]<-sum(c(servicesNRM[i,"carbon"],
                                servicesNRM[i,"wood.carbon"]),na.rm = TRUE)
}
servicesNRM$carbon <- normalise(servicesNRM$carbon)
servicesNRM$wood.carbon <- NULL

summary(servicesNRM)

servicesNRM$x <- wood.carbon$x
servicesNRM$y <- wood.carbon$y

servicesNRM_long <- pivot_longer(servicesNRM,
                                 cols = biodiversity:employment,
                                 names_to = "service",
                                 values_to = "value")
ggplot(servicesNRM_long)+
  geom_tile(aes(x,y,fill=value))+
  facet_wrap(~service)+
  scale_fill_viridis()+
  theme_bw()

servicesNRM <- servicesNRM[,-c(11:12)]

write.csv(servicesNRM, paste0(dirData,"/output/services_normalised_Mar21.csv"),row.names = F)

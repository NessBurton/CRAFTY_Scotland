
# date: 28/02/21
# author: VB
# descriptions: check relationships & agent productivities

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(viridis)
#library(fitdistrplus)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
library(vroom)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")

# data -------------------------------------------------------------------------

capitals <- vroom(paste0(dirData,"/output/capitals_normalised_Feb21.csv"))
services <- vroom(paste0(dirData,"/output/services_normalised_Mar21.csv"))

head(capitals);head(services)
summary(capitals);summary(services)

capitals[is.na(capitals)] <- 0

ser.cap.data <- merge(capitals, services, by="id")

ser.cap.data %>% 
  pivot_longer(cols = biodiversity:employment,
               names_to = "service",
               values_to = "value") %>% 
  ggplot()+
  geom_histogram(aes(value))+
  facet_wrap(~service)

# productivities ---------------------------------------------------------------

# looks at the top 50 service producing agents and looks at the strength of the relationship
# calculate for all services and all agents (assuming the previous step shows that they produce that service)
# look at service distribution for each agent
# take the estimates of mean and sd (from summary(dist)) as parameters for production

#lstAFT <- unique(ser.cap.data$Agent)

# BIODIVERSITY
#prodBio <- list()

#for (i in lstAFT){
  
  #i <- lstAFT[11]
  
  #top <- sort(ser.cap.data$biodiversity[ser.cap.data$Agent==i],decreasing=T)[1:50]
  #top <- top[-which(is.na(top))]
  
  #if (sum(top,na.rm = T) > 0){
  #fit <- gamlss::fitDist(top100, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
  #mean.prod <- coef(fit)[[1]]
  #fit <- fitdistrplus::fitdist(top, "norm")
  #print(plot(fit))
  #mean.prod <- fit$estimate[[1]]
  #}else{
  #mean.prod <- 0
  #}
  
  #prodBio <- append(prodBio,mean.prod)
  
#}

#data.frame(lstAFT,as.numeric(prodBio))

# SOFTWOOD TIMBER
#prodST <- list()

#for (i in lstAFT){
  
  #i <- lstAFT[11]
  
  #top <- sort(ser.cap.data$softwood.timber[ser.cap.data$Agent==i],decreasing=T)[1:50]
  #dat1 <- ser.cap.data$softwood.timber[ser.cap.data$Agent==i]
  #hist(top)
  
  #if (sum(top,na.rm = T) > 0){
    #fit <- gamlss::fitDist(top100, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
    #mean.prod <- coef(fit)[[1]]
    #fit <- fitdistrplus::fitdist(top, "norm")
    #print(plot(fit))
    #mean.prod <- fit$estimate[[1]]
  #}else{
   # mean.prod <- mean(top,na.rm = T)
  #}
  
  #prodST <- append(prodST,mean.prod)
  
  #}

#as.numeric(prodST)

# list all agents
afts <- unique(ser.cap.data$Agent)

# create empty list
totalprod.prop<-rep(0,18)
avprod.prop<-rep(0,18)
top.prod <- rep(0,18)

# SOFTWOOD TIMBER (total timber service from softwood agents)
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$softwood.timber[ser.cap.data$softwood.timber > 0 &
                                                          ser.cap.data$Agent==AFTi])
  avprod.prop[i] <- mean(ser.cap.data$softwood.timber[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$softwood.timber[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
s.timber.prod.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(s.timber.prod.by.AFT)+
  geom_col(mapping = aes(afts,avprod.prop))+
  ggtitle('Average softwood timber production by AFT')+
  xlab("AFT") + ylab("softwood timber service")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# HARDWOOD TIMBER (total timber service from hardwood agents)
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$hardwood.timber[ser.cap.data$hardwood.timber > 0 & ser.cap.data$Agent==AFTi])
  avprod.prop[i] <- mean(ser.cap.data$hardwood.timber[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$hardwood.timber[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
h.timber.prod.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(h.timber.prod.by.AFT)+
  geom_col(mapping = aes(afts,top.prod))+
  ggtitle('Average hardwood timber production by AFT')+
  xlab("AFT") + ylab("hardwood timber service")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# BIODIVERSITY (JHI)
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$biodiversity[ser.cap.data$biodiversity > 0 & ser.cap.data$Agent==AFTi])
  avprod.prop[i] <- mean(ser.cap.data$biodiversity[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$biodiversity[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
bio.prod.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(bio.prod.by.AFT)+
  geom_col(mapping = aes(afts,top.prod))+
  ggtitle('Average biodiversity production by AFT')+
  xlab("AFT") + ylab("biodiversity service")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# CARBON (JHI soil.carbon and FR woodland carbon)
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$carbon[ser.cap.data$carbon > 0 & ser.cap.data$Agent==AFTi])
  avprod.prop[i] <- mean(ser.cap.data$carbon[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$carbon[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
carbon.prod.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(carbon.prod.by.AFT)+
  geom_col(mapping = aes(afts,top.prod))+
  ggtitle('Average carbon production by AFT')+
  xlab("AFT") + ylab("carbon service")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# RECREATION (JHI)
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$recreation[ser.cap.data$recreation > 0 & ser.cap.data$Agent==AFTi])
  avprod.prop[i] <- mean(ser.cap.data$recreation[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$recreation[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
recreation.prod.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(recreation.prod.by.AFT)+
  geom_col(mapping = aes(afts,top.prod))+
  ggtitle('Average recreation production by AFT')+
  xlab("AFT") + ylab("recreation service")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# CROPS (IAP average from crops - all except oilseed rape)
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$crop.service[ser.cap.data$crop.service > 0 & ser.cap.data$Agent==AFTi], na.rm = TRUE)
  avprod.prop[i] <- mean(ser.cap.data$crop.service[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$crop.service[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
crop.prod.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(crop.prod.by.AFT)+
  geom_col(mapping = aes(afts,top.prod))+
  ggtitle('Average crop production by AFT')+
  xlab("AFT") + ylab("crop service")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# LIVESTOCK (based on total of JHI cattle/sheep density - livestock total)
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$livestock[ser.cap.data$livestock > 0 & ser.cap.data$Agent==AFTi])
  avprod.prop[i] <- mean(ser.cap.data$livestock[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$livestock[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
livestock.prod.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(livestock.prod.by.AFT)+
  geom_col(mapping = aes(afts,avprod.prop))+
  ggtitle('Average livestock production by AFT')+
  xlab("AFT") + ylab("livestock service")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# FLOOD REGULATION 
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$flood.regulation[ser.cap.data$flood.regulation > 0 & ser.cap.data$Agent==AFTi],na.rm = TRUE)
  avprod.prop[i] <- mean(ser.cap.data$flood.regulation[ser.cap.data$Agent==AFTi],na.rm=TRUE)
  top.prod[i] <- mean(sort(ser.cap.data$flood.regulation[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
flood.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop,top.prod)
ggplot(flood.by.AFT)+
  geom_col(mapping = aes(afts,avprod.prop))+
  ggtitle('Average flood regulation by AFT')+
  xlab("AFT") + ylab("flood regulation")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))

# EMPLOYMENT 
for (i in c(1:18)) {
  AFTi <- afts[i]
  totalprod.prop[i] <- sum(ser.cap.data$employment[ser.cap.data$employment > 0 & ser.cap.data$Agent==AFTi])
  avprod.prop[i] <- mean(ser.cap.data$employment[ser.cap.data$Agent==AFTi])
  top.prod[i] <- mean(sort(ser.cap.data$employment[ser.cap.data$Agent==AFTi],decreasing=T)[1:50])
}
# put results in a data frame
employment.by.AFT <- data.frame(afts, totalprod.prop, avprod.prop, top.prod)
ggplot(employment.by.AFT)+
  geom_col(mapping = aes(afts,top.prod))+
  ggtitle('Average employment by AFT')+
  xlab("AFT") + ylab("employment indicator")+
  ylim(0,1)+
  theme(axis.text.x = element_text(color="black", size=7, angle=45),
        axis.text.y = element_text(color="black", size=7, angle=45))


### capital service relationships ----------------------------------------------

# NN. CONIFER CAPITAL (SS)

cap.nn.conifer <- data.frame(Service = c("softwood.timber",
                                         "biodiversity",
                                         "carbon",
                                         "flood.regulation",
                                         "recreation",
                                         "employment"),
                             Correlation = rep(NA, 6))

lstCol <- seq(2,19,by=1)

for (i in lstAFT){
  
  i <- lstAFT[1]
  
  colnames(cap.nn.conifer)[i+1]
  
  cap.nn.conifer$Correlation[1] <- cor.test(ser.cap.data$nn.conifer.yc[ser.cap.data$Agent==i],
           ser.cap.data$softwood.timber[ser.cap.data$Agent==i])$estimate
  cap.nn.conifer$Correlation[2] <- cor.test(ser.cap.data$nn.conifer.yc[ser.cap.data$Agent==i],
           ser.cap.data$biodiversity[ser.cap.data$Agent==i])$estimate
  cap.nn.conifer$Correlation[3] <- cor.test(ser.cap.data$nn.conifer.yc[ser.cap.data$Agent==i],
           ser.cap.data$carbon[ser.cap.data$Agent==i])$estimate
  cap.nn.conifer$Correlation[4] <- cor.test(ser.cap.data$nn.conifer.yc[ser.cap.data$Agent==i],
           ser.cap.data$flood.regulation[ser.cap.data$Agent==i])$estimate
  cap.nn.conifer$Correlation[5] <- cor.test(ser.cap.data$nn.conifer.yc[ser.cap.data$Agent==i],
           ser.cap.data$recreation[ser.cap.data$Agent==i])$estimate
  cap.nn.conifer$Correlation[6] <- cor.test(ser.cap.data$nn.conifer.yc[ser.cap.data$Agent==i],
           ser.cap.data$employment[ser.cap.data$Agent==i])$estimate
  
}



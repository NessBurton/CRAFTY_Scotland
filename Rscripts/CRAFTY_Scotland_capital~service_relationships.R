
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
services <- vroom(paste0(dirData,"/output/services_normalised_Feb21.csv"))

head(capitals);head(services)
summary(capitals);summary(services)

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

lstAFT <- unique(ser.cap.data$Agent)

# BIODIVERSITY
prodBio <- list()

for (i in lstAFT){
  
  #i <- lstAFT[11]
  
  top <- sort(ser.cap.data$biodiversity[ser.cap.data$Agent==i],decreasing=T)[1:50]
  top <- top[-which(is.na(top))]
  
  if (sum(top,na.rm = T) > 0){
  #fit <- gamlss::fitDist(top100, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
  #mean.prod <- coef(fit)[[1]]
  fit <- fitdistrplus::fitdist(top, "norm")
  #print(plot(fit))
  mean.prod <- fit$estimate[[1]]
  }else{
  mean.prod <- 0
  }
  
  prodBio <- append(prodBio,mean.prod)
  
}

data.frame(lstAFT,as.numeric(prodBio))

# SOFTWOOD TIMBER
prodST <- list()

for (i in lstAFT){
  
  #i <- lstAFT[11]
  
  top <- sort(ser.cap.data$softwood.timber[ser.cap.data$Agent==i],decreasing=T)[1:50]
  #dat1 <- ser.cap.data$softwood.timber[ser.cap.data$Agent==i]
  #hist(top)
  
  #if (sum(top,na.rm = T) > 0){
    #fit <- gamlss::fitDist(top100, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
    #mean.prod <- coef(fit)[[1]]
    #fit <- fitdistrplus::fitdist(top, "norm")
    #print(plot(fit))
    #mean.prod <- fit$estimate[[1]]
  #}else{
    mean.prod <- mean(top,na.rm = T)
  #}
  
  prodST <- append(prodST,mean.prod)
  
  }

as.numeric(prodST)


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



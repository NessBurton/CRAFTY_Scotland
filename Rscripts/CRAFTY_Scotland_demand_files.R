
# date: 12/03/21
# author: VB
# descriptions: get initial service supply from baseline, use to calculate demand files

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland/")

### inital demand (supply after 1 yr, baseline run)

demandInitial <- read.csv(paste0(wd,"output/V1/Baseline/Baseline-0-99-Scotland-AggregateServiceDemand.csv"))

### calc service curves (inital supply over 1000?)

servicesInitial <- demandInitial[1,c(1:9)]
serviceCurves <- 1/servicesInitial

st<-servicesInitial[[1]]
ht<-servicesInitial[[2]]
b<-servicesInitial[[3]]
c<-servicesInitial[[4]]
f<-servicesInitial[[5]]
r<-servicesInitial[[6]]
l<-servicesInitial[[7]]
cr<-servicesInitial[[8]]
e<-servicesInitial[[9]]

# now demand through time

Year <- c(2020:2100)
softwood.timber<-rep(st,length(Year))
hardwood.timber<-rep(ht,length(Year))
biodiversity<-rep(b,length(Year))
carbon<-rep(c,length(Year))
flood.regulation<-rep(f,length(Year))
recreation<-rep(r,length(Year))
livestock<-rep(l,length(Year))
crop.service<-rep(cr,length(Year))
employment<-rep(e,length(Year))
bdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(bdemand, paste0(dirOut,"worlds/Scotland/Baseline/Demand_Baseline.csv"))


# Green Gold demand
Year<-c(2020:2100)
softwood.timber<-seq(st,(st+st*5),length.out = 81) # no1. priority (increase 500%)
hardwood.timber<-seq(ht,(ht+ht*4),length.out = 81) # no2. priority (increase 400%)
biodiversity<-seq(b,(b+b*2),length.out = 81) # no4. priority (increase 200%)
carbon<-seq(c,(c+c*3),length.out = 81) #no3. priority (increase 300%)
flood.regulation<-seq(f,(f+f),length.out = 81) # joint no.5 priority (increase 100%)
recreation<-rep(r,length(Year))
livestock<-seq(l,(l/2),length.out = 81) # reduce by half to represent loss of support for marginal ag
crop.service<-rep(cr,length(Year))
employment<-seq(e,(e+e),length.out = 81) # joint no.5 priority (increase 100%)
ggdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(ggdemand, paste0(dirOut,"/worlds/Scotland/Green_Gold/Demand_Green_Gold.csv"))

# Multiple Benefits demand
Year<-c(2020:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<-seq(b,(b+b*3),length.out = 81) #no3
carbon<-rep(c,length(Year)) 
flood.regulation<-seq(f,(f+f*4),length.out = 81) #no2
recreation<-seq(r,(r+r),length.out = 81) # no5
livestock<-seq(l,(l+l*2),length.out = 81) #joint 4
crop.service<-seq(cr,(cr+cr*2),length.out = 81) # joint4
employment<-seq(e,(e+e*5),length.out = 81) #no1
mbdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(mbdemand, paste0(dirOut,"/worlds/Scotland/Multiple_Benefits/Demand_Multiple_Benefits.csv"))

# Native Networks demand
Year<-c(2020:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<-seq(b,(b+b*5),length.out = 81) #no1
carbon<-seq(c,(c+c*4),length.out = 81) #joint no2 (climate resilience)
flood.regulation<-seq(f,(f+f*4),length.out = 81) #joint no2 (climate resilience)
recreation<-seq(r,(r+r*2),length.out = 81) # no4
livestock<-seq(l,(l+l*3),length.out = 81) # joint no3
crop.service<-seq(cr,(cr+cr*3),length.out = 81) #joint no3
employment<-rep(e,length(Year)) 
nndemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(nndemand, paste0(dirOut,"/worlds/Scotland/Native_Networks/Demand_Native_Networks.csv"))

# Woodland Culture demand
Year<-c(2020:2100)
softwood.timber<-seq(st,(st+st*5),length.out = 81) #no1
hardwood.timber<-seq(ht,(ht+ht*5),length.out = 81) #no1
biodiversity<-seq(b,(b+b*4),length.out = 81)
carbon<-seq(c,(c+c*4),length.out = 81)
flood.regulation<-seq(f,(f+f*4),length.out = 81) 
recreation<-seq(r,(r+r*4),length.out = 81)
livestock<-seq(l,(l+l*4),length.out = 81)
crop.service<-seq(cr,(cr+cr*4),length.out = 81)
employment<-seq(e,(e+e*5),length.out = 81) #no1
wcdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(wcdemand, paste0(dirOut,"/worlds/Scotland/Woodland_Culture/Demand_Woodland_Culture.csv"))

# Wild Woodlands demand
Year<-c(2020:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<-seq(b,(b+b*5),length.out = 81) #no1
carbon<-seq(c,(c+c*3),length.out = 81) #no3. priority (increase 300%)
flood.regulation<-seq(f,(f+f*4),length.out = 81) #no2
recreation<-seq(r,(r+r*2),length.out = 81) # no4
livestock<-seq(l,(l/2),length.out = 81) # reduce by half to represent loss of support for marginal ag
crop.service<-rep(cr,length(Year))
employment<-seq(e,(e+e),length.out = 81) # joint no.5 priority (increase 100%)
wwdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(wwdemand, paste0(dirOut,"/worlds/Scotland/Wild_Woodlands/Demand_Wild_Woodlands.csv"))

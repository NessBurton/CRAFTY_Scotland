
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
dataDisk <- "D:/CRAFTY_Scotland/output/Scotland_natural/V2_June21/"

### inital demand (supply after 1 yr, baseline run)

demandInitial <- read.csv(paste0(dataDisk,"Baseline/Baseline-0-99-Scotland_natural-AggregateServiceDemand.csv"))

### calc service curves (inital supply over 1000?)

servicesInitial <- demandInitial[2,c(1:9)]
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

Year <- c(2015:2100)
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
write.csv(bdemand, paste0(dirOut,"worlds/Scotland_natural/Baseline/Demand_Baseline.csv"))

#1 increase 100%
#2 increase 90%
#3 increase 80%
#4 increase 70%
#5 increase 60%


# V2 increase in 2020 and keep at same level -----------------------------------

# Green Gold demand
Year<-c(2015:2100)
softwood.timber <- c(rep(st,5),rep(st*2,81)) #seq(st,(st*2),length.out = 86) # no1. priority (increase 100%)
hardwood.timber<- c(rep(ht,5),rep(ht*1.9,81)) #seq(ht,(ht*1.9),length.out = 86) # no2. priority (increase 90%)
biodiversity<- c(rep(b,5),rep(b*1.7,81)) #seq(b,(b*1.7),length.out = 86) # no4. priority (increase 70%)
carbon <- c(rep(c,5),rep(c*1.8,81)) #seq(c,(c*1.8),length.out = 86) #no3. priority (increase 80%)
flood.regulation <- c(rep(f,5),rep(f*1.6,81)) #seq(f,(f*1.6),length.out = 86) # joint no.5 priority (increase 60%)
recreation <- rep(r,length(Year))
livestock <- c(rep(l,5),rep(l/2,81)) #seq(l,(l/2),length.out = 86) # reduce by half to represent loss of support for marginal ag
crop.service <- rep(cr,length(Year))
employment <- c(rep(e,5),rep(e*1.6,81)) #seq(e,(e*1.6),length.out = 86) # joint no.5 priority (increase 60%)
ggdemand <- data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(ggdemand, paste0(dirOut,"/worlds/Scotland_natural/Green_Gold/Demand_Green_Gold.csv"),row.names = F)

# Multiple Benefits demand
Year<-c(2015:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<- c(rep(b,5),rep(b*1.8,81)) #seq(b,(b*1.8),length.out = 86) #no3
carbon<-rep(c,length(Year)) 
flood.regulation<- c(rep(f,5),rep(f*1.9,81)) #seq(f,(f*1.9),length.out = 86) #no2
recreation<- c(rep(r,5),rep(r*1.6,81)) #seq(r,(r*1.6),length.out = 86) # no5
livestock<- c(rep(l,5),rep(l*1.7,81)) #seq(l,(l*1.7),length.out = 86) #joint 4
crop.service<- c(rep(cr,5),rep(cr*1.7,81)) #seq(cr,(cr*1.7),length.out = 86) # joint4
employment<- c(rep(e,5),rep(e*2,81)) #seq(e,(e*2),length.out = 86) #no1
mbdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(mbdemand, paste0(dirOut,"/worlds/Scotland_natural/Multiple_Benefits/Demand_Multiple_Benefits.csv"),row.names = F)

# Native Networks demand
Year<-c(2015:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<- c(rep(b,5),rep(b*2,81)) #seq(b,(b*2),length.out = 86) #no1
carbon<- c(rep(c,5),rep(c*1.9,81)) #seq(c,(c*1.9),length.out = 86) #joint no2 (climate resilience)
flood.regulation<- c(rep(f,5),rep(f*1.9,81)) #seq(f,(f*1.9),length.out = 86) #joint no2 (climate resilience)
recreation<- c(rep(r,5),rep(r*1.7,81)) #seq(r,(r*1.7),length.out = 86) # no4
livestock<- c(rep(1,5),rep(l*1.8,81)) #seq(l,(l*1.8),length.out = 86) # joint no3
crop.service<- c(rep(cr,5),rep(cr*1.8,81)) #seq(cr,(cr*1.8),length.out = 86) #joint no3
employment<-rep(e,length(Year)) 
nndemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(nndemand, paste0(dirOut,"/worlds/Scotland_natural/Native_Networks/Demand_Native_Networks.csv"),row.names = F)

# Woodland Culture demand
Year<-c(2015:2100)
softwood.timber<- c(rep(st,5),rep(st*2,81)) #seq(st,(st*2),length.out = 86) #no1
hardwood.timber<- c(rep(ht,5),rep(ht*2,81)) #seq(ht,(st*2),length.out = 86) #no1 - increase to same as softwood
biodiversity<- c(rep(b,5),rep(b*1.5,81)) #seq(b,(b*1.5),length.out = 86) # all other services increase 50%
carbon<- c(rep(c,5),rep(c*1.5,81))  #seq(c,(c*1.5),length.out = 86)
flood.regulation<- c(rep(f,5),rep(f*1.5,81)) #seq(f,(c*1.5),length.out = 86) 
recreation<-c(rep(r,5),rep(r*1.5,81))  #seq(r,(r*1.5),length.out = 86)
livestock<- c(rep(l,5),rep(l*1.5,81)) #seq(l,(l*1.5),length.out = 86)
crop.service<- c(rep(cr,5),rep(cr*1.5,81)) #seq(cr,(cr*1.5),length.out = 86)
employment<- c(rep(e,5),rep(e*2,81))#seq(e,(e*2),length.out = 86) #no1
wcdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(wcdemand, paste0(dirOut,"/worlds/Scotland_natural/Woodland_Culture/Demand_Woodland_Culture.csv"),row.names = F)

# Wild Woodlands demand
Year<-c(2015:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<- c(rep(b,5),rep(b*2,81)) #seq(b,(b*2),length.out = 86) #no1
carbon<- c(rep(c,5),rep(c*1.8,81)) #seq(c,(c*1.8),length.out = 86) #no3. priority 
flood.regulation<- c(rep(f,5),rep(f*1.9,81)) #seq(f,(f*1.9),length.out = 86) #no2
recreation<- c(rep(r,5),rep(r*1.7,81)) #seq(r,(r*1.7),length.out = 86) # no4
livestock<- c(rep(l,5),rep(l/2,81)) #seq(l,(l/2),length.out = 86) # reduce by half to represent loss of support for marginal ag
crop.service<-rep(cr,length(Year))
employment<- c(rep(e,5),rep(e*1.6,81)) #seq(e,(e*1.6),length.out = 86) # joint no.5 priority 
wwdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(wwdemand, paste0(dirOut,"/worlds/Scotland_natural/Wild_Woodlands/Demand_Wild_Woodlands.csv"),row.names = F)


# V1 increase gradually through time -------------------------------------------

# Green Gold demand
Year<-c(2015:2100)
softwood.timber<-seq(st,(st*2),length.out = 86) # no1. priority (increase 100%)
hardwood.timber<-seq(ht,(ht*1.9),length.out = 86) # no2. priority (increase 90%)
biodiversity<-seq(b,(b*1.7),length.out = 86) # no4. priority (increase 70%)
carbon<-seq(c,(c*1.8),length.out = 86) #no3. priority (increase 80%)
flood.regulation<-seq(f,(f*1.6),length.out = 86) # joint no.5 priority (increase 60%)
recreation<-rep(r,length(Year))
livestock<-seq(l,(l/2),length.out = 86) # reduce by half to represent loss of support for marginal ag
crop.service<-rep(cr,length(Year))
employment<-seq(e,(e*1.6),length.out = 86) # joint no.5 priority (increase 60%)
ggdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(ggdemand, paste0(dirOut,"worlds/Scotland_V2/Green_Gold/Demand_Green_Gold.csv"),row.names = F)

# Multiple Benefits demand
Year<-c(2015:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<-seq(b,(b*1.8),length.out = 86) #no3
carbon<-rep(c,length(Year)) 
flood.regulation<-seq(f,(f*1.9),length.out = 86) #no2
recreation<-seq(r,(r*1.6),length.out = 86) # no5
livestock<-seq(l,(l*1.7),length.out = 86) #joint 4
crop.service<-seq(cr,(cr*1.7),length.out = 86) # joint4
employment<-seq(e,(e*2),length.out = 86) #no1
mbdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(mbdemand, paste0(dirOut,"/worlds/Scotland_V2/Multiple_Benefits/Demand_Multiple_Benefits.csv"),row.names = F)

# Native Networks demand
Year<-c(2015:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<-seq(b,(b*2),length.out = 86) #no1
carbon<-seq(c,(c*1.9),length.out = 86) #joint no2 (climate resilience)
flood.regulation<-seq(f,(f*1.9),length.out = 86) #joint no2 (climate resilience)
recreation<-seq(r,(r*1.7),length.out = 86) # no4
livestock<-seq(l,(l*1.8),length.out = 86) # joint no3
crop.service<-seq(cr,(cr*1.8),length.out = 86) #joint no3
employment<-rep(e,length(Year)) 
nndemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(nndemand, paste0(dirOut,"/worlds/Scotland_V2/Native_Networks/Demand_Native_Networks.csv"),row.names = F)

# Woodland Culture demand
Year<-c(2015:2100)
softwood.timber<-seq(st,(st*2),length.out = 86) #no1
hardwood.timber<-seq(ht,(st*2),length.out = 86) #no1 - increase to same as softwood
biodiversity<-seq(b,(b*1.5),length.out = 86) # all other services increase 50%
carbon<-seq(c,(c*1.5),length.out = 86)
flood.regulation<-seq(f,(c*1.5),length.out = 86) 
recreation<-seq(r,(r*1.5),length.out = 86)
livestock<-seq(l,(l*1.5),length.out = 86)
crop.service<-seq(cr,(cr*1.5),length.out = 86)
employment<-seq(e,(e*2),length.out = 86) #no1
wcdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(wcdemand, paste0(dirOut,"/worlds/Scotland_V2/Woodland_Culture/Demand_Woodland_Culture.csv"),row.names = F)

# Wild Woodlands demand
Year<-c(2015:2100)
softwood.timber<-rep(st,length(Year)) 
hardwood.timber<-rep(ht,length(Year)) 
biodiversity<-seq(b,(b*2),length.out = 86) #no1
carbon<-seq(c,(c*1.8),length.out = 86) #no3. priority 
flood.regulation<-seq(f,(f*1.9),length.out = 86) #no2
recreation<-seq(r,(r*1.7),length.out = 86) # no4
livestock<-seq(l,(l/2),length.out = 86) # reduce by half to represent loss of support for marginal ag
crop.service<-rep(cr,length(Year))
employment<-seq(e,(e*1.6),length.out = 86) # joint no.5 priority 
wwdemand<-data.frame(Year,softwood.timber,hardwood.timber,biodiversity,carbon,flood.regulation,recreation,livestock,crop.service,employment)
write.csv(wwdemand, paste0(dirOut,"/worlds/Scotland_V2/Wild_Woodlands/Demand_Wild_Woodlands.csv"),row.names = F)

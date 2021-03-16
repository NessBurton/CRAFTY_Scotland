
# date: 25/02/21
# author: VB
# descriptions: read in results and plot

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggplot2)
library(spatstat)
library(wesanderson)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")
dirResults <- paste0(wd,"output/V1/")
dirMetrics <- paste0(wd,"vision_metrics")

### palettes -------------------------------------------------------------------

aftColours <- c("prodnnconifer" = "#005A32",
                "prodnconifer" = "#005A32",
                "multinc" = "#005A32",
                "multinnc"= "#005A32",
                "multimixed" = "#238B45",
                "prodnn.broad" = "#41AB5D", 
                "prodnbroad" = "#41AB5D",
                "multinb" = "#41AB5D",
                "multinnb" = "#41AB5D",
                "consvnative" = "#1B9E77",
                "agroforestry" = "#D8B70A",
                "intarable" = "darkkhaki",#9C964A", 
                "extarable" = "khaki4", #C3B091", 
                "intpastoral" = "#A2A475", 
                "extpastoral" = "#A6DBA0", #"grey65", 
                "estatemulti" = "#C2A5CF", 
                "estatesport" = "#9970AB",
                "estateconsv" = "#762A83", 
                "marginal" = "lightgrey",
                "waterurban" = "white")

lu.colours<-c("mixed.estate" = "#C2A5CF",
              "traditional.sporting.management" = "#9970AB",
              "conservation.management" = "darkolivegreen",
              "extensive.agriculture" = "#A6DBA0",
              "intensive.agriculture" = "darkkhaki", 
              "mixed.woodland" = "#238B45",
              "native.woodland" = "#41AB5D",
              "non.native.woodland" = "#005A32",
              "marginal" ="lightgrey")

### baseline -------------------------------------------------------------------

dfBaseline <-
  list.files(path = paste0(dirResults,"Baseline/"),
             pattern = "*.csv", 
             full.names = T) %>% 
  grep("-Cell-", value=TRUE, .) %>% 
  #map_df(~read_csv(., col_types = cols(.default = "c")))
  map_df(~read.csv(.))

head(dfBaseline)
summary(dfBaseline)
dfBaseline$Tick <- factor(dfBaseline$Tick)
dfBaseline$Agent <- factor(dfBaseline$Agent)

dfBaselineAFTcomp <- read.csv(paste0(dirResults,"Baseline/Baseline-0-99-Scotland-AggregateAFTCompetitiveness.csv"))
head(dfBaselineAFTcomp)

dfBaselineAFTs <- read.csv(paste0(dirResults,"Baseline/Baseline-0-99-Scotland-AggregateAFTComposition.csv"))

dfBaselineService <- read.csv(paste0(dirResults,"Baseline/Baseline-0-99-Scotland-AggregateServiceDemand.csv"))

dfBaselineActions <- read.csv(paste0(dirResults,"Baseline/Baseline-0-99-Scotland-Actions.csv"))

dfBaselineGI <- read.csv(paste0(dirResults,"Baseline/Baseline-0-99-Scotland-GivingInStatistics.csv"))

dfBaselineTO <- read.csv(paste0(dirResults,"Baseline/Baseline-0-99-Scotland-TakeOvers.csv"))

# baseline woodland cover
reference <- filter(dfBaseline, Tick == 2020)
wood.cover<- length(reference$Agent[reference$Agent == "consv.native"|
                                      reference$Agent == "multi.nb"|
                                      reference$Agent == "multi.mixed"|
                                      reference$Agent == "prod.n.conifer"|
                                      reference$Agent == "prod.n.broad"|
                                      reference$Agent =="prod.nn.conifer"|
                                      reference$Agent == "prod.nn.broad"|
                                      reference$Agent == "multi.nc"|
                                      reference$Agent == "multi.nnb"|
                                      reference$Agent == "multi.nnc"])/length(reference$Agent)*100
estmW<-length(reference$Agent[which(reference$Agent=='estate.multi'&reference$Service.softwood.timber>0)])/length(reference$Agent)*100
agroW<-length(reference$Agent[which(reference$Agent=='agroforestry'&reference$Service.softwood.timber>0|reference$Service.hardwood.timber>0)])/length(reference$Agent)*100
estsW<-length(reference$Agent[which(reference$Agent=='estate.sport'&reference$Service.softwood.timber>0|reference$Service.hardwood.timber>0)])/length(reference$Agent)*100 
estcW<-length(reference$Agent[which(reference$Agent=='estate.consv'&reference$Service.softwood.timber>0|reference$Service.hardwood.timber>0)])/length(reference$Agent)*100
referenceWC<-wood.cover+agroW+estsW+estcW+estmW


### Visions --------------------------------------------------------------------

lstVisions <- c("Multiple_Benefits","Wild_Woodlands")

for (vision in lstVisions){
  
  dfVision <-
    list.files(path = paste0(dirResults,vision,"/"),
               pattern = "*.csv", 
               full.names = T) %>% 
    grep("-Cell-", value=TRUE, .) %>% 
    #map_df(~read_csv(., col_types = cols(.default = "c")))
    map_df(~read.csv(.))
  
  dfVision$Tick <- factor(dfVision$Tick)
  dfVision$Agent <- factor(dfVision$Agent)
  
  # regions
  dfVision$Capital.region[which(dfVision$Capital.region==0)]<-NA
  dfVision$Capital.region[which(dfVision$Capital.region==1)]<-"South Scotland"
  dfVision$Capital.region[which(dfVision$Capital.region==2)]<-"Central"
  dfVision$Capital.region[which(dfVision$Capital.region==3)]<-"Perth & Argyll"
  dfVision$Capital.region[which(dfVision$Capital.region==4)]<-"Grampian"
  dfVision$Capital.region[which(dfVision$Capital.region==5)]<-"Highlands"
  dfVision$Capital.region[which(dfVision$Capital.region==6)]<-"Islands"
  dfVision$Capital.region <- factor(dfVision$Capital.region)
  
  # reclassify agent types to produce simplified land use categories
  dfVision$reclass <- NA
  dfVision$reclass[which(dfVision$Agent=="prodnconifer"|
                                      dfVision$Agent=="prodnbroad"|
                                      dfVision$Agent=="consvnative"|
                                      dfVision$Agent=="multinc"|
                                      dfVision$Agent=="multinb")] <-'native.woodland'
  dfVision$reclass[which(dfVision$Agent=="prodnnconifer"|
                                      dfVision$Agent=="prodnnbroad"|
                                      dfVision$Agent=="multinnc"|
                                      dfVision$Agent=="multinnb")] <- "non.native.woodland"
  dfVision$reclass[which(dfVision$Agent=="multimixed")] <- "mixed.woodland"
  dfVision$reclass[which(dfVision$Agent=="intarable"|
                                      dfVision$Agent=="intpastoral")] <- "intensive.agriculture"
  dfVision$reclass[which(dfVision$Agent=="extarable"|
                                      dfVision$Agent=="extpastoral"|
                                      dfVision$Agent=="agroforestry")] <- "extensive.agriculture"
  dfVision$reclass[which(dfVision$Agent=="estatemulti")] <- 'mixed.estate'
  dfVision$reclass[which(dfVision$Agent=="estatesport")] <- "traditional.sporting.management"
  dfVision$reclass[which(dfVision$Agent=="estateconsv")] <- "conservation.management"
  dfVision$reclass[which(dfVision$Agent=="marginal")] <- "marginal"
  dfVision$reclass[which(dfVision$Agent=="waterurban")] <- "urban.&.waterbodies"
  
  # create simplified all.woodland type for connectivity metric
  dfVision$all.woodland <- NA
  dfVision$all.woodland[which(dfVision$Agent=="prodnconifer"|
                                           dfVision$Agent=="prodnbroad"|
                                           dfVision$Agent=="consvnative"|
                                           dfVision$Agent=="multinc"|
                                           dfVision$Agent=="multinb"|
                                           dfVision$Agent=="prodnnconifer"|
                                           dfVision$Agent=="prodnnbroad"|
                                           dfVision$Agent=="multinnc"|
                                           dfVision$Agent=="multinnb"|
                                           dfVision$Agent=="multimixed")] <- "all.woodland"
  
  ### woodland % cover per yr
  year_list<-c(2020:2100)
  wood.cover<-rep(0,length(year_list))
  estmW<-rep(0,length(year_list))
  estsW<-rep(0,length(year_list))
  estcW<-rep(0,length(year_list))
  agroW<-rep(0,length(year_list))
  
  # loop to count length of woodland agents
  for (i in c(1:81)) {
    
    timestep<-filter(dfVision, Tick==year_list[i])
    wood.cover[i]<- length(timestep$Agent[timestep$Agent == "consvnative"|
                                            timestep$Agent == "multinb"|
                                            timestep$Agent == "multimixed"|
                                            timestep$Agent == "prodnconifer"|
                                            timestep$Agent == "prodnbroad"|
                                            timestep$Agent =="prodnnconifer"|
                                            timestep$Agent == "prodnnbroad"|
                                            timestep$Agent == "multinc"|
                                            timestep$Agent == "multinnb"|
                                            timestep$Agent == "multinnc"])/length(timestep$Agent)*100
    estmW[i]<-length(timestep$Agent[which(timestep$Agent=='estatemulti'&timestep$Service.softwood.timber>0|timestep$Service.hardwood.timber>0)])/length(timestep$Agent)*100
    estsW[i]<-length(timestep$Agent[which(timestep$Agent=='estatesport'&timestep$Service.softwood.timber>0|timestep$Service.hardwood.timber>0)])/length(timestep$Agent)*100
    estcW[i]<-length(timestep$Agent[which(timestep$Agent=='estateconsv'&timestep$Service.softwood.timber>0|timestep$Service.hardwood.timber>0)])/length(timestep$Agent)*100
    agroW[i]<-length(timestep$Agent[which(timestep$Agent=='agroforestry'&timestep$Service.softwood.timber>0|timestep$Service.hardwood.timber>0)])/length(timestep$Agent)*100
    
  }
  
  wood_cover<-wood.cover+estcW+estmW+estsW+agroW
  #wood_cover<-wood_cover # adjust?
  
  ### total.ES.diversity
  
  tES.div<-rep(0,80)
  year_id <- year_list
  
  Ss.timber<-rep(0,length(year_id))
  Sh.timber<-rep(0,length(year_id))
  S.bio<-rep(0,length(year_id))
  S.carbon<-rep(0,length(year_id))
  S.flood<-rep(0,length(year_id))
  S.rec<-rep(0,length(year_id))
  S.lstock<-rep(0,length(year_id))
  S.crop<-rep(0,length(year_id))
  S.emp<-rep(0,length(year_id))
  tot<-rep(0,length(year_id))
  
  # loop to sum production for each service
  for (i in c(1:81)) {
    
    timestep<-filter(dfVision, Tick==year_list[i])
    Ss.timber[i]<-sum(timestep$Service.softwood.timber)
    Sh.timber[i]<-sum(timestep$Service.hardwood.timber)
    S.bio[i]<-sum(timestep$Service.biodiversity)
    S.carbon[i]<-sum(timestep$Service.carbon)
    S.flood[i]<-sum(timestep$Service.flood.regulation)
    S.rec[i]<-sum(timestep$Service.recreation)
    S.lstock[i]<-sum(timestep$Service.livestock)
    S.crop[i]<-sum(timestep$Service.crop.service)
    S.emp[i]<-sum(timestep$Service.employment)
    tot[i]<-(Ss.timber[i]+Sh.timber[i]+S.bio[i]+S.carbon[i]+S.flood[i]+S.rec[i]+S.lstock[i]
             +S.crop[i]+S.emp[i])
    
    # metric based on simpson's diversity index
    tES.div[i]<-1-(((Ss.timber[i]/tot[i])^2)+((Sh.timber[i]/tot[i])^2)+((S.bio[i]/tot[i])^2)
                   +((S.carbon[i]/tot[i])^2)+((S.flood[i]/tot[i])^2)+((S.rec[i]/tot[i])^2)
                   +((S.lstock[i]/tot[i])^2)+((S.crop[i]/tot[i])^2)+((S.emp[i]/tot[i])^2))
  }
  
  ### total.LU.diversity
  
  tLU.div<-rep(0,81)
  
  # empty columns to be populated by loop
  prod.nn.conifer<-rep(0,length(year_id))
  prod.n.conifer<-rep(0,length(year_id))
  prod.nn.broad<-rep(0,length(year_id))
  prod.n.broad<-rep(0,length(year_id))
  consv.native<-rep(0,length(year_id))
  multi.mixed<-rep(0,length(year_id))
  multi.nnc<-rep(0,length(year_id))
  multi.nc<-rep(0,length(year_id))
  multi.nnb<-rep(0,length(year_id))
  multi.nb<-rep(0,length(year_id))
  agroforestry<-rep(0,length(year_id))
  int.pastoral<-rep(0,length(year_id))
  ext.pastoral<-rep(0,length(year_id))
  int.arable<-rep(0,length(year_id))
  ext.arable<-rep(0,length(year_id))
  estate.sport<-rep(0,length(year_id))
  estate.multi<-rep(0,length(year_id))
  estate.consv<-rep(0,length(year_id))
  marginal<-rep(0,length(year_id))
  water.urban<-rep(0,length(year_id))
  tot<-rep(0,length(year_id))
  
  year_list<-c(2020:2100)
  
  # loop through years
  for(i in c(1:81)) {
    
    timestep<-filter(dfVision, Tick==year_list[i])
    prod.nn.conifer[i]<-length(timestep$Agent[timestep$Agent=="prodnnconifer"])
    prod.n.conifer[i]<-length(timestep$Agent[timestep$Agent=="prodnconifer"])
    prod.nn.broad[i]<-length(timestep$Agent[timestep$Agent=="prodnnbroad"])
    prod.n.broad[i]<-length(timestep$Agent[timestep$Agent=="prodnbroad"])
    consv.native[i]<-length(timestep$Agent[timestep$Agent=="consvnative"])
    multi.mixed[i]<-length(timestep$Agent[timestep$Agent=="multimixed"])
    multi.nnc[i]<-length(timestep$Agent[timestep$Agent=="multinnc"])
    multi.nc[i]<-length(timestep$Agent[timestep$Agent=="multinc"])
    multi.nnb[i]<-length(timestep$Agent[timestep$Agent=="multinnb"])
    multi.nb[i]<-length(timestep$Agent[timestep$Agent=="multinb"])
    agroforestry[i]<-length(timestep$Agent[timestep$Agent=="agroforestry"])
    int.pastoral[i]<-length(timestep$Agent[timestep$Agent=="intpastoral"])
    ext.pastoral[i]<-length(timestep$Agent[timestep$Agent=="extpastoral"])
    int.arable[i]<-length(timestep$Agent[timestep$Agent=="intarable"])
    ext.arable[i]<-length(timestep$Agent[timestep$Agent=="extarable"])
    estate.sport[i]<-length(timestep$Agent[timestep$Agent=="estatesport"])
    estate.multi[i]<-length(timestep$Agent[timestep$Agent=="estatemulti"])
    estate.consv[i]<-length(timestep$Agent[timestep$Agent=="estateconsv"])
    marginal[i]<-length(timestep$Agent[timestep$Agent=="marginal"])
    water.urban[i]<-length(timestep$Agent[timestep$Agent=="waterurban"])
    tot[i]<-length(timestep$Agent)
    
    # Simpon index for diversity of land uses
    
    tLU.div[i]<-1-(((prod.nn.conifer[i]/tot[i])^2)+((prod.n.conifer[i]/tot[i])^2)+((prod.nn.broad[i]/tot[i])^2)+
                     ((prod.n.broad[i]/tot[i])^2)+((consv.native[i]/tot[i])^2)+((multi.mixed[i]/tot[i])^2)+
                     ((multi.nnc[i]/tot[i])^2)+((multi.nc[i]/tot[i])^2)+((multi.nnb[i]/tot[i])^2)+((multi.nb[i]/tot[i])^2)+
                     ((agroforestry[i]/tot[i])^2)+((int.pastoral[i]/tot[i])^2)+((ext.pastoral[i]/tot[i])^2)+
                     ((int.arable[i]/tot[i])^2)+((ext.arable[i]/tot[i])^2)+((estate.sport[i]/tot[i])^2)+
                     ((estate.multi[i]/tot[i])^2)+((estate.consv[i]/tot[i])^2)+((marginal[i]/tot[i])^2)+((water.urban[i]/tot[i])^2))
    
  }
  
  ### agent extents

  native.wood.ext<-c()
  consv.ext<-c()
  int.agri.ext<-c()
  int.timb.ext<-c()
  estates.ext<-c()
  ext.agri.ext<-c()
  
  
  for(i in c(1:81)) {
    timestep<-filter(dfVision, Tick==year_list[i])
    native.wood.ext[i]<-length(timestep$Agent[timestep$Agent=="consvnative"])
    consv.ext[i]<-length(timestep$Agent[timestep$Agent=="estateconsv"|
                                          timestep$Agent=="consvnative"])
    int.agri.ext[i]<-length(timestep$Agent[timestep$Agent=="intpastoral" | timestep$Agent=="intarable"])
    int.timb.ext[i]<-length(timestep$Agent[timestep$Agent=="prodnnconifer" | timestep$Agent=="prodnnbroad" 
                                           | timestep$Agent=="prodnconifer" | timestep$Agent=="prodnbroad"])
    estates.ext[i]<-length(timestep$Agent[timestep$Agent=="estatemulti"|timestep$Agent=="estatesport"|
                                            timestep$Agent=="estateconsv"])
    ext.agri.ext[i]<-length(timestep$Agent[timestep$Agent=="extpastoral"|timestep$Agent=="extarable"])
  }
  
  metrics <- cbind(year_id,
                   wood_cover,
                   tES.div,
                   tLU.div,
                   native.wood.ext,
                   consv.ext,
                   int.agri.ext,
                   int.timb.ext,
                   estates.ext,
                   ext.agri.ext)
  dfMetrics <- as.data.frame(metrics)
  
  write.csv(dfMetrics, paste0(dirMetrics,"/",vision,"_metrics_March21.csv"))
  
  rm(dfVision)
  
}


### plots ----------------------------------------------------------------------

dfMB <- read.csv(paste0(dirMetrics,"/Multiple_Benefits_metrics_March21.csv"))
dfWW <- read.csv(paste0(dirMetrics,"/Wild_Woodlands_metrics_March21.csv"))

dfMB$vision <- "Multiple Benefits"
dfWW$vision <- "Wild Woodlands"

dfAll <- rbind(dfMB,dfWW)

# convert extents to percentages
# 80053 is number of agents per timestep with NA removed
dfAll$native.wood.ext<-dfAll$native.wood.ext/82135*100
dfAll$consv.ext<-dfAll$consv.ext/82135*100
dfAll$int.agri.ext<-dfAll$int.agri.ext/82135*100
dfAll$int.timb.ext<-dfAll$int.timb.ext/82135*100
dfAll$estates.ext<-dfAll$estates.ext/82135*100
dfAll$ext.agri.ext<-dfAll$ext.agri.ext/82135*100

ggplot(dfAll, aes(year_id,wood_cover, colour=vision))+
  geom_smooth()+
  theme_bw()

### ----------------------------------------------------------------------------

dfMultiple_Benefits %>% 
  filter(reclass!='urban.&.waterbodies') %>% 
  group_by(Tick, reclass) %>% 
  summarise(count = n()) %>% 
  ggplot()+
  geom_point(mapping = aes(x=Tick, y=count, col=reclass))+
  theme_bw()+
  scale_fill_manual(name = "Land Management Type", values=lu.colours,
                    labels = c("mixed.estate" = "Mixed estate",
                               "traditional.sporting.management" = "Sporting estate",
                               "conservation.management" = "Conservation",
                               "extensive.agriculture" = "Extensive agriculture",
                               "intensive.agriculture" = "Intensive agriculture", 
                               "mixed.woodland" = "Mixed woodland",
                               "native.woodland" = "Native woodland",
                               "non.native.woodland" = "Non-native woodland",
                               "marginal" = "Marginal"))+
  labs( x = "Year", y = "Extent (square km)")+
  theme(axis.title.x = element_text(family = "Avenir", size=10),
        axis.title.y = element_text(family = "Avenir", size = 10),
        axis.text.x = element_text(family = "Avenir",color="black", size=7),
        axis.text.y = element_text(family = "Avenir",color="black", size=7),
        legend.title = element_text(family = "Avenir", size = 11),
        legend.text = element_text(family = "Avenir", size = 8),
        strip.text = element_text(family="Avenir"))



# date: 25/02/21
# author: VB
# descriptions: read in results and plot

### libraries ------------------------------------------------------------------

library(vroom)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(spatstat)
library(wesanderson)
library(foreach)
library(doSNOW)

### directories ----------------------------------------------------------------

wd <- "~/Documents/Dropbox/"
#wd <- "D:/"
dirResults <- paste0(wd,"Thresholds/")
#dirResults <- paste0(wd,"CRAFTY_Scotland/output/")
dirMetrics <- paste0(wd,"vision_metrics")
#dirMetrics <- paste0(wd,"CRAFTY_Scotland/vision_metrics")

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
                "intarable" = "darkkhaki", #9C964A", 
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

# use baseline to mask out urban areas & waterbodies
#baseline <- read.csv(paste0(dirResults,"BehaviouralBaseline/Baseline/Baseline-0-99-Scotland_natural-Cell-2015.csv"))
#mask <- baseline$Agent
#mask <- rep(mask, 86)

dateRun <- "08July"

### Visions --------------------------------------------------------------------

lstParamsets <- c("BehaviouralBaseline","Thresholds")

n.paramsets <- length(lstParamsets)

lstVisions <- c("Baseline","Green_Gold","Multiple_Benefits","Native_Networks","Wild_Woodlands","Woodland_Culture")

n.scenario <- length(lstVisions)

lstWorlds <- c("natural-Cell","financial-Cell")

n.worlds <- length(lstWorlds)

parallelize <- TRUE # VM has 8 cores and 32GB dynamic RAM
if (parallelize) {
  # 6 cores - 1 per scenario
  n_thread <- 6 # detectCores() # the current version uses 5 GB per process, therefore max 5-6 threads if 32 GB memory, 3 if 16 GB memory, and no parallelisation recommended if 8 GB.
  cl <- makeCluster(n_thread)
  registerDoSNOW(cl)

}

#for (p.idx in 1:n.paramsets){
#foreach(p.idx = 1:n.paramsets, .errorhandling = "stop",.packages = c("doSNOW", "tidyverse","vroom"), .verbose = T) %dopar% {
  
  p.idx <- 1
  paramset <- lstParamsets[p.idx]
  
  for (s.idx in 1:n.scenario){
  #foreach(s.idx = 1:n.scenario, .errorhandling = "stop",.packages = c("doSNOW", "tidyverse","vroom"), .verbose = T) %do% {
    #for (vision in lstVisions){
    
    #vision <- lstVisions[6]
    #s.idx <- 2
    vision <- lstVisions[s.idx]
    
    print(vision)
    
    #foreach(w.idx = 1:n.worlds, .errorhandling = "stop",.packages = c("doSNOW", "tidyverse","vroom"), .verbose = T) %do% {
    for (w.idx in 1:n.worlds){
      
      #w.idx <- 1
      world <- lstWorlds[w.idx]
      print(world)
      
      # dfVision <-
      #   list.files(path = paste0(dirResults,paramset,"/",vision,"/"),
      #              pattern = "*.csv", 
      #              full.names = T) %>% 
      #   grep(world, value=TRUE, .) %>% 
      #   #map_df(~read_csv(., col_types = cols(.default = "c")))
      #   map_df(~read.csv(.))
      
      lstFiles <- list.files(path = paste0(dirResults,paramset,"/",vision,"/"),pattern = "*.csv", full.names = T)
      lstFiles <- grep(world, lstFiles, value = TRUE)
      
      dfVision <- vroom(lstFiles, id="path")
      
      print(paste0("Results read in for",vision," - ",world))
      
      #names(dfVision)
      names(dfVision) <- str_replace(names(dfVision),":", ".")
      
      # region
      dfVision$Capital.region[which(dfVision$Capital.region==0)]<-NA
      dfVision$Capital.region[which(dfVision$Capital.region==1)]<-"South Scotland"
      dfVision$Capital.region[which(dfVision$Capital.region==2)]<-"Central"
      dfVision$Capital.region[which(dfVision$Capital.region==3)]<-"Perth & Argyll"
      dfVision$Capital.region[which(dfVision$Capital.region==4)]<-"Grampian"
      dfVision$Capital.region[which(dfVision$Capital.region==5)]<-"Highlands"
      dfVision$Capital.region[which(dfVision$Capital.region==6)]<-"Islands"
      dfVision$Capital.region <- factor(dfVision$Capital.region)
      
      #dfVision$mask <- mask
      #dfVision$Agent[which(dfVision$mask == "waterurban")] <- "waterurban"
      #dfVision <- filter(dfVision, Agent != "waterurban")
      
      # reclassify agent types to produce simplified land use categories
      dfVision$reclass <- NA
      dfVision$reclass[which(dfVision$Agent=="prodnconifer"|
                               dfVision$Agent=="prodnbroad"|
                               dfVision$Agent=="prodnnconifer"|
                               dfVision$Agent=="prodnnbroad")] <-'Production woodland'
      dfVision$reclass[which(dfVision$Agent=="multimixed"|
                               dfVision$Agent=="multinb"|
                               dfVision$Agent=="multinc"|
                               dfVision$Agent=="multinnc"|
                               dfVision$Agent=="multinnb")] <- "Multifunctional woodland"
      dfVision$reclass[which(dfVision$Agent=="consvnative")] <- "Conservation woodland"
      dfVision$reclass[which(dfVision$Agent=="agroforestry")] <- "Agroforestry"
      dfVision$reclass[which(dfVision$Agent=="intarable"|
                               dfVision$Agent=="intpastoral")] <- "Intensive agriculture"
      dfVision$reclass[which(dfVision$Agent=="extarable"|
                               dfVision$Agent=="extpastoral")] <- "Extensive agriculture"
      dfVision$reclass[which(dfVision$Agent=="estatemulti")] <- 'Multifunctional Estate'
      dfVision$reclass[which(dfVision$Agent=="estatesport")] <- "Sporting Estate"
      dfVision$reclass[which(dfVision$Agent=="estateconsv")] <- "Conservation Estate"
      dfVision$reclass[which(dfVision$Agent=="marginal")] <- "Unmanaged"
      dfVision$reclass[which(dfVision$Agent=="waterurban")] <- "Urban or Waterbody"
      
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
                                    dfVision$Agent=="multimixed")] <- "Woodland"
      
      print(paste0("Simplified categories assigned for vision: ", vision))
      
      ### woodland % cover per yr
      year_list<-c(2015:2100)
      wood.cover<-rep(0,length(year_list))
      estmW<-rep(0,length(year_list))
      estsW<-rep(0,length(year_list))
      estcW<-rep(0,length(year_list))
      agroW<-rep(0,length(year_list))
      
      # loop to count length of woodland agents
      for (i in c(1:86)) {
        
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
      
      #wood_cover<-wood.cover+estcW+estmW+estsW+agroW
      #wood_cover<-wood_cover # adjust?
      wood_cover <- wood.cover
      wood_cover2 <- wood.cover+estcW+estmW+estsW+agroW
      
      # head(dfVision)
      # 
      # woodlandSummary <- dfVision %>% 
      #   group_by(Tick, Agent) %>% 
      #   count()
      
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
      
      print(paste0("Total ES diversity calculated for vision: ", vision))
      
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
      
      print(paste0("Total land use diversity calculated for vision: ", vision))
      
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
      
      print(paste0("Agent extents calculated for vision: ", vision))
      
      metrics <- cbind(year_id,
                       wood_cover,
                       wood_cover2,
                       tES.div,
                       tLU.div,
                       native.wood.ext,
                       consv.ext,
                       int.agri.ext,
                       int.timb.ext,
                       estates.ext,
                       ext.agri.ext)
      dfMetrics <- as.data.frame(metrics)
      
      write.csv(dfMetrics, paste0(dirMetrics,"/",paramset,"_",vision,"_",world,"_metrics_",dateRun,".csv"))
      
      print(paste0("Metrics file written for vision: ", vision))
      
      rm(dfVision)
      
    }
    
  }
  
}
  
stopCluster(cl)   


### plots ----------------------------------------------------------------------

lstMetrics <- list.files(dirMetrics, full.names = T)
lstMetrics <- grep(dateRun, lstMetrics, value = TRUE)

dfAll <- vroom(lstMetrics, id="path")

names(dfAll)

dfAll$paramset <- ifelse(grepl("BehaviouralBaseline", dfAll$path, "BehaviouralBaseline",
                               ifelse(grepl("Thresholds", dfAll$path, "Thresholds"))))

dfAll$world<- ifelse(grepl("financial", dfAll$path, "Financial",
                                          ifelse(grepl("natural", dfAll$path, "Natural"))))

dfAll$vision <- ifelse(grepl("Baseline", dfAll$path), 'Baseline',
                            ifelse(grepl("Green_Gold", dfAll$path), 'Green Gold',
                                   ifelse(grepl("Native_Networks", dfAll$path), 'Native Networks',
                                          ifelse(grepl("Multiple_Benefits", dfAll$path), 'Multiple Benefits',
                                                 ifelse(grepl("Wild_Woodlands", dfAll$path), 'Wild Woodlands',
                                                        ifelse(grepl("Woodland_Culture", dfAll$path), "Woodland Culture", NA))))))

# convert extents to percentages
# 80053 is number of agents per timestep with region == NA removed
dfAll$native.wood.ext<-dfAll$native.wood.ext/82135*100
dfAll$consv.ext<-dfAll$consv.ext/82135*100
dfAll$int.agri.ext<-dfAll$int.agri.ext/82135*100
dfAll$int.timb.ext<-dfAll$int.timb.ext/82135*100
dfAll$estates.ext<-dfAll$estates.ext/82135*100
dfAll$ext.agri.ext<-dfAll$ext.agri.ext/82135*100

ggplot(dfAll, aes(year_id,wood_cover, colour=vision))+
  geom_line()+
  theme_bw()
ggplot(dfAll, aes(year_id,wood_cover2, colour=vision))+
  geom_line()+
  theme_bw()

dfAll %>% 
  pivot_longer(cols = native.wood.ext:ext.agri.ext, names_to = "land.type", values_to = "percentage") %>% 
  ggplot()+
  geom_line(aes(year_id,percentage,col=vision))+
  facet_wrap(~land.type)+
  theme_bw()

dfAll %>% 
  pivot_longer(cols = tES.div:tLU.div, names_to = "metric", values_to = "proportion") %>% 
  ggplot()+
  geom_line(aes(year_id,proportion,col=vision))+
  facet_wrap(~metric)+
  ylim(c(0,1))+
  theme_bw()



### ----------------------------------------------------------------------------

head(dfAll)

filter(dfAll, year_id == 2032) # 21% target
filter(dfAll, year_id == 2050) # 25% target

dfAll %>% 
  #filter(reclass!='urban.&.waterbodies') %>% 
  pivot_longer(native.wood.ext:ext.agri.ext, names_to = "Land.use", values_to = "Percentage") %>% 
  #group_by(vision, year_id, reclass) %>% 
  #summarise(count = n()) %>% 
  ggplot()+
  geom_point(mapping = aes(x=year_id, y=Percentage, col=Land.use))+
  theme_bw()+
  facet_wrap(~vision)+
  # scale_fill_manual(name = "Land Management Type", values=lu.colours,
  #                   labels = c("mixed.estate" = "Mixed estate",
  #                              "traditional.sporting.management" = "Sporting estate",
  #                              "conservation.management" = "Conservation",
  #                              "extensive.agriculture" = "Extensive agriculture",
  #                              "intensive.agriculture" = "Intensive agriculture", 
  #                              "mixed.woodland" = "Mixed woodland",
  #                              "native.woodland" = "Native woodland",
  #                              "non.native.woodland" = "Non-native woodland",
  #                              "marginal" = "Marginal"))+
  labs( x = "Year", y = "Extent (%)")+
  theme(axis.title.x = element_text(family = "Avenir", size=10),
        axis.title.y = element_text(family = "Avenir", size = 10),
        axis.text.x = element_text(family = "Avenir",color="black", size=7),
        axis.text.y = element_text(family = "Avenir",color="black", size=7),
        legend.title = element_text(family = "Avenir", size = 11),
        legend.text = element_text(family = "Avenir", size = 8),
        strip.text = element_text(family="Avenir"))


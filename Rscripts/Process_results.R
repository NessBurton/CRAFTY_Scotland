
library(tidyverse)
library(ggplot2)

#wd <- "~/Documents/Dropbox/"
wd <- "D:/CRAFTY_Scotland/output/"

dirResults <- paste0(wd) # will add output folder name here
dirMetrics <- "D:/CRAFTY_Scotland/vision_metrics/"
dir.create(dirMetrics)

### data info -----

# run ID 
runid <- "0"
# random seed 
seedid <- "99"
# behavioural parameters
paramsets <- c("BehaviouralBaseline","Thresholds")
# version names
version_suffix <- "financial" 

### functions ----

getFname <- function(paramset, scenario, year ) { 

  fs::path_expand(paste0(paramset, "/", scenario, "/", scenario, "-", runid, "-99-Scotland_", version_suffix,"-Cell-", year, ".csv"))

}

#getFname("Thresholds","Wild_Woodlands",2015)

getCSV <- function(filename_in) { 
  
  local_path_tmp <- paste0(dirResults, filename_in)
  
  res <- read.csv(paste0(local_path_tmp), sep = ",")
  
  return(res)
}

#getCSV(getFname("Thresholds","Wild_Woodlands",2015))

getSPDF_Scot <- function(tmp_in_name) {
  
  result_raw <- getCSV(tmp_in_name, location)
  
  result_joined <- inner_join(scot_coords, result_raw, by = c("x" = "X", "y" = "Y"))
  
  result_tmp <- result_joined[, indicator_names_dot]
  
  # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
  result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(scot_coords$long, scot_coords$lat), proj4string = crs(proj4.OSGB1936)), data = data.frame(result_tmp))
  
  # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
  return(result.spdf)
  
}

### do stuff ----

# check competitiveness

dfComp <- read.csv(paste0(dirResults,"BehaviouralBaseline/Baseline/Baseline-0-99-Scotland_financial-AggregateAFTCompetitiveness.csv"))
summary(dfComp)
# why getting 0 for so many?

# pre loop

lstParams <- c("Thresholds","BehaviouralBaseline")

lstVisions <- c( "Baseline", "Green_Gold", "Multiple_Benefits", "Native_Networks", "Wild_Woodlands", "Woodland_Culture")

lstYrs <- 2015:2100

lstWoodlandAgents <- c("prodnconifer","prodnbroad","prodnnconifer","prodnnbroad",
                       "multimixed","multinb","multinc","multinnc","multinnb",
                       "consvnative")

dfMetrics <- tibble()
colnames(dfMetrics) <- c("Year","Wood.cover","tES.div","tLU.div")
dfPercCover <- tibble()

# to put in loop

for (paramset in lstParams){
  
  #paramset <- lstParams[1]
  print(paramset)
  
  for (vision in lstVisions){
    
    #vision <- lstVisions[2]
    print(vision)
    
    for (yr in lstYrs){
      
      #yr <- lstYrs[50]
      year <- round(yr,0)
      print(year)
      
      df_test <- getCSV(getFname(paramset,vision,year))
      
      # region
      df_test$Capital.region[which(df_test$Capital.region==0)]<-NA
      df_test$Capital.region[which(df_test$Capital.region==1)]<-"South Scotland"
      df_test$Capital.region[which(df_test$Capital.region==2)]<-"Central"
      df_test$Capital.region[which(df_test$Capital.region==3)]<-"Perth & Argyll"
      df_test$Capital.region[which(df_test$Capital.region==4)]<-"Grampian"
      df_test$Capital.region[which(df_test$Capital.region==5)]<-"Highlands"
      df_test$Capital.region[which(df_test$Capital.region==6)]<-"Islands"
      df_test$Capital.region <- factor(df_test$Capital.region)
      
      df_test <- filter(df_test, !is.na(Capital.region))
      
      # reclassify agent types to produce simplified land use categories
      df_test$reclass <- NA
      df_test$reclass[which(df_test$Agent=="prodnconifer"|
                              df_test$Agent=="prodnbroad"|
                              df_test$Agent=="prodnnconifer"|
                              df_test$Agent=="prodnnbroad")] <-'Production woodland'
      df_test$reclass[which(df_test$Agent=="multimixed"|
                              df_test$Agent=="multinb"|
                              df_test$Agent=="multinc"|
                              df_test$Agent=="multinnc"|
                              df_test$Agent=="multinnb")] <- "Multifunctional woodland"
      df_test$reclass[which(df_test$Agent=="consvnative")] <- "Conservation woodland"
      df_test$reclass[which(df_test$Agent=="agroforestry")] <- "Agroforestry"
      df_test$reclass[which(df_test$Agent=="intarable"|
                              df_test$Agent=="intpastoral")] <- "Intensive agriculture"
      df_test$reclass[which(df_test$Agent=="extarable"|
                              df_test$Agent=="extpastoral")] <- "Extensive agriculture"
      df_test$reclass[which(df_test$Agent=="estatemulti")] <- 'Multifunctional Estate'
      df_test$reclass[which(df_test$Agent=="estatesport")] <- "Sporting Estate"
      df_test$reclass[which(df_test$Agent=="estateconsv")] <- "Conservation Estate"
      df_test$reclass[which(df_test$Agent=="marginal")] <- "Marginal"
      df_test$reclass[which(df_test$Agent=="waterurban")] <- "Urban or Waterbody"
      df_test$reclass[which(df_test$Agent=="Lazy FR")] <- "Unmanaged"
      
      # calulate woodland cover
      wood.cover <- df_test %>% 
        count(Agent) %>% 
        mutate(perc_cover = n/nrow(df_test)*100) %>% 
        summarise(wood_cover = sum(perc_cover[which(Agent %in% lstWoodlandAgents)])) %>% 
        .[[1]]
      
      # service provision
      tES.div <- df_test %>% pivot_longer(Service.softwood.timber:Service.employment,
                               names_to = "Service",
                               values_to = "Provision") %>% 
        group_by(Service) %>% 
        summarise(service_tot = sum(Provision)) %>% 
        mutate(total_prov = sum(service_tot),
               service_div = (service_tot/total_prov)^2,
               tES_div = 1-(sum(service_div))) %>% 
        summarise(tES_div = mean(tES_div)) %>% 
        .[[1]]
      
      # land use diversity
      tLU.div <- df_test %>% 
        count(Agent) %>% 
        mutate(total = sum(n),
               land_div = (n/total)^2,
               tLU_div = 1-(sum(land_div))) %>% 
        summarise(tLU_div = mean(tLU_div)) %>% 
        .[[1]]
      
      metrics <- c(paramset, vision, year, wood.cover, tES.div, tLU.div)
      dfMetrics <- rbind(dfMetrics, metrics)
      colnames(dfMetrics) <- c("Paramset","Vision","Year","Wood.cover","tES.div","tLU.div")

      
      # reclass extents
      LU_summary <- df_test %>% 
        count(reclass) %>% 
        mutate(perc_cover = n/nrow(df_test)*100)
      
    colnames(LU_summary) <- c("Land.use.type","n","Percentage")
    LU_summary$Year <- year
    LU_summary$Paramset <- paramset
    LU_summary$Vision <- vision
    
    dfPercCover <- rbind(dfPercCover,LU_summary)
      
    }
    
    
  }
  
}

dfMetrics %>% 
  pivot_longer(Wood.cover:tLU.div, names_to = "Metric", values_to = "Value") %>%
  ggplot()+
  geom_line(aes(x=as.numeric(Year),y=as.numeric(Value),colour=Vision))+
  facet_grid(Paramset~Metric, scales = "free")


# # supply demand gap
# vision <- "Wild_Woodlands"
# world <- "financial"
# dfDemand <-  read.csv(paste0(dirResults,paramsets[2],"/",vision, "/", vision, "-", runid, "-99-Scotland_",world, "-AggregateServiceDemand.csv"))
# 
# dfDemand[,-19] %>% 
#   pivot_longer(cols = ServiceSupply.softwood.timber:ServiceSupply.employment, #starts_with("Service"), 
#                names_to="Supply", values_to="SupVal") %>%
#   .[,10:12]
#   pivot_longer(starts_with("Demand"), 
#                names_to="Demand", values_to="DemVal")

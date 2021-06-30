
### libraries ------------------------------------------------------------------

library(gplots) # color palette
library(RColorBrewer)
library(grid)
library(DT)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(maptools)
# library(spatstat) # density map
library(dplyr)    # reshaping data frame 
library(leaflet)  # leaflet.js
library(leaflet.extras)
library(shinyjs) # hidden function
library(wesanderson)
library(markdown)
library(Gmisc) # transition plot 


### source data script ---------------------------------------------------------

source("Data_Scotland.R") # shiny runs at the folder in which server and ui scripts exist.
 

### plot parameters ------------------------------------------------------------

RESOLUTION_WEB <- 1E3 # 1.5E4
RESOLUTION_SN <- 1.5E4
RESOLUTION_CRAFTY <- 1.5E4
PLOT_HEIGHT <- 1000 
SIDEBAR_WIDTH <- 2
MAINPANEL_WIDTH <- 12 - SIDEBAR_WIDTH
TRANSPARENCY_DEFAULT <- 0.9 
LEGEND_MAR <- -0.4
LEGEND_CEX <- 1


### projections ----------------------------------------------------------------

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";

# British National Grid
proj4.BNG <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.1502,0.247,0.8421,-20.4894 +units=m +no_defs"
proj4.OSGB1936 <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"  # proj4string(LAD2019_shp) # EPSG:27700

# WGS 84 / Pseudo-Mercator -- Spherical Mercator, Google Maps, OpenStreetMap, Bing, ArcGIS, ESRI
proj4.spherical <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs" # EPSG:3857


### basemap providers ----------------------------------------------------------

provider_names <- c(
  "OpenStreetMap.Mapnik"
  , "OpenTopoMap"  
  , "Stamen.Terrain"
  # , "Thunderforest"      
  , "Esri.WorldImagery"             
  , "Esri.WorldPhysical"              
  , "Esri.NatGeoWorldMap" 
  # , "CartoDB"
  # , "NASAGIBS.ModisTerraTrueColorCR"
  # , "NASAGIBS.ModisTerraBands367CR"      
  , "NASAGIBS.ViirsEarthAtNight2012"
  # , "Wikimedia"      
)


### create temp folders --------------------------------------------------------

app_init <- function() { 
  
  # create caching folders
  if(!dir.exists(path_filecache)) { 
    dir.create(path_filecache, recursive = T)
    dir.create(path_rastercache, recursive = T)
  }
  
  # begin cluster for raster processing
  # endCluster()
  beginCluster(n_thread)
}

app_init()

### get data -------------------------------------------------------------------

getCSV <- function(filename_in, location = "Dropbox") { 
  
  
  #cat("Read ", filename_in, " from ", location)
  
  #if (location == "Dropbox") {
    
    #cached_path <-  paste0(path_filecache, filename_in)
    #print(cached_path)
    
    #if(!file.exists(cached_path)) {
      
      #cached_dir <- dirname(cached_path)
      
      #if (!file.exists(cached_dir)) { 
        
        #dir.create( cached_dir, recursive = T)
        
      #}
      
      #res <- drop_read_csv(fs::path_expand(paste0(path_data_dropbox, filename_in)), dest = cached_dir)
      
    #} else {
      
      #res <- read.csv(cached_path, sep = ",")
      
    #}
    
  #} else { # assume the files in the cached folder already
    
    local_path_tmp <- paste0(path_data_local, filename_in)
    
    res <- read.csv(paste0(local_path_tmp), sep = ",")
    
  #}
  
  return(res)
}


default_df <- getCSV(getFname("Scotland_natural","V2_June21","Green_Gold","2015"))

### create spatial data frame --------------------------------------------------

getSPDF_Scot <- function(tmp_in_name, location = "Dropbox") {
  
  # Target outcome
  # tmp_in_name <- "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/1May2021/Normal/BehaviouralBaseline/Baseline-SSP3/Baseline-SSP3-0-99-UK-Cell-2050.csv"
  #tmp_in_name <- "Baseline/Baseline-0-99-Scotland_V2-Cell-2015.csv"
  # result_raw <- read.csv(paste0( tmp_in_name))
  
  result_raw <- getCSV(tmp_in_name, location)
  
  result_joined <- inner_join(scot_coords, result_raw, by = c("x" = "X", "y" = "Y"))
  
  result_tmp <- result_joined[, indicator_names_dot]
  
  # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
  result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(scot_coords$long, scot_coords$lat), proj4string = crs(proj4.OSGB1936)), data = data.frame(result_tmp))# , tolerance = 0.0011)
  # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
  return(result.spdf)
  
}


### rasterise ------------------------------------------------------------------

getRaster<- function(fname, band.name, location = location_UK, resolution = RESOLUTION_WEB, printPath=TRUE) {
  
  
  localtif_path <- paste0(path_rastercache, fname, "_", band.name, ".tif")
  band.name_dot <- indicator_names_dot[match(band.name, indicator_names)]
  print(band.name_dot)
  
  if (printPath) { 
    print(localtif_path)
  }
  
  if(!file.exists(localtif_path)) {
    
    localdir_path <-  dirname(localtif_path)
    if (!dir.exists(localdir_path)) {
      dir.create(localdir_path, recursive = T)
    }
    
    if (printPath) { 
      print(location)
      print(fname)
    }
    
    spdf.out <- getSPDF_Scot(fname, location = location)
    
    
    # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
    rs.LL <- stack(spdf.out)
    print(rs.LL)
    
    
    print("reprojection")
    
    # print(names(rs.LL))
    
    out.reproj = projectRaster(rs.LL[[band.name_dot]], crs = proj4.spherical, method = "ngb", res = resolution)
    writeRaster(out.reproj, filename = localtif_path, overwrite=T)
    
    print("reprojection done")  
  } else {
    out.reproj <- raster(localtif_path) 
  }
  
  
  return(out.reproj)
}


r_dummy = r_default
r_dummy[!is.na(r_dummy)] = 1

# ### read in rasters and write?? ------------------------------------------------
# to improve performacne - not necessary before production -  (14June2021 by ABS)
# 
# # call once on a local workstation
# # write tifs
# # create a report 
# 
# createTempFiles <- function() {
#   # price = "Normal"
#   # demand = "Normal"
#   # paramset = "Paramset1"
#   # scenario = "RCP8_5-SSP3"
#   
#   endCluster()
#   library(parallel)
#   library(doMC)
#   registerDoMC(6)
#   
#   foreach(paramset = paramsets, .errorhandling = "stop") %do% {
#     print(paramset)
#     
#     res1 <- foreach(scenario = scenario_names,  .errorhandling = "stop") %do% {
#       print(scenario)
#       
#       res2 <- sapply(target_years_other, FUN = function(year) sapply(indicator_names, FUN = function(b_name) {
#         
#         res3 <- getRaster(getFname(version_tmp, paramset = paramset, scenario = scenario, year =  year), band.name =  b_name, location = location_UK, printPath = FALSE);
#         
#         return(TRUE);
#         
#       }))
#       
#       print("ok")
#       
#       return(res)
#     }
#     
#     
#   }
#   
#   
#   return(TRUE)
# }


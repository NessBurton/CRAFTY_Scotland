
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
library(spatstat) # density map
library(dplyr)    # reshaping data frame 
library(leaflet)  # leaflet.js
library(leaflet.extras)
library(shinyjs) # hidden function
library(wesanderson)
library(markdown)
library(Gmisc) # transition plot 


### source other script --------------------------------------------------------

source("RScripts/Data_Scotland.R")

### get data -------------------------------------------------------------------

getCSV <- function(filename_in, location = "Dropbox") { 
  
  
  cat("Read ", filename_in, " from ", location)
  
  if (location == "Dropbox") {
    
    cached_path <-  paste0(path_filecache, filename_in)
    print(cached_path)
    
    if(!file.exists(cached_path)) {
      
      cached_dir <- dirname(cached_path)
      
      if (!file.exists(cached_dir)) { 
        
        dir.create( cached_dir, recursive = T)
        
      }
      
      res <- drop_read_csv(fs::path_expand(paste0(path_data_dropbox, filename_in)), dest = cached_dir)
      
    } else {
      
      res = read.csv(cached_path, sep = ",")
      
    }
    
  } else { # assume the files in the cached folder already
    
    local_path_tmp =  paste0(path_data_local, filename_in)
    
    res <- read.csv(paste0(local_path_tmp), sep = ",")
    
  }
  
  return(res)
}
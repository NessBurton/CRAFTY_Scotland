
wd <- "~/Documents/Dropbox/"
dirResults <- paste0(wd) # will add output folder name here
dirMetrics <- paste0(wd,"vision_metrics")

### data info -----

# run ID 
runid <- "0"
# random seed 
seedid <- "99"
# behavioural parameters
paramsets <- c("BehaviouralBaseline","Thresholds")
# version names
version_suffix <- "natural" 

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

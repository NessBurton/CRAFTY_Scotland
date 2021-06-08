

### libraries ------------------------------------------------------------------

library(dplyr)
library(gplots)
library(leaflet)
library(raster)


### description ----------------------------------------------------------------

# adapting from Bumsuk's CRAFTY-UK script here: https://github.com/CRAFTY-ABM/CRAFTY_WEB/blob/UK/RScripts/Data_UK.R#L56


### set up file paths ----------------------------------------------------------

# run ID 
runid <- "0"
# random seed 
seedid <- "99"

# number of threads to process raster
n_thread <- 4

# dropbox relative path 
#path_dropbox <- "KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/"

# local data archive (Sandbox data drive)
path_localstorage <- "D:/CRAFTY_Scotland/output/" #paste0("~/CRAFTY_WEB_UK_DATA/")

# data version
data_prefix <- "V1/"
#data_prefix = "16May2021_v5_NewProduction_SN2_originaldemand/"
#data_prefix = "16May2021_v6_NewProduction_originaldemand/"
#data_prefix <- ""
#data_prefix = "21May2021_v9_NotRemovingNegative/"

version_names <- c("V1", "V2")
#version_prefix =c("SN_Removal", "SN_NoRemoval", "NoSN_Removal", "NoSN_NoRemoval") 
version_default <- version_names[1]

# absolute path (for local)
path_data_local <- paste0(path_localstorage, data_prefix)

# relative path (for dropbox)
#path_data_dropbox = paste0(path_dropbox, data_prefix)

path_shinywd <- "~/shiny_tmp_dev"
path_filecache <- paste0(path_shinywd, "/filetmp/")
path_rastercache <- paste0(path_shinywd, "/rastertmp/")

# dummy name
default_fname <- paste0(version_default, "/Baseline/Baseline-0-99-Scotland_V2-Cell-2015.csv")

getFname <- function(version, paramset, scenario, year ) { 
  
  # fs::path_expand(paste0( fooddemand, "/" ,foodprice,"/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  fs::path_expand(paste0(version_prefix[match(version,version_names)], "/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-Scotland_V2-Cell-", year, ".csv"))
  
}


### unsure about this part for now ---------------------------------------------

#scenarioname.default <- "Baseline"
#r_default <- raster("GISData/UK_default.tif")

# ext = extent(projectRaster(r.default, crs = proj4.LL))
#ext <- c(-8.439121, 2.794859, 49.77235, 60.93977 )

#drop_token_name <- "Authentication/droptoken.rds"


### import the data ------------------------------------------------------------

# Cell ID and coordinates 

BNG_csv <- read.csv("~/eclipse-workspace/CRAFTY_Scotland/data_raw/intermediate/lcm_props.csv") 

#BNG_csv <- BNG_csv[, c("FID", "POINT_X", "POINT_Y")]

#scot_coords <- #read.csv("Tables/Cell_ID_XY_UK.csv")
  
# scenarios 
scenario_names <- c("Baseline","Green_Gold","Multiple_Benefits","Native_Networks","Wild_Woodlands","Woodland_Culture")

paramsets_fullnames <- c("V1") #"V2"

n_paramset <- length(paramsets_fullnames)
# paramsets <- paste0("Paramset", 1:n.paramset)
paramsets <-  c("V1") # "V2", 

service_tb <- read.csv("~/eclipse-workspace/CRAFTY_Scotland/data_Scotland/csv/Services.csv") %>% as.data.frame
serviceNames <- service_tb$Name
# adapt palette
#serviceColours = c("Food.crops" = "coral1", "Fodder.crops" ="goldenrod1", "GF.redMeat" = "turquoise", "Fuel" = "tan4", "Softwood" = "black", "Hardwood" = "grey", "Biodiversity" = "dodgerblue2", "Carbon"="darkgreen", "Recreation" = "orange", "Flood.reg" = "lightblue", "Employment" = "purple", "Ldiversity" = "brown", "GF.milk" = "green", "Sus.Prod" = "pink")

capital_tb <- read.csv("~/eclipse-workspace/CRAFTY_Scotland/data_Scotland/csv/Capitals.csv") %>% as.data.frame
capitalNames <- capital_tb$Name
# adapt palette
#capital_colours <- (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "NNBroadleaf"="orange", "NBroadleaf" = "lightblue", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")


indicator_names <- c(paste0("Service:", serviceNames), paste0("Capital:", capitalNames), "LandUseIndex") #, "Agent")
indicator_names_dot <- c(paste0("Service.", serviceNames), paste0("Capital.", capitalNames), "LandUseIndex") #, "Agent")


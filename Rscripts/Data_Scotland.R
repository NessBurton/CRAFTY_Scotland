

### libraries ------------------------------------------------------------------

library(dplyr)
library(gplots)
library(leaflet)
library(raster)

proj4.LL <- CRS("+proj=longlat +datum=WGS84")

### description ----------------------------------------------------------------

# adapting from Bumsuk's CRAFTY-UK script here: https://github.com/CRAFTY-ABM/CRAFTY_WEB/blob/UK/RScripts/Data_UK.R#L56


if (Sys.getenv()["USER"] %in% c("alan", "seo-b")) { 
  # ABS
  path_data_raw = "~/Nextcloud/CRAFTY/CRAFTY_Scotland_rawdata/"
  path_base = "~/Nextcloud/CRAFTY/CRAFTY_Scotland/data_Scotland"
  path_localstorage <- "~/Nextcloud/CRAFTY/CRAFTY_Scotland_output/" #paste0("~/CRAFTY_WEB_UK_DATA/")
  
} else { 
  path_data_raw = "~/eclipse-workspace/CRAFTY_Scotland/data_raw/"
  path_base = "~/eclipse-workspace/CRAFTY_Scotland/data_Scotland"
  # local data archive (Sandbox data drive)
  path_localstorage <- "D:/CRAFTY_Scotland/output/" #paste0("~/CRAFTY_WEB_UK_DATA/")
  
}

### set up file paths ----------------------------------------------------------

# run ID 
runid <- "0"
# random seed 
seedid <- "99"

# number of threads to process raster
n_thread <- 4

location_UK <- "Local"

# dropbox relative path 
#path_dropbox <- "KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/"

 
# For UI
start_year = 2015
end_year= 2100
default_year = 2015



# [1] "Tick"                      "X"                         "Y"                         "Service.softwood.timber"  
# [5] "Service.hardwood.timber"   "Service.biodiversity"      "Service.carbon"            "Service.flood.regulation" 
# [9] "Service.recreation"        "Service.livestock"         "Service.crop.service"      "Service.employment"       
# [13] "Capital.region"            "Capital.nn.conifer.yc"     "Capital.nn.broad.yc"       "Capital.n.conifer.yc"     
# [17] "Capital.n.broad.yc"        "Capital.n.broad.consv"     "Capital.agro.yc"           "Capital.mixed.yc"         
# [21] "Capital.crop.productivity" "Capital.grassland"         "Capital.water.runoff"      "Capital.human"            
# [25] "Capital.social"            "Capital.manufactured"      "Capital.financial"         "Capital.moreNW"           
# [29] "Capital.lessNW"            "Capital.moreF"             "Capital.lessF"             "Capital.moreNAT"          
# [33] "Capital.lessNAT"           "Capital.deep.peat"         "Capital.deer.density"      "Capital.wild.land"        
# [37] "LandUseIndex"              "Agent"                     "Competitiveness"         
 

## versions 
#production_names <- c("V2_June21")#"V1", "Thesis", "V2_June21")

world_shortnames <- c("BehaviouralBaseline","Thresholds")#c("Natural")#"Financial", "Natural")
world_names <- c("BehaviouralBaseline","Thresholds")#c("Scotland_natural")#"Scotland_financial", "Scotland_natural")
 
version_suffix <- "financial" 

# absolute path (for local)
path_data_local <- paste0(path_localstorage)

# relative path (for dropbox)
#path_data_dropbox = paste0(path_dropbox)

path_shinywd <- "~/shiny_tmp_dev"
path_filecache <- paste0(path_shinywd, "/filetmp/")
path_rastercache <- paste0(path_shinywd, "/rastertmp/")

getFname <- function(world_name, scenario, year ) { 
#getFname <- function(world_name, production_name, scenario, year ) { 
  
  fs::path_expand(paste0(world_name, "/", scenario, "/", scenario, "-", runid, "-99-Scotland_", version_suffix,"-Cell-", year, ".csv"))
  #fs::path_expand(paste0(world_name, "/", production_name, "/", scenario, "/", scenario, "-", runid, "-99-Scotland_", version_suffix,"-Cell-", year, ".csv"))
   
}

#getFname("BehaviouralBaseline","Green_Gold","2015")  

 

### raster for extent ----------------------------------------------------------

r_default <- raster(paste0(path_data_raw, "/input/lcm15_1k.tif"))
ext <- extent(projectRaster(r_default, crs = proj4.LL))
#ext <- c(-8.439121, 2.794859, 49.77235, 60.93977 )

#drop_token_name <- "Authentication/droptoken.rds"


### import the data ------------------------------------------------------------

# Cell ID and coordinates 

BNG_csv <- read.csv(paste0(path_data_raw, "/input/lcm_iap_coords.csv"))
BNG_csv <- BNG_csv[, c("landcover.id", "landcover.X", "landcover.Y")]
names(BNG_csv) <- c("id","long","lat")

scot_coords <- read.csv(paste0(path_data_raw, "/output/capitals_normalised_Feb21.csv"))
scot_coords <- scot_coords %>% filter(Agent != "water.urban")
scot_coords <- scot_coords[, c("id","x","y")]

scot_coords <- left_join(scot_coords,BNG_csv,by="id")

# Scenarios and agent parameter sets

scenario_names <- c("Baseline","Green_Gold","Multiple_Benefits","Native_Networks","Wild_Woodlands","Woodland_Culture")

scenarioname.default <- scenario_names[1]



# Services and capitals

service_tb <- read.csv(paste0(path_base, "/csv/Services.csv")) %>% as.data.frame
serviceNames <- service_tb$Name
# adapt palette
#serviceColours = c("Food.crops" = "coral1", "Fodder.crops" ="goldenrod1", "GF.redMeat" = "turquoise", "Fuel" = "tan4", "Softwood" = "black", "Hardwood" = "grey", "Biodiversity" = "dodgerblue2", "Carbon"="darkgreen", "Recreation" = "orange", "Flood.reg" = "lightblue", "Employment" = "purple", "Ldiversity" = "brown", "GF.milk" = "green", "Sus.Prod" = "pink")


capital_tb <- read.csv(paste0(path_base, "/csv/Capitals.csv")) %>% as.data.frame
capitalNames <- capital_tb$Name
# adapt palette
#capital_colours <- (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "NNBroadleaf"="orange", "NBroadleaf" = "lightblue", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")


indicator_names <- c(paste0("Service:", serviceNames), paste0("Capital:", capitalNames), "LandUseIndex") #, "Agent")
indicator_names_dot <- c(paste0("Service.", serviceNames), paste0("Capital.", capitalNames), "LandUseIndex") #, "Agent")

# For UI
# default column (csv)
indicator_output_default_idx = 34
indicator_input_default_idx = 10

# column (csv)
indicator_output_idxs = c(1:9, 34)
indicator_input_idxs = c(10:33)


# AFTs

aftnames <- data.frame(rbind(c("agroforestry","agroforestry","Agroforestry"),
                             c("consvnative","consvnative","Conservationist native woodland"),
                             c("estateconsv","estateconsv","Conservation estate"),
                             c("estatemulti","estatemulti","Traditional multifunctional"),
                             c("estatesport","estatesport","Sporting estate"),
                             c("extarable","extarable","Extensive arable farming"),
                             c("extpastoral","extpastoral","Extensive pastoral farming"),
                             c("intarable","intarable","Intensive arable farming"),
                             c("intpastoral","intpastoral","Intensive pastoral farming"),
                             c("marginal","marginal","Marginal land"),
                             c("multimixed","multimixed","Multifunctional mixed woodland"),
                             c("multinc" ,"multinc","Multifunctional native conifer"),
                             c("multinnc","multinnc","Multifunctional non-native conifer"),
                             c("multinb","multinb","Multifunctional native broadleaf"),
                             c("multinnb","multinnb","Multifunctional non-native broadleaf"),
                             c("prodnbroad","prodnbroad","Productive native broadleaf"),
                             c("prodnconifer","prodnconifer","Productive native conifer"),
                             c("prodnnbroad","prodnnbroad","Productive non-native broadleaf"),
                             c("prodnnconifer","prodnnconifer","Productive non-native conifer")))

colnames(aftnames) <- c("AFT", "AFT_cb", "Description")

aft_shortnames_fromzero <- as.character(aftnames$AFT)
#aft_shortnames_fromzero[20] = "Unmanaged"

aft_names_fromzero <-  as.character(aftnames$Description)

n_aft <- length(aft_shortnames_fromzero)

capital_names <- data.frame(Capital = capitalNames)

aft_tb <- read.csv(paste0(path_base, "/csv/AgentColors.csv"), strip.white = T, stringsAsFactors = F) %>% as.data.frame
#aft_tb <- aft_tb[-21,]
#aft_tb[aft_tb$Name == "Lazy FR", ]$Name = "Unmanaged"

aft_colors_alpha <- aft_tb$Color[match(aft_shortnames_fromzero, aft_tb$Name)]

aft_colors_fromzero <- col2hex(paste0("#", substr(aft_colors_alpha, start = 4, stop = 10), substr(aft_colors_alpha, start = 2, stop = 3))) # ignore alpha channel

# 17 colours
aft_colors_fromzero_17 <- aft_colors_fromzero

# reduced colours
# aft_colors_fromzero[aft_shortnames_fromzero %in% c("prodnnconifer",
#                                                    "prodnconifer",
#                                                    "multinc" ,
#                                                    "multinnc",
#                                                    "multimixed",
#                                                    "prodnnbroad",
#                                                    "prodnbroad",
#                                                    "multinb",
#                                                    "multinnb",
#                                                    "consvnative")] = col2hex("darkblue")


target_years_aggcsv <- seq(2015, 2100, 5)
target_years_other <-  seq(2015, 2100, 5)

aft_colors_fromzero_ts <- aft_colors_fromzero
#aft_colors_fromzero_ts[17] <- "black" 
aft_lty_ts <- c(rep(1, 11), 2)

n_cell_total <- nrow(scot_coords)

aft_pal <- colorFactor(col2hex(as.character(aft_colors_fromzero)),  levels = as.character(c(0:20, -1)), na.color = "transparent")

# aft.pal(6)

# 
# # reduced
# aft_group_colors =  aft_colors_fromzero_17[ c(1:5, 7:9, 14:17)]
# aft_group_colors[7] = "darkblue"
# aft_group_colors[12] = "black"
# 
# aft_group_names = c( aft_names_fromzero)[ c(1:5, 7:9, 14:17)]
# aft_group_names[5] = "Intensive Agriculture"
# aft_group_names[7] = "Productive Woodland"
# aft_group_shortnames = c( aft_shortnames_fromzero  )[ c(1:5, 7:9, 14:17)]
# aft_group_shortnames[5] = "IA"
# aft_group_shortnames[7] = "PW"

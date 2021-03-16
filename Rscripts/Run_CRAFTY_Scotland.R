
library(tidyverse)
library(sf)
library(viridis)
library(ggplot2)
library(rJava)
library(jdx)
library(xml2)
library(foreach)
library(tictoc)

### directories/ file paths ----------------------------------------------------

dirWorking<- "~/eclipse-workspace/CRAFTY_Scotland"

dirCRAFTYInput <- path.expand(paste0(dirWorking, "/data_Scotland/"))
dirCRAFTYOutput <- path.expand(paste0(dirWorking, "/output"))
dirFigs <- path.expand(paste0(dirWorking, "/figures"))

setwd(dirWorking)

source("RScripts/Functions_CRAFTY_rJava.R")

### CRAFTY set-up --------------------------------------------------------------

# agent names
aft_names_fromzero <- c("agroforestry",
                        "consvnative",
                        "estateconsv",
                        "estatemulti",
                        "estatesport",
                        "extarable",
                        "extpastoral",
                        "intarable",
                        "intpastoral",
                        "marginal",
                        "multimixed",
                        "multinb",
                        "multinc",
                        "multinnb",
                        "multinnc",
                        "prodnbroad",
                        "prodnconifer",
                        "prodnnbroad",
                        "prodnnconifer",
                        "waterurban")

# location of the CRAFTY Jar file
path_crafty_jar <- path.expand(paste0(dirWorking, "/lib/CRAFTY_KIT_engineOct2020.jar"))

# location of the CRAFTY lib files
path_crafty_libs <- path.expand(paste0(dirWorking, "/lib/"))
crafty_libs <- list.files(paste0(path_crafty_libs), pattern = "jar")
# make sure that in the classpath setting , gt-opengis-9.0.jar must be included before geoapi-20050403.jar. Otherwise it throws an uncatchable error during the giving up process: loading libraries without ordering them particularly, the opengis library is loaded after the geoapi library following alphabetical order.
# related commit - https://github.com/CRAFTY-ABM/CRAFTY_CoBRA/commit/4ce1041cae349572032fc7e25be49652781f5866
crafty_libs <- crafty_libs[crafty_libs != "geoapi-20050403.jar"  ] 
crafty_libs <- c(crafty_libs,  "geoapi-20050403.jar")

# java configuration
crafty_jclasspath <- c(path_crafty_jar, paste0(path_crafty_libs, crafty_libs))

# Random seed used in CRAFTY
random_seed_crafty <- 99 

# CRAFTY timesteps
start_year_idx <- 2020 
end_year_idx <- 2100 

parallelize <- FALSE 

# if getting random Java errors, restart Rstudio
# initialise Java only once
if (!rJava::.jniInitialized) { 
  
  .jinit(parameters="-Dlog4j.configuration=log4j2020_normal.properties")
  .jinit(parameters = "-Dfile.encoding=UTF-8", silent = FALSE, force.init = FALSE)
  .jinit( parameters=paste0("-Xms", java.ms, " -Xmx", java.mx)) # The .jinit returns 0 if the JVM got initialized and a negative integer if it did not. A positive integer is returned if the JVM got initialized partially. Before initializing the JVM, the rJava library must be loaded.

}

# add java classpath
.jclassPath() # print out the current class path settings.
for (i in 1:length(crafty_jclasspath)) { 
  .jaddClassPath(crafty_jclasspath[i])
}

### Run for each scenario ------------------------------------------------------

scenario.filenames <- c("Scenario_Baseline_noGUI.xml",
                        "Scenario_Green_Gold_noGUI.xml",
                        "Scenario_Multiple_Benefits_noGUI.xml",
                        "Scenario_Native_Networks_noGUI.xml",
                        "Scenario_Wild_Woodlands_noGUI.xml",
                        "Scenario_Woodland_Culture_noGUI.xml") 

version <- "V1"

for (scenario in scenario.filenames){
  
  scenario <- scenario.filenames[1]
  scenario.filename <- scenario
  scenario.split <- strsplit(scenario, "[_]")[[1]][2]
  
  # scenario file
  CRAFTY_sargs <- c("-d", dirCRAFTYInput, "-f", scenario.filename, "-o", random_seed_crafty, "-r", "1",  "-n", "1", "-sr", "0", "-e", "2100") 
 
  # set up CRAFTY job
  # Create a new instance (to call non-static methods)
  CRAFTY_jobj <- new(J(CRAFTY_main_name)) 
  
  # prepares a run and returns run information 
  CRAFTY_RunInfo_jobj <- CRAFTY_jobj$EXTprepareRrun(CRAFTY_sargs)
  
  # set the schedule
  CRAFTY_loader_jobj <- CRAFTY_jobj$EXTsetSchedule(as.integer(start_year_idx), as.integer(end_year_idx))
  
  timesteps <- start_year_idx:end_year_idx
  
  ### pre-process CRAFTY Java object
  region <- CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()$'next'()
  
  # change wd to a scenario folder to store output files
  dirCRAFTYscenario <- paste0(dirCRAFTYOutput,"/",version,"/",scenario.split)
  
  # set the batch run folder (dirCRAFTYOutput)
  .jcall( 'java/lang/System', 'S', 'setProperty', 'user.dir',  dirCRAFTYOutput)
  
  # assertion
  stopifnot(dirCRAFTYOutput == .jcall( 'java/lang/System', 'S', 'getProperty', 'user.dir' ))
  
  for (CRAFTY_tick in timesteps) {
    
    print(paste0("============CRAFTY JAVA-R API: Running CRAFTY tick = ", CRAFTY_tick))
    
    CRAFTY_nextTick = CRAFTY_jobj$EXTtick()
    
    stopifnot(CRAFTY_nextTick == (CRAFTY_tick + 1 )) 
    
    print(paste0("============CRAFTY JAVA-R API: CRAFTY run complete = ", CRAFTY_tick))
    
    if (CRAFTY_nextTick <= end_year_idx) {
      (paste0("============CRAFTY JAVA-R API: NextTick=", CRAFTY_nextTick))
      } else {
        print(paste0("============CRAFTY JAVA-R API: Simulation done (tick=", CRAFTY_tick, ")"))
      }
    }
  
}


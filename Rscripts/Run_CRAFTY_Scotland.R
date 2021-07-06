
library(tidyverse)
library(sf)
library(viridis)
library(ggplot2)
# set java home
#Sys.setenv(JAVA_HOME="C:\Program Files (x86)\Java\jre1.8.0_281\bin\javaw.exe") 
library(rJava)
library(jdx)
library(xml2)
library(foreach)
library(doSNOW)
library(tictoc)



### directories/ file paths ----------------------------------------------------

dirWorking<- "~/eclipse-workspace/CRAFTY_Scotland"
dataDisk <- "D:/CRAFTY_Scotland"

dirCRAFTYInput <- path.expand(paste0(dirWorking, "/data_Scotland/"))
dirCRAFTYOutput <- path.expand(paste0(dataDisk, "/output"))
#dirCRAFTYOutput <- path.expand(paste0(dataDisk, "/output"))

#dirFigs <- path.expand(paste0(dirWorking, "/figures"))

setwd(dirWorking)

source("Rscripts/Functions_CRAFTY_rJava.R")



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
path_crafty_jar <- path.expand(paste0(dirWorking, "/lib/CRAFTY_KIT_engine_2021_JDK15.jar"))

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
start_year_idx <- 2015 
end_year_idx <- 2100 



### Run in parallel ------------------------------------------------------------

path_crafty_batch_run <- "D:/CRAFTY_Scotland"

setwd(path_crafty_batch_run)

scenarios <- c( "Baseline", "Green_Gold", "Multiple_Benefits", "Native_Networks", "Wild_Woodlands", "Woodland_Culture")
n.scenario <- length(scenarios)
#scenario.filenames <- paste0("Scenario_", scenarios, "_noGUI")
scenario.filenames <- paste0("Scenario_", scenarios, "_everyyear_relative_GUI")

n.paramset <- 1

parallelize <- TRUE # VM has 8 cores and 32GB dynamic RAM
if (parallelize) { 
  # 6 cores - 1 per scenario
  n_thread <- 6 # detectCores() # the current version uses 5 GB per process, therefore max 5-6 threads if 32 GB memory, 3 if 16 GB memory, and no parallelisation recommended if 8 GB. 
  cl <- makeCluster(n_thread)
  registerDoSNOW(cl)
  
}


#s.idx <- p.idx <- 2 

foreach(s.idx = 1:n.scenario, .errorhandling = "stop",.packages = c("doSNOW"), .verbose = T) %dopar% {
  
  #s.idx <- 1
  scenario <- scenarios[s.idx]
  
  # must change to the output folder for getting the output files correctly
  setwd(path_crafty_batch_run) 
  
  # initialise jvm in forked processes / not before parallelism is initiated
  # https://stackoverflow.com/questions/24337383/unloading-rjava-and-or-restarting-jvm
  # "There is a way to run expressions using rJava in parallel based on running the parallel processes to get and assemble all results BEFORE you load the rJava library in the main process. As the main R process has not initiated jvm then java is started in each single subprocess and this particular instance will die together with subprocess as well."
  
  library(rJava)
  
  .jinit(parameters="-Dlog4j.configuration=log4j2020_normal.properties")
  .jinit(parameters = "-Dfile.encoding=UTF-8", silent = FALSE, force.init = FALSE)
  .jinit( parameters=paste0("-Xms", java.ms, " -Xmx", java.mx)) 
  # The .jinit returns 0 if the JVM got initialized and a negative integer if it did not. A positive integer is returned if the JVM got initialized partially. Before initializing the JVM, the rJava library must be loaded.
  
  # add java classpath
  .jclassPath() # print out the current class path settings.
  for (i in 1:length(crafty_jclasspath)) { 
    .jaddClassPath(crafty_jclasspath[i])
  }
  
  .jcall( 'java/lang/System', 'S', 'setProperty', 'user.dir', path_crafty_batch_run)
  
  print(  .jcall( 'java/lang/System', 'S', 'getProperty', 'user.dir' ))
  
  # Only one parameter set for the moment
  #foreach(p.idx = 1:n.paramset, .errorhandling = "stop", .verbose = T) %do% { 
    
    #paramset =  paste0("Paramset", p.idx)
    #scenario.filename <- paste0(scenario.filenames[s.idx], "_", paramset, ".xml") 
  
    scenario.filename <- paste0(scenario.filenames[s.idx], ".xml") 
    
    #}

    # Read the scenario file
    scenario.xml <- xml2::read_xml(paste0(dirCRAFTYInput, scenario.filename))
    # str(scenario.xml)
    scenario.l <- xml2::as_list(scenario.xml)
    
    # Replace scenario name 
    attr(scenario.l$scenario, "scenario") <- scenario
    # Replace version info 
    #attr(scenario.l$scenario, "version") <- paramset
    
    # Write the modified competition file
    scenario.xml.modified <- xml2::as_xml_document(scenario.l)
    
    xml2::write_xml(scenario.xml.modified, paste0(dirCRAFTYInput, scenario.filename), options = "no_empty_tags")
    
    # Model configuration
    #CRAFTY_sargs <- c("-d", dirCRAFTYInput, "-f", scenario.filename, "-o", "99", "-r", "1",  "-n", "1", "-sr", "0") # change the argument as you wish 
    CRAFTY_sargs <- c("-d", dirCRAFTYInput, "-f", scenario.filename, "-o", random_seed_crafty, "-r", "1",  "-n", "1", "-sr", "0", "-e", "2100")
    
    ### Model running ----------------------------------------------------------
    
    print(paste0("============CRAFTY JAVA-R API: Create the instance"))
    
    CRAFTY_jobj <- new(J(CRAFTY_main_name)) # Create a new instance (to call non-static methods)
    
    # prepares a run and returns run information 
    CRAFTY_RunInfo_jobj <- CRAFTY_jobj$EXTprepareRrun(CRAFTY_sargs)
    print(paste0("============CRAFTY JAVA-R API: Run preparation done"))
    
    # running from the first timestep to the fifth
    CRAFTY_loader_jobj <- CRAFTY_jobj$EXTsetSchedule(as.integer(start_year_idx), as.integer(end_year_idx))
    
    for (tick in start_year_idx:end_year_idx) {
      
      nextTick <- CRAFTY_jobj$EXTtick()
      
      stopifnot(nextTick == (tick + 1 ))
      
      if (nextTick <= end_year_idx) {
        print(paste0("============CRAFTY JAVA-R API: NextTick=", nextTick))
      } else {
        print(paste0("============CRAFTY JAVA-R API: Simulation done (tick=", tick, ")"))
        
      }
    
    }  
    
    CRAFTY_jobj$EXTcloseRrun()
    #print(paste0("============CRAFTY JAVA-R API: Finished for scenario = ", scenario.split))
    
}
stopCluster(cl)   
 


### Run CRAFTY (not parallel) --------------------------------------------------

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

# run for each scenario 
scenario.filenames <- c("Scenario_Baseline_everyyear_relative_GUI.xml",
                        "Scenario_Green_Gold_everyyear_relative_GUI.xml",
                        "Scenario_Multiple_Benefits_everyyear_relative_GUI.xml",
                        "Scenario_Native_Networks_everyyear_relative_GUI.xml",
                        "Scenario_Wild_Woodlands_everyyear_relative_GUI.xml",
                        "Scenario_Woodland_Culture_everyyear_relative_GUI.xml") 

version <- "V2_June21"

# set up CRAFTY job
if (!exists(x = "CRAFTY_jobj")) {   # not to create CRAFTY_jobj multiple times
  # Create a new instance (to call non-static methods)
  CRAFTY_jobj <- new(J(CRAFTY_main_name)) 
}

# loop through all scenarios

for (scenario in scenario.filenames){
  
  scenario <- scenario.filenames[1]
  scenario.filename <- scenario
  scenario.split <- paste0(strsplit(scenario, "[_]")[[1]][2])#,"_",strsplit(scenario, "[_]")[[1]][3])
  
  print(paste0("============CRAFTY JAVA-R API: Running for scenario = ", scenario.split))
  
  # scenario file
  CRAFTY_sargs <- c("-d", dirCRAFTYInput, "-f", scenario.filename, "-o", random_seed_crafty, "-r", "1",  "-n", "1", "-sr", "0", "-e", "2100") 
  
  
  # prepares a run and returns run information 
  CRAFTY_RunInfo_jobj <- CRAFTY_jobj$EXTprepareRrun(CRAFTY_sargs)
  
  # set the schedule
  CRAFTY_loader_jobj <- CRAFTY_jobj$EXTsetSchedule(as.integer(start_year_idx), as.integer(end_year_idx))
  
  timesteps <- start_year_idx:end_year_idx
  
  ### pre-process CRAFTY Java object
  # region <- CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()$'next'()
  
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
  
  CRAFTY_jobj$EXTcloseRrun()
  print(paste0("============CRAFTY JAVA-R API: Finished for scenario = ", scenario.split))
  
}

# delete java objects
rm( CRAFTY_jobj, CRAFTY_RunInfo_jobj, CRAFTY_loader_jobj)


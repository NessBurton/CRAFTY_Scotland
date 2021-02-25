
# date: 25/02/21
# author: VB
# descriptions: read in results and plot

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

### directories ----------------------------------------------------------------

wd <- "~/eclipse-workspace/CRAFTY_Scotland/"
dirData <- paste0(wd,"data_raw")
dirOut <- paste0(wd,"data_Scotland")
dirResults <- paste0(wd,"output/V1/")


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

rm(list=ls())
library(tidyverse)
library(sf)

load(here("Baseline_Survey/data/gps.RData"))
load(here("Baseline_Survey/data/menage.RData"))

gps = gps %>%
    rename(
    hh1a = gps1a  
    )

#Remove White Space 
dfList <- list(menage, gps)

dfList <- lapply(dfList, function(x) {
  cols = names(x)[vapply(x, is.character, logical(1))]
  x[,cols] <- lapply(x[,cols], trimws)
  x
  } )

menage = dfList[[1]]
gps = dfList[[2]]

##Merge
merged = merge(gps, menage, by = "hh1a")

#save
write.csv(merged, "menage_latlong.csv")


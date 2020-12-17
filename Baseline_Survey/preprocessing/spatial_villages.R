library(here)
library(sf)

#Load GPS data
load(here("Baseline_Survey/data/GPS.RData"))

#Transform GPS data into vector data
gps<-st_as_sf(x=gps,
              crs="+proj=longlat +datum=WGS84 +no_defs",
              coords=c("gpslon", "gpslat"))


#Testing results
library(leaflet)

leaflet(gps) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(radius=5,
                         color="red",
                         opacity=1,
                         fillOpacity = 1,
                         label = ~gps1a,
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE))

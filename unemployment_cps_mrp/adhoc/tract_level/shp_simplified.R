library(colorout)
library(tidycensus)
library(sqldf)
library(doMC)
library(foreach)
library(gtools)
library(dplyr)
library(maps)
library(mapproj)
library(maptools)
library(sp)
library(rgdal)
library(rgeos)
library(raster)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd(ifelse(Sys.info()[['sysname']]=="Darwin", 
  "~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/adhoc/tract_level/", 
  "~"))

################################################################################################################

shp <- readRDS("shp_tract_county_state.rds")

################################################################################################################
# make alaska smaller

# tract
ak <- shp$tract[["02"]]
ak <- elide(ak, scale=800000)
ak <- elide(ak, shift=c(-1500000, -2200000))
shp$tract[["02"]] <- ak

# county
ok <- shp$county@data$STATE == "02"
ak <- shp$county[ok,]
ak <- elide(ak, scale=800000)
ak <- elide(ak, shift=c(-1500000, -2200000))
shp$county <- bind(shp$county[!ok,], ak)

# state
ok <- shp$state@data$STATE == "02"
ak <- shp$state[ok,]
ak <- elide(ak, scale=800000)
ak <- elide(ak, shift=c(-1500000, -2200000))
shp$state <- bind(shp$state[!ok,], ak)

################################################################################################################
# merge tracts into a single object

tract <- shp$tract[[1]]
for (i in 2:length(shp$tract)) {
  message(i, " of ", length(shp$tract))
  tract <- bind(tract, shp$tract[[i]])
}

shp$tract <- tract

################################################################################################################
# make sure it looks good

jpeg("test.jpeg", width=10, height=8, units="in", res=300)
par(mar=c(0,0,0,0))

plot(shp$tract, col=NA, border="red", lwd=0.1)
plot(shp$county, col=NA, border="grey", lwd=0.5, add=TRUE)
plot(shp$state, col=NA, border="black", lwd=1, add=TRUE)

dev.off()

################################################################################################################
# save

saveRDS(shp, file="shp_simplified.rds")

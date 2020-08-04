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

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd(ifelse(Sys.info()[['sysname']]=="Darwin", 
  "~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/tract_level/", 
  "~"))

################################################################################################################

AlaskaHawaiiFix <- function(shp, ak, hi, 
  projection="+init=epsg:2163",
  ak_params=c(-35, 2, -2000000, -2600000),
  hi_params=c(-35, 1.25, -3300000, -1700000)) {

  fix1 <- function(object, params) {
    r <- params[1]
    scale <- params[2]
    shift <- params[3:4]
    object <- elide(object, rotate=r)
    size <- max(apply(bbox(object), 1, diff)) / scale
    object <- elide(object, scale=size)
    object <- elide(object, shift=shift)
    return(object)
  }

  if (all(ak)) {
    shp_ak <- fix1(shp[ak,], ak_params)
    proj4string(shp_ak) <- proj4string(shp)
    shp <- shp_ak
  } else if (all(hi)) {
    shp_hi <- fix1(shp[hi,], hi_params)
    proj4string(shp_hi) <- proj4string(shp)
    shp <- shp_hi
  } else {
    shp_ak <- fix1(shp[ak,], ak_params)
    proj4string(shp_ak) <- proj4string(shp)
    shp_hi <- fix1(shp[hi,], hi_params)
    proj4string(shp_hi) <- proj4string(shp)
    shp <- shp[!(ak | hi),]
    shp <- rbind(shp, shp_ak)
    shp <- rbind(shp, shp_hi)
  }
  return(shp)
}

shp <- load("shp.RData")

load("~/Documents/0Projects/chris_lefkovitz/201808_realestate/census/shp.RData")
projection <- "+init=epsg:2163"

shp$county <- readShapePoly(
  paste0("~/Documents/0Projects/shapefiles/Shapefiles2010/2000-county/co99_d00_shp/co99_d00.shp"), 
  proj4string=CRS("+proj=longlat"))
shp$county <- spTransform(shp$county, CRS(projection))
shp$county@data$fips <- apply(shp$county@data[, c("STATE", "COUNTY")], 1, paste, collapse="")
shp$county <- AlaskaHawaiiFix(shp$county, ak=shp$county$STATE=="02", hi=shp$county$STATE=="15")

saveRDS(shp, file="shp_tract_county_state.rds")

library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(gtools)
library(foreach)
library(doMC)
library(openxlsx)
library(glmnet)
library(stringr)
library(maps)
library(mapproj)
library(maptools)
library(sp)
library(rgdal)
library(rgeos)
library(plotrix)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/adhoc/tract_smoothing")

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans")
}

source("../../helper_functions/FindDelta.R")

################################################################################################################

source("../../helper_functions/RawACSToMarginalOcc.R")
source("../../helper_functions/PrepGeographicData.R")

geog <- readRDS("../../downloaded_data/acs/tract_data_raw_occind_2017.rds")
acs_orig <- RawACSToMarginalOcc(geog)
acs_tract <- PrepGeographicData(acs_orig)

# total size for each column
combs <- rbind(
  expand.grid(prefix=c("occ", "ind"), suffix=c("female", "male"), stringsAsFactors=FALSE), 
  expand.grid(prefix="emp", suffix=c("female_married", "female_single", "male_married", "male_single"), stringsAsFactors=FALSE))
registerDoMC(30)
raw <- foreach (i_comb = 1:nrow(combs)) %dopar% {
  prefix <- combs$prefix[i_comb]
  suffix <- combs$suffix[i_comb]
  X <- acs_orig[,grep(paste0(prefix, "__.+_", suffix, "$"), colnames(acs_orig), value=TRUE)]
  cn <- colnames(X)
  X <- rowSums(X)
  out <- data.frame(matrix(rep(X, length(cn)), nrow=length(X), byrow=FALSE))
  colnames(out) <- cn
  return(out)
}
acs_size <- bind_cols(raw)
for (i in grep("^inc__", colnames(acs_tract), value=TRUE))
  acs_size[,i] <- rowSums(acs_orig[,grep("emp__employed", colnames(acs_orig))])

geog <- readRDS("../../downloaded_data/acs/county_data_raw_2017.rds")
acs_county <- RawACSToMarginalOcc(geog)
acs_county <- PrepGeographicData(acs_county)

ys <- grep("_", colnames(acs_tract), value=TRUE)
ys <- grep("_total$", ys, invert=TRUE, value=TRUE)

adj_list <- readRDS("../tract_level/adj_list.rds")

remove(geog)
gc()

################################################################################################################
# load shapefiles

shp <- readRDS("../tract_level/shp_tract_county_state.rds")

data(us.cities)
projection <- "+init=epsg:2163"
us.cities$name <- substr(us.cities$name, 1, nchar(us.cities$name) - 3)
usc <- us.cities
usc <- usc[order(usc$pop, decreasing=TRUE),]
drop <- duplicated(round(usc[, c("lat", "long")]))
usc <- usc[!drop,]
us.cities.bubs <- SpatialPoints(coords=usc[,c("long", "lat")], proj4string=CRS("+proj=longlat"))
us.cities.bubs <- spTransform(us.cities.bubs, CRS(projection))

xlim <- c(-1900000, 2400000)
ylim <- c(-2550000, 650000)
ratio <- (xlim[2] - xlim[1]) / (ylim[2] - ylim[1])

################################################################################################################

registerDoMC(30)
raw <- foreach(y = ys) %dopar% {

  ################################################################################################################
  # smooth the tract values 
  # weighted average with it's neighbors
  # weights are 2 for original value, 1 for average of it's neighbors (weighted by population)

  x <- acs_tract[,y]
  n <- acs_size[,y]
  if (substr(y, 1, 5) == "inc__") {
    x[is.na(x) | x == Inf] <- mean(x[!(is.na(x) | x == Inf)])
  } else {
    x[is.na(x) | x == Inf] <- 0
  }
  names(x) <- names(n) <- acs_tract$fips

  registerDoMC(30)
  raw <- foreach (i = 1:length(x)) %dopar% {
    if (i %% 1000 == 1)
      message("smoothing tracts: ", y, ", ", i, " of ", length(x))

    tract <- names(x)[i]
    if (!(tract %in% names(adj_list)))
      return(x[i])

    neighbors <- adj_list[[tract]]
    neighbors <- neighbors[neighbors %in% names(x)]
    if (length(neighbors) == 0) {
      return(x[i])
    } else {
      neighbors_x <- x[neighbors]
      neighbors_n <- n[neighbors]
      neighbors_mu <- sum(neighbors_x * neighbors_n) / sum(neighbors_n)
      if (is.na(neighbors_mu))
        return(x[i])
      return((2 * x[i] + neighbors_mu) / 3)
    }
  }
  smoothed <- as.numeric(unlist(raw))

  ################################################################################################################
  # add things up to the county number

  if (substr(y, 1, 5) == "inc__") {
     corrected <- smoothed
  } else {
    county_ref <- acs_county[,y]
    names(county_ref) <- acs_county$fips
    county_ref <- county_ref[!is.na(county_ref) & county_ref != Inf]

    fips <- split(acs_tract$fips, f=substr(acs_tract$fips, 1, 5))
    x <- split(smoothed, f=substr(acs_tract$fips, 1, 5))
    n <- split(acs_size[,y], f=substr(acs_tract$fips, 1, 5))

    registerDoMC(30)
    raw <- foreach (i = 1:length(x)) %dopar% {
      if (names(x)[i] %in% names(county_ref)) {
        delta <- FindDelta(y=x[[i]], n=n[[i]], yhat=county_ref[names(x)[i]])
        out <- invlogit(logit(x[[i]]) + delta)
        names(out) <- fips[[i]]
        return(out)
      } else {
        out <- x[[i]]
        names(out) <- fips[[i]]
        return(x[[i]])      
      }
    }
    corrected <- unlist(raw)
    corrected <- as.numeric(corrected[acs_tract$fips])
  }

  saveRDS(corrected, file=paste0(y, "_corrected.rds"))

}

registerDoMC(30)
raw <- foreach(y = grep("^emp__", ys, value=TRUE)) %dopar% {

  corrected <- readRDS(paste0(y, "_corrected.rds"))

  ################################################################################################################
  # tract map, raw

  x_raw <- acs_tract[,y]
  cols <- colorRampPalette(c("dark red", "yellow", "forest green", "#00FFFCFF", "dark blue"))(101)
  qs <- quantile(x_raw, c(0.1, 0.9), na.rm=TRUE)

  dat <- data.frame(fips=acs_tract$fips, y=x_raw, stringsAsFactors=FALSE)
  dat$col <- dat$y - mean(qs)
  rng <- max(abs(qs - mean(qs)))
  dat$col <- dat$col + rng
  dat$col <- pmin(1, pmax(0, dat$col / (2 * rng)))
  dat$col <- cols[round(100 * dat$col) + 1]

  jpeg(paste0(y, "_raw.jpeg"), width=8, height=8 / ratio, res=1000, units="in")
  par(mar=c(0,0,0,0), family="PT Sans")
  plot(shp$state, xlim=xlim, ylim=ylim, border=NA)

  for (i in 1:length(shp$tract)) {
    message("mapping tracts, raw ", y, ", state ", i, " of ", length(shp$tract))
    tmp <- shp$tract[[i]]
    tmp@data$ix <- 1:nrow(tmp@data)
    tmp@data <- left_join(tmp@data, y=dat[, c("fips", "y", "col")], by="fips")
    plot(tmp, add=TRUE, border=NA, col=tmp@data$col)
  }
  text(us.cities.bubs@coords[,1], us.cities.bubs@coords[,2], usc$name, cex=0.1)
  plot(shp$state, add=TRUE)

  par(lty=0)
  labs <- paste0(formatC(round(100 * seq(qs[1], qs[2], length.out=5), 1), format="f", digits=1), "%")
  text(mean(c(500000, 2000000)), -2290000, y, cex=1)
  color.legend(xl=500000, xr=2000000, yb=-2470000, yt=-2370000, rect.col=cols, gradient="x", align="rb", legend="")
  text(seq(500000, 2000000, length.out=5), rep(-2565000, 5), labs, cex=0.8)
  par(lty=1)

  dev.off()

  ################################################################################################################
  # tract map, smoothed

  dat <- data.frame(fips=acs_tract$fips, y=corrected, stringsAsFactors=FALSE)
  dat$col <- dat$y - mean(qs)
  rng <- max(abs(qs - mean(qs)))
  dat$col <- dat$col + rng
  dat$col <- pmin(1, pmax(0, dat$col / (2 * rng)))
  dat$col <- cols[round(100 * dat$col) + 1]

  jpeg(paste0(y, "_smoothed.jpeg"), width=8, height=8 / ratio, res=1000, units="in")
  par(mar=c(0,0,0,0), family="PT Sans")
  plot(shp$state, xlim=xlim, ylim=ylim, border=NA)

  for (i in 1:length(shp$tract)) {
    message("mapping tracts, smoothed ", y, ", state ", i, " of ", length(shp$tract))
    tmp <- shp$tract[[i]]
    tmp@data$ix <- 1:nrow(tmp@data)
    tmp@data <- left_join(tmp@data, y=dat[, c("fips", "y", "col")], by="fips")
    plot(tmp, add=TRUE, border=NA, col=tmp@data$col)
  }
  text(us.cities.bubs@coords[,1], us.cities.bubs@coords[,2], usc$name, cex=0.1)
  plot(shp$state, add=TRUE)

  par(lty=0)
  labs <- paste0(formatC(round(100 * seq(qs[1], qs[2], length.out=5), 1), format="f", digits=1), "%")
  text(mean(c(500000, 2000000)), -2290000, y, cex=1)
  color.legend(xl=500000, xr=2000000, yb=-2470000, yt=-2370000, rect.col=cols, gradient="x", align="rb", legend="")
  text(seq(500000, 2000000, length.out=5), rep(-2565000, 5), labs, cex=0.8)
  par(lty=1)

  dev.off()

}

################################################################################################################

registerDoMC(30)
raw <- foreach(y = ys) %dopar% {
  return(readRDS(paste0(y, "_corrected.rds")))
}

out <- data.frame(state=acs_tract$state, fips=acs_tract$fips, bind_cols(raw), stringsAsFactors=FALSE)
colnames(out) <- c("state", "fips", ys)

if (any(!complete.cases(out)))
  stop("missing data")

saveRDS(out, file="tract_smoothing.rds")

library(maps)
library(mapproj)
library(maptools)
library(sp)
library(rgdal)
library(rgeos)
library(plotrix)
library(raster)

Map <- function(shp, geo, y, filename, 
  cols=NULL, qs=NULL, plot_cities=TRUE, cities_cex=0.1, 
  states=NULL, state_border_lwd=1, county_border_lwd=NULL, 
  smooth_tract=FALSE, adj_list=NULL, 
  plot_legend=TRUE, legend_title="", legend_digits=1, legend_labs=NULL, 
  width=NULL, height=NULL, res=500, usa_dimensions=TRUE, cores=30, 
  pop=NULL, alpha=NULL, alpha_pop_col=FALSE, qs_alpha=c(0.01, 0.5), alpha_pop_min=0.01, 
  font_family="PT Sans", state_labels=FALSE, state_labels_cex=0.75, 
  highlight_states=NULL, highlight_states_col="blue", highlight_states_lwd=2, 
  chloropleth=TRUE, bubs_size=50, bubs_alpha=0.5, add_county_borders_lwd=NULL, close_jpeg=TRUE) {

  #############################################################################################################################
  # state info

  state_from_fips <- c(
    "02"="AK", "01"="AL", "05"="AR", "04"="AZ", "06"="CA", "08"="CO", "09"="CT", "11"="DC", 
    "10"="DE", "12"="FL", "13"="GA", "15"="HI", "19"="IA", "16"="ID", "17"="IL", "18"="IN", 
    "20"="KS", "21"="KY", "22"="LA", "25"="MA", "24"="MD", "23"="ME", "26"="MI", "27"="MN", 
    "29"="MO", "28"="MS", "30"="MT", "37"="NC", "38"="ND", "31"="NE", "33"="NH", "34"="NJ", 
    "35"="NM", "32"="NV", "36"="NY", "39"="OH", "40"="OK", "41"="OR", "42"="PA", "44"="RI", 
    "45"="SC", "46"="SD", "47"="TN", "48"="TX", "49"="UT", "51"="VA", "50"="VT", "53"="WA", 
    "55"="WI", "54"="WV", "56"="WY")
  fips_from_state <- names(state_from_fips)
  names(fips_from_state) <- state_from_fips

  dat <- data.frame(fips=geo, y=y, stringsAsFactors=FALSE)
  if (all(dat$fips %in% c("DC", state.abb))) {
    dat$fips <- fips_from_state[dat$fips]
  } else {
    dat$state <- state_from_fips[as.character(substr(dat$fips, 1, 2))]
  }

  if (!is.null(pop))
    dat$n <- pop

  dat <- dat[complete.cases(dat),]

  if (!is.null(states))
    dat <- dat[dat$state %in% states,]

  #############################################################################################################################
  # colors

  if (is.null(cols))
    cols <- colorRampPalette(c("white", "lemonchiffon1", "orange2", "red", "dark red"))(101)

  if (is.null(qs))
    qs <- quantile(dat$y, c(0.025, 0.975), na.rm=TRUE)

  dat$col <- dat$y - qs[1]
  dat$col <- dat$col / (qs[2] - qs[1])
  dat$col <- pmin(1, pmax(0, dat$col))
  dat$col <- cols[round((length(cols) - 1) * dat$col) + 1]

  #############################################################################################################################
  # tract smoothing

  if (all(nchar(dat$fips) == 2)) {

    shp_main <- shp$state
    shp_main$fips <- as.character(shp_main$STATE)

  } else if (all(nchar(dat$fips) == 5)) {

    shp_main <- shp$county
  
  } else if (all(nchar(dat$fips) == 11)) {

    shp_main <- shp$tract

    if (smooth_tract) {
      if (is.null(adj_list))
        stop("no adjacency list provided for tract smoothing")
      x <- dat$y
      names(x) <- dat$fips
      registerDoMC(cores)
      raw <- foreach (i = 1:nrow(dat)) %dopar% {
        if (i %% 10000 == 1)
          message(filename, ": smoothing tracts, ", i, " of ", nrow(dat))

        tract <- names(x)[i]
        if (!(tract %in% names(adj_list)))
          return(x[i])

        neighbors <- adj_list[[tract]]
        neighbors <- neighbors[neighbors %in% names(x)]
        if (length(neighbors) == 0) {
          return(x[i])
        } else {
          neighbors_mu <- mean(x[neighbors])
          if (is.na(neighbors_mu))
            return(x[i])
          return((2 * x[i] + neighbors_mu) / 3)
        }
      }
      dat$y <- as.numeric(unlist(raw))
      dat$col <- dat$y - mean(qs)
      rng <- max(abs(qs - mean(qs)))
      dat$col <- dat$col + rng
      dat$col <- pmin(1, pmax(0, dat$col / (2 * rng)))
      dat$col <- cols[round((length(cols) - 1) * dat$col) + 1]
    }

  }

  #############################################################################################################################
  # alphas

  if (!is.null(alpha) & !is.null(pop))
    stop("both alpha and pop are null")

  if (!is.null(alpha)) {
    alpha <- sapply(alpha, function(i)
      substr(rgb(0, 0, 0, alpha=i), 8, 9))
    ok <- !is.na(dat$col)
    dat$col[ok] <- paste0(dat$col[ok], alpha[ok])
  }

  if (!is.null(pop)) {
    pdat <- left_join(shp_main@data, dat[, c("fips", "col", "n")], by="fips")
    if (alpha_pop_col) {
      pdat$alpha <- pdat$n / pdat$CENSUSAREA
      qs_alpha <- quantile(pdat$alpha, qs_alpha, na.rm=TRUE)
      pdat$alpha <- pmin(qs_alpha[2], pmax(qs_alpha[1], pdat$alpha))
      pdat$alpha <- (pdat$alpha - qs_alpha[1]) / (qs_alpha[2] - qs_alpha[1])
      pdat$alpha <- pdat$alpha * (1 - alpha_pop_min) + alpha_pop_min
      ok <- !is.na(pdat$alpha) & !is.na(pdat$col)
      pdat$alpha[ok] <- sapply(pdat$alpha[ok], function(i)
        substr(rgb(0, 0, 0, alpha=i), 8, 9))
      pdat$col[ok] <- paste0(pdat$col[ok], pdat$alpha[ok])
    }
  } else {
    pdat <- left_join(shp_main@data, dat[, c("fips", "col")], by="fips")
  }

  #############################################################################################################################
  # plot

  if (!is.null(states)) {
    shp$state@data$state <- state_from_fips[as.character(shp$state@data$STATE)]
    shp_main@data$state <- state_from_fips[as.character(shp_main@data$STATE)]

    shp_outline <- shp$state[shp$state@data$state %in% states,]
    ok <- shp_main@data$state %in% states
    shp_main <- shp_main[ok,]
    pdat <- pdat[ok,]

    bb <- bbox(shp_outline)
    xlim <- bb[1,]
    ylim <- bb[2,]
  } else {
    shp_outline <- shp$state
  }

  if (usa_dimensions & (!is.null(width) | !is.null(height))) {
    stop("input problem: can't input both usa_dimensions and width/height")
  } else if ((!is.null(width) & is.null(height)) | (is.null(width) & !is.null(height))) {
    stop("input problem: have to input width + height or neither")
  } else if (usa_dimensions) {
    xlim <- c(-1900000, 2400000)
    ylim <- c(-2550000, 650000)
    ratio <- (xlim[2] - xlim[1]) / (ylim[2] - ylim[1])
    width <- 8
    height <- 8 / ratio    
  } else if (is.null(width)) {
    width <- 10
    height <- width * (ylim[2] - ylim[1]) / (xlim[2] - xlim[1])
  }

  jpeg(filename, width=width, height=height, res=res, units="in")
  par(mar=c(0,0,0,0), family=font_family)

  plot(shp_outline, xlim=xlim, ylim=ylim, border=NA)

  if (chloropleth) {
    plot(shp_main, border=NA, add=TRUE, col=pdat$col)
  } else {
    cents <- t(sapply(1:nrow(shp_main@data), function(i) gCentroid(shp_main[i,])@coords))
    ok <- !is.na(pdat$col)
    pdat$col[ok] <- paste0(pdat$col[ok], substr(rgb(0, 0, 0, alpha=bubs_alpha), 8, 9))
    symbols(cents[,1], cents[,2], 
      circles=bubs_size * sqrt(pdat$n), inch=FALSE, 
      add=TRUE, fg=NA, bg=pdat$col)
  }
  
  if (plot_cities) {
    data(us.cities)
    projection <- "+init=epsg:2163"
    us.cities$name <- substr(us.cities$name, 1, nchar(us.cities$name) - 3)
    usc <- us.cities
    usc <- usc[order(usc$pop, decreasing=TRUE),]
    drop <- duplicated(round(usc[, c("lat", "long")]))
    usc <- usc[!drop,]
    if (!is.null(states))
      usc <- usc[usc$country.etc %in% states,]
    us.cities.bubs <- SpatialPoints(coords=usc[,c("long", "lat")], proj4string=CRS("+proj=longlat"))
    us.cities.bubs <- spTransform(us.cities.bubs, CRS(projection))
    text(us.cities.bubs@coords[,1], us.cities.bubs@coords[,2], usc$name, cex=cities_cex)
  }

  if (!is.null(add_county_borders_lwd)) {
    plot(shp$county, add=TRUE, lwd=add_county_borders_lwd)
  }

  if (state_labels) {
    shp$state@data$labels <- state_from_fips[as.character(shp$state@data$STATE)]
    cents <- t(sapply(state.abb, function(i) {
      ok <- which(!is.na(shp$state@data$labels) & shp$state@data$labels == i)
      ok <- ok[which.max(shp$state@data$AREA[ok])]
      return(gCentroid(shp$state[ok,])@coords)
    }))
    text(cents[,1], cents[,2], state.abb, cex=state_labels_cex)
  }

  if (state_border_lwd > 0)
    plot(shp_outline, add=TRUE, lwd=state_border_lwd)

  if (!is.null(highlight_states)) {
    shp$state@data$labels <- state_from_fips[as.character(shp$state@data$STATE)]
    shp_highlight <- aggregate(shp$state[shp$state@data$labels %in% highlight_states,])
    plot(shp_highlight, add=TRUE, lwd=highlight_states_lwd, border=highlight_states_col)
  }

  if (plot_legend) {
    par(lty=0)
    if (is.null(legend_labs))
      legend_labs <- paste0(formatC(100 * seq(qs[1], qs[2], length.out=5), format="f", digits=legend_digits), "%")
    text(mean(c(500000, 2000000)), -2290000, legend_title, cex=1)
    color.legend(xl=500000, xr=2000000, yb=-2470000, yt=-2370000, rect.col=cols, gradient="x", align="rb", legend="")
    text(seq(500000, 2000000, length.out=length(legend_labs)), rep(-2565000, length(legend_labs)), legend_labs, cex=0.8)
    par(lty=1)
  }

  if (close_jpeg)
    dev.off()

}

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

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################

total_pop <- "B12006_001"
labor_force <- c("B12006_004", "B12006_009", "B12006_015", "B12006_020", "B12006_026", "B12006_031", "B12006_037", "B12006_042", "B12006_048", "B12006_053")
employed <- c("B12006_005", "B12006_010", "B12006_016", "B12006_021", "B12006_027", "B12006_032", "B12006_038", "B12006_043", "B12006_049", "B12006_054")
variables <- c(total_pop, labor_force, employed)
states <- unique(c("DC", state.abb))
years <- 2018:2009
combs <- expand.grid(state=states, year=years, variable=variables, stringsAsFactors=FALSE)

registerDoMC(4)
print(system.time(raw <- foreach (i = 1:nrow(combs), .errorhandling="pass") %dopar% {
  # if (i %% 100 == 1)
    message(i, " of ", nrow(combs))
  tmp <- get_acs(
    geography="tract", 
    variables=combs$variable[i], 
    year=combs$year[i], 
    state=combs$state[i])
  return(data.frame(state=combs$state[i], year=combs$year[i], tmp, stringsAsFactors=FALSE))
}))

full <- bind_rows(raw)

dat <- sqldf(paste0("
  select 
  state, year, geoid as fips, 
  sum(case when variable in ('", paste0(total_pop, collapse="','"), "') then estimate else 0 end) as pop_16plus, 
  sum(case when variable in ('", paste0(labor_force, collapse="','"), "') then estimate else 0 end) as laborforce, 
  sum(case when variable in ('", paste0(employed, collapse="','"), "') then estimate else 0 end) as employed
  from full
  group by 1, 2, 3
"))

################################################################################################################

load("shp.RData")

cols <- colorRampPalette(c("darkorchid4", "white", "forest green"))(101)

dat$y <- 1 - dat$employed / dat$laborforce
qs <- quantile(dat$y, c(0.025, 0.5, 0.975), na.rm=TRUE)
dat$col <- dat$y - qs[2]
rng <- max(abs(qs - qs[2]))
dat$col <- dat$col + rng
dat$col <- pmin(1, pmax(0, dat$col / (2 * rng)))
dat$col <- cols[round(100 * dat$col) + 1]

xlim <- c(-1900000, 2400000)
ylim <- c(-2550000, 650000)
ratio <- (xlim[2] - xlim[1]) / (ylim[2] - ylim[1])

jpeg(paste0("unemployment_rate_2018.jpeg"), width=8, height=8 / ratio, res=300, units="in")
par(mar=c(0,0,0,0))
plot(shp$state, xlim=xlim, ylim=ylim, border=NA)
for (i in 1:length(shp$tract)) {
  message("mapping tracts, state ", i, " of ", length(shp$tract))
  tmp <- shp$tract[[i]]
  tmp@data$ix <- 1:nrow(tmp@data)
  tmp@data <- left_join(tmp@data, y=dat[, c("fips", "y", "col")], by="fips")
  plot(tmp, add=TRUE, border=NA, col=tmp@data$col)
}
plot(shp$state, add=TRUE)
dev.off()

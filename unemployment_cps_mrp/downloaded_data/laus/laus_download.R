library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(doMC)
library(foreach)
library(tidyr)
library(stringr)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd(ifelse(Sys.info()[['sysname']]=="Darwin", 
  "~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/downloaded_data/laus",
  "~/unemployment/unemployment_cps_mrp/downloaded_data/laus"))

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################

series <- fread("https://download.bls.gov/pub/time.series/la/la.series", data.table=FALSE)

################################################################################################################
# county data

dat <- fread("https://download.bls.gov/pub/time.series/la/la.data.64.County", data.table=FALSE)
dat$fips <- substr(dat$series_id, 6, 10)
dat$value <- as.numeric(dat$value)
dat$month <- paste0(dat$year, "_", gsub("M", "", dat$period))
dat$type <- as.numeric(substr(dat$series_id, 20, 20))
dat <- dat[dat$type %in% 4:6,]
dat$type <- c(NA, NA, NA, "unemployed", "employed", "laborforce")[dat$type]

pivot <- data.frame(
  dat[, c("fips", "month", "type", "value")] %>% 
  pivot_wider(names_from=type, values_from=value))
pivot <- pivot[, c("fips", "month", "laborforce", "employed", "unemployed")]
pivot <- pivot[complete.cases(pivot),]

saveRDS(pivot, file="laus.rds")

################################################################################################################

dat <- fread("https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS", data.table=FALSE)

dat <- left_join(dat, series[, c("series_id", "series_title")])
dat$fips <- substr(dat$series_id, 6, 7)
state_from_fips <- c(
  "02"="AK", "01"="AL", "05"="AR", "04"="AZ", "06"="CA", "08"="CO", "09"="CT", "11"="DC", 
  "10"="DE", "12"="FL", "13"="GA", "15"="HI", "19"="IA", "16"="ID", "17"="IL", "18"="IN", 
  "20"="KS", "21"="KY", "22"="LA", "25"="MA", "24"="MD", "23"="ME", "26"="MI", "27"="MN", 
  "29"="MO", "28"="MS", "30"="MT", "37"="NC", "38"="ND", "31"="NE", "33"="NH", "34"="NJ", 
  "35"="NM", "32"="NV", "36"="NY", "39"="OH", "40"="OK", "41"="OR", "42"="PA", "44"="RI", 
  "45"="SC", "46"="SD", "47"="TN", "48"="TX", "49"="UT", "51"="VA", "50"="VT", "53"="WA", 
  "55"="WI", "54"="WV", "56"="WY")
dat$state <- state_from_fips[dat$fips]

dat$value <- as.numeric(dat$value)
dat$month <- paste0(dat$year, "_", gsub("M", "", dat$period))
dat$type <- as.numeric(substr(dat$series_id, 20, 20))
dat <- dat[dat$type %in% 4:6,]
dat$type <- c(NA, NA, NA, "unemployed", "employed", "laborforce")[dat$type]

pivot <- data.frame(
  dat[, c("state", "fips", "month", "type", "value")] %>% 
  pivot_wider(names_from=type, values_from=value))
pivot <- pivot[, c("state", "fips", "month", "laborforce", "employed", "unemployed")]
pivot <- pivot[complete.cases(pivot),]

saveRDS(pivot, file="laus_state.rds")

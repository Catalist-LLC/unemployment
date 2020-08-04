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
  "~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/downloaded_data/cps_timeseries",
  "~/unemployment/unemployment_cps_mrp/downloaded_data/cps_timeseries"))

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################

files <- c(
  "absn", 
  "activity", 
  "ages", 
  "born", 
  "cert", 
  "chld", 
  "class", 
  "data.1.AllData", 
  "disa", 
  "duration", 
  "education", 
  "entr", 
  "expr", 
  "footnote", 
  "hheader", 
  "hour", 
  "indy", 
  "jdes", 
  "lfst", 
  "look", 
  "mari", 
  "mjhs", 
  "occupation", 
  "orig", 
  "pcts", 
  "periodicity", 
  "race", 
  "rjnw", 
  "rnlf", 
  "rwns", 
  "seasonal", 
  "seek", 
  "series", 
  "sexs", 
  "tdat", 
  "vets", 
  "wkst")

registerDoMC(4)
orig <- foreach(i = files) %dopar% {
  return(fread(paste0("https://download.bls.gov/pub/time.series/ln/ln.", i), data.table=FALSE))
}
names(orig) <- files

################################################################################################################

dat <- orig$data.1.AllData
dat <- dat[substr(dat$period, 1, 1) == "M",]
dat$period <- as.numeric(gsub("^M", "", dat$period))
colnames(dat)[colnames(dat) == "period"] <- "month"

saveRDS(dat, file="cps_timeseries.rds")

series <- orig$series
colnames(series) <- gsub("_code", "", colnames(series))
codes <- colnames(series)[colnames(series) %in% names(orig)]
for (i_code in 1:length(codes)) {
  tmp <- orig[[codes[i_code]]]
  lookup <- tmp[, paste0(codes[i_code], "_text")]
  names(lookup) <- tmp[, paste0(codes[i_code], "_code")]
  series[,codes[i_code]] <- lookup[as.character(series[,codes[i_code]])]
}

series[series == "N/A"] <- "--"
write.table(series, file=pipe("pbcopy"), sep="\t", row.names=FALSE, na="--")

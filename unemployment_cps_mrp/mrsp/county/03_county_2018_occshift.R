library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(tidyr)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/mrsp/county")

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans")
}

source("../../helper_functions/FindDelta.R")

################################################################################################################
# get targets

source("../../helper_functions/RawACSToMarginalOcc.R")
source("../../helper_functions/PrepGeographicData.R")

geog <- readRDS("../../downloaded_data/acs/county_data_raw_2017.rds")
marg <- RawACSToMarginalOcc(geog)
target <- PrepGeographicData(marg)

################################################################################################################
# load joint data

joint <- readRDS("county_2018_joint.rds")
occscore <- readRDS("county_2018_occscore.rds")

fipss <- sort(unique(joint$fips))
fipss <- fipss[fipss %in% target$fips]
target <- target[target$fips %in% fipss,]

################################################################################################################
# occ / ind by gender
# emp by gender / marital

joint$ix <- 1:nrow(joint)
suffixs <- c("female", "male", "female_married", "female_single", "male_married", "male_single")
raw <- foreach (suffix = suffixs) %do% {
  X <- target[, c("fips", grep(paste0("_", suffix, "$"), colnames(target), value=TRUE))]
  colnames(X) <- gsub(paste0("_", suffix, "$"), "", colnames(X))
  if (suffix %in% c("female", "male")) {
    ok_rows <- joint$female == as.numeric(suffix == "female")
  } else {
    ok_rows <- 
      joint$female == as.numeric(suffix %in% c("female_married", "female_single")) & 
      joint$married == as.numeric(suffix %in% c("female_married", "male_married"))
  }
  dat <- left_join(joint[ok_rows, c("ix", "n", "fips")], X, by="fips")

  ys <- grep("__", colnames(dat), value=TRUE)
  ns <- split(x=dat$n, f=dat$fips)
  registerDoMC(length(ys))
  deltas <- foreach(y = ys) %dopar% {
    yref <- X[,y]
    names(yref) <- X$fips
    xs <- split(x=dat[,y], f=dat$fips)
    deltas <- sapply(1:length(fipss), function(i) {
      if (i %% 1000 == 1)
        message("getting delta: ", suffix, ", ", y, ", ", i, " of ", length(fipss))
      FindDelta(y=xs[[fipss[i]]], n=ns[[fipss[i]]], yhat=yref[fipss[i]])
    })
    names(deltas) <- fipss
    return(deltas)
  }
  names(deltas) <- ys

  registerDoMC(length(ys))
  raw_corrected <- foreach(y = ys) %dopar% {
    correction <- deltas[[y]][dat$fips]
    return(round(invlogit(logit(dat[,y]) + correction), 3))
  }
  names(raw_corrected) <- ys
  out <- data.frame(suffix=suffix, ix=dat$ix, bind_cols(raw_corrected), stringsAsFactors=FALSE)

  return(out)
}
names(raw) <- suffixs

out <- bind_rows(raw$female, raw$male)
out <- out[order(out$ix),]
out <- out[, !(colnames(out) %in% c("suffix", "ix"))]

tmp <- bind_rows(raw$female_married, raw$female_single, raw$male_married, raw$male_single)
tmp$emp__laborforce <- 1 - tmp$emp__notinlaborforce
tmp <- tmp[order(tmp$ix),]
tmp <- tmp[, !(colnames(tmp) %in% c("suffix", "ix"))]
out <- cbind(out, tmp)

gc()

################################################################################################################
# inc by age

  # case when age < 18 then 1
  #      when age < 20 then 2
  #      when age < 25 then 3
  #      when age < 30 then 4
  #      when age < 35 then 5
  #      when age < 40 then 6
  #      when age < 45 then 7
  #      when age < 50 then 8
  #      when age < 55 then 9
  #      when age < 60 then 10
  #      when age < 65 then 11
  #      when age < 70 then 12
  #      when age < 75 then 13
  #      when age >= 75 then 14 else NULL end as agegrp,

lookups <- c(
  "under25", "under25", "under25", 
  "25to44", "25to44", "25to44", "25to44", 
  "45to64", "45to64", "45to64", "45to64", 
  "65plus", "65plus", "65plus")
inc <- NULL
for (i in 1:length(lookups))
  inc <- rbind(inc, data.frame(fips=target$fips, agegrp=i, inc=target[, paste0("inc__", lookups[i])], stringsAsFactors=FALSE))

tmp <- left_join(joint[, c("fips", "agegrp")], inc)
out$inc_hh <- tmp$inc

gc()

################################################################################################################
# save

if (any(!complete.cases(out)))
  stop("missing data")
colnames(out) <- gsub("__", "_", colnames(out))

saveRDS(out, file="county_2018_occshift.rds")

################################################################################################################
# how much did it change?

WRMSE <- function(x, y, n) {
  errors <- (x - y)
  wrmse <- sqrt(sum(errors^2 * n) / sum(n))
  return(wrmse)
}

ys <- colnames(out)
ys <- ys[ys %in% colnames(occscore)]

registerDoMC(length(ys))
raw <- foreach(y = ys) %dopar% {
  c("wcorr"=cov.wt(cbind(out[,y], occscore[,y]), cor=TRUE)$cor[2], 
    "wrmse"=WRMSE(out[,y], occscore[,y], joint$n), 
    "avg_x"=sum(occscore[,y] * joint$n) / sum(joint$n), 
    "avg_y"=sum(out[,y] * joint$n) / sum(joint$n))
}
stats <- data.frame(t(sapply(raw, function(i) i)))
rownames(stats) <- ys
print(stats[order(stats$wcorr),])

################################################################################################################

CheckDelta <- function(dat) {

  dat$fips <- joint$fips
  dat$n <- joint$n
  dat$married <- joint$married
  dat$female <- joint$female
  # if (!("emp_notinlaborforce" %in% colnames(dat)))
    dat$emp_notinlaborforce <- 1 - dat$emp_laborforce

  for (suffix in suffixs) {
    message(suffix)
    X <- target[, c("fips", grep(paste0("_", suffix, "$"), colnames(target), value=TRUE))]

    if (suffix %in% c("female", "male")) {
      ok_rows <- dat$female == as.numeric(suffix == "female")
    } else {
      ok_rows <- 
        dat$female == as.numeric(suffix %in% c("female_married", "female_single")) & 
        dat$married == as.numeric(suffix %in% c("female_married", "male_married"))
    }
    ys <- colnames(X)[colnames(X) != "fips"]

    tmp <- dat[ok_rows,]
    for (y in ys)
      tmp[,y] <- tmp$n * tmp[, gsub("__", "_", gsub(paste0("_", suffix, "$"), "", y))]
    tmp <- data.frame(tmp %>% group_by(fips) %>% summarize_at(c("n", ys), sum))
    for (y in ys)
      tmp[,y] <- tmp[,y] / tmp$n

    tmp <- inner_join(tmp, X, by="fips")

    for (y in ys) {
      message("  ", y)
      plot(tmp[, paste0(y, ".x")], tmp[, paste0(y, ".y")], pch=20, 
        xlim=c(0,1), ylim=c(0,1), 
        xlab="", ylab="", main=substr(y, 1, 25), axes=FALSE)
      box(lwd=par()$lwd)
      abline(a=0, b=1, col="grey")
      abline(h=0.5, col="grey")
      abline(v=0.5, col="grey")
    }

  }
}

jpeg("check_delta_before.jpeg", width=20, height=24, units="in", res=300)
par(mfrow=c(9,9), mar=c(1,1,3,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans")
CheckDelta(dat=occscore)
dev.off()

jpeg("check_delta_after.jpeg", width=20, height=24, units="in", res=300)
par(mfrow=c(9,9), mar=c(1,1,3,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans")
CheckDelta(dat=out)
dev.off()

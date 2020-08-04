library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(tidyr)
library(glmnet)
library(stringr)
library(RJDBC)
library(gtools)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/run_mrp/")

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans")
}

################################################################################################################
# surveys

joint_level <- "tract"
check_corrected <- TRUE

surveys <- system(paste0("ls model_save/yhat_", joint_level, "_*.rds"), intern=TRUE)
surveys <- sort(unique(substr(gsub(paste0(".+yhat_", joint_level, "_"), "", surveys), 1, 7)))

################################################################################################################
# load requisite data

joint_orig <- readRDS(paste0("../mrsp/", joint_level, "/", joint_level, "_2018_joint.rds"))
joint_orig <- joint_orig[, colnames(joint_orig) != "n"]

laus_orig <- readRDS("../downloaded_data/laus/laus.rds")
laus_county_surveys <- table(laus_orig$month)
laus_county_surveys <- names(which(laus_county_surveys > 3000))

laus_orig_state <- readRDS("../downloaded_data/laus/laus_state.rds")
laus_state_surveys <- table(laus_orig_state$month)
laus_state_surveys <- names(which(laus_state_surveys > 50))

cps_orig <- system("ls data/cps*.rds", intern=TRUE)
registerDoMC(length(cps_orig))
cps_orig <- foreach(i = cps_orig) %dopar% {
  readRDS(i)
}
cps_orig <- bind_rows(cps_orig)
cps_orig <- cps_orig[cps_orig$survey %in% surveys,]

cps_timeseries <- readRDS("../downloaded_data/cps_timeseries/cps_timeseries.rds")
cps_timeseries$survey <- paste0(cps_timeseries$year, "_", str_pad(cps_timeseries$month, width=2, pad="0"))
cps_timeseries <- cps_timeseries[cps_timeseries$survey %in% surveys,]
cps_timeseries$value <- as.numeric(cps_timeseries$value)

xs_binary <- c("female", "married", "citizen")
xs_cat <- c("region", "state", "agegrp", "race", "edu", "whitecollege", "whitefemale", "marriedfemale")
xs <- c(xs_binary, xs_cat)
xs <- xs[!(xs %in% c("region", "state"))]

inters <- rbind(
  data.frame(V1=xs, V2=NA, stringsAsFactors=FALSE), 
  as.data.frame(combinations(n=length(xs), r=2, v=xs), stringsAsFactors=FALSE))
colnames(inters) <- c("x1", "x2")

gc()

################################################################################################################
# get all the comparison data

raw <- list()
for (survey in surveys) {
# raw <- foreach(survey = surveys) %do% {

  message(survey)

  ####################################################################
  # demographic check

  # cps
  cps <- cps_orig[cps_orig$survey == survey,]
  registerDoMC(45)
  cps_agg <- foreach (i_inter = 1:nrow(inters)) %dopar% {
    x1 <- inters$x1[i_inter]
    x2 <- inters$x2[i_inter]
    cps$cat1 <- x1
    cps$x1 <- cps[,x1]
    if (is.na(x2)) {
      cps$cat2 <- NA
      cps$x2 <- NA
    } else {
      cps$cat2 <- x2
      cps$x2 <- cps[,x2]
    }
    out <- data.frame(
      cps %>% 
      group_by(cat1, cat2, x1, x2) %>% 
      summarize_at(grep("^n_", colnames(cps)), sum, na.rm=TRUE), 
      stringsAsFactors=FALSE)
    out$x1 <- as.character(out$x1)
    out$cat1 <- as.character(out$cat1)
    out$x2 <- as.character(out$x2)
    out$cat2 <- as.character(out$cat2)
    return(out)
  }
  cps_agg <- bind_rows(cps_agg)
  cps_agg$cps_n <- cps_agg$n_cnip
  cps_agg$cps_laborforce <- cps_agg$n_laborforce / cps_agg$n_cnip
  cps_agg$cps_employed <- cps_agg$n_employed / cps_agg$n_laborforce
  cps_agg$cps_atwork <- cps_agg$n_atwork / cps_agg$n_employed

  # mrp
  if (check_corrected & survey %in% laus_county_surveys) {
    corrected_filename <- paste0("model_save/yhat_", joint_level, "correctedcounty_", survey, ".rds")
  } else if (check_corrected & survey %in% laus_state_surveys) {
    corrected_filename <- paste0("model_save/yhat_", joint_level, "correctedstate_", survey, ".rds")
  } else {
    corrected_filename <- paste0("model_save/yhat_", joint_level, "_", survey, ".rds")
  }
  yhat <- readRDS(corrected_filename)
  colnames(yhat) <- gsub(paste0("_", survey), "", colnames(yhat))
  joint <- data.frame(joint_orig, yhat, stringsAsFactors=FALSE)

  registerDoMC(45)
  tract_agg <- foreach (i_inter = 1:nrow(inters)) %dopar% {
    x1 <- inters$x1[i_inter]
    x2 <- inters$x2[i_inter]
    joint$cat1 <- x1
    joint$x1 <- joint[,x1]
    if (is.na(x2)) {
      joint$cat2 <- NA
      joint$x2 <- NA
    } else {
      joint$cat2 <- x2
      joint$x2 <- joint[,x2]
    }
    out <- data.frame(
      joint %>% 
      group_by(cat1, cat2, x1, x2) %>% 
      summarize_at(c("n", "laborforce", "employed", "atwork"), sum, na.rm=TRUE), 
      stringsAsFactors=FALSE)
    out$x1 <- as.character(out$x1)
    out$cat1 <- as.character(out$cat1)
    out$x2 <- as.character(out$x2)
    out$cat2 <- as.character(out$cat2)
    return(out)
  }
  tract_agg <- bind_rows(tract_agg)
  tract_agg$mrp_n <- tract_agg$n
  tract_agg$mrp_laborforce <- tract_agg$laborforce / tract_agg$n
  tract_agg$mrp_employed <- tract_agg$employed / tract_agg$laborforce
  tract_agg$mrp_atwork <- tract_agg$atwork / tract_agg$employed

  demographics <- inner_join(
    cps_agg[, c("cat1", "cat2", "x1", "x2", "cps_n", "cps_laborforce", "cps_employed", "cps_atwork")], 
    tract_agg[, c("cat1", "cat2", "x1", "x2", "mrp_n", "mrp_laborforce", "mrp_employed", "mrp_atwork")], 
    by=c("cat1", "cat2", "x1", "x2"))

  ####################################################################
  # laus check

  if (survey %in% laus_county_surveys) {

    yhat <- readRDS(paste0("model_save/yhat_", joint_level, "_", survey, ".rds"))
    colnames(yhat) <- gsub(paste0("_", survey), "", colnames(yhat))
    joint <- data.frame(joint_orig, yhat, stringsAsFactors=FALSE)

    joint$fips <- substr(joint$fips, 1, 5)
    county <- data.frame(
      joint %>% 
      group_by(fips) %>% 
      summarize_at(c("n", "laborforce", "employed"), sum), 
      stringsAsFactors=FALSE)
    colnames(county) <- c("fips", "mrp_n", "mrp_laborforce", "mrp_employed")
    county$mrp_employed <- county$mrp_employed / county$mrp_laborforce
    county$mrp_laborforce <- county$mrp_laborforce / county$mrp_n

    laus <- laus_orig[laus_orig$month == survey, c("fips", "laborforce", "employed")]
    colnames(laus) <- c("fips", "laus_laborforce", "laus_employed")
    laus$laus_employed <- laus$laus_employed / laus$laus_laborforce
    county <- inner_join(county, laus, by="fips")
    county$col <- rgb(0, 0, 0, alpha=0.25)

    if (check_corrected) {
      if (survey %in% laus_county_surveys) {
        corrected_filename <- paste0("model_save/yhat_", joint_level, "correctedcounty_", survey, ".rds")
      } else {
        corrected_filename <- paste0("model_save/yhat_", joint_level, "correctedstate_", survey, ".rds")
      }
      yhat <- readRDS(corrected_filename)
      colnames(yhat) <- gsub(paste0("_", survey), "", colnames(yhat))
      joint <- data.frame(joint_orig, yhat, stringsAsFactors=FALSE)

      joint$fips <- substr(joint$fips, 1, 5)
      county_corrected <- data.frame(
        joint %>% 
        group_by(fips) %>% 
        summarize_at(c("n", "laborforce", "employed"), sum), 
        stringsAsFactors=FALSE)
      colnames(county_corrected) <- c("fips", "mrp_n", "mrp_laborforce", "mrp_employed")
      county_corrected$mrp_employed <- county_corrected$mrp_employed / county_corrected$mrp_laborforce
      county_corrected$mrp_laborforce <- county_corrected$mrp_laborforce / county_corrected$mrp_n

      laus <- laus_orig[laus_orig$month == survey, c("fips", "laborforce", "employed")]
      colnames(laus) <- c("fips", "laus_laborforce", "laus_employed")
      laus$laus_employed <- laus$laus_employed / laus$laus_laborforce
      county_corrected <- inner_join(county_corrected, laus, by="fips")

    } else {
      county_corrected <- NULL    
    }

  } else {

    yhat <- readRDS(paste0("model_save/yhat_", joint_level, "_", survey, ".rds"))
    colnames(yhat) <- gsub(paste0("_", survey), "", colnames(yhat))
    joint <- data.frame(joint_orig, yhat, stringsAsFactors=FALSE)

    joint$fips <- substr(joint$fips, 1, 5)
    county <- data.frame(
      joint %>% 
      group_by(fips) %>% 
      summarize_at(c("n", "laborforce", "employed"), sum), 
      stringsAsFactors=FALSE)
    colnames(county) <- c("fips", "mrp_n", "mrp_laborforce", "mrp_employed")
    county$mrp_employed <- county$mrp_employed / county$mrp_laborforce
    county$mrp_laborforce <- county$mrp_laborforce / county$mrp_n

    survey_month <- as.numeric(gsub(".+_", "", survey))
    survey_year <- as.numeric(gsub("_.+", "", survey))
    if (survey_month == 1) {
      laus <- laus_orig[laus_orig$month == paste0(survey_year - 1, "_12"), c("fips", "laborforce", "employed")]
    } else {
      laus <- laus_orig[laus_orig$month == paste0(survey_year, "_", str_pad(survey_month - 1, width=2, pad="0")), c("fips", "laborforce", "employed")]
    }
    colnames(laus) <- c("fips", "laus_laborforce", "laus_employed")
    laus$laus_employed <- laus$laus_employed / laus$laus_laborforce
    county <- inner_join(county, laus, by="fips")
    county$col <- rgb(0.5, 0.5, 0.5, alpha=0.25)

    county_corrected <- NULL    
  }

  ####################################################################
  # check against time series

  tmp <- cps_timeseries[cps_timeseries$survey == survey,]
  timeseries_check <- rbind(
    data.frame(
      type="mrp", 
      cnip=sum(joint$n) / 1e6, 
      laborforce=sum(joint$laborforce) / sum(joint$n), 
      unemployed=1 - sum(joint$employed) / sum(joint$laborforce), 
      notatwork=1 - sum(joint$atwork) / sum(joint$laborforce), 
      stringsAsFactors=FALSE), 
    data.frame(
      type="cps", 
      cnip=tmp$value[tmp$series_id == "LNU00000000"] / 1000,  
      laborforce=tmp$value[tmp$series_id == "LNU01000000"] / tmp$value[tmp$series_id == "LNU00000000"], 
      unemployed=1 - tmp$value[tmp$series_id == "LNU02000000"] / tmp$value[tmp$series_id == "LNU01000000"], 
      notatwork=NA, 
      stringsAsFactors=FALSE))
  timeseries_check <- rbind(timeseries_check, 
    data.frame(
      type="diff", 
      cnip=1 - timeseries_check$cnip[1] / timeseries_check$cnip[2], 
      laborforce=timeseries_check$laborforce[1] - timeseries_check$laborforce[2], 
      unemployed=timeseries_check$unemployed[1] - timeseries_check$unemployed[2], 
      notatwork=NA, 
      stringsAsFactors=FALSE))

  # return(list(demographics=demographics, county=county, county_corrected=county_corrected, timeseries_check=timeseries_check))
  raw[[survey]] <- list(demographics=demographics, county=county, county_corrected=county_corrected, timeseries_check=timeseries_check)

}
names(raw) <- surveys

################################################################################################################
# plots

message("diffs between MRP and CPS Time Series (Not Seasonally Adjusted): ")
diffs <- data.frame(t(sapply(raw, function(i) unlist(i$timeseries_check[3, 2:4]))))
for (i in colnames(diffs))
  diffs[,i] <- round(abs(diffs[,i]), 4)
print(diffs)

registerDoMC(30)
foreach (survey = surveys) %dopar% {

  jpeg(paste0("output_plots/cps_check_", survey, ".jpeg"), width=20, height=4, units="in", res=300)
  par(mfrow=c(1,7), mar=c(2.5,2.5,3,1), mgp=c(1.5,0.1,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans", 
    cex.main=0.8, cex.lab=0.7, cex.axis=0.6)

  dat <- raw[[survey]]$county
  if (is.null(dat)) {
    for (i in 1:2)
      plot(0, 0, type="n", xlab="", ylab="", axes=FALSE)
  } else {

    dat$laus_laborforce <- dat$laus_laborforce / dat$mrp_n

    rng <- 100
    plot(pmin(rng, 100 * dat$mrp_laborforce), pmin(rng, 100 * dat$laus_laborforce), 
      cex=sqrt(dat$mrp_n) / 500, pch=20, col=dat$col, 
      xlim=c(0, rng), ylim=c(0, rng), xaxs="i", yaxs="i", 
      xlab="MRP", ylab="LAUS", main=paste0("Labor Force, County:\n", survey, " (MRP = ", joint_level, ")"), axes=FALSE)
    axis(1, lwd=par()$lwd)
    axis(2, lwd=par()$lwd)
    abline(a=0, b=1, col="red")
    abline(h=50, col="red")
    abline(v=50, col="red")
    box(lwd=par()$lwd)

    rng <- 50
    plot(pmin(rng, 100 - 100 * dat$mrp_employed), pmin(rng, 100 - 100 * dat$laus_employed), 
      cex=sqrt(dat$mrp_n) / 500, pch=20, col=dat$col, 
      xlim=c(0, rng), ylim=c(0, rng), xaxs="i", yaxs="i", 
      xlab="MRP", ylab="LAUS", main=paste0("Unemployment, County:\n", survey, " (MRP = ", joint_level, ")"), axes=FALSE)
    axis(1, lwd=par()$lwd)
    axis(2, lwd=par()$lwd)
    abline(a=0, b=1, col="red")
    abline(h=50, col="red")
    abline(v=50, col="red")
    box(lwd=par()$lwd)

  }

  ####################################

  dat <- raw[[survey]]$county_corrected
  if (is.null(dat)) {
    for (i in 1:2)
      plot(0, 0, type="n", xlab="", ylab="", axes=FALSE)
  } else {

    dat$laus_laborforce <- dat$laus_laborforce / dat$mrp_n

    rng <- 100
    plot(pmin(rng, 100 * dat$mrp_laborforce), pmin(rng, 100 * dat$laus_laborforce), 
      cex=sqrt(dat$mrp_n) / 500, pch=20, col=rgb(0, 0, 0, alpha=0.25), 
      xlim=c(0, rng), ylim=c(0, rng), xaxs="i", yaxs="i", 
      xlab="MRP", ylab="LAUS", main=paste0("Labor Force, County (Corrected) :\n", survey, " (MRP = ", joint_level, ")"), axes=FALSE)
    axis(1, lwd=par()$lwd)
    axis(2, lwd=par()$lwd)
    abline(a=0, b=1, col="red")
    abline(h=50, col="red")
    abline(v=50, col="red")
    box(lwd=par()$lwd)

    rng <- 50
    plot(pmin(rng, 100 - 100 * dat$mrp_employed), pmin(rng, 100 - 100 * dat$laus_employed), 
      cex=sqrt(dat$mrp_n) / 500, pch=20, col=rgb(0, 0, 0, alpha=0.25), 
      xlim=c(0, rng), ylim=c(0, rng), xaxs="i", yaxs="i", 
      xlab="MRP", ylab="LAUS", main=paste0("Unemployment, County (Corrected) :\n", survey, " (MRP = ", joint_level, ")"), axes=FALSE)
    axis(1, lwd=par()$lwd)
    axis(2, lwd=par()$lwd)
    abline(a=0, b=1, col="red")
    abline(h=50, col="red")
    abline(v=50, col="red")
    box(lwd=par()$lwd)

  }

  ####################################

  dat <- raw[[survey]]$demographics

  rng <- 100
  plot(pmin(rng, 100 * dat$mrp_laborforce), pmin(rng, 100 * dat$cps_laborforce), 
    cex=sqrt(dat$cps_n) / 5000, pch=20, col=rgb(0, 0, 0, alpha=0.25), 
    xlim=c(0, rng), ylim=c(0, rng), xaxs="i", yaxs="i", 
    xlab="MRP", ylab="CPS", main=paste0("Labor Force, Demographics:\n", survey, " (MRP = ", joint_level, ")"), axes=FALSE)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  abline(a=0, b=1, col="red")
  abline(h=50, col="red")
  abline(v=50, col="red")
  box(lwd=par()$lwd)

  rng <- 50
  plot(pmin(rng, 100 - 100 * dat$mrp_employed), pmin(rng, 100 - 100 * dat$cps_employed), 
    cex=sqrt(dat$cps_n) / 5000, pch=20, col=rgb(0, 0, 0, alpha=0.25), 
    xlim=c(0, rng), ylim=c(0, rng), xaxs="i", yaxs="i", 
    xlab="MRP", ylab="CPS", main=paste0("Unemployment, Demographics:\n", survey, " (MRP = ", joint_level, ")"), axes=FALSE)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  abline(a=0, b=1, col="red")
  abline(h=50, col="red")
  abline(v=50, col="red")
  box(lwd=par()$lwd)

  rng <- 50
  plot(pmin(rng, 100 - 100 * dat$mrp_atwork), pmin(rng, 100 - 100 * dat$cps_atwork), 
    cex=sqrt(dat$cps_n) / 5000, pch=20, col=rgb(0, 0, 0, alpha=0.25), 
    xlim=c(0, rng), ylim=c(0, rng), xaxs="i", yaxs="i", 
    xlab="MRP", ylab="CPS", main=paste0("Percent Not At Work, Demographics:\n", survey, " (MRP = ", joint_level, ")"), axes=FALSE)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  abline(a=0, b=1, col="red")
  abline(h=50, col="red")
  abline(v=50, col="red")
  box(lwd=par()$lwd)

  dev.off()

}

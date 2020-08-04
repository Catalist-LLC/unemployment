library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(openxlsx)
library(tidycensus)
library(RJDBC)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/cross_dataset_variable_lineups/occupation_and_industry/")

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

vertica_username <- system("echo $VERTICA_USERNAME", intern=TRUE)
vertica_password <- system("echo $VERTICA_PASSWORD", intern=TRUE)
vertica_host <- system("echo $VERTICA_HOST", intern=TRUE)

drv <- JDBC("com.vertica.jdbc.Driver", "~/RJDBC/vertica-jdk5-6.1.3-0.jar")
conn <- dbConnect(drv, paste0("jdbc:vertica://", vertica_host, ":5433/vertica4"), vertica_username, vertica_password)       ### analytics cluster
V <- function(x) dbGetQuery(conn, x)

################################################################################################################
# helper code

tmp <- read.xlsx("occ_ind_lookup.xlsx", sheet="acs_emp")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(unique(tmp$grp), function(i) {
  paste0("emp__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$var[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

tmp <- read.xlsx("occ_ind_lookup.xlsx", sheet="acs_occ")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(unique(tmp$grp), function(i) {
  paste0("occ__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$var[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

tmp <- read.xlsx("occ_ind_lookup.xlsx", sheet="acs_ind")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(unique(tmp$grp), function(i) {
  paste0("ind__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$var[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

tmp <- read.xlsx("occ_ind_lookup.xlsx", sheet="acs_inc")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(unique(tmp$grp), function(i) {
  paste0("inc__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$var[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")


################################################################################################################
# get raw data

geog <- readRDS("../../downloaded_data/acs/tract_data_raw_occind_2017.rds")
source("../../helper_functions/RawACSToMarginalOcc.R")
marg <- RawACSToMarginalOcc(geog)
ys <- grep("_", grep("__total", colnames(marg), invert=TRUE, value=TRUE), value=TRUE)

################################################################################################################
# check against state-level occ survey data

occ <- readRDS("../run_mrp/data/occ.rds")

################################################################################################################

jpeg("occ_survey_to_geog.jpeg", width=20, height=15, units="in", res=300)
par(mfrow=c(5,8), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif", 
  cex.main=0.9, cex.lab=0.8, cex.axis=0.7)

for (y in ys) {
  occ$n <- occ[,paste0("n_", gsub("_.+", "", y))]
  occ$y <- occ[,y] * occ$n
  marg$y <- marg[,y]
  marg$n <- marg[,gsub("_.+", "_total", y)]
  tmp <- data.frame(inner_join(
    occ %>% group_by(state) %>% summarize(survey_y=sum(y, na.rm=TRUE) / 5, survey_n=sum(n, na.rm=TRUE) / 5), 
    marg %>% group_by(state) %>% summarize(marg_y=sum(y, na.rm=TRUE), marg_n=sum(n, na.rm=TRUE)), 
    by="state"))
  tmp$survey <- 100 * tmp$survey_y / tmp$survey_n
  tmp$marg <- 100 * tmp$marg_y / tmp$marg_n
  rng <- range(c(tmp$survey, tmp$marg))
  plot(0, 0, xlab="Survey", ylab="Geog", axes=FALSE, 
    main=substr(y, 1, 25), xlim=rng, ylim=rng)
  text(tmp$survey, tmp$marg, tmp$state, cex=0.5)
  box(lwd=par()$lwd)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  abline(a=0, b=1, col="grey")
}

dev.off()

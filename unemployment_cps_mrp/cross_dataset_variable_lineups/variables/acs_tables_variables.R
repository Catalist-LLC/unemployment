library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(openxlsx)
library(tidycensus)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/cross_dataset_variable_lineups/variables/")

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################
# helper code

tmp <- read.xlsx("acs_tables_variables.xlsx", sheet="sex_age")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(sort(unique(tmp$grp)), function(i) {
  paste0("sex_age__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$var[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

tmp <- read.xlsx("acs_tables_variables.xlsx", sheet="sex_age_edu")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(sort(unique(tmp$grp)), function(i) {
  paste0("sex_age_edu__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$var[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

tmp <- read.xlsx("acs_tables_variables.xlsx", sheet="sex_age_marital")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(sort(unique(tmp$grp)), function(i) {
  paste0("sex_age_marital__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$a[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

tmp <- read.xlsx("acs_tables_variables.xlsx", sheet="sex_age_race")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(sort(unique(tmp$grp)), function(i) {
  paste0("sex_age_race__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$CONCAT[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

tmp <- read.xlsx("acs_tables_variables.xlsx", sheet="sex_age_race_citizenship")
tmp <- tmp[!is.na(tmp$grp),]
tmp <- paste0("        ", 
as.character(sapply(sort(unique(tmp$grp)), function(i) {
  paste0("sex_age_race_citizenship__", i, "=sum(estimate[variable %in% c('", paste0(sort(tmp$CONCAT[tmp$grp == i]), collapse="', '"), "')], na.rm=TRUE), ")
})))
print(cbind(tmp))
print("\n\n")

################################################################################################################
# get raw data

geog <- readRDS("../../downloaded_data/acs/mi_2018_acs_geography_data_raw.rds")
source("../../helper_functions/RawACSToMarginal.R")
marg <- RawACSToMarginal(geog)

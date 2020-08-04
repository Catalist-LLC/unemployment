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
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/downloaded_data/acs")

################################################################################################################

variables <- read.xlsx("../../cross_dataset_variable_lineups/variables/acs_tables_variables.xlsx", sheet="variables")
variables <- variables[variables$keep %in% c("TEST", "y", "occ", "ind", "emp", "inc"),]
variables <- grep("--", variables$CONCAT, value=TRUE, invert=TRUE)

registerDoMC(4)
foreach (year = 2018:2000) %dopar% {
  geog <- get_acs(
    geography="state", 
    variables=variables, 
    year=year, 
    survey=ifelse(year >= 2012, "acs1", "acs3"))
  geog <- data.frame(year=year, geog, stringsAsFactors=FALSE)
  saveRDS(geog, file=paste0("state_data_raw_", year, ".rds"))
}

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
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/mrsp/downloaded_data")

################################################################################################################

variables <- read.xlsx("../variables/acs_tables_variables.xlsx", sheet="variables")
variables <- variables[variables$keep %in% c("TEST", "y", "occ", "ind", "emp"),]
variables <- grep("--", variables$CONCAT, value=TRUE, invert=TRUE)

registerDoMC(2)
surveys <- c("acs1", "acs5")
raw <- foreach(survey = surveys) %dopar% {
  tmp <- get_acs(
    geography="congressional district", 
    variables=variables, 
    year=2018, 
    survey=survey)
  tmp <- data.frame(year=2018, survey=survey, tmp, stringsAsFactors=FALSE)
}

geog <- bind_rows(raw)
saveRDS(geog, file="cd_data_raw_2018.rds")

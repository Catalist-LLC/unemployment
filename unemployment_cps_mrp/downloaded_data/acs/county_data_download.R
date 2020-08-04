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

for (year in 2018:2017) {
  registerDoMC(2)
  surveys <- c("acs1", "acs5")
  raw <- foreach(survey = surveys) %dopar% {
    tmp <- get_acs(
      geography="county", 
      variables=variables, 
      year=year, 
      survey=survey)
    tmp <- data.frame(year=year, survey=survey, tmp, stringsAsFactors=FALSE)
  }

  geog <- bind_rows(raw)
  saveRDS(geog, file=paste0("county_data_raw_", year, ".rds"))
}

################################################################################################################

registerDoMC(4)
raw <- foreach (year = 2018:2007, .errorhandling="pass") %dopar% {
  tmp <- get_acs(
    geography="county", 
    variables="B12006_001", 
    year=year, 
    survey="acs5")
  tmp <- data.frame(year=year, tmp, stringsAsFactors=FALSE)
  return(tmp)
}

out <- NULL
for (i in raw)
  if (class(i)[1] == "data.frame")
    out <- rbind(out, i)

years <- sort(unique(out$year))
out <- sqldf(paste0("
  select 
  GEOID as fips, 
  ", paste0("sum(case when year = ", years, " then estimate else 0 end) as cnip", years, collapse=", "), "
  from out
  group by 1
"))
saveRDS(out, file="county_cnip.rds")

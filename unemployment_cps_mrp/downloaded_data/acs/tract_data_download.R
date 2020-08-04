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
variables <- variables[variables$keep %in% c("TEST", "y"),]
variables <- grep("--", variables$CONCAT, value=TRUE, invert=TRUE)
states <- unique(c("DC", state.abb))

for (year in c(2018)) {
  registerDoMC(4)
  print(system.time(raw <- foreach (state = states, .errorhandling="pass") %dopar% {
    message(year, ", ", state)
    tmp <- get_acs(
      geography="tract", 
      variables=variables, 
      year=year, 
      state=state)
    geog <- data.frame(state=state, year=year, tmp, stringsAsFactors=FALSE)
    saveRDS(geog, file=paste0("states/tract_data_raw_", state, "_", year, ".rds"))
  }))

  print(system.time(raw <- foreach (state = states, .errorhandling="pass") %dopar% {
    return(readRDS(paste0("states/tract_data_raw_", state, "_", year, ".rds")))
  }))
  geog <- bind_rows(raw)
  saveRDS(geog, file=paste0("tract_data_raw_", year, ".rds"))

}

################################################################################################################
# fill in years that didn't make it in for some reason?

variables <- read.xlsx("../../cross_dataset_variable_lineups/variables/acs_tables_variables.xlsx", sheet="variables")
variables <- variables[variables$keep %in% c("TEST", "y"),]
variables <- grep("--", variables$CONCAT, value=TRUE, invert=TRUE)

files <- rev(system("ls tract_data_raw_20*.rds", intern=TRUE))
for (file in files) {

  remove(old)
  remove(geog)
  gc()

  year <- as.numeric(gsub(".rds", "", gsub("tract_data_raw_", "", file)))
  old <- readRDS(paste0("tract_data_raw_", year, ".rds"))
  states <- unique(c("DC", state.abb))
  states <- states[!(states %in% old$state)]
  message("************ ", year, ", ", length(states), " states")
  registerDoMC(4)
  print(system.time(raw <- foreach (state = states, .errorhandling="pass") %dopar% {
    message(year, ", ", state)
    tmp <- get_acs(
      geography="tract", 
      variables=variables, 
      year=year, 
      state=state)
    return(data.frame(state=state, year=year, tmp, stringsAsFactors=FALSE))
  }))
  geog <- data.frame(bind_rows(raw))

  geog <- rbind(old, geog)
  saveRDS(geog, file=paste0("tract_data_raw_", year, ".rds"))

}

################################################################################################################
# oops, i forgot occupation and industry data

variables <- read.xlsx("../../cross_dataset_variable_lineups/variables/acs_tables_variables.xlsx", sheet="variables")
variables <- variables[variables$keep %in% c("occ", "ind", "emp", "inc"),]
variables <- grep("--", variables$CONCAT, value=TRUE, invert=TRUE)
variables <- c("B00001_001", variables)
states <- unique(c("DC", state.abb))

for (year in c(2018:2017)) {
  registerDoMC(4)
  print(system.time(raw <- foreach (state = states, .errorhandling="pass") %dopar% {
    message(year, ", ", state)
    tmp <- get_acs(
      geography="tract", 
      variables=variables, 
      year=year, 
      state=state)
    return(data.frame(state=state, year=year, tmp, stringsAsFactors=FALSE))
  }))

  geog <- data.frame(bind_rows(raw))
  saveRDS(geog, file=paste0("tract_data_raw_occind_", year, ".rds"))
}

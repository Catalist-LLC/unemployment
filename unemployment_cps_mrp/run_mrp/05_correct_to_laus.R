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

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/run_mrp/")

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

source("../helper_functions/FindDelta.R")

################################################################################################################

joint <- readRDS("../mrsp/tract/tract_2018_joint.rds")

surveys <- system("ls model_save/yhat_tract_*.rds", intern=TRUE)
surveys <- gsub(".rds$", "", gsub(".+yhat_tract_", "", surveys))

county_scored_surveys <- system("ls model_save/yhat_tractcorrectedcounty_*.rds", intern=TRUE)
county_scored_surveys <- gsub(".rds$", "", gsub(".+yhat_tractcorrectedcounty_", "", county_scored_surveys))

state_scored_surveys <- system("ls model_save/yhat_tractcorrectedstate_*.rds", intern=TRUE)
state_scored_surveys <- gsub(".rds$", "", gsub(".+yhat_tractcorrectedstate_", "", state_scored_surveys))

surveys <- surveys[!(surveys %in% county_scored_surveys)]

laus_state <- readRDS("../downloaded_data/laus/laus_state.rds")
laus_state_surveys <- table(laus_state$month)
laus_state_surveys <- names(which(laus_state_surveys == 51))
surveys <- surveys[surveys %in% laus_state_surveys]

laus <- readRDS("../downloaded_data/laus/laus.rds")
laus_county_surveys <- table(laus$month)
laus_county_surveys <- names(which(laus_county_surveys > 3000))

gc()

################################################################################################################

source("../helper_functions/NVL.R")
source("../helper_functions/GetYHat.R")

state_joint <- readRDS("../run_mrp/data/acs.rds")
state_years <- sort(unique(state_joint$year))
county_fips <- data.frame(fips=readRDS("../mrsp/county/county_2018_joint.rds")$fips, stringsAsFactors=FALSE)

registerDoMC(length(surveys))
print(system.time(raw <- foreach (i_survey = 1:length(surveys)) %dopar% {

  survey <- surveys[i_survey]
  message("scoring survey ", i_survey, " of ", length(surveys), ": ", survey)

  # county target
  if (survey %in% laus_county_surveys) {
    output_prefix <- "yhat_tractcorrectedcounty"
    county_target <- data.frame(county_fips, readRDS(paste0("model_save/yhat_county_", survey, ".rds")))
    colnames(county_target) <- gsub(paste0("_", survey), "", colnames(county_target))
    county_target <- data.frame(
      county_target %>% 
      group_by(fips) %>% 
      summarize(n=sum(n)), 
      stringsAsFactors=FALSE)
    county_target <- inner_join(county_target, laus[laus$month == survey, c("fips", "laborforce", "employed")], by="fips")
    county_target$employed <- county_target$employed / county_target$laborforce
    county_target$laborforce <- county_target$laborforce / county_target$n
    county_target <- county_target[, c("fips", "laborforce", "employed")]
  } else {
    output_prefix <- "yhat_tractcorrectedstate"
    county_target <- NULL
  }

  # state target
  state_target <- data.frame(county_fips, readRDS(paste0("model_save/yhat_county_", survey, ".rds")))
  colnames(state_target) <- gsub(paste0("_", survey), "", colnames(state_target))
  state_target$fips <- substr(state_target$fips, 1, 2)
  state_target <- data.frame(
    state_target %>% 
    group_by(fips) %>% 
    summarize(n=sum(n)), 
    stringsAsFactors=FALSE)
  state_target <- inner_join(state_target, laus_state[laus_state$month == survey, c("fips", "laborforce", "employed")], by="fips")
  state_target$employed <- state_target$employed / state_target$laborforce
  state_target$laborforce <- state_target$laborforce / state_target$n
  state_target <- state_target[, c("fips", "laborforce", "employed")]

  # demo target
  demo_target <- readRDS(paste0("model_save/yhat_state_", survey, ".rds"))
  colnames(demo_target) <- gsub(paste0("_", survey), "", colnames(demo_target))
  demo_target <- data.frame(
    demo_target, 
    state_joint[state_joint$year == state_years[which.min(abs(state_years - as.numeric(substr(survey, 1, 4))))],], 
    stringsAsFactors=FALSE)
  demo_target <- data.frame(
    demo_target %>% 
    group_by(agegrp, female, race, edu, married, citizen) %>% 
    summarize_at(c("n", "laborforce", "employed", "atwork"), sum))
  demo_target$atwork <- demo_target$atwork / demo_target$laborforce
  demo_target$employed <- demo_target$employed / demo_target$laborforce
  demo_target$laborforce <- demo_target$laborforce / demo_target$n
  demo_target$demoid <- apply(demo_target[,c("agegrp", "female", "race", "edu", "married", "citizen")], 1, paste0, collapse="_")

  return(GetYHat(
    survey, 
    joint, 
    model_directory="model_save", 
    output_prefix=output_prefix, 
    laus=NULL, 
    county_target=county_target, 
    state_target=state_target, 
    demo_target=demo_target, 
    starting_model=paste0("yhat_tract_", survey, ".rds")))

}))
names(raw) <- surveys

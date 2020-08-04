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

joint <- data.frame(
  readRDS("../mrsp/county/county_2018_joint.rds"), 
  readRDS("../mrsp/county/county_2018_occshift.rds"), 
  stringsAsFactors=FALSE)
joint_inter <- readRDS("../mrsp/county/county_2018_jointinter.rds")

models <- system("ls model_save/cps*.rds", intern=TRUE)
models <- gsub(".rds$", "", gsub(".+/", "", models))
surveys <- sort(unique(substr(models, 5, 11)))

scored_surveys <- system("ls model_save/yhat_county*.rds", intern=TRUE)
scored_surveys <- gsub(".rds$", "", gsub(".+yhat_county_", "", scored_surveys))
surveys <- surveys[!(surveys %in% scored_surveys)]

gc()

################################################################################################################
# score models

source("../helper_functions/NVL.R")
source("../helper_functions/GetYHat.R")
laus <- readRDS("../downloaded_data/laus/laus.rds")
state_joint <- readRDS("../run_mrp/data/acs.rds")
state_years <- sort(unique(state_joint$year))

registerDoMC(length(surveys))
raw <- foreach (i_survey = 1:length(surveys)) %dopar% {
  survey <- surveys[i_survey]
  message("scoring survey ", i_survey, " of ", length(surveys), ": ", survey)

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

  return(GetYHat(survey, joint, joint_inter, model_directory="model_save", output_prefix="yhat_county", laus, demo_target=demo_target))
}
names(raw) <- surveys

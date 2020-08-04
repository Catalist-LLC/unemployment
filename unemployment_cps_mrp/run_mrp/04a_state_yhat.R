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
  readRDS("../run_mrp/data/acs.rds"), 
  readRDS("../mrsp/state/state_occshift.rds"), 
  stringsAsFactors=FALSE)
joint_inter <- readRDS("../run_mrp/data/acs_inter.rds")

state_from_fips <- c(
  "02"="AK", "01"="AL", "05"="AR", "04"="AZ", "06"="CA", "08"="CO", "09"="CT", "11"="DC", 
  "10"="DE", "12"="FL", "13"="GA", "15"="HI", "19"="IA", "16"="ID", "17"="IL", "18"="IN", 
  "20"="KS", "21"="KY", "22"="LA", "25"="MA", "24"="MD", "23"="ME", "26"="MI", "27"="MN", 
  "29"="MO", "28"="MS", "30"="MT", "37"="NC", "38"="ND", "31"="NE", "33"="NH", "34"="NJ", 
  "35"="NM", "32"="NV", "36"="NY", "39"="OH", "40"="OK", "41"="OR", "42"="PA", "44"="RI", 
  "45"="SC", "46"="SD", "47"="TN", "48"="TX", "49"="UT", "51"="VA", "50"="VT", "53"="WA", 
  "55"="WI", "54"="WV", "56"="WY")
fips_from_state <- names(state_from_fips)
names(fips_from_state) <- state_from_fips
joint$fips <- fips_from_state[joint$state]
joint_years <- sort(unique(joint$year))

models <- system("ls model_save/cps*.rds", intern=TRUE)
models <- gsub(".rds$", "", gsub(".+/", "", models))
surveys <- sort(unique(substr(models, 5, 11)))

scored_surveys <- system("ls model_save/yhat_state*.rds", intern=TRUE)
scored_surveys <- gsub(".rds$", "", gsub(".+yhat_state_", "", scored_surveys))
surveys <- surveys[!(surveys %in% scored_surveys)]

gc()

################################################################################################################
# score models

source("../helper_functions/NVL.R")
source("../helper_functions/GetYHat.R")
laus <- readRDS("../downloaded_data/laus/laus_state.rds")

registerDoMC(length(surveys))
raw <- foreach (i_survey = 1:length(surveys)) %dopar% {
  survey <- surveys[i_survey]
  message("scoring survey ", i_survey, " of ", length(surveys), ": ", survey)
  ok_rows <- joint$year == joint_years[which.min(abs(joint_years - as.numeric(substr(survey, 1, 4))))]
  return(GetYHat(
    survey=survey, 
    joint=joint[ok_rows,], 
    joint_inter=joint_inter[ok_rows,], 
    model_directory="model_save",
    output_prefix="yhat_state", 
    laus=laus))
}

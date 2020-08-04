library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(glmnet)
library(gtools)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/mrsp/state")

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################

mrsp <- readRDS("../../run_mrp/data/acs.rds")
mrsp_inter <- readRDS("../../run_mrp/data/acs_inter.rds")
occ <- readRDS("../../run_mrp/data/occ.rds")
# occ <- occ[occ$agegrp > 1,]

xs_binary <- c("female", "married", "citizen")
xs_cat <- c("region", "state", "agegrp", "race", "edu", "whitecollege", "whitefemale", "marriedfemale")
xs <- c(xs_binary, xs_cat)

# check if all xs have the same values
check <- cbind(sapply(xs, function(i)
  all(sort(unique(mrsp[,i])) == sort(unique(occ[,i])))
))
if (any(!check))
  stop("some xs have different values")

gc()

################################################################################################################
# occ/ind yhats

ys <- c(
  system("ls ../../run_mrp/model_save/occ_*.rds", intern=TRUE), 
  system("ls ../../run_mrp/model_save/ind_*.rds", intern=TRUE), 
  system("ls ../../run_mrp/model_save/emp_*.rds", intern=TRUE))
ys <- gsub(".rds$", "", gsub("^.+model_save/", "", ys))

source("../../helper_functions/NVL.R")
source("../../helper_functions/GetYHatOcc.R")

registerDoMC(12)
yhats <- foreach(y = ys) %dopar% {
  models <- readRDS(paste0("../../run_mrp/model_save/", y, ".rds"))
  return(GetYHatOcc(mrsp, mrsp_inter, models, y))
}
yhats <- data.frame(bind_cols(yhats))
colnames(yhats) <- ys

if (any(!complete.cases(yhats)))
  stop("missing data")

saveRDS(yhats, file="state_occscore.rds")

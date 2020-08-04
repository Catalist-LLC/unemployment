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
setwd("~/unemployment/unemployment_cps_mrp/mrsp/tract")

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################

mrsp <- readRDS("tract_2018_joint.rds")
occ <- readRDS("../../run_mrp/data/occ.rds")

xs_binary <- c("female", "married", "citizen")
xs_cat <- c("region", "state", "agegrp", "race", "edu", "whitecollege", "whitefemale", "marriedfemale")
xs <- c(xs_binary, xs_cat)

# check if all xs have the same values
check <- cbind(sapply(xs, function(i)
  all(sort(unique(mrsp[,i])) == sort(unique(occ[,i])))
))
if (any(!check))
  stop("some xs have different values")

inters <- as.data.frame(combinations(n=length(xs), r=2, v=xs), stringsAsFactors=FALSE)
colnames(inters) <- c("x1", "x2")
drop <- 
  (inters$x1 %in% c("region", "state") & inters$x2 %in% c("region", "state")) |
  inters$x1 %in% c("whitecollege", "whitefemale", "marriedfemale") | 
  inters$x2 %in% c("whitecollege", "whitefemale", "marriedfemale")
inters <- inters[!drop,]
inters$full <- paste0(inters$x1, "__", inters$x2)

source("../../helper_functions/GetInteractionsMatrix.R")
print(system.time(mrsp_inter <- GetInteractionsMatrix(dat=mrsp, inters)))

saveRDS(mrsp_inter, file="tract_2018_jointinter.rds")

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

saveRDS(yhats, file="tract_2018_occscore.rds")

library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(gtools)
library(foreach)
library(doMC)
library(openxlsx)
library(glmnet)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/run_mrp")

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################

occ <- readRDS("data/occ.rds")
occ_inter <- readRDS("data/occ_inter.rds")

xs_binary <- c("female", "married", "citizen")
xs_cat <- c("region", "state", "agegrp", "race", "edu", "whitecollege", "whitefemale", "marriedfemale")
xs <- c(xs_binary, xs_cat)

occ$emp_laborforce <- 1 - occ$emp_notinlaborforce
occ <- occ[, colnames(occ) != "emp_notinlaborforce"]

ys <- c(
  grep("^occ_", colnames(occ), value=TRUE), 
  grep("^ind_", colnames(occ), value=TRUE), 
  grep("^emp_", colnames(occ), value=TRUE))
ok <- sapply(ys, function(i) length(unique(na.exclude(occ[,i]))))
ys <- ys[which(ok > 1)]
ys <- ys[ys != "occ_militaryspecific"]

################################################################################################################

registerDoMC(length(ys))
raw <- foreach(y = ys) %dopar% {
  message(y)

  ok_rows <- !is.na(occ[,y])
  dat <- occ[ok_rows,]
  dat$n <- dat[, paste0("n_", gsub("_.+", "", y))]
  dat$y <- dat[,y] * dat$n
  dat_inter <- occ_inter[ok_rows,]

  out <- list()
  if (substr(y, 1, 4) == "emp_") {
    for (female in c(0, 1)) {
      for (married in c(0, 1)) {
        tmp <- dat[dat$female == female & dat$married == married,]
        tmp_inter <- dat_inter[dat$female == female & dat$married == married,]
        y_bin <- round(cbind(tmp$y, tmp$n - tmp$y))

        message("fitting model: ", y, ", lmer, female=", female, ", married=", married)
        print(system.time(eval(parse(text=paste0("
          M_lmer <- glmer(
            y_bin ~ 1 + citizen + 
            ", paste0("(1 | ", xs_cat[xs_cat != "marriedfemale"], ")", collapse=" + "), ", 
            data=tmp, family='binomial', 
            control=glmerControl(optimizer='nloptwrap'), 
            verbose=FALSE)
        ")))))

        message("fitting model: ", y, ", glmnet, female=", female, ", married=", married)
        yhat_lmer <- sparseMatrix(i=1:nrow(tmp), j=rep(1, nrow(tmp)), x=as.numeric(predict(M_lmer)))
        colnames(yhat_lmer) <- "yhat_lmer"
        print(system.time(M_glmnet <- cv.glmnet(y=y_bin[,2:1], x=cbind(yhat_lmer, tmp_inter), family="binomial")))

        out[[paste0(female, married)]] <- list(fix=fixef(M_lmer), ran=ranef(M_lmer), M_glmnet=M_glmnet)
      }
    }
  } else {
    for (female in c(0, 1)) {
      tmp <- dat[dat$female == female,]
      tmp_inter <- dat_inter[dat$female == female,]
      y_bin <- round(cbind(tmp$y, tmp$n - tmp$y))

      message("fitting model: ", y, ", lmer, female=", female)
      print(system.time(eval(parse(text=paste0("
        M_lmer <- glmer(
          y_bin ~ 1 + citizen + married + 
          ", paste0("(1 | ", xs_cat, ")", collapse=" + "), ", 
          data=tmp, family='binomial', 
          control=glmerControl(optimizer='nloptwrap'), 
          verbose=FALSE)
      ")))))

      message("fitting model: ", y, ", glmnet, female=", female)
      yhat_lmer <- sparseMatrix(i=1:nrow(tmp), j=rep(1, nrow(tmp)), x=as.numeric(predict(M_lmer)))
      colnames(yhat_lmer) <- "yhat_lmer"
      print(system.time(M_glmnet <- cv.glmnet(y=y_bin[,2:1], x=cbind(yhat_lmer, tmp_inter), family="binomial")))

      out[[as.character(female)]] <- list(fix=fixef(M_lmer), ran=ranef(M_lmer), M_glmnet=M_glmnet)
    }
  }

  saveRDS(out, file=paste0("model_save/", y, ".rds"))

}

################################################################################################################
# occ/ind yhats

source("../helper_functions/NVL.R")
source("../helper_functions/GetYHatOcc.R")

registerDoMC(length(ys))
yhats <- foreach(y = ys) %dopar% {
  models <- readRDS(paste0("model_save/", y, ".rds"))
  return(GetYHatOcc(occ, occ_inter, models, y))
}
yhats <- data.frame(bind_cols(yhats))
colnames(yhats) <- ys

################################################################################################################

jpeg("output_plots/check_occ_models.jpeg", width=20, height=15, units="in", res=300)
par(mfrow=c(5,8), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans", 
  cex.main=0.9, cex.lab=0.8, cex.axis=0.7)

for (y in ys) {
  message(y)
  tmp <- data.frame(
    x=round(yhats[,y], 2), 
    y=occ[,y] * occ[, paste0("n_", gsub("_.+", "", y))], 
    n=occ[, paste0("n_", gsub("_.+", "", y))])
  tmp <- tmp[!is.na(tmp$y),]
  tmp <- tmp %>% 
    group_by(x) %>% 
    summarize(
      n=sum(n), 
      y=sum(y) / sum(n))

  plot(tmp$x, tmp$y, 
    pch=20, cex=pmin(5, sqrt(tmp$n) / 1000), col=rgb(0, 0, 0, alpha=0.25), 
    xlim=c(0, 1), ylim=c(0, 1), 
    xlab="", ylab="", main=substr(y, 1, 25), axes=FALSE)
  abline(a=0, b=1)
  abline(h=0.5)
  abline(v=0.5)
  box(lwd=par()$lwd)
}

dev.off()

################################################################################################################

jpeg("output_plots/occ_state_yhats.jpeg", width=20, height=15, units="in", res=300)
par(mfrow=c(5,8), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans", 
  cex.main=0.9, cex.lab=0.8, cex.axis=0.7)

for (y in ys) {
  occ$n <- occ[,paste0("n_", gsub("_.+", "", y))]
  occ$y <- occ[,y] * occ$n
  occ$yhat <- yhats[,y] * occ$n
  tmp <- data.frame(
    occ %>% 
    group_by(state) %>% 
    summarize(
      yhat=sum(yhat, na.rm=TRUE) / sum(n, na.rm=TRUE), 
      y=sum(y, na.rm=TRUE) / sum(n, na.rm=TRUE))) 
  rng <- range(c(tmp$yhat, tmp$y))
  plot(0, 0, xlab="Modeled", ylab="Survey", axes=FALSE, 
    main=substr(y, 1, 25), xlim=rng, ylim=rng)
  text(tmp$yhat, tmp$y, tmp$state, cex=0.5)
  box(lwd=par()$lwd)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  abline(a=0, b=1, col="grey")
}

dev.off()

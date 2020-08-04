################################################################################################################
# prep

ys <- grep("^y_", colnames(cps), value=TRUE)
combs <- expand.grid(surveys, ys, stringsAsFactors=FALSE)
colnames(combs) <- c("survey", "y")
combs <- combs[nrow(combs):1,]

combs <- combs[combs$survey %in% surveys,]
ok <- cps$survey %in% combs$survey
cps <- cps[ok,]

state_claims_weekly <- readRDS("../../unemployment_insurance_claims/state_claims_weekly.rds")

xs_binary <- c("female", "married", "citizen")
xs_cat <- c("region", "state", "agegrp", "agegrp2", "race", "edu", "whitecollege", "whitefemale", "marriedfemale")
xs <- c(xs_binary, xs_cat)

cps$fips <- str_pad(cps$fips, 5, pad="0")
cps$fips[cps$fips == "00000"] <- NA
cps$metarea[cps$metarea >= 9990] <- NA
cps$cpsix <- 1:nrow(cps)

cps$agegrp2 <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)[cps$agegrp]

################################################################################################################
# occupation predictors

lookup <- read.xlsx("../cross_dataset_variable_lineups/county_metroarea/fips_to_metarea_cps.xlsx")
lookup <- lookup[, c("fips", "metarea_truncated")]
colnames(lookup) <- c("fips", "metarea")

occ <- data.frame(
  readRDS("../mrsp/county/county_2018_joint.rds"), 
  readRDS("../mrsp/county/county_2018_occshift.rds"), 
  stringsAsFactors=FALSE)
occ <- occ[, grep("^inc_", colnames(occ), invert=TRUE)]
occ$agegrp2 <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)[occ$agegrp]

occ$emp_laborforce <- 1 - occ$emp_notinlaborforce
occ <- occ[, colnames(occ) != "emp_notinlaborforce"]
for (x in grep("_", colnames(occ), value=TRUE))
  occ[,x] <- occ[,x] * occ$n

# prep for laus adjustment
laus <- readRDS("../downloaded_data/laus/laus.rds")
fips_n <- data.frame(occ[, c("fips", "n")] %>% group_by(fips) %>% summarize(cnip=sum(n)))
occ$ix <- 1:nrow(occ)
occ_split <- lapply(c("ix", "n", "emp_laborforce", "emp_employed"), function(i) split(x=occ[,i], f=occ$fips))
names(occ_split) <- c("ix", "n", "laborforce", "employed")

registerDoMC(30)
surveys <- sort(unique(cps$survey))
raw <- foreach(i_survey = 1:length(surveys)) %dopar% {
  message("producing geographic predictors: ", i_survey, " of ", length(surveys))
  
  survey <- surveys[i_survey]

  # laus adjustment
  survey_month <- as.numeric(gsub(".+_", "", survey))
  survey_year <- as.numeric(gsub("_.+", "", survey))
  if (survey_month == 1) {
    laus_ref <- laus[laus$month == paste0(survey_year - 1, "_12"),]
  } else {
    laus_ref <- laus[laus$month == paste0(survey_year, "_", str_pad(survey_month - 1, width=2, pad="0")),]
  }
  laus_ref <- inner_join(fips_n, laus_ref, by="fips")
  laus_ref <- laus_ref[complete.cases(laus_ref),]
  laus_ref$employed <- laus_ref$employed / laus_ref$cnip
  laus_ref$laborforce <- laus_ref$laborforce / laus_ref$cnip

  occ_laus <- data.frame(ix=occ$ix)
  deltas <- NULL
  for (y in c("laborforce", "employed")) {
    raw_adj <- foreach (i = 1:nrow(laus_ref)) %do% {
      n <- occ_split$n[[laus_ref$fips[i]]]
      x <- occ_split[[y]][[laus_ref$fips[i]]] / n
      delta <- FindDelta(y=x, n=n, yhat=min(1, laus_ref[i, y]))
      out <- data.frame(
        ix=occ_split$ix[[laus_ref$fips[i]]], 
        y=invlogit(logit(x) + delta) * n)
      colnames(out) <- c("ix", paste0("laus_", y))
      return(list(delta=delta, out=out))
    }
    deltas <- rbind(deltas, data.frame(
      fips=laus_ref$fips, 
      y=y, 
      delta=sapply(raw_adj, function(i) i$delta), 
      stringsAsFactors=FALSE))
    raw_adj <- lapply(raw_adj, function(i) i$out)
    occ_laus <- left_join(occ_laus, bind_rows(raw_adj), by="ix")
    ok <- is.na(occ_laus[, paste0("laus_", y)])
    occ_laus[ok, paste0("laus_", y)] <- occ[ok, paste0("emp_", y)]
  }

  occ_laus <- occ_laus[, colnames(occ_laus) != "ix"]
  for (i in colnames(occ_laus))
    occ[,i] <- occ_laus[,i]

  xs_occ <- grep("_", colnames(occ), value=TRUE)

  fipss <- sort(unique(na.exclude(cps$fips[cps$survey == survey])))
  lookup_survey <- lookup[!(lookup$fips %in% fipss),]
  occ_fips <- occ[occ$fips %in% fipss,]
  occ_metarea <- occ[occ$fips %in% lookup_survey$fips,]
  occ_metarea <- left_join(occ_metarea, lookup_survey, by="fips")
  occ_metarea <- occ_metarea[!is.na(occ_metarea$metarea),] %>% 
    group_by(state, agegrp, agegrp2, female, race, edu, married, citizen, whitecollege, whitefemale, marriedfemale, metarea) %>% 
    summarize_at(c("n", xs_occ), sum)
  occ_nogeo <- occ[!(occ$fips %in% c(fipss, lookup_survey$fips)),]
  occ_nogeo <- occ_nogeo %>% 
    group_by(state, agegrp, agegrp2, female, race, edu, married, citizen, whitecollege, whitefemale, marriedfemale) %>% 
    summarize_at(c("n", xs_occ), sum)

  occ_fips$laus_employed <- pmin(1, occ_fips$laus_employed / pmax(0.00001, occ_fips$laus_laborforce))
  occ_metarea$laus_employed <- pmin(1, occ_metarea$laus_employed / pmax(0.00001, occ_metarea$laus_laborforce))
  occ_nogeo$laus_employed <- pmin(1, occ_nogeo$laus_employed / pmax(0.00001, occ_nogeo$laus_laborforce))
  for (x in xs_occ[xs_occ != "laus_employed"]) {
    occ_fips[,x] <- occ_fips[,x] / occ_fips$n
    occ_metarea[,x] <- occ_metarea[,x] / occ_metarea$n
    occ_nogeo[,x] <- occ_nogeo[,x] / occ_nogeo$n
  }

  out <- cps[cps$survey == survey,]
  X <- left_join(out, occ_fips, by=c("fips", "state", "agegrp", "agegrp2", "female", "race", "edu", "married", "citizen", "whitecollege", "whitefemale", "marriedfemale"))[,xs_occ]
  X_metarea <- left_join(out, occ_metarea, by=c("metarea", "state", "agegrp", "agegrp2", "female", "race", "edu", "married", "citizen", "whitecollege", "whitefemale", "marriedfemale"))[,xs_occ]
  ok <- is.na(X[,1])
  X[ok,] <- X_metarea[ok,]
  X_nogeo <- left_join(out, occ_nogeo, by=c("state", "agegrp", "agegrp2", "female", "race", "edu", "married", "citizen", "whitecollege", "whitefemale", "marriedfemale"))[,xs_occ]
  ok <- is.na(X[,1])
  X[ok,] <- X_nogeo[ok,]
  X$cpsix <- out$cpsix

  deltas <- data.frame(deltas %>% pivot_wider(names_from=y, values_from=delta))

  return(list(X=X, deltas=deltas))
}

deltas <- lapply(raw, function(i) i$deltas)
names(deltas) <- surveys

cps_occ <- lapply(raw, function(i) i$X)
cps_occ <- bind_rows(cps_occ)
cps_occ <- cps_occ[order(cps_occ$cpsix),]
cps_occ <- cps_occ[, grep("^emp_", colnames(cps_occ), invert=TRUE)]
cps_occ <- cps_occ[, colnames(cps_occ) != "cpsix"]

##################################################################################################################################
# for <1% of respondents, the geo/demo combination doesn't exist on ACS
# missing value imputation for those occupation predictors

registerDoMC(34)
raw_mi <- foreach(i_y = 1:ncol(cps_occ)) %dopar% {
  message("missing value imputation for occupation predictors: ", i_y, " of ", ncol(cps_occ))

  yhat_out <- data.frame(
    survey=cps$survey[is.na(cps_occ[,i_y])], 
    yhat=NA, 
    stringsAsFactors=FALSE)

  for (survey in surveys) {
    mi_dat <- cps
    mi_dat$y <- cps_occ[,i_y]

    mi_dat <- mi_dat[mi_dat$survey == survey,]
    X <- mi_dat[,xs_binary]
    for (i in xs_cat) {
      tmp <- dummy(mi_dat[,i])
      colnames(tmp) <- paste0(i, "__", colnames(tmp))
      X <- data.frame(X, tmp)
    }
    xs_mi <- colnames(X)
    X$y <- mi_dat$y
    X$n <- mi_dat$n_cnip_raw

    glmnet_dat <- sqldf(paste0("
      select 
      ", paste0(xs_mi, collapse=", "), ", 
      sum(n) as n, 
      sum(n * y) as y
      from X
      where y is not NULL
      group by ", paste0(xs_mi, collapse=", "), "
    "))

    y_bin <- cbind(glmnet_dat$n - glmnet_dat$y, glmnet_dat$y)
    M_mi <- cv.glmnet(y=y_bin, x=as.matrix(glmnet_dat[,xs_mi]), family="binomial")

    ok <- is.na(mi_dat$y)
    yhat_out$yhat[yhat_out$survey == survey] <- invlogit(as.numeric(predict(M_mi, newx=as.matrix(X[ok, xs_mi]))))
  }

  return(yhat_out$yhat)
}

for (i_y in 1:ncol(cps_occ)) {
  ok <- is.na(cps_occ[,i_y])
  cps_occ[ok, i_y] <- raw_mi[[i_y]]
}

gc()

##################################################################################################################################
# xs and interactions

inters <- as.data.frame(combinations(n=length(xs), r=2, v=xs), stringsAsFactors=FALSE)
colnames(inters) <- c("x1", "x2")
drop <- 
  (inters$x1 %in% c("region", "state") & inters$x2 %in% c("region", "state")) | 
  inters$x1 %in% c("whitecollege", "whitefemale", "marriedfemale") | 
  inters$x2 %in% c("whitecollege", "whitefemale", "marriedfemale")
inters <- inters[!drop,]
inters$full <- paste0(inters$x1, "__", inters$x2)

source("../helper_functions/GetInteractionsMatrix.R")

##############################################################################################
# big multilevel models

registerDoMC(30)
print(system.time(raw_M <- foreach(i_comb = 1:nrow(combs)) %dopar% {

  ################################################################################################################
  # prep data for modeling

  ok <- cps$survey == combs$survey[i_comb]
  y <- combs$y[i_comb]

  tmp <- cps[ok,]
  OCC <- as.matrix(cps_occ[ok,])
  tmp$y <- tmp[,y]
  tmp$n <- tmp[, gsub("^y_", "n_", y)]
  ok <- !is.na(tmp$y)
  tmp <- tmp[ok,]
  OCC <- OCC[ok,]
  y_bin <- cbind(tmp$y, tmp$n - tmp$y)

  ok <- complete.cases(OCC)
  tmp <- tmp[ok,]
  OCC <- OCC[ok,]
  y_bin <- y_bin[ok,]

  ################################################################################################################
  # baseline model

  message("fitting model: ", i_comb, " of ", nrow(combs), ": baseline model")

  if (y == "y_laborforce_cnip") {
    tmp$ypct <- tmp$y / tmp$n
    baseline <- "laus_laborforce"
    M_baseline <- lm(ypct ~ 1, offset=OCC[,baseline], w=n, data=tmp)
    tmp$baseline <- coef(M_baseline)[1] + OCC[,baseline]
  } else if (y == "y_employed_laborforce") {
    tmp$ypct <- tmp$y / tmp$n
    baseline <- "laus_employed"
    M_baseline <- lm(ypct ~ OCC[,baseline], w=n, data=tmp)
    tmp$baseline <- coef(M_baseline)[1] + coef(M_baseline)[2] * OCC[,baseline]
  } else if (y == "y_atwork_employed") {
    M_baseline <- NULL
  }

  y_bin <- round(y_bin)

  ################################################################################################################
  # occupation model

  registerDoMC(ceiling(30 / nrow(combs)))
  message("fitting model: ", i_comb, " of ", nrow(combs), ": occupation model")
  if (y == "y_atwork_employed") {
    M_occ <- cv.glmnet(x=OCC, y=y_bin[,2:1], family="binomial", parallel=TRUE)
    tmp$occ <- round(invlogit(predict(M_occ, newx=OCC)[,1]), 2)
  } else {
    M_occ <- cv.glmnet(x=OCC[, colnames(OCC) != baseline], y=y_bin[,2:1], family="binomial", offset=tmp$baseline, parallel=TRUE)
    tmp$occ <- round(invlogit(predict(M_occ, newx=OCC[, colnames(OCC) != baseline], newoffset=tmp$baseline)[,1]), 2)
  }

  ################################################################################################################
  # weekly state claims data, aggregated using closest 4 weeks

  # CPS surveys are generally, the calendar week (Sunday through Saturday) that includes the 12th day of the month
  reference_date <- as.Date(paste0(combs$survey[i_comb], "_12"), "%Y_%m_%d")

  # check to make sure state claims weekly data is up to date
  if (month(reference_date) == 12) {
    state_claims_next_month <- as.Date(paste0(year(reference_date) + 1, "_1_1"), "%Y_%m_%d")
  } else {
    state_claims_next_month <- as.Date(paste0(year(reference_date), "_", month(reference_date) + 1, "_1"), "%Y_%m_%d")
  }
  if (max(state_claims_weekly$date) < state_claims_next_month)
    stop("weekly claims data isn't up to date")

  dates <- unique(state_claims_weekly$date)
  ok_dates <- order(abs(dates - reference_date))
  ok_dates <- dates[ok_dates][1:4]
  
  weekly_agg <- data.frame(
    state_claims_weekly[state_claims_weekly$date %in% ok_dates,] %>% 
    group_by(state) %>% 
    summarize(claims=sum(claims)))
  weekly_agg <- left_join(weekly_agg, 
    tmp %>% 
    group_by(state) %>% 
    summarize(cnip=sum(n_cnip)), by="state")
  weekly_agg$weekly <- weekly_agg$claims / weekly_agg$cnip
  weekly_agg$z_weekly <- (weekly_agg$weekly - mean(weekly_agg$weekly)) / (2 * sd(weekly_agg$weekly))
  tmp <- left_join(tmp, weekly_agg[, c("state", "z_weekly")], by="state")

  ################################################################################################################
  # lmer model

  message("fitting model: ", i_comb, " of ", nrow(combs), ": lmer model")
  lmer_dat <- data.frame(
    tmp %>% 
      group_by(occ, z_weekly, region, state, agegrp, agegrp2, female, race, edu, married, citizen, whitecollege, whitefemale, marriedfemale) %>% 
      summarize_at(c("n", "y"), sum))
  lmer_dat$occ <- logit(lmer_dat$occ)
  y_bin <- round(cbind(lmer_dat$y, lmer_dat$n - lmer_dat$y))

  eval(parse(text=paste0("
    M_try <- try(M_lmer <- glmer(
      y_bin ~ 1 + z_weekly + ", paste0(xs_binary, collapse=" + "), " + 
      ", paste0("(1 | ", xs_cat, ")", collapse=" + "), ", 
      data=lmer_dat, family='binomial', offset=occ, 
      control=glmerControl(optimizer='nloptwrap'), 
      verbose=FALSE), silent=TRUE)
  ")))
  if (class(M_try) == "try-error") {
    if (y != "y_atwork_employed")
      stop("lmer didn't run for some reason")
    yhat_lmer <- lmer_dat$occ
    fix <- NULL
    ran <- NULL
  } else {
    yhat_lmer <- as.numeric(predict(M_lmer))
    fix <- fixef(M_lmer)
    ran <- ranef(M_lmer)
  }

  ################################################################################################################
  # interactions model

  # only run interactions model if there's enough variation in response
  # otherwise the model takes forever to run
  if (abs(sum(y_bin[,1]) / sum(y_bin) - 0.5) > 0.485) {
    if (y != "y_atwork_employed")
      stop("interactions didn't run for some reason")
    M_inter <- NULL
  } else {
    message("fitting model: ", i_comb, " of ", nrow(combs), ": interactions model")
    inter_dat <- GetInteractionsMatrix(dat=lmer_dat, inters)
    registerDoMC(ceiling(30 / nrow(combs)))
    M_inter <- cv.glmnet(y=y_bin[,2:1], x=inter_dat, offset=yhat_lmer, family="binomial", parallel=TRUE)
  }

  ################################################################################################################
  # national target

  ok <- cps$survey == combs$survey[i_comb]
  tmp <- cps[ok,]
  national_cnip <- sum(tmp$n_cnip)
  if (y == "y_laborforce_cnip") {
    national_target <- sum(tmp$n_laborforce) / sum(tmp$n_cnip)
  } else if (y == "y_employed_laborforce") {
    national_target <- sum(tmp$n_employed) / sum(tmp$n_laborforce)
  } else if (y == "y_atwork_employed") {
    national_target <- sum(tmp$n_atwork) / sum(tmp$n_employed)
  } else {
    stop("bad target")    
  }

  ################################################################################################################
  # close it out

  out <- list(
    M_baseline=M_baseline, 
    M_occ=M_occ, 
    weekly_agg=weekly_agg, 
    fix=fix, 
    ran=ran, 
    M_inter=M_inter, 
    national_cnip=national_cnip, 
    national_target=national_target)

  saveRDS(out, file=paste0("model_save/cps_", combs$survey[i_comb], "_", combs$y[i_comb], ".rds"))

  message("****** ", i_comb, " of ", nrow(combs), ": done")

  return(out)

}))

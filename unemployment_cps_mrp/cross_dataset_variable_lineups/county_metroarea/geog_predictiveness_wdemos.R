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
library(stringr)
library(tidycensus)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/run_mrp/county_metroarea/")

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

logit <- function(x, digits=5)
  return(-1 * log(1/pmin(1 - 1 * 10^(-1 * digits), pmax(1 * 10^(-1 * digits), x)) - 1))

################################################################################################################

vertica_username <- system("echo $VERTICA_USERNAME", intern=TRUE)
vertica_password <- system("echo $VERTICA_PASSWORD", intern=TRUE)
vertica_host <- system("echo $VERTICA_HOST", intern=TRUE)

VerticaLoadFromText <- function(table_name=NULL, query=NULL, colClasses=NULL) {
  if (is.null(table_name)) {
    keep_table <- FALSE
    table_name <- paste0("yg_tmp_", round(runif(1, 1, 10000000)))
  } else {
    keep_table <- TRUE
  }
  if (!is.null(query))
    system(paste0("
    /opt/vertica/bin/vsql -U ", vertica_username, " -w ", vertica_password, " -h ", vertica_host, " -c \"
    drop table if exists analytics.", table_name, " cascade;
    create table analytics.", table_name, " as ", query, "
    \""))

  system(paste0("~/vertica_unload.sh ", table_name, ".txt ", table_name))
  dat <- as.data.frame(data.table::fread(paste0(table_name, ".txt"), colClasses=colClasses))
  colnames(dat) <- tolower(colnames(dat))
  system(paste0("rm ", table_name, ".txt"))

  if (!keep_table) {
    system(paste0("
    /opt/vertica/bin/vsql -U ", vertica_username, " -w ", vertica_password, " -h ", vertica_host, " -c \"
    truncate table analytics.", table_name, ";
    drop table analytics.", table_name, " cascade;
    \""))
  }
  return(dat)
}

################################################################################################################
# cps survey data

cps <- VerticaLoadFromText(query="
  select 
  year, month, 
  NULL as region, 
  county as fips, 
  floor(case when metarea = 4130 then 4120 else metarea end / 10) * 10 as metarea, 
  decode(statefip, 
    2, 'AK', 
    1, 'AL', 
    5, 'AR', 
    4, 'AZ', 
    6, 'CA', 
    8, 'CO', 
    9, 'CT', 
    11, 'DC', 
    10, 'DE', 
    12, 'FL', 
    13, 'GA', 
    15, 'HI', 
    19, 'IA', 
    16, 'ID', 
    17, 'IL', 
    18, 'IN', 
    20, 'KS', 
    21, 'KY', 
    22, 'LA', 
    25, 'MA', 
    24, 'MD', 
    23, 'ME', 
    26, 'MI', 
    27, 'MN', 
    29, 'MO', 
    28, 'MS', 
    30, 'MT', 
    37, 'NC', 
    38, 'ND', 
    31, 'NE', 
    33, 'NH', 
    34, 'NJ', 
    35, 'NM', 
    32, 'NV', 
    36, 'NY', 
    39, 'OH', 
    40, 'OK', 
    41, 'OR', 
    42, 'PA', 
    44, 'RI', 
    45, 'SC', 
    46, 'SD', 
    47, 'TN', 
    48, 'TX', 
    49, 'UT', 
    51, 'VA', 
    50, 'VT', 
    53, 'WA', 
    55, 'WI', 
    54, 'WV', 
    56, 'WY',
    NULL) as state, 
  case when age < 18 then 1
       when age < 20 then 2
       when age < 25 then 3
       when age < 30 then 4
       when age < 35 then 5
       when age < 40 then 6
       when age < 45 then 7
       when age < 50 then 8
       when age < 55 then 9
       when age < 60 then 10
       when age < 65 then 11
       when age < 70 then 12
       when age < 75 then 13
       when age >= 75 then 14 else NULL end as agegrp,
  decode(sex, 1, 1, 2, 2, NULL) as female,
  case when hispan in (0, 901, 902) then
    case when race in (100, 999) then 1 -- white
         when race = 200 then 2 -- black
         when race in (650, 651, 652) then 4 -- asian
         when race = 300 then 5 -- native american
         else 6 end -- other
    else 3 end as race, -- hispanic
  case when educ = 999 then NULL
       when educ <= 72 then 1 -- no HS
       when educ <= 73 then 2 -- HS
       when educ <= 110 then 3 -- some college
       when educ <= 122 then 4 -- college
       when educ <= 125 then 5 -- post-grad
       else NULL end as edu,
  case when marst in (1, 2) then 2 -- married 
       when marst in (3, 4, 5, 6, 7) then 1 -- non-married
       else NULL end as married,
  case when citizen <= 4 then 1 else 0 end as citizen, 
  -- raw sample size
  count(*) as n_cnip_raw, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then 1 else 0 end) as n_laborforce_raw, 
  sum(case when empstat in ('1', '10', '12') then 1 else 0 end) as n_employed_raw, 
  -- weighted population sizes
  sum(wtfinl) as n_cnip, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then wtfinl else 0 end) as n_laborforce, 
  sum(case when empstat in ('1', '10', '12') then wtfinl else 0 end) as n_employed, 
  -- design effect vars
  stddev(wtfinl) as sd_wt, 
  avg(wtfinl) as mu_wt
  from publicdata.cps_raw
  where ((year = 2019 and month >= 3) or (year = 2020 and month <= 2))
  and age >= 16
  group by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
")
for (i in grep("^[ny]_", colnames(cps), value=TRUE))
  cps[,i] <- as.numeric(cps[,i])
order_vars <- colnames(cps)[1:(grep("^[ny]_", colnames(cps))[1] - 1)]
eval(parse(text=paste0("ord <- order(", paste0("cps$", order_vars, collapse=", "), ")")))
cps <- cps[ord,]

# design effect

# design effect
design_effect <- data.frame(
  year=cps$year, 
  month=cps$month, 
  de=1 + (cps$sd_wt / cps$mu_wt)^2, 
  stringsAsFactors=FALSE)
design_effect$de[is.na(design_effect$de)] <- 1
design_effect <- sqldf("
  select 
  year, month, 
  avg(de) as de
  from design_effect
  group by 1, 2
")
cps <- left_join(cps, design_effect)

# response variables
cps$n_laborforce_cnip <- (cps$n_cnip_raw / cps$de)
cps$y_laborforce_cnip <- cps$n_laborforce_cnip * (cps$n_laborforce / cps$n_cnip)
cps$n_employed_laborforce <- (cps$n_laborforce_raw / cps$de)
cps$y_employed_laborforce <- cps$n_employed_laborforce * (cps$n_employed / cps$n_laborforce)
# cps$n_employed_cnip <- (cps$n_cnip_raw / cps$de)
# cps$y_employed_cnip <- cps$n_employed_cnip * (cps$n_employed / cps$n_cnip)

region_lookup <- c(
  'AK'=4, 'AL'=3, 'AR'=3, 'AZ'=4, 'CA'=4, 'CO'=4, 'CT'=1, 'DC'=5, 'DE'=1, 'FL'=3, 'GA'=3, 
  'HI'=4, 'IA'=2, 'ID'=4, 'IL'=2, 'IN'=2, 'KS'=2, 'KY'=3, 'LA'=3, 'MA'=1, 'MD'=1, 'ME'=1, 
  'MI'=2, 'MN'=2, 'MO'=2, 'MS'=3, 'MT'=4, 'NC'=3, 'ND'=2, 'NE'=2, 'NH'=1, 'NJ'=1, 'NM'=4, 
  'NV'=4, 'NY'=1, 'OH'=2, 'OK'=3, 'OR'=4, 'PA'=1, 'RI'=1, 'SC'=3, 'SD'=2, 'TN'=3, 'TX'=3, 
  'UT'=4, 'VA'=3, 'VT'=1, 'WA'=4, 'WI'=2, 'WV'=1, 'WY'=4)
cps$region <- region_lookup[cps$state]

cps$fips <- str_pad(cps$fips, 5, pad="0")
cps$fips[cps$fips == "00000"] <- NA
cps$metarea[cps$metarea >= 9990] <- NA

cps$survey <- paste0(cps$year, "_", stringr::str_pad(cps$month, width=2, pad="0"))

################################################################################################################
# predictors

lookup <- read.xlsx("fips_to_metarea_cps.xlsx")
lookup <- lookup[, c("fips", "metarea_truncated")]
colnames(lookup) <- c("fips", "metarea")

occ <- data.frame(
  readRDS("../../mrsp/create_joint/county/county_2018_joint.rds"), 
  readRDS("../../mrsp/create_joint/county/county_2018_occshift.rds"), 
  stringsAsFactors=FALSE)
gc()

xs <- grep("_", colnames(occ), value=TRUE)
cps_occ <- left_join(cps, occ)[,xs]

for (x in xs)
  occ[,x] <- occ[,x] * occ$n

registerDoMC(3)
surveys <- sort(unique(cps$survey))
raw <- foreach(i_survey = 1:length(surveys)) %dopar% {
  message("finding metarea xs: ", i_survey, " of ", length(surveys))
  survey <- surveys[i_survey]
  fipss <- unique(na.exclude(cps$fips[cps$survey == survey]))
  lookup_survey <- lookup[!(lookup$fips %in% fipss),]
  occ_survey <- occ[occ$fips %in% lookup_survey$fips,]
  occ_survey <- left_join(occ_survey, lookup_survey, by="fips")
  occ_survey <- occ_survey[!is.na(occ_survey$metarea),] %>% 
    group_by(state, agegrp, female, race, edu, married, citizen, metarea) %>% 
    summarize_at(c("n", xs), sum)
  for (x in xs)
    occ_survey[,x] <- occ_survey[,x] / occ_survey$n
  ix <- which(cps$survey == survey & is.na(cps_occ[,1]))
  X <- left_join(cps[ix,], occ_survey, by=c("metarea", "state", "agegrp", "female", "race", "edu", "married", "citizen"))[,xs]
  return(list(ix=ix, X=X))
}

for (i_survey in 1:length(surveys)) {
  message("inserting metarea xs: ", i_survey, " of ", length(surveys))
  cps_occ[raw[[i_survey]]$ix,] <- raw[[i_survey]]$X
}

cps_occ$baseline_laborforce <- 1 - cps_occ$emp_notinlaborforce
cps_occ$baseline_employed <- pmin(1, cps_occ$emp_employed / pmax(0.00001, cps_occ$baseline_laborforce))
cps_occ <- cps_occ[, grep("^emp_", colnames(cps_occ), invert=TRUE)]

# tmp <- data.frame(
#   x=round(invlogit(cps_occ$baseline_laborforce), 2), 
#   y=cps$y_laborforce_cnip / cps$n_laborforce_cnip, 
#   n=cps$n_laborforce_cnip)
# tmp <- tmp[complete.cases(tmp),]
# tmp <- tmp %>% group_by(x) %>% summarize(y=sum(y * n) / sum(n), n=sum(n))
# plot(tmp$x, tmp$y, cex=sqrt(tmp$n) / 100, xlim=c(0,1), ylim=c(0,1))
# abline(a=0, b=1)

# tmp <- data.frame(
#   x=round(invlogit(cps_occ$baseline_employed), 2), 
#   y=cps$y_employed_laborforce / cps$n_employed_laborforce, 
#   n=cps$n_employed_laborforce)
# tmp <- tmp[complete.cases(tmp),]
# tmp <- tmp %>% group_by(x) %>% summarize(y=sum(y * n) / sum(n), n=sum(n))
# plot(tmp$x, tmp$y, cex=sqrt(tmp$n) / 100, xlim=c(0,1), ylim=c(0,1))
# abline(a=0, b=1)


################################################################################################################

PlotXY <- function(x, y, n, main) {

  dat <- data.frame(x=round(x, 3), y=round(y, 3), n)
  dat <- dat[!is.na(dat$y),]

  M <- lm(x ~ y, w=n, data=dat)
  dat$xhat <- coef(M)[1] + coef(M)[2] * dat$y
  dat$xhat <- round(rnorm(length(dat$xhat), mean=dat$xhat, sd=sd(resid(M))), 3)
  dat$xhat <- pmax(min(dat$x, na.rm=TRUE), pmin(max(dat$x, na.rm=TRUE), dat$xhat))
  dat$xhat[!is.na(dat$x)] <- NA

  agg <- dat %>% group_by(x) %>% summarize(y=sum(y * n) / sum(n), n=sum(n))
  plot(agg$x, agg$y, cex=sqrt(agg$n) / 10, pch=20, col=rgb(0, 0, 0, alpha=0.25), xlim=c(0,1), ylim=c(0,1), 
    xlab="Occupation Prediction", ylab="CPS Actual", axes=FALSE, main=main)
  abline(lm(y ~ x, w=n, data=agg), lwd=4)
  agg <- dat %>% group_by(xhat) %>% summarize(y=sum(y * n) / sum(n), n=sum(n))
  points(agg$xhat, agg$y, cex=sqrt(agg$n) / 10, pch=20, col=rgb(1, 0, 0, alpha=0.25))
  abline(lm(y ~ xhat, w=n, data=agg), lwd=4, col="red")
  abline(a=0, b=1, col="grey", lwd=4)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  box(lwd=par()$lwd)

}

jpeg("check_synthetic_occs.jpeg", width=30, height=23, units="in", res=300)
par(mfrow=c(7,10), mar=c(1,1,3,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")

for (i in colnames(cps_occ)) {
  message(paste0("Labor Force Participation: ", substr(i, 1, 25)))
  PlotXY(
    x=cps_occ[,i], 
    y=cps$y_laborforce_cnip / cps$n_laborforce_cnip, 
    n=cps$n_laborforce_cnip, 
    main=paste0("Labor Force Participation:\n", substr(i, 1, 25)))
}

for (i in colnames(cps_occ)) {
  message(paste0("Labor Unemployment Rate: ", substr(i, 1, 25)))
  PlotXY(
    x=cps_occ[,i], 
    y=cps$y_employed_laborforce / cps$n_employed_laborforce, 
    n=cps$n_employed_laborforce, 
    main=paste0("Unemployment Rate:\n", substr(i, 1, 25)))
}

dev.off()

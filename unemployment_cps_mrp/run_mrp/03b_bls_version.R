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
# library(tidyr)
library(SDMTools)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/run_mrp/")

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="PT Sans")
}

source("../helper_functions/FindDelta.R")

################################################################################################################
# which surveys? 

survey <- c("feb20")

######################################################################################################
# download and read data

system(paste0("wget https://www2.census.gov/programs-surveys/cps/datasets/2020/basic/", survey, "pub.dat.gz"))
system(paste0("zcat ", survey, "pub.dat.gz | cut -c 16-17,18-21,93-94,96-100,101-103,122-123,129-130,137-138,139-140,157-158,159-160,172-173,180-181,210-211,613-622,856-859,860-863 --output-delimiter=\\| > ", survey, ".txt"))
dat <- fread(paste0(survey, ".txt"), data.table=FALSE)
colnames(dat) <- c("month", "year", "statefip", "metarea", "county", "age", "sex", "educ", "race", "hispan", "marst", "citizen", "empstat", "reason", "wtfinl", "ind", "occ")
system(paste0("rm ", survey, ".txt"))
system(paste0("mv ", survey, "pub.dat.gz ../downloaded_data/cps_microdata"))

metarea_lookup <- read.xlsx("../cross_dataset_variable_lineups/county_metroarea/bls_metarea_lookup.xlsx")
tmp <- metarea_lookup$ipums_metarea
names(tmp) <- metarea_lookup$bls_metarea
tmp <- tmp[as.character(dat$metarea)]
tmp[is.na(tmp)] <- 9990
dat$metarea <- tmp

ok <- !is.na(dat$county) & dat$county != 0
dat$county[ok] <- as.numeric(paste0(str_pad(dat$statefip, width=2, pad="0"), str_pad(dat$county, width=3, pad="0")))[ok]
dat$wtfinl <- as.numeric(dat$wtfinl / 10000)

cps <- sqldf("
  select 
  year, 
  month, 
  county as fips, 
  floor(case when metarea = 4130 then 4120 else metarea end / 10) * 10 as metarea, 
  NULL as region, 
  case 
    when statefip = 2 then 'AK'
    when statefip = 1 then 'AL'
    when statefip = 5 then 'AR'
    when statefip = 4 then 'AZ'
    when statefip = 6 then 'CA'
    when statefip = 8 then 'CO'
    when statefip = 9 then 'CT'
    when statefip = 11 then 'DC'
    when statefip = 10 then 'DE'
    when statefip = 12 then 'FL'
    when statefip = 13 then 'GA'
    when statefip = 15 then 'HI'
    when statefip = 19 then 'IA'
    when statefip = 16 then 'ID'
    when statefip = 17 then 'IL'
    when statefip = 18 then 'IN'
    when statefip = 20 then 'KS'
    when statefip = 21 then 'KY'
    when statefip = 22 then 'LA'
    when statefip = 25 then 'MA'
    when statefip = 24 then 'MD'
    when statefip = 23 then 'ME'
    when statefip = 26 then 'MI'
    when statefip = 27 then 'MN'
    when statefip = 29 then 'MO'
    when statefip = 28 then 'MS'
    when statefip = 30 then 'MT'
    when statefip = 37 then 'NC'
    when statefip = 38 then 'ND'
    when statefip = 31 then 'NE'
    when statefip = 33 then 'NH'
    when statefip = 34 then 'NJ'
    when statefip = 35 then 'NM'
    when statefip = 32 then 'NV'
    when statefip = 36 then 'NY'
    when statefip = 39 then 'OH'
    when statefip = 40 then 'OK'
    when statefip = 41 then 'OR'
    when statefip = 42 then 'PA'
    when statefip = 44 then 'RI'
    when statefip = 45 then 'SC'
    when statefip = 46 then 'SD'
    when statefip = 47 then 'TN'
    when statefip = 48 then 'TX'
    when statefip = 49 then 'UT'
    when statefip = 51 then 'VA'
    when statefip = 50 then 'VT'
    when statefip = 53 then 'WA'
    when statefip = 55 then 'WI'
    when statefip = 54 then 'WV'
    when statefip = 56 then 'WY'
    else NULL end as state, 
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
  case when sex = 1 then 1
       when sex = 2 then 2
       else NULL end as female,
  case when hispan = 2 then
    case when race = 1 or race is NULL then 1 -- white
         when race = 2 then 2 -- black
         when race in (4, 5) then 4 -- asian
         when race = 3 then 5 -- native american
         else 6 end -- other
    else 3 end as race, -- hispanic
  case when educ <= 38 then 1 -- no HS
       when educ <= 39 then 2 -- HS
       when educ <= 42 then 3 -- some college
       when educ <= 43 then 4 -- college
       when educ <= 46 then 5 -- post-grad
       else NULL end as edu,
  case when marst in (1, 2, 3) then 2 -- married 
       when marst in (4, 5, 6, 7) then 1 -- non-married
       else NULL end as married,
  case when citizen <= 4 then 1 
       when citizen = 5 then 0
       else NULL end as citizen, 
  -- raw sample size
  count(*) as n_cnip_raw, 
  sum(case when empstat in (1, 2, 3, 4) then 1 else 0 end) as n_laborforce_raw, 
  sum(case when empstat in (1, 2) then 1 else 0 end) as n_employed_raw, 
  sum(case when empstat = 1 then 1 else 0 end) as n_atwork_raw, 
  -- weighted population sizes
  sum(wtfinl) as n_cnip, 
  sum(case when empstat in (1, 2, 3, 4) then wtfinl else 0 end) as n_laborforce, 
  sum(case when empstat in (1, 2) then wtfinl else 0 end) as n_employed, 
  -- sum(case when empstat = 1 then wtfinl else 0 end) as n_atwork, 
  -- design effect vars
  stdev(wtfinl) as sd_wt, 
  avg(wtfinl) as mu_wt
  from dat
  where age >= 16
  group by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
")

for (i in grep("^[ny]_", colnames(cps), value=TRUE))
  cps[,i] <- as.numeric(cps[,i])
order_vars <- colnames(cps)[1:(grep("^[ny]_", colnames(cps))[1] - 1)]
eval(parse(text=paste0("ord <- order(", paste0("cps$", order_vars, collapse=", "), ")")))
cps <- cps[ord,]

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
cps$n_atwork_employed <- (cps$n_employed_raw / cps$de)
cps$y_atwork_employed <- cps$n_atwork_employed * (cps$n_atwork / cps$n_employed)

region_lookup <- c(
  'AK'=4, 'AL'=3, 'AR'=3, 'AZ'=4, 'CA'=4, 'CO'=4, 'CT'=1, 'DC'=5, 'DE'=1, 'FL'=3, 'GA'=3, 
  'HI'=4, 'IA'=2, 'ID'=4, 'IL'=2, 'IN'=2, 'KS'=2, 'KY'=3, 'LA'=3, 'MA'=1, 'MD'=1, 'ME'=1, 
  'MI'=2, 'MN'=2, 'MO'=2, 'MS'=3, 'MT'=4, 'NC'=3, 'ND'=2, 'NE'=2, 'NH'=1, 'NJ'=1, 'NM'=4, 
  'NV'=4, 'NY'=1, 'OH'=2, 'OK'=3, 'OR'=4, 'PA'=1, 'RI'=1, 'SC'=3, 'SD'=2, 'TN'=3, 'TX'=3, 
  'UT'=4, 'VA'=3, 'VT'=1, 'WA'=4, 'WI'=2, 'WV'=1, 'WY'=4)
cps$region <- region_lookup[cps$state]

cps$survey <- paste0(cps$year, "_", stringr::str_pad(cps$month, width=2, pad="0"))

cps$female <- cps$female - 1
cps$married <- cps$married - 1
cps$whitecollege <- paste0(as.numeric(cps$race == 1), as.numeric(cps$edu >= 4))
cps$whitefemale <- paste0(as.numeric(cps$race == 1), cps$female)
cps$marriedfemale <- paste0(cps$married, cps$female)

saveRDS(cps, file=paste0("data/cps_", sort(unique(cps$survey)), ".rds"))

surveys <- sort(unique(cps$survey))
print(cbind(apply(is.na(cps), 2, sum)))

gc()

################################################################################################################

source("../helper_functions/run_cps_models.R")

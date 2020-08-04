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
library(tidyr)
library(SDMTools)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/run_mrp/")

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

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

source("../helper_functions/FindDelta.R")

################################################################################################################
# which surveys? 

# surveys <- c("2020_06", "2020_05", "2020_04", "2020_03", "2020_02", "2020_01", "2019_02", "2019_01", "2010_01", "2007_01")
surveys <- c("2009_10", "2010_06", "2009_06")

cps <- VerticaLoadFromText(query=paste0("
  select 
  year, 
  month, 
  county as fips, 
  floor(case when metarea = 4130 then 4120 else metarea end / 10) * 10 as metarea, 
  NULL as region, 
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
  -- raw sample size
  count(*) as n_cnip_raw, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then 1 else 0 end) as n_laborforce_raw, 
  sum(case when empstat in ('1', '10', '12') then 1 else 0 end) as n_employed_raw, 
  sum(case when empstat in ('1', '10', '12') and whyabsnt <> '15' then 1 else 0 end) as n_atwork_raw, 
  -- weighted population sizes
  sum(wtfinl) as n_cnip, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then wtfinl else 0 end) as n_laborforce, 
  sum(case when empstat in ('1', '10', '12') then wtfinl else 0 end) as n_employed, 
  sum(case when empstat in ('1', '10', '12') and whyabsnt <> '15' then wtfinl else 0 end) as n_atwork, 
  -- design effect vars
  stddev(wtfinl) as sd_wt, 
  avg(wtfinl) as mu_wt
  from publicdata.cps_raw
  where age >= 16
  and year >= 2000
  and empstat > 0
  and year || '_' || lpad(cast(month as char(2)), 2, '0') in ('", paste0(surveys, collapse="','"), "')
  group by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
"))
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

print(cbind(apply(is.na(cps), 2, sum)))

registerDoMC(30)
foreach (survey = surveys) %dopar% {
  message("saving rds: ", survey)
  saveRDS(cps[cps$survey == survey,], paste0("data/cps_", survey, ".rds"))
}

gc()

################################################################################################################
# run models

source("../helper_functions/run_cps_models.R")

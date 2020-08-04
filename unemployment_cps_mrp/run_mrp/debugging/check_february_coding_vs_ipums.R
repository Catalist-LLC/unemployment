library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(gtools)
library(foreach)
library(doMC)
library(RJDBC)
library(stringr)
library(openxlsx)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/run_mrp/")

######################################################################################################

vertica_username <- system("echo $VERTICA_USERNAME", intern=TRUE)
vertica_password <- system("echo $VERTICA_PASSWORD", intern=TRUE)
vertica_host <- system("echo $VERTICA_HOST", intern=TRUE)

drv <- JDBC("com.vertica.jdbc.Driver", "~/RJDBC/vertica-jdk5-6.1.3-0.jar")
conn <- dbConnect(drv, paste0("jdbc:vertica://", vertica_host, ":5433/vertica4"), vertica_username, vertica_password)       ### analytics cluster
V <- function(x) dbGetQuery(conn, x)

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

######################################################################################################
# download and read data

system("wget https://www2.census.gov/programs-surveys/cps/datasets/2020/basic/feb20pub.dat.gz")
system("zcat feb20pub.dat.gz | cut -c 16-17,18-21,93-94,96-100,101-103,122-123,129-130,137-138,139-140,157-158,159-160,172-173,180-181,613-622,856-859,860-863 --output-delimiter=\\| > feb20.txt")
dat <- fread("feb20.txt", data.table=FALSE)
colnames(dat) <- c("month", "year", "statefip", "metarea", "county", "age", "sex", "educ", "race", "hispan", "marst", "citizen", "empstat", "wtfinl", "ind", "occ")
system("rm feb20.txt")
system("mv feb20pub.dat.gz ../downloaded_data/cps_microdata")

metarea_lookup <- read.xlsx("../cross_dataset_variable_lineups/county_metroarea/bls_metarea_lookup.xlsx")
tmp <- metarea_lookup$ipums_metarea
names(tmp) <- metarea_lookup$bls_metarea
tmp <- tmp[as.character(dat$metarea)]
tmp[is.na(tmp)] <- 9990
dat$metarea <- tmp

ok <- !is.na(dat$county) & dat$county != 0
dat$county[ok] <- as.numeric(paste0(str_pad(dat$statefip, width=2, pad="0"), str_pad(dat$county, width=3, pad="0")))[ok]
dat$wtfinl <- as.numeric(dat$wtfinl / 10000)

######################################################################################################
# roll up

bls <- sqldf("
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
       when age >= 70 then 13 else NULL end as agegrp,
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
  sum(case when empstat = 1 then wtfinl else 0 end) as n_atwork, 
  -- design effect vars
  stdev(wtfinl) as sd_wt, 
  avg(wtfinl) as mu_wt
  from dat
  where age >= 16
  group by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
")

######################################################################################################
# get ipums data

ipums <- V("
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
       when age >= 70 then 13 else NULL end as agegrp,
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
  sum(case when empstat in ('1', '10') then 1 else 0 end) as n_atwork_raw, 
  -- weighted population sizes
  sum(wtfinl) as n_cnip, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then wtfinl else 0 end) as n_laborforce, 
  sum(case when empstat in ('1', '10', '12') then wtfinl else 0 end) as n_employed, 
  sum(case when empstat in ('1', '10') then wtfinl else 0 end) as n_atwork, 
  -- design effect vars
  stddev(wtfinl) as sd_wt, 
  avg(wtfinl) as mu_wt
  from publicdata.cps_raw
  where age >= 16
  and year = 2020
  and month = 2
  and empstat > 0
  group by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
  order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
")

######################################################################################################

agg <- data.frame(full_join(
  bls %>% group_by(fips) %>% summarize(bls=sum(n_cnip_raw)), 
  ipums %>% group_by(fips) %>% summarize(ipums=sum(n_cnip_raw))))

agg <- data.frame(full_join(
  bls %>% group_by(metarea) %>% summarize(bls=sum(n_cnip_raw)), 
  ipums %>% group_by(metarea) %>% summarize(ipums=sum(n_cnip_raw))))

######################################################################################################
# check xs

print(table(unlist(bls[,1:12] == ipums[,1:12])))

par(mfrow=c(4,2))
ns <- grep("^n_", colnames(bls), value=TRUE)
for (i in ns) {
  print(i)
  print(cbind(table(bls[,i] == ipums[,i], exclude=NULL)))
  plot(log(bls[,i]), log(ipums[,i]), pch=20, main=i)
  abline(a=0, b=1, col="grey")
}

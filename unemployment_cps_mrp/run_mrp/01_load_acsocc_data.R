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
library(RJDBC)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/run_mrp")

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################

vertica_username <- system("echo $VERTICA_USERNAME", intern=TRUE)
vertica_password <- system("echo $VERTICA_PASSWORD", intern=TRUE)
vertica_host <- system("echo $VERTICA_HOST", intern=TRUE)

drv <- JDBC("com.vertica.jdbc.Driver", "~/RJDBC/vertica-jdk5-6.1.3-0.jar")
conn <- dbConnect(drv, paste0("jdbc:vertica://", vertica_host, ":5433/vertica4"), vertica_username, vertica_password)       ### analytics cluster
V <- function(x) dbGetQuery(conn, x)

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
# acs

acs <- V("
  select 
  year, 
  NULL as region, 
  decode(stateicp, 
    '01', 'CT', 
    '02', 'ME', 
    '03', 'MA', 
    '04', 'NH', 
    '05', 'RI', 
    '06', 'VT', 
    '11', 'DE', 
    '12', 'NJ', 
    '13', 'NY', 
    '14', 'PA', 
    '21', 'IL', 
    '22', 'IN', 
    '23', 'MI', 
    '24', 'OH', 
    '25', 'WI', 
    '31', 'IA', 
    '32', 'KS', 
    '33', 'MN', 
    '34', 'MO', 
    '35', 'NE', 
    '36', 'ND', 
    '37', 'SD', 
    '40', 'VA', 
    '41', 'AL', 
    '42', 'AR', 
    '43', 'FL', 
    '44', 'GA', 
    '45', 'LA', 
    '46', 'MS', 
    '47', 'NC', 
    '48', 'SC', 
    '49', 'TX', 
    '51', 'KY', 
    '52', 'MD', 
    '53', 'OK', 
    '54', 'TN', 
    '56', 'WV', 
    '61', 'AZ', 
    '62', 'CO', 
    '63', 'ID', 
    '64', 'MT', 
    '65', 'NV', 
    '66', 'NM', 
    '67', 'UT', 
    '68', 'WY', 
    '71', 'CA', 
    '72', 'OR', 
    '73', 'WA', 
    '81', 'AK', 
    '82', 'HI', 
    '98', 'DC', 
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
  case when hispan in (0, 9) then
    case when race = 1 then 1 -- white
         when race = 2 then 2 -- black
         when race in (4, 5, 6) then 4 -- asian
         when race = 3 then 5 -- native american
         else 6 end -- other
    else 3 end as race, -- hispanic
  case when educd = 999 then NULL
       when educd < 62 then 1 -- no HS
       when educd in (62, 63, 64) then 2 -- HS
       when educd < 101 then 3 -- some college
       when educd < 114 then 4 -- college
       when educd <= 116 then 5 -- post-grad
       else NULL end as edu,
  case when marst in (1, 2) then 2 -- married 
       when marst in (3, 4, 5, 6) then 1 -- non-married
       else NULL end as married,
  case when citizen <= 2 then 1 else 0 end as citizen, 
  sum(perwt) as n
  from publicdata.longterm_pums_data
  where age >= 16
  and year >= 2000
  group by 1, 2, 3, 4, 5, 6, 7, 8, 9
  order by 1, 2, 3, 4, 5, 6, 7, 8, 9
")

region_lookup <- c(
  'AK'=4, 'AL'=3, 'AR'=3, 'AZ'=4, 'CA'=4, 'CO'=4, 'CT'=1, 'DC'=5, 'DE'=1, 'FL'=3, 'GA'=3, 
  'HI'=4, 'IA'=2, 'ID'=4, 'IL'=2, 'IN'=2, 'KS'=2, 'KY'=3, 'LA'=3, 'MA'=1, 'MD'=1, 'ME'=1, 
  'MI'=2, 'MN'=2, 'MO'=2, 'MS'=3, 'MT'=4, 'NC'=3, 'ND'=2, 'NE'=2, 'NH'=1, 'NJ'=1, 'NM'=4, 
  'NV'=4, 'NY'=1, 'OH'=2, 'OK'=3, 'OR'=4, 'PA'=1, 'RI'=1, 'SC'=3, 'SD'=2, 'TN'=3, 'TX'=3, 
  'UT'=4, 'VA'=3, 'VT'=1, 'WA'=4, 'WI'=2, 'WV'=1, 'WY'=4)
acs$region <- region_lookup[acs$state]

################################################################################################################
# occupation and industry data

occ <- read.xlsx("../cross_dataset_variable_lineups/occupation_and_industry/occ_ind_lookup.xlsx", sheet="ipums_occ")
occ <- occ[!is.na(occ$occ2010),]
occ$lab <- gsub("[^a-z]", "", tolower(occ$joint_category))
occ <- split(x=occ$occ2010, f=occ$lab)
occ <- sapply(occ, function(i) paste0(i, collapse=","))

ind <- read.xlsx("../cross_dataset_variable_lineups/occupation_and_industry/occ_ind_lookup.xlsx", sheet="ipums_ind")
colnames(ind) <- tolower(colnames(ind))
ind <- ind[!is.na(as.numeric(ind$ind)) & ind$ind != "0",]
ind$lab <- gsub("[^a-z]", "", tolower(ind$code))
ind <- split(x=ind$ind, f=ind$lab)
ind <- sapply(ind, function(i) paste0(i, collapse=","))

occ <- V(paste0("
  select 
  decode(stateicp, 
    '01', 'CT', 
    '02', 'ME', 
    '03', 'MA', 
    '04', 'NH', 
    '05', 'RI', 
    '06', 'VT', 
    '11', 'DE', 
    '12', 'NJ', 
    '13', 'NY', 
    '14', 'PA', 
    '21', 'IL', 
    '22', 'IN', 
    '23', 'MI', 
    '24', 'OH', 
    '25', 'WI', 
    '31', 'IA', 
    '32', 'KS', 
    '33', 'MN', 
    '34', 'MO', 
    '35', 'NE', 
    '36', 'ND', 
    '37', 'SD', 
    '40', 'VA', 
    '41', 'AL', 
    '42', 'AR', 
    '43', 'FL', 
    '44', 'GA', 
    '45', 'LA', 
    '46', 'MS', 
    '47', 'NC', 
    '48', 'SC', 
    '49', 'TX', 
    '51', 'KY', 
    '52', 'MD', 
    '53', 'OK', 
    '54', 'TN', 
    '56', 'WV', 
    '61', 'AZ', 
    '62', 'CO', 
    '63', 'ID', 
    '64', 'MT', 
    '65', 'NV', 
    '66', 'NM', 
    '67', 'UT', 
    '68', 'WY', 
    '71', 'CA', 
    '72', 'OR', 
    '73', 'WA', 
    '81', 'AK', 
    '82', 'HI', 
    '98', 'DC', 
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
  case when hispan in (0, 9) then
    case when race = 1 then 1 -- white
         when race = 2 then 2 -- black
         when race in (4, 5, 6) then 4 -- asian
         when race = 3 then 5 -- native american
         else 6 end -- other
    else 3 end as race, -- hispanic
  case when educd = 999 then NULL
       when educd < 62 then 1 -- no HS
       when educd in (62, 63, 64) then 2 -- HS
       when educd < 101 then 3 -- some college
       when educd < 114 then 4 -- college
       when educd <= 116 then 5 -- post-grad
       else NULL end as edu,
  case when marst in (1, 2) then 2 -- married 
       when marst in (3, 4, 5, 6) then 1 -- non-married
       else NULL end as married,
  case when citizen <= 2 then 1 else 0 end as citizen, 
  sum(case when empstat = 1 and occ2010 is not null then perwt else 0 end) as n_occ, 
  sum(case when empstat = 1 and ind is not null then perwt else 0 end) as n_ind, 
  sum(perwt) as n_emp, 
  ", paste0("sum(case when empstat = 1 and occ2010 in (", occ, ") then perwt else 0 end) / 
    sum(case when empstat = 1 and occ2010 is not null then perwt else 0 end) as occ_", names(occ), collapse=", "), ", 
  ", paste0("sum(case when empstat = 1 and ind in (", ind, ") then perwt else 0 end) / 
    sum(case when empstat = 1 and ind is not null then perwt else 0 end) as ind_", names(ind), collapse=", "), ", 
  sum(case when empstat = 1 then perwt else 0 end) / sum(perwt) as emp_employed, 
  sum(case when empstat = 2 then perwt else 0 end) / sum(perwt) as emp_unemployed, 
  sum(case when empstat = 3 then perwt else 0 end) / sum(perwt) as emp_notinlaborforce
  from publicdata.longterm_pums_data
  where age >= 16
  and year >= 2013
  and year <= 2017
  group by 1, 2, 3, 4, 5, 6, 7
  order by 1, 2, 3, 4, 5, 6, 7
"))
occ$region <- region_lookup[occ$state]

################################################################################################################
# xs and interactions

xs <- c("region", "state", "agegrp", "female", "race", "edu", "married", "citizen")

# check if all xs have the same values
check <- cbind(sapply(xs, function(i)
  all(sort(unique(acs[,i])) == sort(unique(occ[,i])))
))
if (any(!check)) {
  print(check)
  stop("some xs have different values")
}

inters <- as.data.frame(combinations(n=length(xs), r=2, v=xs), stringsAsFactors=FALSE)
colnames(inters) <- c("x1", "x2")
drop <- 
  (inters$x1 %in% c("region", "state") & inters$x2 %in% c("region", "state"))
inters <- inters[!drop,]
inters$full <- paste0(inters$x1, "__", inters$x2)

source("../helper_functions/GetInteractionsMatrix.R")
print(system.time(acs_inter <- GetInteractionsMatrix(dat=acs, inters)))
print(system.time(occ_inter <- GetInteractionsMatrix(dat=occ, inters)))

################################################################################################################
# some interactions + recoding

acs$female <- acs$female - 1
acs$married <- acs$married - 1
acs$whitecollege <- paste0(as.numeric(acs$race == 1), as.numeric(acs$edu >= 4))
acs$whitefemale <- paste0(as.numeric(acs$race == 1), acs$female)
acs$marriedfemale <- paste0(acs$married, acs$female)

occ$female <- occ$female - 1
occ$married <- occ$married - 1
occ$whitecollege <- paste0(as.numeric(occ$race == 1), as.numeric(occ$edu >= 4))
occ$whitefemale <- paste0(as.numeric(occ$race == 1), occ$female)
occ$marriedfemale <- paste0(occ$married, occ$female)

################################################################################################################

saveRDS(acs, file="data/acs.rds")
saveRDS(occ, file="data/occ.rds")
saveRDS(acs_inter, file="data/acs_inter.rds")
saveRDS(occ_inter, file="data/occ_inter.rds")

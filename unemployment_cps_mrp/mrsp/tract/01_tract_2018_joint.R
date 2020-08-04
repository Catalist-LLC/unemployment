library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(openxlsx)
library(gtools)
library(tidyr)
library(RJDBC)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/mrsp/tract")

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

################################################################################################################
################################################################################################################
################################################################################################################
# load data

vertica_username <- system("echo $VERTICA_USERNAME", intern=TRUE)
vertica_password <- system("echo $VERTICA_PASSWORD", intern=TRUE)
vertica_host <- system("echo $VERTICA_HOST", intern=TRUE)

drv <- JDBC("com.vertica.jdbc.Driver", "~/RJDBC/vertica-jdk5-6.1.3-0.jar")
conn <- dbConnect(drv, paste0("jdbc:vertica://", vertica_host, ":5433/vertica4"), vertica_username, vertica_password)       ### analytics cluster
V <- function(x) dbGetQuery(conn, x)

################################################################################################################
# load survey data
# use 2014-2018 as the survey joint

survey <- V("
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
  sum(perwt) as n
  from publicdata.longterm_pums_data
  where age >= 16
  and year >= 2014
  and year <= 2018
  group by 1, 2, 3, 4, 5, 6, 7
  order by 1, 2, 3, 4, 5, 6, 7
")

################################################################################################################
# load 2018 geographic data

geog <- readRDS("../../downloaded_data/acs/tract_data_raw_2018.rds")

state_from_fips <- c(
  "02"="AK", "01"="AL", "05"="AR", "04"="AZ", "06"="CA", "08"="CO", "09"="CT", "11"="DC", 
  "10"="DE", "12"="FL", "13"="GA", "15"="HI", "19"="IA", "16"="ID", "17"="IL", "18"="IN", 
  "20"="KS", "21"="KY", "22"="LA", "25"="MA", "24"="MD", "23"="ME", "26"="MI", "27"="MN", 
  "29"="MO", "28"="MS", "30"="MT", "37"="NC", "38"="ND", "31"="NE", "33"="NH", "34"="NJ", 
  "35"="NM", "32"="NV", "36"="NY", "39"="OH", "40"="OK", "41"="OR", "42"="PA", "44"="RI", 
  "45"="SC", "46"="SD", "47"="TN", "48"="TX", "49"="UT", "51"="VA", "50"="VT", "53"="WA", 
  "55"="WI", "54"="WV", "56"="WY")
geog <- geog[substr(geog$GEOID, 1, 2) %in% names(state_from_fips),]
geog$state <- state_from_fips[substr(geog$GEOID, 1, 2)]
gc()

################################################################################################################
# get joint

source("../../helper_functions/RawACSToMarginal.R")
source("../../helper_functions/MarginalToJoint.R")
source("../../helper_functions/NVL.R")

states <- sort(unique(geog$state))

registerDoMC(26)
raw <- foreach(state = states) %dopar% {
  marg <- RawACSToMarginal(geog[geog$state == state,])
  return(MarginalToJoint(marg, survey, state))
}
mrsp <- bind_rows(raw)

mrsp$female <- mrsp$female - 1
mrsp$married <- mrsp$married - 1
mrsp$whitecollege <- paste0(as.numeric(mrsp$race == 1), as.numeric(mrsp$edu >= 4))
mrsp$whitefemale <- paste0(as.numeric(mrsp$race == 1), mrsp$female)
mrsp$marriedfemale <- paste0(mrsp$married, mrsp$female)

region_lookup <- c(
  'AK'=4, 'AL'=3, 'AR'=3, 'AZ'=4, 'CA'=4, 'CO'=4, 'CT'=1, 'DC'=5, 'DE'=1, 'FL'=3, 'GA'=3, 
  'HI'=4, 'IA'=2, 'ID'=4, 'IL'=2, 'IN'=2, 'KS'=2, 'KY'=3, 'LA'=3, 'MA'=1, 'MD'=1, 'ME'=1, 
  'MI'=2, 'MN'=2, 'MO'=2, 'MS'=3, 'MT'=4, 'NC'=3, 'ND'=2, 'NE'=2, 'NH'=1, 'NJ'=1, 'NM'=4, 
  'NV'=4, 'NY'=1, 'OH'=2, 'OK'=3, 'OR'=4, 'PA'=1, 'RI'=1, 'SC'=3, 'SD'=2, 'TN'=3, 'TX'=3, 
  'UT'=4, 'VA'=3, 'VT'=1, 'WA'=4, 'WI'=2, 'WV'=1, 'WY'=4)
mrsp$region <- region_lookup[mrsp$state]

### add demoid
n_cores <- 30
cores <- ceiling(1:nrow(mrsp) / nrow(mrsp) * n_cores)
registerDoMC(30)
demoid <- foreach(i = 1:n_cores) %dopar% {
  return(apply(mrsp[cores == i, c("agegrp", "female", "race", "edu", "married", "citizen")], 1, paste0, collapse="_"))
}
mrsp$demoid <- unlist(demoid)

################################################################################################################
# save

if (!all(complete.cases(mrsp)))
  stop("missing data")

saveRDS(mrsp, file="tract_2018_joint.rds")

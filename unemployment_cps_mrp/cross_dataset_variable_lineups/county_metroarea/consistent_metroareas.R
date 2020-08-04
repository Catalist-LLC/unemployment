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
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/run_mrp/adhoc/county_metroarea/")

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
# cps

cps <- V("
  select 
  year, month, county, metarea, count(*) as n
  from publicdata.cps_raw
  where year >= 2000
  group by 1, 2, 3, 4
  order by 1, 2, 3, 4
")

years <- sort(unique(cps$year))
years <- years[years <= 2019]
tmp <- sqldf(paste0("
  select 
  metarea, 
  ", paste0("sum(case when year = ", years, " then n else 0 end) as n", years, collapse=", "), "
  from cps
  group by 1
"))

write.table(tmp, file=pipe("pbcopy"), sep="\t", row.names=FALSE)

tmp <- sqldf(paste0("
  select 
  county, 
  sum(n) as n
  from cps
  group by 1
"))
tmp$p <- tmp$n / sum(tmp$n)

cps$bad <- cps$metarea >= 9997 & cps$county == 0

################################################################################################################

dat <- V("
  select 
  case when county is not null and county != 0 then '1 county'
       when metarea is not null and metarea < 9997 then '2 metarea'
       else '3 other' end as geo, 
  count(*) as n, 
  sum(wtfinl) as cnip, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then wtfinl else 0 end) as laborforce, 
  sum(case when empstat in ('1', '10', '12') then wtfinl else 0 end) as employed
  from publicdata.cps_raw
  where year >= 2010
  and age >= 25
  and age <= 54
  group by 1
  order by 1
")
dat$employed <- dat$employed / dat$laborforce
dat$laborforce <- dat$laborforce / dat$cnip

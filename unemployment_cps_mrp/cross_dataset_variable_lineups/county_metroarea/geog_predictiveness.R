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

################################################################################################################

cps <- V("
  select 
  county, 
  case when metarea = 4130 then 4120 else metarea end as metarea, 
  count(*) as n_cnip_raw, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then 1 else 0 end) as n_laborforce_raw, 
  sum(case when empstat in ('1', '10', '12') then 1 else 0 end) as n_employed_raw, 
  sum(wtfinl) as n_cnip, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then wtfinl else 0 end) as n_laborforce, 
  sum(case when empstat in ('1', '10', '12') then wtfinl else 0 end) as n_employed
  from publicdata.cps_raw
  where ((year = 2019 and month >= 3) or (year = 2020 and month <= 2))
  -- where year >= 2000
  and age >= 16
  group by 1, 2
  order by 1, 2
")
cps$unemployed <- 1 - cps$n_employed / cps$n_laborforce
cps$laborforce <- cps$n_laborforce / cps$n_cnip

cps$county[cps$county == 0] <- NA
cps$metarea[cps$metarea >= 9997] <- NA
for (i in c("county")) {
  ok <- !is.na(cps[,i])
  cps[,i] <- str_pad(cps[,i], width=5, pad="0")
}

################################################################################################################

variables <- read.xlsx("../../../mrsp/variables/acs_tables_variables.xlsx", sheet="variables")
variables <- variables[variables$keep %in% c("emp"),]
variables <- grep("--", variables$CONCAT, value=TRUE, invert=TRUE)

geog <- get_acs(
  geography="county", 
  variables=variables, 
  year=2017, 
  survey="acs5")

marg <- data.frame(
  geog %>% 
    group_by(GEOID) %>% 
    summarize(
      emp_total=sum(estimate[variable %in% c('B23025_001')], na.rm=TRUE), 
      emp_employed=sum(estimate[variable %in% c('B23025_004')], na.rm=TRUE), 
      emp_unemployed=sum(estimate[variable %in% c('B23025_005')], na.rm=TRUE), 
      emp_military=sum(estimate[variable %in% c('B23025_006')], na.rm=TRUE), 
      emp_notinlaborforce=sum(estimate[variable %in% c('B23025_007')], na.rm=TRUE)
    ))

acs <- data.frame(
  county=marg$GEOID, 
  acs_total=marg$emp_total, 
  acs_laborforce=1 - marg$emp_notinlaborforce / marg$emp_total, 
  acs_unemployed=marg$emp_unemployed / (marg$emp_unemployed + marg$emp_employed), 
  stringsAsFactors=FALSE)
plot(acs$acs_laborforce, acs$acs_unemployed, cex=sqrt(acs$acs_total) / 1000)

################################################################################################################

county_to_cbsa <- read.xlsx("list1_2020.xlsx", sheet="machine")
county_to_cbsa <- county_to_cbsa[complete.cases(county_to_cbsa),]
county_to_cbsa$county <- str_pad(county_to_cbsa$county, width=5, pad="0")

metarea_to_cbsa <- read.xlsx("cps_metarea_lookup.xlsx")
metarea_to_cbsa$metarea <- str_pad(metarea_to_cbsa$metarea, width=5, pad="0")

check <- left_join(cps[, c("county", "metarea")], county_to_cbsa, by="county")
check <- left_join(check, metarea_to_cbsa, by="metarea")

################################################################################################################

PlotXY <- function(x, y, n, main) {

  col <- rep(rgb(0, 0, 0, alpha=0.5), length(x))
  col[is.na(x)] <- rgb(1, 0, 0, alpha=0.5)

  M <- lm(x ~ y, w=n)
  M_raw <- lm(y ~ x, w=n)
  xhat <- coef(M)[1] + coef(M)[2] * y
  xhat <- rnorm(length(xhat), mean=xhat, sd=sd(resid(M)))
  ok <- is.na(x)
  x[ok] <- xhat[ok]

  rng <- range(c(x, y))
  plot(x, y, col=col, pch=20, cex=sqrt(n) / 50, 
    xlim=rng, ylim=rng, xlab="ACS", ylab="ACS", axes=FALSE, main=main)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  box(lwd=par()$lwd)
  abline(a=0, b=1, col="grey")

  M <- lm(y ~ x, w=n)
  abline(M, col="red")
  abline(M_raw, col="blue")
  display(M)

}

dat <- left_join(cps, acs)
par(mfrow=c(3,1))

PlotXY(
  x=dat$acs_laborforce, 
  y=dat$laborforce, 
  n=dat$n_cnip_raw, 
  main="Labor Force Participation")

PlotXY(
  x=dat$acs_unemployed, 
  y=dat$unemployed, 
  n=dat$n_laborforce_raw, 
  main="Unemployment Rate")

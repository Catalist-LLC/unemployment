library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(openxlsx)
library(tidycensus)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd(ifelse(Sys.info()[['sysname']]=="Darwin", 
  "~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/mrsp/",
  "~/unemployment_cps_mrp"))

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
# get data

cps <- V("
  select 
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
  -- weighted population sizes
  sum(wtfinl) as n_cnip, 
  sum(case when empstat in ('1', '10', '12', '20', '21', '22') then wtfinl else 0 end) as n_laborforce, 
  sum(case when empstat in ('1', '10', '12') then wtfinl else 0 end) as n_employed, 
  -- design effect vars
  stddev(wtfinl) as sd_wt, 
  avg(wtfinl) as mu_wt
  from publicdata.cps_raw
  where age >= 16
  and year >= 2012
  and empstat > 0
  group by 1, 2, 3, 4, 5, 6, 7
  order by 1, 2, 3, 4, 5, 6, 7
")

cps$n_laborforce_cnip <- cps$n_cnip
cps$y_laborforce_cnip <- cps$n_laborforce / cps$n_cnip
cps$n_employed_laborforce <- cps$n_laborforce
cps$y_employed_laborforce <- cps$n_employed / cps$n_laborforce
cps$n_employed_cnip <- cps$n_cnip
cps$y_employed_cnip <- cps$n_employed / cps$n_cnip

################################################################################################################

ys <- grep("^y_", colnames(cps), value=TRUE)
xs <- grep("_", colnames(cps), invert=TRUE, value=TRUE)

inters <- as.data.frame(combinations(n=length(xs), r=2, v=xs), stringsAsFactors=FALSE)
colnames(inters) <- c("x1", "x2")
drop <- 
  (inters$x1 %in% c("region", "state") & inters$x2 %in% c("region", "state"))
inters <- inters[!drop,]
inters$full <- paste0(inters$x1, "__", inters$x2)
xs_full <- c(xs, inters$full)

for (i in 1:nrow(inters)) {
  message("make interactions, ", i, " of ", nrow(inters))
  cps[, inters$full[i]] <- paste0(cps[,inters$x1[i]], "__", cps[,inters$x2[i]])
}

registerDoMC(length(ys))
raw <- foreach(i_y = ys) %dopar% {
  y <- cps[, i_y]
  n <- cps[, gsub("^y_", "n_", i_y)]
  ok <- !is.na(y)
  y <- y[ok]
  n <- n[ok]
  y_bin <- round(cbind(y * n, (1 - y) * n))
  return(sapply(xs_full, function(i_x) {
    message(i_y, ", ", i_x)
    x <- cps[ok, i_x]
    M <- glmer(y_bin ~ 1 + (1 | x), family="binomial")
    cov.wt(cbind(fitted(M), y), wt=n, cor=TRUE)$cor[2]
  }))
}

out <- data.frame(ord=1:length(xs_full), x=xs_full, bind_cols(raw), stringsAsFactors=FALSE)
colnames(out) <- c("ord", "x", gsub("^y_", "", ys))
colnames(out) <- gsub("_", " / ", colnames(out))
write.table(out, file=pipe("pbcopy"), sep="\t", row.names=FALSE)

################################################################################################################

i_y <- "y_employed_laborforce"

  y <- cps[, i_y]
  n <- cps[, gsub("^y_", "n_", i_y)]
  ok <- !is.na(y)
  y <- y[ok]
  n <- n[ok]
  y_bin <- round(cbind(y * n, (1 - y) * n))

  for (i_x in xs) {
    message(i_x)
    x <- cps[ok, i_x]
    M <- glmer(y_bin ~ 1 + (1 | x), family="binomial")
    print(1 - invlogit(coef(M)$x))
  }

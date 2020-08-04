library(colorout)
library(dplyr)
library(htmltab)
library(foreach)
library(doMC)
library(sqldf)
library(RJDBC)
library(tidyr)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd(ifelse(Sys.info()[['sysname']]=="Darwin", 
  "~/Documents/0Projects/covid19/unemployment/unemployment_insurance_claims/",
  "~/"))

################################################################################################################

PlottingWindow <- function() {
  quartz(width=2.75, height=7.75)
  par(mfrow=c(3,1), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")
}

vertica_username <- system("echo $VERTICA_USERNAME", intern=TRUE)
vertica_password <- system("echo $VERTICA_PASSWORD", intern=TRUE)
vertica_host <- system("echo $VERTICA_HOST", intern=TRUE)

drv <- JDBC("com.vertica.jdbc.Driver", 
            ifelse(Sys.info()[['sysname']]=="Darwin", "~/RJDBC/vertica-jdk5-6.1.3-0.jar",
                                                      "~/RJDBC/vertica-jdk5-6.1.3-0.jar"))
conn <- dbConnect(drv, paste0("jdbc:vertica://", vertica_host, ":5433/vertica4"), vertica_username, vertica_password)
V <- function(x) dbGetQuery(conn, x)

################################################################################################################
# get data

system("sh 04_upload_to_server.sh")

dat <- readRDS("state_claims_weekly.rds")
dat <- dat[dat$date >= "2020-02-15",]
twoway2016 <- readRDS("twoway2016.rds")

laus <- openxlsx::read.xlsx("laus_data/ststdnsadata_202002.xlsx", sheet="input")
state.abb <- unique(c("DC", state.abb))
state.name <- unique(c("District of Columbia", state.name))
names(state.abb) <- state.name
laus$state <- state.abb[laus$state]
laus <- laus[!is.na(laus$state),]
for (i in 3:ncol(laus))
  laus[,i] <- as.numeric(laus[,i])
laus <- laus[, c("state", "year", "month", "civilian_noninstitutional_population", "civilian_labor_force", "unemployed")]
laus <- rbind(laus, sqldf("
  select 
  'USA' as state, year, month, 
  sum(civilian_noninstitutional_population) as civilian_noninstitutional_population, 
  sum(civilian_labor_force) as civilian_labor_force, 
  sum(unemployed) as unemployed
  from laus
  group by 1, 2, 3
"))

laus_feb <- laus[laus$year == 2020 & laus$month == 2,]
laus_feb <- laus_feb[order(-1 * laus_feb$year, -1 * laus_feb$month, laus_feb$state),]
laus_feb <- laus_feb[!duplicated(laus_feb$state),]
laus_feb <- laus_feb[, c("state", "civilian_noninstitutional_population", "civilian_labor_force", "unemployed")]

################################################################################################################

jpeg("unemployed_claims_over_feb_unemployment.jpeg", width=8, height=9, units="in", res=300)
par(mfrow=c(1,1), mar=c(3,3,3,1), mgp=c(1.5,0.1,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")

pdat <- full_join(twoway2016, laus_feb)
pdat <- full_join(pdat, 
  dat %>% 
  group_by(state) %>% 
  summarize(claims=sum(claims, na.rm=TRUE)))
pdat$claims[pdat$state == "USA"] <- sum(pdat$claims[pdat$state != "USA"])

pdat$y <- pdat$claims / pdat$unemployed
pdat$col <- "black"
pdat$col[pdat$state == "USA"] <- "red"
plot(0, 0, xlim=c(0,1), ylim=c(0, 1.05 * max(pdat$y)), type="n", axes=FALSE, yaxs="i", 
  main=paste0("Increase Over Mid-Feb 2020 Unemployment, Past ", length(unique(dat$date)), " Weeks\n(Not Seasonally Adjusted, Biased by State-Specific Reporting Delays)"), 
  xlab="2016 Clinton Percent (Two-Way Vote Share)", 
  ylab=paste0("Cumulative Unemployment Claims / Feb 2020 Number Unemployed"))
abline(v=0.5, col="grey")
abline(h=1:20, col="grey", lty=3)
text(pdat$twoway, pdat$y, pdat$state, col=pdat$col, cex=0.75)
axis(1, at=seq(0, 1, by=0.25), lab=paste0(100 * seq(0, 1, by=0.25), "%"), lwd=par()$lwd)
axis(2, at=seq(0, 20, by=1), lab=paste0("+", seq(0, 20, by=1), "x"), lwd=par()$lwd)
box(lwd=par()$lwd)

dev.off()

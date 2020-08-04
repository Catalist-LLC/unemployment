require(colorout)
require(dplyr)
require(htmltab)
require(foreach)
require(doMC)

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

################################################################################################################

dates <- seq(
  as.Date("11/23/2002", format="%m/%d/%Y"), 
  as.Date("12/31/2020", format="%m/%d/%Y"), 
  by=7)
dates <- dates[dates <= Sys.Date()]

registerDoMC(4)
raw <- foreach (i_date = 1:length(dates), .errorhandling="pass") %dopar% {
  message(i_date, " of ", length(dates))
  url <- paste0("https://oui.doleta.gov/unemploy/page8/", format(dates[i_date], "%Y"), "/", format(dates[i_date], "%m%d%y"), ".html")
  # bad urls
  if (dates[i_date] == "2002-12-21")
    url <- "https://oui.doleta.gov/unemploy/page8/2002/122103.html"
  if (dates[i_date] == "2002-12-28")
    url <- "https://oui.doleta.gov/unemploy/page8/2002/122803.html"

  tmp <- htmltab(doc=url)
  tmp <- tmp[,1:6]
  colnames(tmp) <- gsub(".+ >> ", "", colnames(tmp))
  tmp <- data.frame(date=dates[i_date], tmp, stringsAsFactors=FALSE)
}

################################################################################################################

cleaned <- list()
ix <- 1
for (i_date in 1:length(dates))
  if (class(raw[[i_date]])[1] == "data.frame") {
    tmp <- raw[[i_date]]
    colnames(tmp) <- c("date", "state", "claims", "compared_to_last_week", "compared_to_year_ago", "ucfe", "ucx")
    cleaned[[ix]] <- tmp
    ix <- ix + 1
  }
dat <- bind_rows(cleaned)
dat$state <- gsub(" +", " ", dat$state)
dat$state <- gsub("\\*", "", dat$state)
dat$state <- gsub(" +$", "", dat$state)

state.abb <- unique(c("DC", state.abb))
state.name <- unique(c("District of Columbia", state.name))
names(state.abb) <- state.name
dat$state <- state.abb[dat$state]
dat <- dat[!is.na(dat$state),]

################################################################################################################
# add recent data

recent <- openxlsx::read.xlsx("01_state_claims_weekly_recent.xlsx")
recent$date <- as.Date(recent$date, format="%m-%d-%Y")
recent$state <- gsub("\\*", "", recent$state)
recent$state <- state.abb[recent$state]

drop <- recent$date %in% dat$date | is.na(recent$state)
recent <- recent[!drop,]
recent$compared_to_last_week <- NA
recent$compared_to_year_ago <- NA
recent$ucfe <- NA
recent$ucx <- NA

dat <- rbind(dat, recent)
for (i in 3:ncol(dat))
  dat[,i] <- as.numeric(as.character(gsub(",", "", dat[,i])))

if (any(is.na(dat$claims)))
  stop("missing data")

saveRDS(dat, file="state_claims_weekly.rds")

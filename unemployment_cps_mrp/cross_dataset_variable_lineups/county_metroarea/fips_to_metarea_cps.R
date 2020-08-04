library(colorout)
library(stringdist)
library(openxlsx)
library(RJDBC)
library(dplyr)
library(stringr)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/run_mrp/adhoc/county_metroarea/")

################################################################################################################

vertica_username <- system("echo $VERTICA_USERNAME", intern=TRUE)
vertica_password <- system("echo $VERTICA_PASSWORD", intern=TRUE)
vertica_host <- system("echo $VERTICA_HOST", intern=TRUE)

drv <- JDBC("com.vertica.jdbc.Driver", "~/RJDBC/vertica-jdk5-6.1.3-0.jar")
conn <- dbConnect(drv, paste0("jdbc:vertica://", vertica_host, ":5433/vertica4"), vertica_username, vertica_password)       ### analytics cluster
V <- function(x) dbGetQuery(conn, x)

################################################################################################################
# ipums to fips

catalist <- V("
  select 
  state, countyname, statecountyfips as fips
  from catalist_mdr.district
  group by 1, 2, 3
  order by 1, 2, 3
")

ipums <- read.xlsx("ipums_metarea_2000_2011.xlsx")
ipums <- ipums[ipums$state != "PR",]
ipums$countyname <- toupper(ipums$countyname)
ipums$state <- toupper(ipums$state)
ipums$countyname <- gsub("\\(P\\)", "", ipums$countyname)
ipums$countyname <- gsub("^ST\\.", "SAINT", ipums$countyname)
ipums$countyname <- gsub("^ST ", "SAINT", ipums$countyname)
ipums$countyname <- gsub("[^A-Z ]", "", ipums$countyname)
ipums$countyname <- gsub(" CITY$", "", ipums$countyname)
ipums$countyname <- gsub(" CITY $", "", ipums$countyname)
ipums <- ipums[order(ipums$state, ipums$countyname),]
ipums <- ipums[grep("--$", ipums[, "2000-2011"], invert=TRUE),]
ipums <- ipums[grep("\\(p\\)$", ipums[, "2000-2011"], invert=TRUE),]
ipums <- ipums[ipums[,"2000-2011"] != "Bedford city*",]

# specifics
ipums$countyname[ipums$countyname == "CRITTENDON" & ipums$state == "AR"] <- "CRITTENDEN"
ipums$countyname[ipums$countyname == "LANOKE" & ipums$state == "AR"] <- "LONOKE"
ipums$countyname[ipums$countyname == "GASDEN" & ipums$state == "FL"] <- "GADSDEN"
ipums$countyname[ipums$countyname == "MIAMIDADE" & ipums$state == "FL"] <- "MIAMI-DADE"
ipums$countyname[ipums$countyname == "BLACKHAWK" & ipums$state == "IA"] <- "BLACK HAWK"
ipums$countyname[ipums$countyname == "DEKALB" & ipums$state == "IL"] <- "DE KALB"
ipums$countyname[ipums$countyname == "DUPAGE" & ipums$state == "IL"] <- "DU PAGE"
ipums$countyname[ipums$countyname == "DEKALB" & ipums$state == "IN"] <- "DE KALB"
ipums$countyname[ipums$countyname == "JESSANINE" & ipums$state == "KY"] <- "JESSAMINE"
ipums$countyname[ipums$countyname == "E BATON ROUGE" & ipums$state == "LA"] <- "EAST BATON ROUGE"
ipums$countyname[ipums$countyname == "SAINT JOHN THE BAPTIST" & ipums$state == "LA"] <- "ST JOHN THE BAPTIST"
ipums$countyname[ipums$countyname == "W BATON ROUGE" & ipums$state == "LA"] <- "WEST BATON ROUGE"
ipums$countyname[ipums$countyname == "HAMDEN" & ipums$state == "MA"] <- "HAMPDEN"
ipums$state[ipums$countyname == "DISTRICT OF COLUMBIA" & ipums$state == "MD/VA/WV"] <- "DC"
ipums$countyname[ipums$countyname == "GENESSEE" & ipums$state == "MI"] <- "GENESEE"
ipums$countyname[ipums$countyname == "CHEATHEM" & ipums$state == "TN"] <- "CHEATHAM"
ipums$countyname[ipums$countyname == "LOUDOUN" & ipums$state == "TN"] <- "LOUDON"
ipums$countyname[ipums$countyname == "CHARLES" & ipums$state == "VA"] <- "CHARLES CITY"
ipums$countyname[ipums$countyname == "JAMES" & ipums$state == "VA"] <- "JAMES CITY"
ipums$countyname[ipums[,"2000-2011"] == "Baltimore City*" & ipums$state == "MD"] <- "BALTIMORE CITY"
ipums$countyname[ipums[,"2000-2011"] == "St. Louis City*" & ipums$state == "MO/IL"] <- "SAINT LOUIS CITY"
ipums$countyname[ipums[,"2000-2011"] == "Bedford city*" & ipums$state == "VA"] <- "BEDFORD CITY"
ipums$countyname[ipums[,"2000-2011"] == "Fairfax city, VA" & ipums$state == "VA"] <- "FAIRFAX CITY"
ipums$countyname[ipums[,"2000-2011"] == "Roanoke City *" & ipums$state == "VA"] <- "ROANOKE CITY"

out <- left_join(ipums, catalist)
ix <- grep("/", out$state)
out$fips[ix] <- sapply(ix, function(i_row) {
  countyname <- out$countyname[i_row]
  states <- strsplit(out$state[i_row], "/")[[1]]
  for (state in states) {
    ok <- which(catalist$state == state & catalist$countyname == countyname)
    if (length(ok) == 1)
      return(catalist$fips[ok])
  }
  return(NA)
})

out[is.na(out$fips), c("metarea", "2000-2011", "countyname", "state")]
out <- data.frame(
  years="2010-2011", 
  out[, c("fips", "countyname", "code", "metarea")], 
  stringsAsFactors=FALSE)

colnames(out) <- c("years", "fips", "countyname", "ipums_code", "ipums_metarea")
out$ipums_metarea <- toupper(out$ipums_metarea)
out$ipums_metarea <- gsub("#", "", out$ipums_metarea)
out$ipums_metarea <- gsub(" \\(CONT\\.\\)$", "", out$ipums_metarea)

if (any(is.na(out$fips) | duplicated(out$fips)))
  stop("fips problem")

################################################################################################################
# cps code

ClosestMatch2 <- function(string, stringVector)
  stringVector[amatch(string, stringVector, maxDist=Inf)]

cps <- read.xlsx("cps_metarea_lookup.xlsx")
cps$cbsa <- toupper(cps$cbsa)
out$cbsa <- sapply(out$ipums_metarea, function(i) ClosestMatch2(i, cps$cbsa))

# out[out$ipums_metarea != out$cbsa, c("ipums_metarea", "cbsa")]

out$cbsa[out$ipums_metarea == "DOTHAN, AL"] <- ""
out$cbsa[out$ipums_metarea == "JONESBORO, AR"] <- ""
out$cbsa[out$ipums_metarea == "ALEXANDRIA, LA"] <- ""
out$cbsa[out$ipums_metarea == "ALTOONA, PA"] <- "ALTOONA, PA MSA"
out$cbsa[out$ipums_metarea == "BISMARK, ND"] <- ""
out$cbsa[out$ipums_metarea == "BREMERTON, WA"] <- "BREMERTON-SILVERDALE, WA"
out$cbsa[out$ipums_metarea == "BUFFALO, NY"] <- "BUFFALO-NIAGARA FALLS, NY"
out$cbsa[out$ipums_metarea == "CASPER, WY"] <- ""
out$cbsa[out$ipums_metarea == "CHARLESTON, SC"] <- "CHARLESTON-NORTH CHARLESTON, SC"
out$cbsa[out$ipums_metarea == "CHEYENNE, WY"] <- ""
out$cbsa[out$ipums_metarea == "CHICAGO, IL"] <- "CHICAGO-GARY-LAKE IL"
out$cbsa[out$ipums_metarea == "CORVALLIS, OR"] <- ""
out$cbsa[out$ipums_metarea == "CUMBERLAND, MD/WV"] <- ""
out$cbsa[out$ipums_metarea == "DALLAS, TX"] <- "DALLAS-FORT WORTH, TX"
out$cbsa[out$ipums_metarea == "DANVILLE, VA"] <- ""
out$cbsa[out$ipums_metarea == "DENVER, CO"] <- "DENVER-AURORA, CO"
out$cbsa[out$ipums_metarea == "DUBUQUE, IA"] <- ""
out$cbsa[out$ipums_metarea == "ELMIRA, NY"] <- ""
out$cbsa[out$ipums_metarea == "ENID, OK"] <- ""
out$cbsa[out$ipums_metarea == "FLAGSTAFF, AZ-UT"] <- ""
out$cbsa[out$ipums_metarea == "GARY-HAMMOND, IN"] <- "GARY-HAMOND-EAST CHICAGO, IN"
out$cbsa[out$ipums_metarea == "GRAND FORKS, ND"] <- ""
out$cbsa[out$ipums_metarea == "GRAND JUNCTION, CO"] <- ""
out$cbsa[out$ipums_metarea == "GREAT FALLS, MT"] <- ""
out$cbsa[out$ipums_metarea == "HATTIESBURG, MS"] <- ""
out$cbsa[out$ipums_metarea == "HOUMA, LA"] <- "HOUMA-THIBODAUX, LA"
out$cbsa[out$ipums_metarea == "HOUSTON, TX"] <- "HOUSTON-BRAZORIA,TX"
out$cbsa[out$ipums_metarea == "JACKSON, TN"] <- ""
out$cbsa[out$ipums_metarea == "KALAMAZOO, MI"] <- "KALAMAZOO-PORTAGE, MI"
out$cbsa[out$ipums_metarea == "KENOSHA, WI"] <- ""
out$cbsa[out$ipums_metarea == "KOKOMO, IN"] <- ""
out$cbsa[out$ipums_metarea == "LAFAYETTE, IN"] <- "LAFAYETTE-WEST LAFAYETTE, IN"
out$cbsa[out$ipums_metarea == "LEWISTON-AUBURGN, ME"] <- "LEWISTON-AUBURN, ME"
out$cbsa[out$ipums_metarea == "MEDFORD-ASHLAND, OR"] <- "MEDFORD, OR"
out$cbsa[out$ipums_metarea == "MIAMI, FL"] <- "MIAMI-HIALEAH, FL"
out$cbsa[out$ipums_metarea == "MISSOULA, MT"] <- ""
out$cbsa[out$ipums_metarea == "MUNCIE, IN"] <- ""
out$cbsa[out$ipums_metarea == "NEWBURGH, NY"] <- "NEWBURGH-MIDDLETOWN, NY"
out$cbsa[out$ipums_metarea == "NEWPORT NEWS-HAMPTON"] <- "NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA"
out$cbsa[out$ipums_metarea == "OWENSBORO, KY"] <- ""
out$cbsa[out$ipums_metarea == "PARKERSBURG-MARIETTA WV/OH"] <- ""
out$cbsa[out$ipums_metarea == "PITTSFIELD, MA"] <- ""
out$cbsa[out$ipums_metarea == "POCATELLO, ID"] <- ""
out$cbsa[out$ipums_metarea == "PROVIDENCE, RI"] <- "PROVIDENCE-FALL RIVER-PAWTUCKET, MA/RI"
out$cbsa[out$ipums_metarea == "RAPID CITY, SD"] <- ""
out$cbsa[out$ipums_metarea == "RICHLAND-KENNEWICK-PASCO, WA"] <- "KENNEWICK-RICHLAND, WA"
out$cbsa[out$ipums_metarea == "ROCHESTER, MN"] <- ""
out$cbsa[out$ipums_metarea == "ROCKY MOUNT, NC"] <- ""
out$cbsa[out$ipums_metarea == "SAN ANGELO, TX"] <- ""
out$cbsa[out$ipums_metarea == "SAN FRANCISCO, CA"] <- "SAN FRANCISCO-OAKLAN-VALLEJO, CA"
out$cbsa[out$ipums_metarea == "SHEBOYGAN, WI"] <- ""
out$cbsa[out$ipums_metarea == "SOUTH BEND, IN"] <- "SOUTH BEND-MISHAWAKA, IN"
out$cbsa[out$ipums_metarea == "SPRINGFIELD-HOLYOKE, MA"] <- "SPRINGFIELD-HOLYOKE-CHICOPEE, MA"
out$cbsa[out$ipums_metarea == "ST. JOSEPH, MO"] <- ""
out$cbsa[out$ipums_metarea == "STATE COLLEGE, PA"] <- ""
out$cbsa[out$ipums_metarea == "STEUBENVILLE-WEIRTON, OH/WV"] <- ""
out$cbsa[out$ipums_metarea == "SUMTER, SC"] <- ""
out$cbsa[out$ipums_metarea == "TEXARKANA, TX/AR"] <- ""
out$cbsa[out$ipums_metarea == "VENTURA-OXNARD, CA"] <- "VENTURA-OXNARD-SIMI VALLEY, CA"

out <- left_join(out, cps)
out <- out[!is.na(out$metarea),]
out <- out[order(out$metarea, out$fips),]

################################################################################################################
# fips that are in CPS

survey <- V("
  select 
  county as fips, metarea, 
  count(*) as n
  from publicdata.cps_raw
  where year >= 2000
  and age >= 16
  and county is not null and county != 0
  group by 1, 2
  order by 1, 2
")
survey$fips <- str_pad(survey$fips, width=5, pad="0")

tmp <- out[, c("fips", "metarea")]
colnames(tmp) <- c("fips", "lookup_metarea")
survey <- left_join(survey, tmp)
survey$check <- is.na(survey$lookup_metarea) | (10 * floor(survey$metarea / 10) == 10 * floor(survey$lookup_metarea / 10))

bad <- survey[!survey$check, colnames(survey) != "check"]
bad <- left_join(bad, out[, c("fips", "countyname")])
bad <- left_join(bad, cps)
colnames(bad)[ncol(bad)] <- "cps"
tmp <- out[, c("ipums_code", "metarea", "ipums_metarea", "cbsa")]
tmp <- tmp[!duplicated(tmp),]
colnames(tmp) <- c("lookup_metarea", "cps_metarea", "ipums_metarea", "cbsa")
bad <- left_join(bad, tmp)
bad <- bad[bad$metarea < 9997,]
bad <- bad[order(bad$n, decreasing=TRUE),]

################################################################################################################

out$metarea_truncated <- 10 * floor(out$metarea / 10)
write.table(out, file=pipe("pbcopy"), sep="\t", row.names=FALSE)

################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
# check coverage

cps <- V("
  select 
  year, month, 
  county, 
  case when metarea = 4130 then 4120 else metarea end as metarea, 
  count(*) as n
  from publicdata.cps_raw
  where year >= 2000
  and age >= 16
  group by 1, 2, 3, 4
  order by 1, 2, 3, 4
")
cps$county[cps$county == 0] <- NA
cps$metarea[cps$metarea >= 9997] <- NA
for (i in c("county")) {
  ok <- !is.na(cps[,i])
  cps[,i] <- str_pad(cps[,i], width=5, pad="0")
}
cps$metarea_truncated <- floor(cps$metarea / 10) * 10
cps$survey <- paste0(cps$year, "_", str_pad(cps$month, width=2, pad="0"))

agg <- data.frame(t(sapply(sort(unique(cps$survey)), function(survey) {
  tmp <- cps[cps$survey == survey,]
  metareas <- out$metarea_truncated[!(out$fips %in% tmp$county)]
  return(c(
    "county"=sum(tmp$n[!is.na(tmp$county)]) / sum(tmp$n), 
    "metarea"=sum(tmp$n[is.na(tmp$county) & tmp$metarea_truncated %in% metareas]) / sum(tmp$n)
    ))
  })))

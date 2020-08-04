library(colorout)
library(sqldf)
library(arm)
library(data.table)
library(dplyr)
library(foreach)
library(doMC)
library(openxlsx)
library(tidycensus)
library(RJDBC)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/Documents/0Projects/covid19/unemployment/unemployment_cps_mrp/cross_dataset_variable_lineups/occupation_and_industry/")

################################################################################################################
# standard errors analysis

modeled <- readRDS("../mrsp/create_joint/tract_2018_occscore_fipsrollup.rds")
modeled <- modeled[order(modeled$fips),]

colnames(marg)[colnames(marg) == "GEOID"] <- "fips"
marg <- marg[marg$fips %in% modeled$fips,]
marg <- marg[order(marg$fips),]

ns <- paste0("n_", gsub("_.+", "", ys))
state_correction <- inner_join(
  sqldf(paste0("
    select 
    state, 
    ", paste0("sum(", ys, " * ", ns, ") / sum(", ns, ") as ", ys, "_16", collapse=", "), "
    from occ
    group by 1
  ")), 
  sqldf(paste0("
    select 
    state, 
    ", paste0("sum(", ys, " * ", ns, ") / sum(", ns, ") as ", ys, "_18", collapse=", "), "
    from occ
    where agegrp > 1
    group by 1
  ")))
for (y in ys)
  state_correction[,y] <- state_correction[, paste0(y, "_18")] - state_correction[, paste0(y, "_16")]
state_correction <- state_correction[, c("state", ys)]
rownames(state_correction) <- state_correction$state

################################################################################################################

moe_orig <- data.frame(
  geog %>% 
    group_by(GEOID) %>% 
    summarize(
      # occ, Civilian Employed Population 16 Years And Over
      occ_total=sqrt(sum(moe[variable %in% c('C24010_001')]^2, na.rm=TRUE)), 
      occ_managementbusinessandfinancialoccupations=sqrt(sum(moe[variable %in% c('C24010_005', 'C24010_006', 'C24010_041', 'C24010_042')]^2, na.rm=TRUE)), 
      occ_computerengineeringandscienceoccupations=sqrt(sum(moe[variable %in% c('C24010_008', 'C24010_009', 'C24010_010', 'C24010_044', 'C24010_045', 'C24010_046')]^2, na.rm=TRUE)), 
      occ_educationlegalcommunityserviceartsandmediaoccupations=sqrt(sum(moe[variable %in% c('C24010_012', 'C24010_013', 'C24010_014', 'C24010_015', 'C24010_048', 'C24010_049', 'C24010_050', 'C24010_051')]^2, na.rm=TRUE)), 
      occ_healthcarepractitionersandtechnicaloccupations=sqrt(sum(moe[variable %in% c('C24010_017', 'C24010_018', 'C24010_053', 'C24010_054')]^2, na.rm=TRUE)), 
      occ_healthcaresupportoccupations=sqrt(sum(moe[variable %in% c('C24010_020', 'C24010_056')]^2, na.rm=TRUE)), 
      occ_protectiveserviceoccupations=sqrt(sum(moe[variable %in% c('C24010_022', 'C24010_023', 'C24010_058', 'C24010_059')]^2, na.rm=TRUE)), 
      occ_foodpreparationandservingrelatedoccupations=sqrt(sum(moe[variable %in% c('C24010_024', 'C24010_060')]^2, na.rm=TRUE)), 
      occ_buildingandgroundscleaningandmaintenanceoccupations=sqrt(sum(moe[variable %in% c('C24010_025', 'C24010_061')]^2, na.rm=TRUE)), 
      occ_personalcareandserviceoccupations=sqrt(sum(moe[variable %in% c('C24010_026', 'C24010_062')]^2, na.rm=TRUE)), 
      occ_salesandofficeoccupations=sqrt(sum(moe[variable %in% c('C24010_028', 'C24010_029', 'C24010_064', 'C24010_065')]^2, na.rm=TRUE)), 
      occ_naturalresourcesconstructionandmaintenanceoccupations=sqrt(sum(moe[variable %in% c('C24010_031', 'C24010_032', 'C24010_033', 'C24010_067', 'C24010_068', 'C24010_069')]^2, na.rm=TRUE)), 
      occ_productiontransportationandmaterialmovingoccupations=sqrt(sum(moe[variable %in% c('C24010_035', 'C24010_036', 'C24010_037', 'C24010_071', 'C24010_072', 'C24010_073')]^2, na.rm=TRUE)), 
      # ind, Civilian Employed Population 16 Years And Over
      ind_total=sqrt(sum(moe[variable %in% c('C24030_001')]^2, na.rm=TRUE)), 
      ind_agricultureforestryfishingandhunting=sqrt(sum(moe[variable %in% c('C24030_004', 'C24030_031')]^2, na.rm=TRUE)), 
      ind_miningquarryingandoilandgasextraction=sqrt(sum(moe[variable %in% c('C24030_005', 'C24030_032')]^2, na.rm=TRUE)), 
      ind_construction=sqrt(sum(moe[variable %in% c('C24030_006', 'C24030_033')]^2, na.rm=TRUE)), 
      ind_manufacturing=sqrt(sum(moe[variable %in% c('C24030_007', 'C24030_034')]^2, na.rm=TRUE)), 
      ind_wholesaletrade=sqrt(sum(moe[variable %in% c('C24030_008', 'C24030_035')]^2, na.rm=TRUE)), 
      ind_retailtrade=sqrt(sum(moe[variable %in% c('C24030_009', 'C24030_036')]^2, na.rm=TRUE)), 
      ind_transportationandwarehousing=sqrt(sum(moe[variable %in% c('C24030_011', 'C24030_038')]^2, na.rm=TRUE)), 
      ind_utilities=sqrt(sum(moe[variable %in% c('C24030_012', 'C24030_039')]^2, na.rm=TRUE)), 
      ind_information=sqrt(sum(moe[variable %in% c('C24030_013', 'C24030_040')]^2, na.rm=TRUE)), 
      ind_financeandinsurance=sqrt(sum(moe[variable %in% c('C24030_015', 'C24030_042')]^2, na.rm=TRUE)), 
      ind_realestateandrentalandleasing=sqrt(sum(moe[variable %in% c('C24030_016', 'C24030_043')]^2, na.rm=TRUE)), 
      ind_professionalscientificandtechnicalservices=sqrt(sum(moe[variable %in% c('C24030_018', 'C24030_045')]^2, na.rm=TRUE)), 
      ind_managementofcompaniesandenterprises=sqrt(sum(moe[variable %in% c('C24030_019', 'C24030_046')]^2, na.rm=TRUE)), 
      ind_administrativeandsupportandwastemanagementservices=sqrt(sum(moe[variable %in% c('C24030_020', 'C24030_047')]^2, na.rm=TRUE)), 
      ind_educationalservices=sqrt(sum(moe[variable %in% c('C24030_022', 'C24030_049')]^2, na.rm=TRUE)), 
      ind_healthcareandsocialassistance=sqrt(sum(moe[variable %in% c('C24030_023', 'C24030_050')]^2, na.rm=TRUE)), 
      ind_artsentertainmentandrecreation=sqrt(sum(moe[variable %in% c('C24030_025', 'C24030_052')]^2, na.rm=TRUE)), 
      ind_accommodationandfoodservices=sqrt(sum(moe[variable %in% c('C24030_026', 'C24030_053')]^2, na.rm=TRUE)), 
      ind_otherservicesexceptpublicadministration=sqrt(sum(moe[variable %in% c('C24030_027', 'C24030_054')]^2, na.rm=TRUE)), 
      ind_publicadministration=sqrt(sum(moe[variable %in% c('C24030_028', 'C24030_055')]^2, na.rm=TRUE)), 
      # emp, Population 16 Years And Over
      emp_total=sqrt(sum(moe[variable %in% c('B23025_001')]^2, na.rm=TRUE)), 
      emp_employed=sqrt(sum(moe[variable %in% c('B23025_004')]^2, na.rm=TRUE)), 
      emp_unemployed=sqrt(sum(moe[variable %in% c('B23025_005')]^2, na.rm=TRUE)), 
      ind_activedutymilitary=sqrt(sum(moe[variable %in% c('B23025_006')]^2, na.rm=TRUE)), 
      emp_notinlaborforce=sqrt(sum(moe[variable %in% c('B23025_007')]^2, na.rm=TRUE))
    ))
colnames(moe_orig)[colnames(moe_orig) == "GEOID"] <- "fips"
moe_orig <- moe_orig[moe_orig$fips %in% modeled$fips,]
moe_orig <- moe_orig[order(moe_orig$fips),]

################################################################################################################

acs <- marg[,ys]
moe <- moe_orig[,ys]

for (y in ys) {
  acs[,y] <- pmin(1, acs[,y] / marg[,gsub("_.+", "_total", y)])
  moe[,y] <- pmin(1, moe[,y] / marg[,gsub("_.+", "_total", y)])
}

size <- data.frame(geog %>% 
  group_by(GEOID) %>% 
  summarize(unwtd=sum(estimate[variable %in% c("B00001_001")])))
colnames(size)[colnames(size) == "GEOID"] <- "fips"
size <- size[size$fips %in% modeled$fips,]
size <- size[order(size$fips),]
size <- size$unwtd
size[is.na(size)] <- 0

################################################################################################################

jpeg("occ_modeled_vs_acs_coloredmoe.jpeg", width=20, height=15, units="in", res=300)
par(mfrow=c(5,8), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")

cols <- paste0(colorRampPalette(c("dark blue", "grey90", "dark red"))(101), "80")
for (y in ys) {

  message(y)

  tmp <- data.frame(
    modeled=100 * modeled[,y], 
    acs=100 * acs[,y], 
    moe=100 * moe[,y], 
    n=marg[,gsub("_.+", "_total", y)])
  for (i in 1:ncol(tmp))
    tmp[tmp[,i] %in% c(-Inf, Inf), i] <- NA
  tmp <- tmp[complete.cases(tmp),]
  tmp$err <- tmp$modeled - tmp$acs
  tmp$sqerr <- tmp$err^2
  tmp$col <- cols[round(10 * pmin(10, tmp$moe)) + 1]
  # tmp$col <- cols[round(0.02 * pmin(5000, tmp$n)) + 1]

  # M <- lm(acs ~ modeled, data=tmp, w=n)
  # if (coef(M)["modeled"] > 1) {
  #   tmp$corrected <- pmin(100, pmax(0, fitted(M)))
  # } else {
  #   tmp$corrected <- tmp$modeled
  # }

  plot(tmp$modeled, tmp$acs, xlab="Modeled", ylab="ACS", pch=20, 
    cex=sqrt(tmp$n) / 200, 
    col=tmp$col, 
    axes=FALSE, 
    main=substr(y, 1, 25), xlim=c(0, 100), ylim=c(0, 100))
  box(lwd=par()$lwd)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  abline(a=0, b=1)
}

dev.off()

################################################################################################################

jpeg("err_vs_size.jpeg", width=20, height=15, units="in", res=300)
par(mfrow=c(5,8), mar=c(2.25,2,2,1), mgp=c(1,0.01,0), tck=0.005, lwd=0.5, pty="s", family="CMU Sans Serif")

cols <- paste0(colorRampPalette(c("dark blue", "grey90", "dark red"))(101), "80")
xrng <- quantile(size, c(0.025, 0.975))
for (y in ys) {

  message(y)

  tmp <- data.frame(
    modeled=100 * modeled[,y], 
    acs=100 * acs[,y], 
    moe=100 * moe[,y], 
    size=size, 
    n=marg[,gsub("_.+", "_total", y)])
  for (i in 1:ncol(tmp))
    tmp[tmp[,i] %in% c(-Inf, Inf), i] <- NA
  tmp <- tmp[complete.cases(tmp),]
  tmp$err <- tmp$modeled - tmp$acs
  tmp$sqerr <- tmp$err^2

  M <- lm(acs ~ modeled, data=tmp, w=n)
  if (coef(M)["modeled"] > 1) {
    tmp$corrected <- pmin(1, pmax(0, fitted(M)))
  } else {
    tmp$corrected <- tmp$modeled
  }
  rng <- 100
  tmp$diff <- 100 * abs(tmp$corrected - tmp$acs) / mean(tmp$acs)
  tmp$diff <- pmin(rng, pmax(-1 * rng, tmp$diff))

  tmp$x <- log10(pmax(xrng[1], pmin(xrng[2], tmp$size)))
  plot(tmp$x, tmp$diff, 
    xlab="Log10(Total Size)", ylab="Model Minus ACS, Scaled", pch=".", 
    col=rgb(0, 0, 0, alpha=0.1), 
    axes=FALSE, 
    main=substr(y, 1, 25), xlim=log10(xrng), ylim=c(0, rng))
  box(lwd=par()$lwd)
  axis(1, lwd=par()$lwd)
  axis(2, lwd=par()$lwd)
  abline(lm(diff ~ x, data=tmp), col="red", lwd=2)
}

dev.off()

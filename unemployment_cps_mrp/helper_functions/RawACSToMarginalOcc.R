RawACSToMarginalOcc <- function(geog) {

  ################################################################################################################
  # roll up relevant variables

  marg <- data.frame(
    geog %>% 
      group_by(GEOID) %>% 
      summarize(
        # emp, Population 16 Years And Over
        emp__total=sum(estimate[variable %in% c('B12006_001')], na.rm=TRUE), 
        emp__employed_male_single=sum(estimate[variable %in% c('B12006_005', 'B12006_027', 'B12006_038', 'B12006_049')], na.rm=TRUE), 
        emp__unemployed_male_single=sum(estimate[variable %in% c('B12006_006', 'B12006_028', 'B12006_039', 'B12006_050')], na.rm=TRUE), 
        emp__notinlaborforce_male_single=sum(estimate[variable %in% c('B12006_007', 'B12006_029', 'B12006_040', 'B12006_051')], na.rm=TRUE), 
        emp__employed_female_single=sum(estimate[variable %in% c('B12006_010', 'B12006_032', 'B12006_043', 'B12006_054')], na.rm=TRUE), 
        emp__unemployed_female_single=sum(estimate[variable %in% c('B12006_011', 'B12006_033', 'B12006_044', 'B12006_055')], na.rm=TRUE), 
        emp__notinlaborforce_female_single=sum(estimate[variable %in% c('B12006_012', 'B12006_034', 'B12006_045', 'B12006_056')], na.rm=TRUE), 
        emp__employed_male_married=sum(estimate[variable %in% c('B12006_016')], na.rm=TRUE), 
        emp__unemployed_male_married=sum(estimate[variable %in% c('B12006_017')], na.rm=TRUE), 
        emp__notinlaborforce_male_married=sum(estimate[variable %in% c('B12006_018')], na.rm=TRUE), 
        emp__employed_female_married=sum(estimate[variable %in% c('B12006_021')], na.rm=TRUE), 
        emp__unemployed_female_married=sum(estimate[variable %in% c('B12006_022')], na.rm=TRUE), 
        emp__notinlaborforce_female_married=sum(estimate[variable %in% c('B12006_023')], na.rm=TRUE), 
        # occ, Civilian Employed Population 16 Years And Over
        occ__total=sum(estimate[variable %in% c('C24010_001')], na.rm=TRUE), 
        occ__managementbusinessandfinancialoccupations_male=sum(estimate[variable %in% c('C24010_005', 'C24010_006')], na.rm=TRUE), 
        occ__computerengineeringandscienceoccupations_male=sum(estimate[variable %in% c('C24010_008', 'C24010_009', 'C24010_010')], na.rm=TRUE), 
        occ__educationlegalcommunityserviceartsandmediaoccupations_male=sum(estimate[variable %in% c('C24010_012', 'C24010_013', 'C24010_014', 'C24010_015')], na.rm=TRUE), 
        occ__healthcarepractitionersandtechnicaloccupations_male=sum(estimate[variable %in% c('C24010_017', 'C24010_018')], na.rm=TRUE), 
        occ__healthcaresupportoccupations_male=sum(estimate[variable %in% c('C24010_020')], na.rm=TRUE), 
        occ__protectiveserviceoccupations_male=sum(estimate[variable %in% c('C24010_022', 'C24010_023')], na.rm=TRUE), 
        occ__foodpreparationandservingrelatedoccupations_male=sum(estimate[variable %in% c('C24010_024')], na.rm=TRUE), 
        occ__buildingandgroundscleaningandmaintenanceoccupations_male=sum(estimate[variable %in% c('C24010_025')], na.rm=TRUE), 
        occ__personalcareandserviceoccupations_male=sum(estimate[variable %in% c('C24010_026')], na.rm=TRUE), 
        occ__salesandofficeoccupations_male=sum(estimate[variable %in% c('C24010_028', 'C24010_029')], na.rm=TRUE), 
        occ__naturalresourcesconstructionandmaintenanceoccupations_male=sum(estimate[variable %in% c('C24010_031', 'C24010_032', 'C24010_033')], na.rm=TRUE), 
        occ__productiontransportationandmaterialmovingoccupations_male=sum(estimate[variable %in% c('C24010_035', 'C24010_036', 'C24010_037')], na.rm=TRUE), 
        occ__managementbusinessandfinancialoccupations_female=sum(estimate[variable %in% c('C24010_041', 'C24010_042')], na.rm=TRUE), 
        occ__computerengineeringandscienceoccupations_female=sum(estimate[variable %in% c('C24010_044', 'C24010_045', 'C24010_046')], na.rm=TRUE), 
        occ__educationlegalcommunityserviceartsandmediaoccupations_female=sum(estimate[variable %in% c('C24010_048', 'C24010_049', 'C24010_050', 'C24010_051')], na.rm=TRUE), 
        occ__healthcarepractitionersandtechnicaloccupations_female=sum(estimate[variable %in% c('C24010_053', 'C24010_054')], na.rm=TRUE), 
        occ__healthcaresupportoccupations_female=sum(estimate[variable %in% c('C24010_056')], na.rm=TRUE), 
        occ__protectiveserviceoccupations_female=sum(estimate[variable %in% c('C24010_058', 'C24010_059')], na.rm=TRUE), 
        occ__foodpreparationandservingrelatedoccupations_female=sum(estimate[variable %in% c('C24010_060')], na.rm=TRUE), 
        occ__buildingandgroundscleaningandmaintenanceoccupations_female=sum(estimate[variable %in% c('C24010_061')], na.rm=TRUE), 
        occ__personalcareandserviceoccupations_female=sum(estimate[variable %in% c('C24010_062')], na.rm=TRUE), 
        occ__salesandofficeoccupations_female=sum(estimate[variable %in% c('C24010_064', 'C24010_065')], na.rm=TRUE), 
        occ__naturalresourcesconstructionandmaintenanceoccupations_female=sum(estimate[variable %in% c('C24010_067', 'C24010_068', 'C24010_069')], na.rm=TRUE), 
        occ__productiontransportationandmaterialmovingoccupations_female=sum(estimate[variable %in% c('C24010_071', 'C24010_072', 'C24010_073')], na.rm=TRUE), 
        # ind, Civilian Employed Population 16 Years And Over
        ind__total=sum(estimate[variable %in% c('C24030_001')], na.rm=TRUE), 
        ind__agricultureforestryfishingandhunting_male=sum(estimate[variable %in% c('C24030_004')], na.rm=TRUE), 
        ind__miningquarryingandoilandgasextraction_male=sum(estimate[variable %in% c('C24030_005')], na.rm=TRUE), 
        ind__construction_male=sum(estimate[variable %in% c('C24030_006')], na.rm=TRUE), 
        ind__manufacturing_male=sum(estimate[variable %in% c('C24030_007')], na.rm=TRUE), 
        ind__wholesaletrade_male=sum(estimate[variable %in% c('C24030_008')], na.rm=TRUE), 
        ind__retailtrade_male=sum(estimate[variable %in% c('C24030_009')], na.rm=TRUE), 
        ind__transportationandwarehousing_male=sum(estimate[variable %in% c('C24030_011')], na.rm=TRUE), 
        ind__utilities_male=sum(estimate[variable %in% c('C24030_012')], na.rm=TRUE), 
        ind__information_male=sum(estimate[variable %in% c('C24030_013')], na.rm=TRUE), 
        ind__financeandinsurance_male=sum(estimate[variable %in% c('C24030_015')], na.rm=TRUE), 
        ind__realestateandrentalandleasing_male=sum(estimate[variable %in% c('C24030_016')], na.rm=TRUE), 
        ind__professionalscientificandtechnicalservices_male=sum(estimate[variable %in% c('C24030_018')], na.rm=TRUE), 
        ind__managementofcompaniesandenterprises_male=sum(estimate[variable %in% c('C24030_019')], na.rm=TRUE), 
        ind__administrativeandsupportandwastemanagementservices_male=sum(estimate[variable %in% c('C24030_020')], na.rm=TRUE), 
        ind__educationalservices_male=sum(estimate[variable %in% c('C24030_022')], na.rm=TRUE), 
        ind__healthcareandsocialassistance_male=sum(estimate[variable %in% c('C24030_023')], na.rm=TRUE), 
        ind__artsentertainmentandrecreation_male=sum(estimate[variable %in% c('C24030_025')], na.rm=TRUE), 
        ind__accommodationandfoodservices_male=sum(estimate[variable %in% c('C24030_026')], na.rm=TRUE), 
        ind__otherservicesexceptpublicadministration_male=sum(estimate[variable %in% c('C24030_027')], na.rm=TRUE), 
        ind__publicadministration_male=sum(estimate[variable %in% c('C24030_028')], na.rm=TRUE), 
        ind__agricultureforestryfishingandhunting_female=sum(estimate[variable %in% c('C24030_031')], na.rm=TRUE), 
        ind__miningquarryingandoilandgasextraction_female=sum(estimate[variable %in% c('C24030_032')], na.rm=TRUE), 
        ind__construction_female=sum(estimate[variable %in% c('C24030_033')], na.rm=TRUE), 
        ind__manufacturing_female=sum(estimate[variable %in% c('C24030_034')], na.rm=TRUE), 
        ind__wholesaletrade_female=sum(estimate[variable %in% c('C24030_035')], na.rm=TRUE), 
        ind__retailtrade_female=sum(estimate[variable %in% c('C24030_036')], na.rm=TRUE), 
        ind__transportationandwarehousing_female=sum(estimate[variable %in% c('C24030_038')], na.rm=TRUE), 
        ind__utilities_female=sum(estimate[variable %in% c('C24030_039')], na.rm=TRUE), 
        ind__information_female=sum(estimate[variable %in% c('C24030_040')], na.rm=TRUE), 
        ind__financeandinsurance_female=sum(estimate[variable %in% c('C24030_042')], na.rm=TRUE), 
        ind__realestateandrentalandleasing_female=sum(estimate[variable %in% c('C24030_043')], na.rm=TRUE), 
        ind__professionalscientificandtechnicalservices_female=sum(estimate[variable %in% c('C24030_045')], na.rm=TRUE), 
        ind__managementofcompaniesandenterprises_female=sum(estimate[variable %in% c('C24030_046')], na.rm=TRUE), 
        ind__administrativeandsupportandwastemanagementservices_female=sum(estimate[variable %in% c('C24030_047')], na.rm=TRUE), 
        ind__educationalservices_female=sum(estimate[variable %in% c('C24030_049')], na.rm=TRUE), 
        ind__healthcareandsocialassistance_female=sum(estimate[variable %in% c('C24030_050')], na.rm=TRUE), 
        ind__artsentertainmentandrecreation_female=sum(estimate[variable %in% c('C24030_052')], na.rm=TRUE), 
        ind__accommodationandfoodservices_female=sum(estimate[variable %in% c('C24030_053')], na.rm=TRUE), 
        ind__otherservicesexceptpublicadministration_female=sum(estimate[variable %in% c('C24030_054')], na.rm=TRUE), 
        ind__publicadministration_female=sum(estimate[variable %in% c('C24030_055')], na.rm=TRUE), 
        # household income by age of householder
        inc__total=sum(estimate[variable %in% c('B19049_001')], na.rm=TRUE), 
        inc__under25=sum(estimate[variable %in% c('B19049_002')], na.rm=TRUE), 
        inc__25to44=sum(estimate[variable %in% c('B19049_003')], na.rm=TRUE), 
        inc__45to64=sum(estimate[variable %in% c('B19049_004')], na.rm=TRUE), 
        inc__65plus=sum(estimate[variable %in% c('B19049_005')], na.rm=TRUE)
      ))

  ################################################################################################################
  # checks and corrections
  
  # totals
  totals <- grep("__total", colnames(marg), value=TRUE)
  check <- sapply(totals, function(i) {
    xs <- grep(paste0("^", gsub("__total", "", i), "_"), colnames(marg), value=TRUE)
    xs <- grep("__total", xs, invert=TRUE, value=TRUE)
    all(rowSums(marg[,xs]) - marg[i] == 0)
  })
  if (any(!check[names(check) != "inc__total"])) {
    print(cbind(check))
    stop("problem in totals")
  }
  
  state_from_fips <- c(
    "02"="AK", "01"="AL", "05"="AR", "04"="AZ", "06"="CA", "08"="CO", "09"="CT", "11"="DC", 
    "10"="DE", "12"="FL", "13"="GA", "15"="HI", "19"="IA", "16"="ID", "17"="IL", "18"="IN", 
    "20"="KS", "21"="KY", "22"="LA", "25"="MA", "24"="MD", "23"="ME", "26"="MI", "27"="MN", 
    "29"="MO", "28"="MS", "30"="MT", "37"="NC", "38"="ND", "31"="NE", "33"="NH", "34"="NJ", 
    "35"="NM", "32"="NV", "36"="NY", "39"="OH", "40"="OK", "41"="OR", "42"="PA", "44"="RI", 
    "45"="SC", "46"="SD", "47"="TN", "48"="TX", "49"="UT", "51"="VA", "50"="VT", "53"="WA", 
    "55"="WI", "54"="WV", "56"="WY")
  marg <- marg[substr(marg$GEOID, 1, 2) %in% names(state_from_fips),]
  marg <- data.frame(
    state=state_from_fips[substr(marg$GEOID, 1, 2)], 
    marg, 
    stringsAsFactors=FALSE)

  # missing data for income
  for (i in grep("inc__", colnames(marg)))
    marg[marg[,i] == 0, i] <- NA

  colnames(marg)[colnames(marg) == "GEOID"] <- "fips"
  return(marg)

}

RawACSToMarginal <- function(geog) {

  ################################################################################################################
  # roll up relevant variables

  marg <- data.frame(
    geog %>% 
      group_by(GEOID) %>% 
      summarize(
        # sex by age by education, 18+
        sex_age_edu__total=sum(estimate[variable %in% c("B15001_001")], na.rm=TRUE), 
        sex_age_edu__female_1824_college=sum(estimate[variable %in% c('B15001_050')], na.rm=TRUE), 
        sex_age_edu__female_1824_hs=sum(estimate[variable %in% c('B15001_047')], na.rm=TRUE), 
        sex_age_edu__female_1824_nohs=sum(estimate[variable %in% c('B15001_045', 'B15001_046')], na.rm=TRUE), 
        sex_age_edu__female_1824_postgrad=sum(estimate[variable %in% c('B15001_051')], na.rm=TRUE), 
        sex_age_edu__female_1824_somecollege=sum(estimate[variable %in% c('B15001_048', 'B15001_049')], na.rm=TRUE), 
        sex_age_edu__female_2534_college=sum(estimate[variable %in% c('B15001_058')], na.rm=TRUE), 
        sex_age_edu__female_2534_hs=sum(estimate[variable %in% c('B15001_055')], na.rm=TRUE), 
        sex_age_edu__female_2534_nohs=sum(estimate[variable %in% c('B15001_053', 'B15001_054')], na.rm=TRUE), 
        sex_age_edu__female_2534_postgrad=sum(estimate[variable %in% c('B15001_059')], na.rm=TRUE), 
        sex_age_edu__female_2534_somecollege=sum(estimate[variable %in% c('B15001_056', 'B15001_057')], na.rm=TRUE), 
        sex_age_edu__female_3544_college=sum(estimate[variable %in% c('B15001_066')], na.rm=TRUE), 
        sex_age_edu__female_3544_hs=sum(estimate[variable %in% c('B15001_063')], na.rm=TRUE), 
        sex_age_edu__female_3544_nohs=sum(estimate[variable %in% c('B15001_061', 'B15001_062')], na.rm=TRUE), 
        sex_age_edu__female_3544_postgrad=sum(estimate[variable %in% c('B15001_067')], na.rm=TRUE), 
        sex_age_edu__female_3544_somecollege=sum(estimate[variable %in% c('B15001_064', 'B15001_065')], na.rm=TRUE), 
        sex_age_edu__female_4564_college=sum(estimate[variable %in% c('B15001_074')], na.rm=TRUE), 
        sex_age_edu__female_4564_hs=sum(estimate[variable %in% c('B15001_071')], na.rm=TRUE), 
        sex_age_edu__female_4564_nohs=sum(estimate[variable %in% c('B15001_069', 'B15001_070')], na.rm=TRUE), 
        sex_age_edu__female_4564_postgrad=sum(estimate[variable %in% c('B15001_075')], na.rm=TRUE), 
        sex_age_edu__female_4564_somecollege=sum(estimate[variable %in% c('B15001_072', 'B15001_073')], na.rm=TRUE), 
        sex_age_edu__female_65plus_college=sum(estimate[variable %in% c('B15001_082')], na.rm=TRUE), 
        sex_age_edu__female_65plus_hs=sum(estimate[variable %in% c('B15001_079')], na.rm=TRUE), 
        sex_age_edu__female_65plus_nohs=sum(estimate[variable %in% c('B15001_077', 'B15001_078')], na.rm=TRUE), 
        sex_age_edu__female_65plus_postgrad=sum(estimate[variable %in% c('B15001_083')], na.rm=TRUE), 
        sex_age_edu__female_65plus_somecollege=sum(estimate[variable %in% c('B15001_080', 'B15001_081')], na.rm=TRUE), 
        sex_age_edu__male_1824_college=sum(estimate[variable %in% c('B15001_009')], na.rm=TRUE), 
        sex_age_edu__male_1824_hs=sum(estimate[variable %in% c('B15001_006')], na.rm=TRUE), 
        sex_age_edu__male_1824_nohs=sum(estimate[variable %in% c('B15001_004', 'B15001_005')], na.rm=TRUE), 
        sex_age_edu__male_1824_postgrad=sum(estimate[variable %in% c('B15001_010')], na.rm=TRUE), 
        sex_age_edu__male_1824_somecollege=sum(estimate[variable %in% c('B15001_007', 'B15001_008')], na.rm=TRUE), 
        sex_age_edu__male_2534_college=sum(estimate[variable %in% c('B15001_017')], na.rm=TRUE), 
        sex_age_edu__male_2534_hs=sum(estimate[variable %in% c('B15001_014')], na.rm=TRUE), 
        sex_age_edu__male_2534_nohs=sum(estimate[variable %in% c('B15001_012', 'B15001_013')], na.rm=TRUE), 
        sex_age_edu__male_2534_postgrad=sum(estimate[variable %in% c('B15001_018')], na.rm=TRUE), 
        sex_age_edu__male_2534_somecollege=sum(estimate[variable %in% c('B15001_015', 'B15001_016')], na.rm=TRUE), 
        sex_age_edu__male_3544_college=sum(estimate[variable %in% c('B15001_025')], na.rm=TRUE), 
        sex_age_edu__male_3544_hs=sum(estimate[variable %in% c('B15001_022')], na.rm=TRUE), 
        sex_age_edu__male_3544_nohs=sum(estimate[variable %in% c('B15001_020', 'B15001_021')], na.rm=TRUE), 
        sex_age_edu__male_3544_postgrad=sum(estimate[variable %in% c('B15001_026')], na.rm=TRUE), 
        sex_age_edu__male_3544_somecollege=sum(estimate[variable %in% c('B15001_023', 'B15001_024')], na.rm=TRUE), 
        sex_age_edu__male_4564_college=sum(estimate[variable %in% c('B15001_033')], na.rm=TRUE), 
        sex_age_edu__male_4564_hs=sum(estimate[variable %in% c('B15001_030')], na.rm=TRUE), 
        sex_age_edu__male_4564_nohs=sum(estimate[variable %in% c('B15001_028', 'B15001_029')], na.rm=TRUE), 
        sex_age_edu__male_4564_postgrad=sum(estimate[variable %in% c('B15001_034')], na.rm=TRUE), 
        sex_age_edu__male_4564_somecollege=sum(estimate[variable %in% c('B15001_031', 'B15001_032')], na.rm=TRUE), 
        sex_age_edu__male_65plus_college=sum(estimate[variable %in% c('B15001_041')], na.rm=TRUE), 
        sex_age_edu__male_65plus_hs=sum(estimate[variable %in% c('B15001_038')], na.rm=TRUE), 
        sex_age_edu__male_65plus_nohs=sum(estimate[variable %in% c('B15001_036', 'B15001_037')], na.rm=TRUE), 
        sex_age_edu__male_65plus_postgrad=sum(estimate[variable %in% c('B15001_042')], na.rm=TRUE), 
        sex_age_edu__male_65plus_somecollege=sum(estimate[variable %in% c('B15001_039', 'B15001_040')], na.rm=TRUE), 
        # sex by age by marital status, 15+
        sex_age_marital__total=sum(estimate[variable %in% c('B12002_001')], na.rm=TRUE), 
        sex_age_marital__female_1517_married=sum(estimate[variable %in% c('B12002_113', 'B12002_144')], na.rm=TRUE), 
        sex_age_marital__female_1517_single=sum(estimate[variable %in% c('B12002_097', 'B12002_129', 'B12002_159', 'B12002_174')], na.rm=TRUE), 
        sex_age_marital__female_1819_married=sum(estimate[variable %in% c('B12002_114', 'B12002_145')], na.rm=TRUE), 
        sex_age_marital__female_1819_single=sum(estimate[variable %in% c('B12002_098', 'B12002_130', 'B12002_160', 'B12002_175')], na.rm=TRUE), 
        sex_age_marital__female_2024_married=sum(estimate[variable %in% c('B12002_115', 'B12002_146')], na.rm=TRUE), 
        sex_age_marital__female_2024_single=sum(estimate[variable %in% c('B12002_099', 'B12002_131', 'B12002_161', 'B12002_176')], na.rm=TRUE), 
        sex_age_marital__female_2529_married=sum(estimate[variable %in% c('B12002_116', 'B12002_147')], na.rm=TRUE), 
        sex_age_marital__female_2529_single=sum(estimate[variable %in% c('B12002_100', 'B12002_132', 'B12002_162', 'B12002_177')], na.rm=TRUE), 
        sex_age_marital__female_3034_married=sum(estimate[variable %in% c('B12002_117', 'B12002_148')], na.rm=TRUE), 
        sex_age_marital__female_3034_single=sum(estimate[variable %in% c('B12002_101', 'B12002_133', 'B12002_163', 'B12002_178')], na.rm=TRUE), 
        sex_age_marital__female_3539_married=sum(estimate[variable %in% c('B12002_118', 'B12002_149')], na.rm=TRUE), 
        sex_age_marital__female_3539_single=sum(estimate[variable %in% c('B12002_102', 'B12002_134', 'B12002_164', 'B12002_179')], na.rm=TRUE), 
        sex_age_marital__female_4044_married=sum(estimate[variable %in% c('B12002_119', 'B12002_150')], na.rm=TRUE), 
        sex_age_marital__female_4044_single=sum(estimate[variable %in% c('B12002_103', 'B12002_135', 'B12002_165', 'B12002_180')], na.rm=TRUE), 
        sex_age_marital__female_4549_married=sum(estimate[variable %in% c('B12002_120', 'B12002_151')], na.rm=TRUE), 
        sex_age_marital__female_4549_single=sum(estimate[variable %in% c('B12002_104', 'B12002_136', 'B12002_166', 'B12002_181')], na.rm=TRUE), 
        sex_age_marital__female_5054_married=sum(estimate[variable %in% c('B12002_121', 'B12002_152')], na.rm=TRUE), 
        sex_age_marital__female_5054_single=sum(estimate[variable %in% c('B12002_105', 'B12002_137', 'B12002_167', 'B12002_182')], na.rm=TRUE), 
        sex_age_marital__female_5559_married=sum(estimate[variable %in% c('B12002_122', 'B12002_153')], na.rm=TRUE), 
        sex_age_marital__female_5559_single=sum(estimate[variable %in% c('B12002_106', 'B12002_138', 'B12002_168', 'B12002_183')], na.rm=TRUE), 
        sex_age_marital__female_6064_married=sum(estimate[variable %in% c('B12002_123', 'B12002_154')], na.rm=TRUE), 
        sex_age_marital__female_6064_single=sum(estimate[variable %in% c('B12002_107', 'B12002_139', 'B12002_169', 'B12002_184')], na.rm=TRUE), 
        sex_age_marital__female_6574_married=sum(estimate[variable %in% c('B12002_124', 'B12002_155')], na.rm=TRUE), 
        sex_age_marital__female_6574_single=sum(estimate[variable %in% c('B12002_108', 'B12002_140', 'B12002_170', 'B12002_185')], na.rm=TRUE), 
        sex_age_marital__female_75plus_married=sum(estimate[variable %in% c('B12002_125', 'B12002_126', 'B12002_156', 'B12002_157')], na.rm=TRUE), 
        sex_age_marital__female_75plus_single=sum(estimate[variable %in% c('B12002_109', 'B12002_110', 'B12002_141', 'B12002_142', 'B12002_171', 'B12002_172', 'B12002_186', 'B12002_187')], na.rm=TRUE), 
        sex_age_marital__male_1517_married=sum(estimate[variable %in% c('B12002_020', 'B12002_051')], na.rm=TRUE), 
        sex_age_marital__male_1517_single=sum(estimate[variable %in% c('B12002_004', 'B12002_036', 'B12002_066', 'B12002_081')], na.rm=TRUE), 
        sex_age_marital__male_1819_married=sum(estimate[variable %in% c('B12002_021', 'B12002_052')], na.rm=TRUE), 
        sex_age_marital__male_1819_single=sum(estimate[variable %in% c('B12002_005', 'B12002_037', 'B12002_067', 'B12002_082')], na.rm=TRUE), 
        sex_age_marital__male_2024_married=sum(estimate[variable %in% c('B12002_022', 'B12002_053')], na.rm=TRUE), 
        sex_age_marital__male_2024_single=sum(estimate[variable %in% c('B12002_006', 'B12002_038', 'B12002_068', 'B12002_083')], na.rm=TRUE), 
        sex_age_marital__male_2529_married=sum(estimate[variable %in% c('B12002_023', 'B12002_054')], na.rm=TRUE), 
        sex_age_marital__male_2529_single=sum(estimate[variable %in% c('B12002_007', 'B12002_039', 'B12002_069', 'B12002_084')], na.rm=TRUE), 
        sex_age_marital__male_3034_married=sum(estimate[variable %in% c('B12002_024', 'B12002_055')], na.rm=TRUE), 
        sex_age_marital__male_3034_single=sum(estimate[variable %in% c('B12002_008', 'B12002_040', 'B12002_070', 'B12002_085')], na.rm=TRUE), 
        sex_age_marital__male_3539_married=sum(estimate[variable %in% c('B12002_025', 'B12002_056')], na.rm=TRUE), 
        sex_age_marital__male_3539_single=sum(estimate[variable %in% c('B12002_009', 'B12002_041', 'B12002_071', 'B12002_086')], na.rm=TRUE), 
        sex_age_marital__male_4044_married=sum(estimate[variable %in% c('B12002_026', 'B12002_057')], na.rm=TRUE), 
        sex_age_marital__male_4044_single=sum(estimate[variable %in% c('B12002_010', 'B12002_042', 'B12002_072', 'B12002_087')], na.rm=TRUE), 
        sex_age_marital__male_4549_married=sum(estimate[variable %in% c('B12002_027', 'B12002_058')], na.rm=TRUE), 
        sex_age_marital__male_4549_single=sum(estimate[variable %in% c('B12002_011', 'B12002_043', 'B12002_073', 'B12002_088')], na.rm=TRUE), 
        sex_age_marital__male_5054_married=sum(estimate[variable %in% c('B12002_028', 'B12002_059')], na.rm=TRUE), 
        sex_age_marital__male_5054_single=sum(estimate[variable %in% c('B12002_012', 'B12002_044', 'B12002_074', 'B12002_089')], na.rm=TRUE), 
        sex_age_marital__male_5559_married=sum(estimate[variable %in% c('B12002_029', 'B12002_060')], na.rm=TRUE), 
        sex_age_marital__male_5559_single=sum(estimate[variable %in% c('B12002_013', 'B12002_045', 'B12002_075', 'B12002_090')], na.rm=TRUE), 
        sex_age_marital__male_6064_married=sum(estimate[variable %in% c('B12002_030', 'B12002_061')], na.rm=TRUE), 
        sex_age_marital__male_6064_single=sum(estimate[variable %in% c('B12002_014', 'B12002_046', 'B12002_076', 'B12002_091')], na.rm=TRUE), 
        sex_age_marital__male_6574_married=sum(estimate[variable %in% c('B12002_031', 'B12002_062')], na.rm=TRUE), 
        sex_age_marital__male_6574_single=sum(estimate[variable %in% c('B12002_015', 'B12002_047', 'B12002_077', 'B12002_092')], na.rm=TRUE), 
        sex_age_marital__male_75plus_married=sum(estimate[variable %in% c('B12002_032', 'B12002_033', 'B12002_063', 'B12002_064')], na.rm=TRUE), 
        sex_age_marital__male_75plus_single=sum(estimate[variable %in% c('B12002_016', 'B12002_017', 'B12002_048', 'B12002_049', 'B12002_078', 'B12002_079', 'B12002_093', 'B12002_094')], na.rm=TRUE), 
        # sex by age by race, total population
        sex_age_race__total=sum(estimate[variable %in% c('B01001B_001', 'B01001C_001', 'B01001D_001', 'B01001E_001', 'B01001F_001', 'B01001G_001', 'B01001H_001', 'B01001I_001')], na.rm=TRUE), 
        sex_age_race__female_1517_asian=sum(estimate[variable %in% c('B01001D_021', 'B01001E_021')], na.rm=TRUE), 
        sex_age_race__female_1517_black=sum(estimate[variable %in% c('B01001B_021')], na.rm=TRUE), 
        sex_age_race__female_1517_hispanic=sum(estimate[variable %in% c('B01001I_021')], na.rm=TRUE), 
        sex_age_race__female_1517_native=sum(estimate[variable %in% c('B01001C_021')], na.rm=TRUE), 
        sex_age_race__female_1517_other=sum(estimate[variable %in% c('B01001F_021', 'B01001G_021')], na.rm=TRUE), 
        sex_age_race__female_1517_white=sum(estimate[variable %in% c('B01001H_021')], na.rm=TRUE), 
        sex_age_race__female_1819_asian=sum(estimate[variable %in% c('B01001D_022', 'B01001E_022')], na.rm=TRUE), 
        sex_age_race__female_1819_black=sum(estimate[variable %in% c('B01001B_022')], na.rm=TRUE), 
        sex_age_race__female_1819_hispanic=sum(estimate[variable %in% c('B01001I_022')], na.rm=TRUE), 
        sex_age_race__female_1819_native=sum(estimate[variable %in% c('B01001C_022')], na.rm=TRUE), 
        sex_age_race__female_1819_other=sum(estimate[variable %in% c('B01001F_022', 'B01001G_022')], na.rm=TRUE), 
        sex_age_race__female_1819_white=sum(estimate[variable %in% c('B01001H_022')], na.rm=TRUE), 
        sex_age_race__female_2024_asian=sum(estimate[variable %in% c('B01001D_023', 'B01001E_023')], na.rm=TRUE), 
        sex_age_race__female_2024_black=sum(estimate[variable %in% c('B01001B_023')], na.rm=TRUE), 
        sex_age_race__female_2024_hispanic=sum(estimate[variable %in% c('B01001I_023')], na.rm=TRUE), 
        sex_age_race__female_2024_native=sum(estimate[variable %in% c('B01001C_023')], na.rm=TRUE), 
        sex_age_race__female_2024_other=sum(estimate[variable %in% c('B01001F_023', 'B01001G_023')], na.rm=TRUE), 
        sex_age_race__female_2024_white=sum(estimate[variable %in% c('B01001H_023')], na.rm=TRUE), 
        sex_age_race__female_2529_asian=sum(estimate[variable %in% c('B01001D_024', 'B01001E_024')], na.rm=TRUE), 
        sex_age_race__female_2529_black=sum(estimate[variable %in% c('B01001B_024')], na.rm=TRUE), 
        sex_age_race__female_2529_hispanic=sum(estimate[variable %in% c('B01001I_024')], na.rm=TRUE), 
        sex_age_race__female_2529_native=sum(estimate[variable %in% c('B01001C_024')], na.rm=TRUE), 
        sex_age_race__female_2529_other=sum(estimate[variable %in% c('B01001F_024', 'B01001G_024')], na.rm=TRUE), 
        sex_age_race__female_2529_white=sum(estimate[variable %in% c('B01001H_024')], na.rm=TRUE), 
        sex_age_race__female_3034_asian=sum(estimate[variable %in% c('B01001D_025', 'B01001E_025')], na.rm=TRUE), 
        sex_age_race__female_3034_black=sum(estimate[variable %in% c('B01001B_025')], na.rm=TRUE), 
        sex_age_race__female_3034_hispanic=sum(estimate[variable %in% c('B01001I_025')], na.rm=TRUE), 
        sex_age_race__female_3034_native=sum(estimate[variable %in% c('B01001C_025')], na.rm=TRUE), 
        sex_age_race__female_3034_other=sum(estimate[variable %in% c('B01001F_025', 'B01001G_025')], na.rm=TRUE), 
        sex_age_race__female_3034_white=sum(estimate[variable %in% c('B01001H_025')], na.rm=TRUE), 
        sex_age_race__female_3544_asian=sum(estimate[variable %in% c('B01001D_026', 'B01001E_026')], na.rm=TRUE), 
        sex_age_race__female_3544_black=sum(estimate[variable %in% c('B01001B_026')], na.rm=TRUE), 
        sex_age_race__female_3544_hispanic=sum(estimate[variable %in% c('B01001I_026')], na.rm=TRUE), 
        sex_age_race__female_3544_native=sum(estimate[variable %in% c('B01001C_026')], na.rm=TRUE), 
        sex_age_race__female_3544_other=sum(estimate[variable %in% c('B01001F_026', 'B01001G_026')], na.rm=TRUE), 
        sex_age_race__female_3544_white=sum(estimate[variable %in% c('B01001H_026')], na.rm=TRUE), 
        sex_age_race__female_4554_asian=sum(estimate[variable %in% c('B01001D_027', 'B01001E_027')], na.rm=TRUE), 
        sex_age_race__female_4554_black=sum(estimate[variable %in% c('B01001B_027')], na.rm=TRUE), 
        sex_age_race__female_4554_hispanic=sum(estimate[variable %in% c('B01001I_027')], na.rm=TRUE), 
        sex_age_race__female_4554_native=sum(estimate[variable %in% c('B01001C_027')], na.rm=TRUE), 
        sex_age_race__female_4554_other=sum(estimate[variable %in% c('B01001F_027', 'B01001G_027')], na.rm=TRUE), 
        sex_age_race__female_4554_white=sum(estimate[variable %in% c('B01001H_027')], na.rm=TRUE), 
        sex_age_race__female_5564_asian=sum(estimate[variable %in% c('B01001D_028', 'B01001E_028')], na.rm=TRUE), 
        sex_age_race__female_5564_black=sum(estimate[variable %in% c('B01001B_028')], na.rm=TRUE), 
        sex_age_race__female_5564_hispanic=sum(estimate[variable %in% c('B01001I_028')], na.rm=TRUE), 
        sex_age_race__female_5564_native=sum(estimate[variable %in% c('B01001C_028')], na.rm=TRUE), 
        sex_age_race__female_5564_other=sum(estimate[variable %in% c('B01001F_028', 'B01001G_028')], na.rm=TRUE), 
        sex_age_race__female_5564_white=sum(estimate[variable %in% c('B01001H_028')], na.rm=TRUE), 
        sex_age_race__female_6574_asian=sum(estimate[variable %in% c('B01001D_029', 'B01001E_029')], na.rm=TRUE), 
        sex_age_race__female_6574_black=sum(estimate[variable %in% c('B01001B_029')], na.rm=TRUE), 
        sex_age_race__female_6574_hispanic=sum(estimate[variable %in% c('B01001I_029')], na.rm=TRUE), 
        sex_age_race__female_6574_native=sum(estimate[variable %in% c('B01001C_029')], na.rm=TRUE), 
        sex_age_race__female_6574_other=sum(estimate[variable %in% c('B01001F_029', 'B01001G_029')], na.rm=TRUE), 
        sex_age_race__female_6574_white=sum(estimate[variable %in% c('B01001H_029')], na.rm=TRUE), 
        sex_age_race__female_75plus_asian=sum(estimate[variable %in% c('B01001D_030', 'B01001D_031', 'B01001E_030', 'B01001E_031')], na.rm=TRUE), 
        sex_age_race__female_75plus_black=sum(estimate[variable %in% c('B01001B_030', 'B01001B_031')], na.rm=TRUE), 
        sex_age_race__female_75plus_hispanic=sum(estimate[variable %in% c('B01001I_030', 'B01001I_031')], na.rm=TRUE), 
        sex_age_race__female_75plus_native=sum(estimate[variable %in% c('B01001C_030', 'B01001C_031')], na.rm=TRUE), 
        sex_age_race__female_75plus_other=sum(estimate[variable %in% c('B01001F_030', 'B01001F_031', 'B01001G_030', 'B01001G_031')], na.rm=TRUE), 
        sex_age_race__female_75plus_white=sum(estimate[variable %in% c('B01001H_030', 'B01001H_031')], na.rm=TRUE), 
        sex_age_race__female_under18_asian=sum(estimate[variable %in% c('B01001D_018', 'B01001D_019', 'B01001D_020', 'B01001E_018', 'B01001E_019', 'B01001E_020')], na.rm=TRUE), 
        sex_age_race__female_under18_black=sum(estimate[variable %in% c('B01001B_018', 'B01001B_019', 'B01001B_020')], na.rm=TRUE), 
        sex_age_race__female_under18_hispanic=sum(estimate[variable %in% c('B01001I_018', 'B01001I_019', 'B01001I_020')], na.rm=TRUE), 
        sex_age_race__female_under18_native=sum(estimate[variable %in% c('B01001C_018', 'B01001C_019', 'B01001C_020')], na.rm=TRUE), 
        sex_age_race__female_under18_other=sum(estimate[variable %in% c('B01001F_018', 'B01001F_019', 'B01001F_020', 'B01001G_018', 'B01001G_019', 'B01001G_020')], na.rm=TRUE), 
        sex_age_race__female_under18_white=sum(estimate[variable %in% c('B01001H_018', 'B01001H_019', 'B01001H_020')], na.rm=TRUE), 
        sex_age_race__male_1517_asian=sum(estimate[variable %in% c('B01001D_006', 'B01001E_006')], na.rm=TRUE), 
        sex_age_race__male_1517_black=sum(estimate[variable %in% c('B01001B_006')], na.rm=TRUE), 
        sex_age_race__male_1517_hispanic=sum(estimate[variable %in% c('B01001I_006')], na.rm=TRUE), 
        sex_age_race__male_1517_native=sum(estimate[variable %in% c('B01001C_006')], na.rm=TRUE), 
        sex_age_race__male_1517_other=sum(estimate[variable %in% c('B01001F_006', 'B01001G_006')], na.rm=TRUE), 
        sex_age_race__male_1517_white=sum(estimate[variable %in% c('B01001H_006')], na.rm=TRUE), 
        sex_age_race__male_1819_asian=sum(estimate[variable %in% c('B01001D_007', 'B01001E_007')], na.rm=TRUE), 
        sex_age_race__male_1819_black=sum(estimate[variable %in% c('B01001B_007')], na.rm=TRUE), 
        sex_age_race__male_1819_hispanic=sum(estimate[variable %in% c('B01001I_007')], na.rm=TRUE), 
        sex_age_race__male_1819_native=sum(estimate[variable %in% c('B01001C_007')], na.rm=TRUE), 
        sex_age_race__male_1819_other=sum(estimate[variable %in% c('B01001F_007', 'B01001G_007')], na.rm=TRUE), 
        sex_age_race__male_1819_white=sum(estimate[variable %in% c('B01001H_007')], na.rm=TRUE), 
        sex_age_race__male_2024_asian=sum(estimate[variable %in% c('B01001D_008', 'B01001E_008')], na.rm=TRUE), 
        sex_age_race__male_2024_black=sum(estimate[variable %in% c('B01001B_008')], na.rm=TRUE), 
        sex_age_race__male_2024_hispanic=sum(estimate[variable %in% c('B01001I_008')], na.rm=TRUE), 
        sex_age_race__male_2024_native=sum(estimate[variable %in% c('B01001C_008')], na.rm=TRUE), 
        sex_age_race__male_2024_other=sum(estimate[variable %in% c('B01001F_008', 'B01001G_008')], na.rm=TRUE), 
        sex_age_race__male_2024_white=sum(estimate[variable %in% c('B01001H_008')], na.rm=TRUE), 
        sex_age_race__male_2529_asian=sum(estimate[variable %in% c('B01001D_009', 'B01001E_009')], na.rm=TRUE), 
        sex_age_race__male_2529_black=sum(estimate[variable %in% c('B01001B_009')], na.rm=TRUE), 
        sex_age_race__male_2529_hispanic=sum(estimate[variable %in% c('B01001I_009')], na.rm=TRUE), 
        sex_age_race__male_2529_native=sum(estimate[variable %in% c('B01001C_009')], na.rm=TRUE), 
        sex_age_race__male_2529_other=sum(estimate[variable %in% c('B01001F_009', 'B01001G_009')], na.rm=TRUE), 
        sex_age_race__male_2529_white=sum(estimate[variable %in% c('B01001H_009')], na.rm=TRUE), 
        sex_age_race__male_3034_asian=sum(estimate[variable %in% c('B01001D_010', 'B01001E_010')], na.rm=TRUE), 
        sex_age_race__male_3034_black=sum(estimate[variable %in% c('B01001B_010')], na.rm=TRUE), 
        sex_age_race__male_3034_hispanic=sum(estimate[variable %in% c('B01001I_010')], na.rm=TRUE), 
        sex_age_race__male_3034_native=sum(estimate[variable %in% c('B01001C_010')], na.rm=TRUE), 
        sex_age_race__male_3034_other=sum(estimate[variable %in% c('B01001F_010', 'B01001G_010')], na.rm=TRUE), 
        sex_age_race__male_3034_white=sum(estimate[variable %in% c('B01001H_010')], na.rm=TRUE), 
        sex_age_race__male_3544_asian=sum(estimate[variable %in% c('B01001D_011', 'B01001E_011')], na.rm=TRUE), 
        sex_age_race__male_3544_black=sum(estimate[variable %in% c('B01001B_011')], na.rm=TRUE), 
        sex_age_race__male_3544_hispanic=sum(estimate[variable %in% c('B01001I_011')], na.rm=TRUE), 
        sex_age_race__male_3544_native=sum(estimate[variable %in% c('B01001C_011')], na.rm=TRUE), 
        sex_age_race__male_3544_other=sum(estimate[variable %in% c('B01001F_011', 'B01001G_011')], na.rm=TRUE), 
        sex_age_race__male_3544_white=sum(estimate[variable %in% c('B01001H_011')], na.rm=TRUE), 
        sex_age_race__male_4554_asian=sum(estimate[variable %in% c('B01001D_012', 'B01001E_012')], na.rm=TRUE), 
        sex_age_race__male_4554_black=sum(estimate[variable %in% c('B01001B_012')], na.rm=TRUE), 
        sex_age_race__male_4554_hispanic=sum(estimate[variable %in% c('B01001I_012')], na.rm=TRUE), 
        sex_age_race__male_4554_native=sum(estimate[variable %in% c('B01001C_012')], na.rm=TRUE), 
        sex_age_race__male_4554_other=sum(estimate[variable %in% c('B01001F_012', 'B01001G_012')], na.rm=TRUE), 
        sex_age_race__male_4554_white=sum(estimate[variable %in% c('B01001H_012')], na.rm=TRUE), 
        sex_age_race__male_5564_asian=sum(estimate[variable %in% c('B01001D_013', 'B01001E_013')], na.rm=TRUE), 
        sex_age_race__male_5564_black=sum(estimate[variable %in% c('B01001B_013')], na.rm=TRUE), 
        sex_age_race__male_5564_hispanic=sum(estimate[variable %in% c('B01001I_013')], na.rm=TRUE), 
        sex_age_race__male_5564_native=sum(estimate[variable %in% c('B01001C_013')], na.rm=TRUE), 
        sex_age_race__male_5564_other=sum(estimate[variable %in% c('B01001F_013', 'B01001G_013')], na.rm=TRUE), 
        sex_age_race__male_5564_white=sum(estimate[variable %in% c('B01001H_013')], na.rm=TRUE), 
        sex_age_race__male_6574_asian=sum(estimate[variable %in% c('B01001D_014', 'B01001E_014')], na.rm=TRUE), 
        sex_age_race__male_6574_black=sum(estimate[variable %in% c('B01001B_014')], na.rm=TRUE), 
        sex_age_race__male_6574_hispanic=sum(estimate[variable %in% c('B01001I_014')], na.rm=TRUE), 
        sex_age_race__male_6574_native=sum(estimate[variable %in% c('B01001C_014')], na.rm=TRUE), 
        sex_age_race__male_6574_other=sum(estimate[variable %in% c('B01001F_014', 'B01001G_014')], na.rm=TRUE), 
        sex_age_race__male_6574_white=sum(estimate[variable %in% c('B01001H_014')], na.rm=TRUE), 
        sex_age_race__male_75plus_asian=sum(estimate[variable %in% c('B01001D_015', 'B01001D_016', 'B01001E_015', 'B01001E_016')], na.rm=TRUE), 
        sex_age_race__male_75plus_black=sum(estimate[variable %in% c('B01001B_015', 'B01001B_016')], na.rm=TRUE), 
        sex_age_race__male_75plus_hispanic=sum(estimate[variable %in% c('B01001I_015', 'B01001I_016')], na.rm=TRUE), 
        sex_age_race__male_75plus_native=sum(estimate[variable %in% c('B01001C_015', 'B01001C_016')], na.rm=TRUE), 
        sex_age_race__male_75plus_other=sum(estimate[variable %in% c('B01001F_015', 'B01001F_016', 'B01001G_015', 'B01001G_016')], na.rm=TRUE), 
        sex_age_race__male_75plus_white=sum(estimate[variable %in% c('B01001H_015', 'B01001H_016')], na.rm=TRUE), 
        sex_age_race__male_under18_asian=sum(estimate[variable %in% c('B01001D_003', 'B01001D_004', 'B01001D_005', 'B01001E_003', 'B01001E_004', 'B01001E_005')], na.rm=TRUE), 
        sex_age_race__male_under18_black=sum(estimate[variable %in% c('B01001B_003', 'B01001B_004', 'B01001B_005')], na.rm=TRUE), 
        sex_age_race__male_under18_hispanic=sum(estimate[variable %in% c('B01001I_003', 'B01001I_004', 'B01001I_005')], na.rm=TRUE), 
        sex_age_race__male_under18_native=sum(estimate[variable %in% c('B01001C_003', 'B01001C_004', 'B01001C_005')], na.rm=TRUE), 
        sex_age_race__male_under18_other=sum(estimate[variable %in% c('B01001F_003', 'B01001F_004', 'B01001F_005', 'B01001G_003', 'B01001G_004', 'B01001G_005')], na.rm=TRUE), 
        sex_age_race__male_under18_white=sum(estimate[variable %in% c('B01001H_003', 'B01001H_004', 'B01001H_005')], na.rm=TRUE), 
        # sex by age by citizenship, total population
        sex_age_race_citizenship__female_18plus_asian_citizen=sum(estimate[variable %in% c('B05003D_020', 'B05003D_022', 'B05003E_020', 'B05003E_022')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_asian_noncitizen=sum(estimate[variable %in% c('B05003D_023', 'B05003E_023')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_black_citizen=sum(estimate[variable %in% c('B05003B_020', 'B05003B_022')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_black_noncitizen=sum(estimate[variable %in% c('B05003B_023')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_hispanic_citizen=sum(estimate[variable %in% c('B05003I_020', 'B05003I_022')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_hispanic_noncitizen=sum(estimate[variable %in% c('B05003I_023')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_native_citizen=sum(estimate[variable %in% c('B05003C_020', 'B05003C_022')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_native_noncitizen=sum(estimate[variable %in% c('B05003C_023')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_other_citizen=sum(estimate[variable %in% c('B05003F_020', 'B05003F_022', 'B05003G_020', 'B05003G_022')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_other_noncitizen=sum(estimate[variable %in% c('B05003F_023', 'B05003G_023')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_white_citizen=sum(estimate[variable %in% c('B05003H_020', 'B05003H_022')], na.rm=TRUE), 
        sex_age_race_citizenship__female_18plus_white_noncitizen=sum(estimate[variable %in% c('B05003H_023')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_asian_citizen=sum(estimate[variable %in% c('B05003D_015', 'B05003D_017', 'B05003E_015', 'B05003E_017')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_asian_noncitizen=sum(estimate[variable %in% c('B05003D_018', 'B05003E_018')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_black_citizen=sum(estimate[variable %in% c('B05003B_015', 'B05003B_017')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_black_noncitizen=sum(estimate[variable %in% c('B05003B_018')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_hispanic_citizen=sum(estimate[variable %in% c('B05003I_015', 'B05003I_017')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_hispanic_noncitizen=sum(estimate[variable %in% c('B05003I_018')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_native_citizen=sum(estimate[variable %in% c('B05003C_015', 'B05003C_017')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_native_noncitizen=sum(estimate[variable %in% c('B05003C_018')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_other_citizen=sum(estimate[variable %in% c('B05003F_015', 'B05003F_017', 'B05003G_015', 'B05003G_017')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_other_noncitizen=sum(estimate[variable %in% c('B05003F_018', 'B05003G_018')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_white_citizen=sum(estimate[variable %in% c('B05003H_015', 'B05003H_017')], na.rm=TRUE), 
        sex_age_race_citizenship__female_under18_white_noncitizen=sum(estimate[variable %in% c('B05003H_018')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_asian_citizen=sum(estimate[variable %in% c('B05003D_009', 'B05003D_011', 'B05003E_009', 'B05003E_011')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_asian_noncitizen=sum(estimate[variable %in% c('B05003D_012', 'B05003E_012')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_black_citizen=sum(estimate[variable %in% c('B05003B_009', 'B05003B_011')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_black_noncitizen=sum(estimate[variable %in% c('B05003B_012')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_hispanic_citizen=sum(estimate[variable %in% c('B05003I_009', 'B05003I_011')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_hispanic_noncitizen=sum(estimate[variable %in% c('B05003I_012')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_native_citizen=sum(estimate[variable %in% c('B05003C_009', 'B05003C_011')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_native_noncitizen=sum(estimate[variable %in% c('B05003C_012')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_other_citizen=sum(estimate[variable %in% c('B05003F_009', 'B05003F_011', 'B05003G_009', 'B05003G_011')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_other_noncitizen=sum(estimate[variable %in% c('B05003F_012', 'B05003G_012')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_white_citizen=sum(estimate[variable %in% c('B05003H_009', 'B05003H_011')], na.rm=TRUE), 
        sex_age_race_citizenship__male_18plus_white_noncitizen=sum(estimate[variable %in% c('B05003H_012')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_asian_citizen=sum(estimate[variable %in% c('B05003D_004', 'B05003D_006', 'B05003E_004', 'B05003E_006')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_asian_noncitizen=sum(estimate[variable %in% c('B05003D_007', 'B05003E_007')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_black_citizen=sum(estimate[variable %in% c('B05003B_004', 'B05003B_006')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_black_noncitizen=sum(estimate[variable %in% c('B05003B_007')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_hispanic_citizen=sum(estimate[variable %in% c('B05003I_004', 'B05003I_006')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_hispanic_noncitizen=sum(estimate[variable %in% c('B05003I_007')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_native_citizen=sum(estimate[variable %in% c('B05003C_004', 'B05003C_006')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_native_noncitizen=sum(estimate[variable %in% c('B05003C_007')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_other_citizen=sum(estimate[variable %in% c('B05003F_004', 'B05003F_006', 'B05003G_004', 'B05003G_006')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_other_noncitizen=sum(estimate[variable %in% c('B05003F_007', 'B05003G_007')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_white_citizen=sum(estimate[variable %in% c('B05003H_004', 'B05003H_006')], na.rm=TRUE), 
        sex_age_race_citizenship__male_under18_white_noncitizen=sum(estimate[variable %in% c('B05003H_007')], na.rm=TRUE), 
        # race correction data, total population
        racenumerator__total=sum(estimate[variable %in% c('B03002_001')], na.rm=TRUE), 
        racenumerator__white=sum(estimate[variable %in% c('B03002_003')], na.rm=TRUE), 
        racenumerator__black=sum(estimate[variable %in% c('B03002_004')], na.rm=TRUE), 
        racenumerator__hispanic=sum(estimate[variable %in% c('B03002_012')], na.rm=TRUE), 
        racenumerator__asian=sum(estimate[variable %in% c('B03002_006', 'B03002_007')], na.rm=TRUE), 
        racenumerator__native=sum(estimate[variable %in% c('B03002_005')], na.rm=TRUE), 
        racenumerator__other=sum(estimate[variable %in% c('B03002_008', 'B03002_009')], na.rm=TRUE), 
        racedenominator__white=sum(estimate[variable %in% c('B03002_003')], na.rm=TRUE), 
        racedenominator__black=sum(estimate[variable %in% c('B03002_004', 'B03002_014')], na.rm=TRUE), 
        racedenominator__hispanic=sum(estimate[variable %in% c('B03002_012')], na.rm=TRUE), 
        racedenominator__asian=sum(estimate[variable %in% c('B03002_006', 'B03002_007', 'B03002_016', 'B03002_017')], na.rm=TRUE), 
        racedenominator__native=sum(estimate[variable %in% c('B03002_005', 'B03002_015')], na.rm=TRUE), 
        racedenominator__other=sum(estimate[variable %in% c('B03002_008', 'B03002_009', 'B03002_018', 'B03002_019')], na.rm=TRUE)
      ))
  
  ################################################################################################################
  # checks and corrections
  
  # totals
  totals <- grep("__total", colnames(marg), value=TRUE)
  check <- sapply(totals, function(i) {
    xs <- grep(paste0("^", gsub("__total", "", i), "__"), colnames(marg), value=TRUE)
    xs <- grep("__total", xs, invert=TRUE, value=TRUE)
    all(rowSums(marg[,xs]) - marg[i] == 0)
  })
  if (any(!check)) {
    print(cbind(check))
    stop("problem in totals")
  }
  
  # race corrections. make it so that: 
  # hispanic = all hispanic
  # everything else = non-hispanic of that version
  races <- c("white", "black", "hispanic", "asian", "native", "other")
  for (race in races) {
    correction <- marg[, paste0("racenumerator__", race)] / marg[, paste0("racedenominator__", race)]
    if (any(correction > 1, na.rm=TRUE))
      stop(race, ": correction > 1")
    xs <- grep(race, colnames(marg), value=TRUE)
    if (any(unlist(marg[is.na(correction), xs]) != 0))
      stop(race, ": non-zero counts with missing correction data")
    correction[is.na(correction)] <- 1
    for (x in xs)
      marg[,x] <- marg[,x] * correction
  }
  
  # check that it added up to racenumerator__total now
  check <- sapply(c("sex_age_race", "sex_age_race_citizenship"), function(i) {
    xs <- grep(paste0("^", i, "__"), colnames(marg), value=TRUE)
    xs <- grep("__total", xs, invert=TRUE, value=TRUE)
    all(round(rowSums(marg[,xs]) - marg$racenumerator__total) == 0)
  })
  if (any(!check)) {
    print(cbind(check))
    stop("problem in totals (post-race correction)")
  }

  # some cleanup  
  marg <- marg[, grep("under18", colnames(marg), invert=TRUE)]
  marg <- marg[, grep("racenumerator", colnames(marg), invert=TRUE)]
  marg <- marg[, grep("racedenominator", colnames(marg), invert=TRUE)]
  
  # citizenship is no longer by age, so fix the labels
  colnames(marg) <- gsub("_18plus_", "_", colnames(marg))
  colnames(marg) <- gsub("sex_age_race_citizenship", "sex_race_citizenship", colnames(marg))

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
 
 colnames(marg)[colnames(marg) == "GEOID"] <- "fips"

 return(marg)

}

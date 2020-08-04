# DEEP-MAPS Model of the Labor Force

This is the source code used to produce DEEP-MAPS estimates of the labor force, as described in this [working paper](deep_maps_20200804.pdf). 

### Data Sources

To run this code, you will have to download (publicly available) data from the following sources: 

- [IPUMS CPS](https://cps.ipums.org/cps/): microdata for CPS surveys [1]
- [IPUMS USA](https://usa.ipums.org/usa/): microdata for ACS surveys [2]
- [Weekly unemployment insurance claims](https://oui.doleta.gov/unemploy/claims_arch.asp): from the US Department of Labor. Code in the [unemployment_insurance_claims](unemployment_insurance_claims) folder scrapes historical data, but the most recent weeks have to be downloaded manually in PDF form. 

Code is provided to download other data: 

- [Geographic ACS averages](unemployment_cps_mrp/downloaded_data/acs): we use the [tidycensus](https://walker-data.com/tidycensus/) package. 
- [CPS Time Series](unemployment_cps_mrp/downloaded_data/cps_timeseries). 
- [Local Area Unemployment Statistics](unemployment_cps_mrp/downloaded_data/laus). 

### Software

The code was primarily written in [R](https://www.r-project.org/). The following packages are used:

- [arm](https://cran.r-project.org/web/packages/arm/index.html)
- [colorout](https://github.com/jalvesaq/colorout)
- [data.table](https://cran.r-project.org/web/packages/data.table/index.html)
- [doMC](https://cran.r-project.org/web/packages/doMC/index.html)
- [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
- [foreach](https://cran.r-project.org/web/packages/foreach/index.html)
- [glmnet](https://cran.r-project.org/web/packages/glmnet/index.html)
- [gtools](https://cran.r-project.org/web/packages/gtools/index.html)
- [mapproj](https://cran.r-project.org/web/packages/mapproj/index.html)
- [maps](https://cran.r-project.org/web/packages/maps/index.html)
- [maptools](https://cran.r-project.org/web/packages/maptools/index.html)
- [openxlsx](https://cran.r-project.org/web/packages/openxlsx/index.html)
- [plotrix](https://cran.r-project.org/web/packages/plotrix/index.html)
- [raster](https://cran.r-project.org/web/packages/raster/index.html)
- [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html)
- [rgeos](https://cran.r-project.org/web/packages/rgeos/index.html)
- [RJDBC](https://cran.r-project.org/web/packages/RJDBC/index.html)
- [SDMTools](https://cran.r-project.org/web/packages/SDMTools/index.html)
- [sp](https://cran.r-project.org/web/packages/sp/index.html)
- [sqldf](https://cran.r-project.org/web/packages/sqldf/index.html)
- [stringdist](https://cran.r-project.org/web/packages/stringdist/index.html)
- [stringr](https://cran.r-project.org/web/packages/stringr/index.html)
- [tidycensus](https://cran.r-project.org/web/packages/tidycensus/index.html)

We also used the [Vertica](https://www.vertica.com/) database to load and transform the CPS/ACS microdata. The transformations are straight forward andwritten in SQL; an alternative database can be used to do these transformations, or they can be done inside of R using the sqldf package (or others), given sufficient memory resources. 

### Directory Structure

Here is a brief description of the most important components of the code. To run the code, you will have to change the working directory name (or remove it entirely), and download data/packages that are listed above. To understand how the model works (and the order in which the code needs to be run), see the [working paper](deep_maps_20200804.pdf). In the future, we intend to clean up and document the code more thoroughly, if there is sufficient outside interest. 

- [unemployment_insurance_claims](unemployment_insurance_claims): downloads data for weekly unemployment insurance claims. 
- [unemployment_cps_mrp](unemployment_cps_mrp)
  - [mrsp](unemployment_cps_mrp/mrsp): creates synthetic joint distributions at state, county, and tract level, built from ACS data. The structure of the code to create the county-level distributions is:
    - [01_county_2018_joint.R](unemployment_cps_mrp/mrsp/county/01_county_2018_joint.R): turns "marginal" demographic distributions into a full joint distribution for every county. 
    - [02_county_2018_occscore.R](unemployment_cps_mrp/mrsp/county/02_county_2018_occscore.R): applies various [occupation/industry models](unemployment_cps_mrp/run_mrp/02_occ_models.R) to each cell. 
    - [03_county_2018_occshift.R](unemployment_cps_mrp/mrsp/county/03_county_2018_occshift.R): adjusts each cell to add up to the correct number at the county level. 
    - Similar code is found for the tract level. For the state level, the joint demographic distribution is pulled directly from ACS microdata, but we still do steps 2 and 3. 
  - [run_mrp](unemployment_cps_mrp/run_mrp): here is where the bulk of the modeling and poststratification are run:
    - [01_load_acsocc_data.R](unemployment_cps_mrp/run_mrp/01_load_acsocc_data.R): load data from the ACS, and prep data for the occupation/industry models. 
    - [02_occ_models.R](unemployment_cps_mrp/run_mrp/02_occ_models.R): Fit occupation/industry models, to be used in the Demographically Adjusted Geographic Predictors (DAGPs). 
    - [03a_ipums_version.R](unemployment_cps_mrp/run_mrp/03a_ipums_version.R): run the labor force models, using IPUMS data as the source for microdata. 
    - [03b_bls_version.R](unemployment_cps_mrp/run_mrp/03b_bls_version.R): run the labor force models, downloading data straight from the BLS website (if IPUMS data is not available yet). 
    - [04a_state_yhat.R](unemployment_cps_mrp/run_mrp/04a_state_yhat.R): apply models to state-level joint distribution. 
    - [04b_county_yhat.R](unemployment_cps_mrp/run_mrp/04b_county_yhat.R): apply models to county-level joint distribution. 
    - [04c_tract_yhat.R](unemployment_cps_mrp/run_mrp/04c_tract_yhat.R): apply models to tract-level joint distribution. 
    - [05_correct_to_laus.R](unemployment_cps_mrp/run_mrp/05_correct_to_laus.R): adjust estimates once new LAUS data is available. 
    - [06_cps_checks.R](unemployment_cps_mrp/run_mrp/06_cps_checks.R): check resulting model output against CPS and LAUS. 
  - [cross_dataset_variable_lineups](unemployment_cps_mrp/cross_dataset_variable_lineups): where we lined up the different coding schemes across data sources. 
  - [downloaded_data](unemployment_cps_mrp/downloaded_data): code to download some of the necessary data. 
  - [helper_functions](unemployment_cps_mrp/helper_functions): functions used throughout the rest of the process. 
  - [adhoc](unemployment_cps_mrp/adhoc): additional pieces, currently holds scripts for prepping additional tract-level data. 

### Contributors

This code was primarily written by [Yair Ghitza](mailto:yair@catalist.us), in collaboration with [Mark Steitz](mailto:msteitz@tsd.biz). Other members of the [Catalist](https://catalist.us/) team contributed ideas and code review. See the [working paper](deep_maps_20200804.pdf) for additional acknowledgements. 

### License

See [LICENSE.md](LICENSE.md). 

### References

[1] Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles and J. Robert Warren. Integrated Public Use Microdata Series, Current Population Survey: Version 7.0 [dataset]. Minneapolis, MN: IPUMS, 2020. https://doi.org/10.18128/D030.V7.0

[2] Steven Ruggles, Sarah Flood, Ronald Goeken, Josiah Grover, Erin Meyer, Jose Pacas and Matthew Sobek. IPUMS USA: Version 10.0 [dataset]. Minneapolis, MN: IPUMS, 2020. https://doi.org/10.18128/D010.V10.0

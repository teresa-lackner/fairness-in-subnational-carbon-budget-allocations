#-----------------------------------------------------------------------------#
# File: subnational_budget_allocation.R                                    ---#      
# Author: Keith Williges                                                   ---#
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# 1. Initial setup, data import etc.                                       ----
#-----------------------------------------------------------------------------#

# 1.1 R setup ----------------------------------------------------------------#
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggmap)
library(lemon)
library(terra)
library(sf)
library(raster)
library(patchwork)
library(eurostat)
library(dplyr)
library(tidyr)
library(httr)
library(readr)
library(scales)
library(stargazer)
library(mice)

# change based on your selected project directory
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# code with calculation fns
source('./code/calc_functions.R')


# 1. Data wrangling ------------------------------------------------------------
# HDI data is in some undefined spatial scale and K stock needs to be          #
# downscaled using nighttime lights data. 1.1 deals with HDI, 1.2 with lights. #
#------------------------------------------------------------------------------#

# 1.1. HDI to NUTS2 level ------------------------------------------------------
# get HDI map 
filename <- "./data/shpfile/gdl_shpfiles/GDL Shapefiles V6.5 large.shp"
shdi <- st_read(filename)

# load EU nuts map
filename <- "./data/shpfile/NUTS2/NUTS_RG_20M_2016_3035.shp"
eu_map <- st_read(filename)

# load the actual subnational HDI data (not in the .shp for some reason)
reg_hdi_dat <- read_csv('./data/raw/GDL-Subnational-HDI-data_2022.csv')

# reproject the shdi map to be the same as the EU nuts map
shdi_reproj <- st_transform(shdi, terra::crs(eu_map))

# clip the shdi map to just the areas of eu nuts regions (space saving)
shdi_n2 <- shdi_reproj[eu_map, ]

# garbage management, remove big old maps
rm(shdi, shdi_reproj)

# add the data on regional HDI to the shapefile
shdi_n2 <- left_join(shdi_n2, reg_hdi_dat)

# spatial join of the reg. hdi and EU nuts2 map
shdi_n2 <- st_join(shdi_n2, eu_map)

# extract just the info we need for further analysis (hdi for 2022)
nuts2_hdi <- as_tibble(shdi_n2) %>% dplyr::select(NUTS_ID, hdi_2022)

# average the values from regions together (since there are overlaps 
# in the polygons from one region to another)
nuts2_hdi <- nuts2_hdi %>% 
  # group by nuts2 code
  group_by(NUTS_ID) %>%
  # find percentage of each country that is in each NUTS2 region
  summarise(hdi = mean(hdi_2022))

rm(shdi_n2, eu_map)

# 1.2 K-stock to NUTS2 level ---------------------------------------------------

# get nighttime lights data
n_lights <- rast('./data/raw/Harmonized_DN_NTL_1995_calDMSP.tif')

# get EU basemap 
filename <- "./data/shpfile/NUTS2/NUTS_RG_20M_2016_3035.shp"
eu_map <- st_read(filename)

# sum light data at NUTS2 level -----------------------------------------------#

# check coordinate systems and reproject
terra::crs(eu_map)
terra::crs(n_lights)

# reproject EU map to be same as lights
eu_map_reproj <- st_transform(eu_map, terra::crs(n_lights))
crs(eu_map_reproj)

# extract the raster values for each NUTS2 region
sum_lights <- terra::extract(n_lights, eu_map_reproj)

sum_lights <-
  sum_lights %>%
  group_by(ID) %>%
  summarize(light_sum = sum(Harmonized_DN_NTL_1995_calDMSP))

# the extract above provides a simple data.frame, with index being the
# row number of the polygon in the vector file. The below code adds the
# summed nightlights data back to the NUTS2 vector file
(
  eu_map_reproj <-
    #--- back to sf ---#
    st_as_sf(eu_map_reproj) %>%
    #--- define ID ---#
    mutate(ID := seq_len(nrow(.))) %>%
    #--- merge by ID ---#
    left_join(., sum_lights, by = "ID")
)

# now get the proportion of totoal country night lights occuring within the
# NUTS2 region
light_frac <- eu_map_reproj %>% 
  # exclude NUTS1 levels 
  subset(LEVL_CODE != 0) %>%
  # group by country
  group_by(CNTR_CODE) %>%
  # find percentage of each country that is in each NUTS2 region
  mutate(prcnt = light_sum / sum(light_sum))

# add the national K-stock estimates to the fractional lights map
k_var <- read_excel('./data/raw/k_stock.xlsx')
k_var$ISO <- NULL
light_frac <- merge(light_frac, k_var)

# calcuate the total k stock for each NUTS2 region
light_frac <- light_frac %>% mutate(k_95 = prcnt * k_95_nuts1, 
                                    k_19 = prcnt * k_19_nuts1)

# extract just the info we need for further analysis
nuts2_k <- as_tibble(light_frac) %>% dplyr::select(NUTS_ID, k_95, k_19)

rm(light_frac, sum_lights, eu_map, eu_map_reproj, n_lights)

# 2. Basic needs emissions calcs------------------------------------------------

# 2.1 Determine threshold emissions  -------------------------------------------
# Linear regression of HDI data vs emissions, to determine appropriate         
# threshold                                                                    

threshold_emi <- function(rDataInput){
  # simple regression emissions ~ hdi and population
  reg3 <- lm((e_21/pop_21) ~ hdi + e_21 + + e_95 +k_19 + k_95 + pop_95, 
             data = rDataInput)
  # summary and plot 
  # summary(reg3)

  
  rDataInput$threshold_epc <- predict(reg3, rDataInput)
  
  # NA's will break calcs in next step, so for now, change them
  # to zero
  rDataInput["hdi"][is.na(rDataInput["hdi"])] <- 0
  rDataInput["threshold_epc"][is.na(rDataInput["threshold_epc"])] <- 0
  return(rDataInput)
}

# 2.2 Determine basic needs emissions ------------------------------------------
poverty_emi <- function(rDataInput){
  # import data on % of people at risk of poverty (from EU), convert percent
  # to normal values, join to rest of data and calcuate # of people at risk
  # for each NUTS2 region
  poverty <- read_excel('./data/raw/poverty_prcnt.xlsx') %>%
    mutate(cap_risk_pov = cap_risk_pov / 100) %>% 
    left_join(rDataInput) %>% mutate(at_risk = cap_risk_pov * pop_21)
  
  # import national estimates of energy needs
  ene_needs <- read_csv('./data/raw/DLE_percapita_ssp2_2050_iso_2021-06-07.csv')
  
  # create new column in poverty data indicating the country and to join w/
  # ene_needs
  poverty <- poverty %>% mutate(country = str_sub(NUTS_ID, 0,2)) %>% 
    left_join(ene_needs)
  
  # assume a fixed emissions intensity for now 
  emi_intens = 120 
  
  # unit conversion factor
  unit_conv_factor <- 1000000000
  
  # calculate emissions needed to meet basic needs for all under poverty 
  # line - the energy needs per person * conversion from GJ to kWhr * 30 years
  # * the number of people at risk * emissions intensity of energy prod / a 
  # unit conversion factor 
  poverty <- poverty %>% mutate(bn_emi = (dle.percapita * 277.778) * 30 * 
                                  at_risk * emi_intens / unit_conv_factor)
  
  return(poverty)
}

# 3 Data import ----
# need 2 datasets - global country level using CBE and EU national 
# level PBE 

# 3.1 CBE data -----

# Import cross-sectional dataset to calculate scenarios - CBE
# since only EPC is being calculated, we only need NUTS2 pop values, K stock
# and hist emi; no poverty/HDI details

nuts2_eu_cbe <- read_csv('./data/raw/NUTS2_pop_imputed_vals_with_notes.csv')

# add nuts2 k stock data to main df
nuts2_eu_cbe <- left_join(nuts2_eu_cbe, nuts2_k) %>% 
  left_join(read_excel('./data/raw/nuts2code_to_eu_countryCode.xlsx')) %>%
  left_join(read_excel('./data/raw/iso_to_eu_countryCode.xlsx'))

nuts2_eu_cbe <- nuts2_eu_cbe %>% rename(pop_21 = pop_16)

# 3.1.1 Calculation of historical regional CBE -------------------------------
#------------------------------------------------------------------------------#

# 3.1.1.1 Define Eurostat dataset codes
codes <- list(
  net_income  = 'nama_10r_2hhinc', # net primary income
  pop         = "tgs00096"     # population
)

# 2. Helper function to download and filter NUTS2 data
get_nuts2 <- function(code) {
  df <- get_eurostat(code, time_format = "num")
  df %>% filter(nchar(geo) == 4)
}

# 3. Download  Eurostat series
net_income_raw  <- get_nuts2(codes$net_income)
pop_raw <- get_eurostat(
  id          = "demo_r_d2jan",
  time_format = "raw"
)

# 2. Tidy & filter (NUTS2 codes have length 4, e.g. "AT11")
pop_nuts2_totals <- pop_raw %>%
  # 1. Turn TIME_PERIOD (char) into numeric year
  mutate(
    year = as.numeric(TIME_PERIOD)
  ) %>%
  # 2. Filter on numeric year (and NUTS2, TOTAL age, total sex)
  filter(
    nchar(geo) == 4,
    year >= 1995,
    age == "TOTAL",
    sex == "T"
  ) %>%
  # 3. Aggregate
  group_by(geo, year) %>%
  summarize(
    population = sum(values, na.rm = TRUE),
    .groups   = "drop"
  )

# Here join the NUTS2_pop_imputed_values to the above dataframe
# as it has 1995 population values. Then impute all missing years in the 
# pop_nuts2_totals df

pop_long_temp <- read_csv('./data/raw/NUTS2_pop_imputed_vals_with_notes.csv') %>%
  pivot_longer(
    cols        = starts_with("pop_"),      # the three pop_* columns
    names_to    = "year",                   # new column that will hold “pop_95”, “pop_16”, “pop_50”
    names_prefix= "pop_",                   # strip off “pop_” so you just get “95”, “16”, “50”
    values_to   = "population"              # all values go into this column
  ) %>%
  mutate(
    year = case_when(                       # convert the codes to full years
      year == "95" ~ 1995L,
      year == "16" ~ 2016L,
      year == "50" ~ 2050L
    )
  ) %>%
  rename(geo = NUTS_ID) %>%                    # rename geographic identifier
  filter(year == 1995)

pop_nuts2_totals <-  bind_rows(pop_nuts2_totals, pop_long_temp) %>% distinct()
rm(pop_long_temp, pop_NUTS2_1995)
# Now we have 1995 values for all countries so later can impute missing vals

income_nuts2 <- net_income_raw %>%
  filter(nchar(geo) == 4,
         na_item == 'B5N',
         unit == "MIO_EUR", 
         TIME_PERIOD >= 2000) %>%
  subset(select = c('geo', 'TIME_PERIOD', 'values')) %>%
  rename(year = TIME_PERIOD) %>%
  # correct the two geo codes for NLD, for some reason they differ from Df to df
  mutate(
    geo = recode(
      geo,
      NL35 = "NL31",
      NL36 = "NL33"
    )
  )

# 1. Read the OWID consumption-based CO₂ emissions dataset directly from OWID
# downloaded October 27, 2025
#cbe_url <- "https://ourworldindata.org/grapher/consumption-co2-emissions.csv"
#cbe <- read_csv(cbe_url)

# save cbe_data 
#saveRDS(cbe, file = "./data/raw/cbe_timeSeries_OWID.rds")

# load CBE data from saved file above
cbe <- readRDS("./data/raw/cbe_timeSeries_OWID.rds")

# 2. Define the EU-27 country list and their ISO/NUTS0 codes
eu27 <- tibble(
  Entity = c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
    "Spain", "Sweden"
  ),
  geo = c(
    "AT", "BE", "BG", "HR", "CY", "CZ",
    "DK", "EE", "FI", "FR", "DE", "EL", "HU",
    "IE", "IT", "LV", "LT", "LU", "MT",
    "NL", "PL", "PT", "RO", "SK", "SI",
    "ES", "SE"
  )
)

# Rename the subscripted column to a friendly name
cbe <- cbe %>%
  rename(
    Consumption_based_CO2 = `Annual consumption-based CO₂ emissions`
  )

# 3. Filter for EU-27 and years from 1995 onward, and join in the geo codes
cbe_eu27_1995 <- cbe %>%
  filter(Entity %in% eu27$Entity, Year >= 1995) %>%
  dplyr::select(Entity, Year, Consumption_based_CO2) %>%
  left_join(eu27, by = "Entity") %>%
  dplyr::select(geo, Year, Consumption_based_CO2) %>%
  rename(year = Year, nuts0 = geo)

# import the data from Ivanova et al 2017
ivanova_path <- './data/raw/ivanova_data.xlsx'
ivanova_data <- read_xlsx(ivanova_path) %>% 
  subset(select = c('rcode', 'rname', 'syear', 'hcf_kg', 'hhsize')) %>%
  rename(geo = rcode, region = rname, year = syear)

# Fix Greek NUTS codes in ivanova_data
ivanova_data <- ivanova_data %>%
  mutate(
    geo = if_else(
      substr(geo, 1, 2) == "GR",
      paste0("EL", substr(geo, 3, 4)),
      geo
    ),
    ind_cf = (hcf_kg/hhsize) * 0.001
  ) %>%
  subset(select = -c(hhsize, hcf_kg))

# Ivanova has carbon budgets for households, but we want per capita 
# so multiply by th HH_size, then convert to tons to make everything
# same units

# investigating missing data / mismatched regions issues ----------
setdiff(pop_nuts2_totals$geo, income_nuts2$geo)
setdiff(income_nuts2$geo, pop_nuts2_totals$geo)
setdiff(pop_nuts2_totals$geo, ivanova_data$geo)
setdiff(ivanova_data$geo, pop_nuts2_totals$geo)

# lots of missing / misatched regions; regress individual CF of overlapping 
# regions with all data (in both Ivanova and EU dataframes. 
# No country-specific FEs etc., so results can be used
# for external predictions)

# 1. Prepare calibration data from Ivanova estimates
cal_df <- ivanova_data %>%
  # Merge regional income & population for the same year
  left_join(income_nuts2 %>% rename(income_mio = values),
            by = c("geo","year")) %>%
  left_join(pop_nuts2_totals,
            by = c("geo","year")) %>%
  # create country codes and calculate per-capita income
  mutate(
    country   = substr(geo,1,2),
    income_pc = income_mio * 1e6 / population
  ) %>%
  # Merge national CBE and compute national per-capita CBE
  left_join(
    cbe_eu27_1995 %>%
      rename(country = nuts0,
             CBE_tons   = Consumption_based_CO2),
    by = c("country","year")
  ) %>%
  group_by(country, year) %>%
  mutate(
    pop_nat = sum(population, na.rm=TRUE),
    Fpc_nat = CBE_tons / pop_nat
  ) %>%
  ungroup() %>%
  # Filter out incomplete rows
  filter(!is.na(income_pc), !is.na(ind_cf), !is.na(Fpc_nat))

# 2. Fit cross-sectional regression
#    Predict regional per-capita footprint (hcf_kg) using income, income^2,
#    and national per-capita CBE
fit <- lm(
  ind_cf ~ income_pc + I(income_pc^2) + Fpc_nat,
  data = cal_df
)

summary(fit)

stargazer(fit)
# R-sq 0.6663, income_pc significant, i^2 not, Fpc somewhat sig

# 3. Prepare full panel for prediction (2000–2022)
# join income and population data
panel_df <- income_nuts2 %>%
  rename(income_mio = values) %>%
  left_join(pop_nuts2_totals, by = c("geo","year"))

#impute missing values of population
md.pattern(panel_df)
# This shows how many rows have each combination of missingness.
# only population is missing vals

# 2. Tell mice not to “impute” variable `geo`:
pred <- make.predictorMatrix(panel_df)
pred[,"geo"] <- 0
pred["geo",] <- 0

# 3. Choose imputation method
meth <- make.method(panel_df)
meth["population"] <- "pmm"

# 4. Run the imputation
#    – m = number of imputed datasets you want (e.g. 5)
#    – maxit = number of iterations per chain
#    – seed for reproducibility
imp <- mice(
  panel_df,
  m              = 5,
  method         = meth,
  predictorMatrix= pred,
  maxit          = 10,
  seed           = 123
)

# 5. Check convergence plots for your target variable
plot(imp, c("population"))

# 6. Extract average across imputations
#    * reshape to long, then group_by & average
avg_imp <- complete(imp, "long") %>%
  group_by(geo, year) %>%
  summarize(
    income_mio  = mean(income_mio),
    population  = mean(population),
    .groups     = "drop"
  )

# create country, income per capita columns, exclude regions not in nuts2_cbe 
# df, add country historical CBE emissions, and calculate total national pop
# and footprint per capita for previous years
panel_df <- avg_imp %>%
  mutate(
    country   = substr(geo,1,2),
    income_pc = income_mio * 1e6 / population
  ) %>%
  semi_join(nuts2_eu_cbe %>% 
              dplyr::select(NUTS_ID) %>% rename(geo = NUTS_ID), by = "geo") %>%
  left_join(
    cbe_eu27_1995 %>%
      rename(country = nuts0,
             CBE_tons   = Consumption_based_CO2),
    by = c("country","year")
  ) %>%
  group_by(country, year) %>%
  mutate(
    pop_nat = sum(population, na.rm=TRUE),
    Fpc_nat = CBE_tons / pop_nat
  ) %>%
  ungroup() 

panel_df %>% filter(is.na(population))

# Predict and scale regional emissions
df_pred <- panel_df %>%
  mutate(
    icf_hat    = predict(fit, newdata = panel_df),
    CO2_raw    = icf_hat * population
  ) %>%
  group_by(country, year) %>%
  mutate(
    scale      = CBE_tons / sum(CO2_raw, na.rm = TRUE),
    CO2_region = CO2_raw * scale
  ) %>%
  ungroup() %>%
  dplyr::select(geo, year, CO2_region, population) %>%
  mutate(co2_per_cap = CO2_region / population)

df_pred <- df_pred %>% dplyr::select(geo, year, CO2_region)

# 3.1.2 Global CBE data -------

# import global data at country level - to determine the initial EU
# allocation
global_cbe <- read_csv('./data/raw/national_CO2_CBE.csv') %>% 
  dplyr::select(-k_95, -k_21, -hdi)

# add nuts2 k stock data to national df and rename k stock vars
global_cbe <- left_join(global_cbe, read_excel('./data/raw/k_stock.xlsx')) %>%
  rename(k_19 = k_19_nuts1, k_95 = k_95_nuts1)

# add HDI data
global_cbe <- left_join(global_cbe, 
                        read_excel('./data/raw/hdi_global_2016.xlsx'))

global_cbe <- threshold_emi(global_cbe)

# Standardize population counts between global and NUTS2 data
# Sum NUTS2 pop data for each country, and assign that to pop values
# for global_cbe data

nuts2_sum_pop <- nuts2_eu_cbe %>% group_by(CNTR_CODE) %>% 
  summarise(pop_21 = sum(pop_21)/1000000000, 
            pop_95 = sum(pop_95, na.rm = TRUE)/1000000000, 
            pop_50 = sum(pop_50)/1000000000)

global_cbe$pop_95[match(nuts2_sum_pop$CNTR_CODE, 
                        global_cbe$CNTR_CODE)] <- nuts2_sum_pop$pop_95
global_cbe$pop_21[match(nuts2_sum_pop$CNTR_CODE, 
                        global_cbe$CNTR_CODE)] <- nuts2_sum_pop$pop_21
global_cbe$pop_50[match(nuts2_sum_pop$CNTR_CODE, 
                        global_cbe$CNTR_CODE)] <- nuts2_sum_pop$pop_50

# 3.2 PBE data ----
# import PBE data (sectors disaggregated, NUTS2 level)
eu_pbe <- read_csv('./data/raw/EUNUTS2_GHG_PBE.csv') %>%
  dplyr::select(-1, -k_95, -k_15, -hdi, -threshold_epc) %>% 
  left_join(read_excel('./data/raw/iso_to_eu_countryCode.xlsx'))

# convert emissions to GT
eu_pbe$e_95 <- eu_pbe$e_95 / 1000000
eu_pbe$e_21 <- eu_pbe$e_21 / 1000000
eu_pbe$e_hist <- eu_pbe$e_hist / 1000000

# add missing population data from imputed dataset
pop95_missing <- nuts2_eu_cbe %>% subset(select = c(NUTS_ID, pop_95, pop_21))

eu_pbe <- eu_pbe %>% subset(select = -c(pop_21)) %>%
  left_join(pop95_missing, by = 'NUTS_ID')  %>% 
  mutate(pop_95 = coalesce(pop_95.x, pop_95.y)) %>% 
  dplyr::select(-pop_95.x, -pop_95.y)

# 3.3 ETS and ES sector data wrangling
# 3.3.1 For ETS sector ---------------------------------------------------------
# Given an EU ETS budget, 2 datasets needed - national-level sectoral disagg
# and NUTS2 

# National df
# Sum NUTS2 sectoral emissions into national level, drop ES sectors
ets_nat_df <- eu_pbe %>% filter(ETS == 1) %>% group_by(ISO, sector) %>% 
  summarise(across(.cols = c(e_95, e_21, e_hist), .fns = sum, na.rm = T), 
            across(.cols = c(pop_21, pop_50, pop_95, capital_lifetime), .fns = 
                     mean, na.rm = T))

# NUTS2 df
# Drop ES sectors, keep everything else
ets_NUTS2_df <- eu_pbe %>% filter(ETS == 1) %>% 
  subset(NUTS_ID != 'ES70' &  NUTS_ID != "PT20" & 
           NUTS_ID != "PT30" & NUTS_ID != "ES70" &
           NUTS_ID != 'FRY1' &  NUTS_ID != "FRY2" & 
           NUTS_ID != "FRY3" & NUTS_ID != "FRY4" & NUTS_ID != 'FRY5')

# 3.3.2 For ES sector ----------------------------------------------------------

# for national level calcs
# Sum ES sector emissions in each NUTS2 region (e_95, e_21, e_hist) to 
# national level, take average of all other columns, drop capital_life, 
es_nat_df <- eu_pbe %>% filter(ETS == 0) %>% group_by(ISO) %>% 
  summarise(across(.cols = c(e_95, e_21, e_hist), .fns = sum, na.rm = T), 
            across(.cols = c(pop_95, pop_21, pop_50), .fns = mean, na.rm = T))

# add nuts2 k stock data to national df and rename k stock columns
es_nat_df <- left_join(es_nat_df, read_excel('./data/raw/k_stock.xlsx')) %>%
  rename(k_19 = k_19_nuts1, k_95 = k_95_nuts1)

# add HDI data
es_nat_df <- left_join(es_nat_df, read_excel('./data/raw/hdi_global_2016.xlsx'))

# find epc_threshold values
es_nat_df <- threshold_emi(es_nat_df)

#NUTS2 df
es_NUTS2_df <- eu_pbe %>% filter(ETS == 0) %>% 
  subset(NUTS_ID != 'ES70' &  NUTS_ID != "PT20" & 
           NUTS_ID != "PT30" & NUTS_ID != "ES70" &
           NUTS_ID != 'FRY1' &  NUTS_ID != "FRY2" & 
           NUTS_ID != "FRY3" & NUTS_ID != "FRY4" & NUTS_ID != 'FRY5')

# 4. Budget calculations -----
# 4.1. Production-based --------------------------------------------------------
# EU ETS budget
ets_bud <- 19750609 / 1000000
# Effort-sharing (non-ETS) budget
es_bud <- 27169497 / 1000000

# 4.1.1. ETS budget allocation -------------------------------------------------

# EU to national level ---------------------------------------------------------
# Empty dataset to hold output
nat_ets_pbe <- ets_nat_df
  
nat_ets_pbe <- nat_ets_pbe %>% 
  mutate(emi_share = e_21 / sum(nat_ets_pbe$e_21)) %>%
  mutate(needed_b = (e_21 * capital_lifetime)/2) 
nat_ets_pbe <- nat_ets_pbe %>%
  mutate(share_of_needed = needed_b / sum(nat_ets_pbe$needed_b)) 
nat_ets_pbe <- nat_ets_pbe %>%
  mutate(sect_b = share_of_needed * ets_bud)

# remove unneeded variables
nat_ets_pbe <- nat_ets_pbe %>% dplyr::select(ISO, sector, sect_b)

# National to NUTS2 ------------------------------------------------------------
nuts2_ets_pbe <- ets_NUTS2_df

nuts2_ets_pbe <- nuts2_ets_pbe %>% 
  mutate(emi_share = e_21 / sum(nuts2_ets_pbe$e_21)) %>%
  mutate(needed_b = (e_21 * capital_lifetime)/2) 

nuts2_ets_pbe <- nuts2_ets_pbe %>%
  mutate(share_of_needed = needed_b / sum(nuts2_ets_pbe$needed_b)) 

nuts2_ets_pbe <- nuts2_ets_pbe %>%
  mutate(sect_b = share_of_needed * ets_bud)

# remove unneeded variables
nuts2_ets_pbe <- nuts2_ets_pbe %>% dplyr::select(NUTS_ID, ISO, sector, sect_b)

write_excel_csv(nuts2_ets_pbe, 
                file = './results/ets_pbe_eu_alloc_new_direct_EU_to_NUTS2.csv')

# 4.1.2. ES budget allocation --------------------------------------------------

# EU to national level ---------------------------------------------------------
pbe_eu_alloc <- epc_nhb(es_nat_df, es_bud) %>% subset(select = c(ISO, epc_nhb))

# To use the ESR-derived budget instead of the calculated one based
# on our method, use the below lines of code!
# ESR Budget -----
pbe_eu_alloc <- read_excel('./data/raw/es_pbe_eu_esr_budget_totals.xlsx')

# National to NUTS2 level ------------------------------------------------------

# Empty dataset to hold output of for loop
nuts2_es_pbe <- es_NUTS2_df 

# indicate which national budgets to use
goal <- 'total_gt'

for (nation in pbe_eu_alloc$ISO){
  # subset for only the country of interest (nation)
  budgets_temp <- pbe_eu_alloc %>% subset(ISO == nation)
  data_temp <- nuts2_es_pbe %>% subset(ISO == nation)
  
  data_temp <- data_temp %>% mutate(emi_share = e_21 / sum(data_temp$e_21)) %>%
    mutate(needed_b = (e_21 * capital_lifetime)/2) 
  data_temp <- data_temp %>%
    mutate(share_of_needed = needed_b / sum(data_temp$needed_b)) 
  data_temp <- data_temp %>%
    mutate(sect_b = share_of_needed * budgets_temp[[goal]])
  
  # remove unneeded variables
  data_temp <- data_temp %>% dplyr::select(NUTS_ID, sector, sect_b)
  
  # join to the full dataset
  nuts2_es_pbe <- nuts2_es_pbe %>% 
    left_join(data_temp, by = c('NUTS_ID', 'sector')) 
  
  # since after the first join, R wants to create multiple columns, this
  # checks if that situation occurs, and combines the duplicated 
  # erroneous columns into one
  if ('sect_b.y' %in% names(nuts2_es_pbe)){
    nuts2_es_pbe <- nuts2_es_pbe %>% 
      mutate(sect_b = coalesce(sect_b.y, sect_b.x)) %>%
      dplyr::select(-sect_b.y, -sect_b.x)
  }
}

write_excel_csv(nuts2_es_pbe, 
                file = './results/es_pbe_eu_alloc_nuts2_scale_ESR.csv')

# 4.2. Consumption-based -------------------------------------------------------
# Global to country level -> EPC-NHBC or EPC/PCC NHB, then country
# to NUTS2 -> EPC-HBC

# 3.2.1 Global to country level
n_cbe_alloc <- epc_nhbc(global_cbe)
n_cbe_alloc$epc_nhb <- epc_nhb(global_cbe)$epc_nhb
n_cbe_alloc$pcc_nhb  <- pcc_nhb(global_cbe)$pcc_nhb
n_cbe_alloc$epc_s <- epc_s(global_cbe)$epc_s

# 3.2.2. National to NUTS2 level - in progress
# Given the above values for countries, distribute them to NUTS2 level using
# EPC-BC approach (make a new function)

# Clean up the national results from above
n_cbe_alloc <- n_cbe_alloc %>% dplyr::select(ISO, epc_nhbc, epc_nhb, pcc_nhb,
                                             epc_s)

avg_pop <- global_cbe %>% 
  subset(select = c(ISO, pop_21, pop_50)) %>%
  mutate(avg_pop = (pop_21 + pop_50)/2)


n_cbe_alloc <- n_cbe_alloc %>% left_join(avg_pop)

# select only EU countries for NUTS2 calc
eu_cbe_alloc <- n_cbe_alloc %>% subset(ISO %in% unique(nuts2_eu_cbe$ISO))

# add historic population from (panel_df) and regional emissions (from 
# df_pred) to the nuts2_eu_cbe dataframe
nuts2_emi_hist <- df_pred %>%
  filter(year >= 2000, year <= 2022) %>%
  group_by(geo) %>%
  summarise(emi_hist = sum(CO2_region, na.rm = TRUE), .groups = "drop")

# 2. Extract population in 2000 by region
nuts2_pop_2000 <- panel_df %>%
  filter(year == 2000) %>%
  dplyr::select(geo, pop_2000 = population)

# 3. Join both onto nuts2_eu_cbe 
nuts2_eu_cbe_final <- nuts2_eu_cbe %>%
  # join the emissions
  left_join(nuts2_emi_hist, by = c("NUTS_ID" = "geo")) %>%
  # join the 2000 population
  left_join(nuts2_pop_2000, by = c("NUTS_ID" = "geo"))



# call the NUTS2 eval function (H qual)
nuts2_cbe_alloc <- nuts2_epc_h(eu_cbe_alloc, nuts2_eu_cbe_final, goal = "epc_nhbc")
nuts2_cbe_alloc$eu_nhb <- nuts2_epc_h(eu_cbe_alloc, 
                                      nuts2_eu_cbe_final, goal = "epc_nhb")$epc_h
nuts2_cbe_alloc$eu_pcc_nhb <- nuts2_epc_h(eu_cbe_alloc, 
                                          nuts2_eu_cbe_final, goal = "pcc_nhb")$epc_h
nuts2_cbe_alloc$eu_epc_s <- nuts2_epc_h(eu_cbe_alloc, 
                                        nuts2_eu_cbe_final, goal = "epc_s")$epc_h
nuts2_cbe_alloc <- nuts2_cbe_alloc %>% rename(eu_nhbc = epc_h)

# write results of CBE allocation
write_excel_csv(nuts2_cbe_alloc, 
                file = './results/cbe_eu_alloc_nuts2_scale_hqual.csv')
write_excel_csv(n_cbe_alloc, 
                file = './results/cbe_global_alloc_nation_scale.csv')

# 5. Results / assessment -----------------------------------------------------
# Since emissions data is still in gigatons, here specify the 
# multiplier to convert values in below maps to emissions (tons) per capita
GtToTonConv <- 1000000000

# CBE NUTS2 map ----
# create dataset of just results to map

#nuts2_cbe_alloc <- subset(nuts2_cbe_alloc, !grepl("^PT", NUTS_ID))
mapResults <- subset(nuts2_cbe_alloc, select = c(NUTS_ID, eu_nhbc, eu_nhb, 
                                                 eu_pcc_nhb, eu_epc_s))

pop21_missing <- nuts2_cbe_alloc %>% 
  subset(select = c(NUTS_ID, pop_21, pop_50)) %>%
  mutate(pop_avg = ((pop_21 + pop_50) / 2)) 
  

mapResults <- mapResults %>% left_join(pop21_missing) %>%
  mutate(eu_nhbc = (eu_nhbc / pop_avg) * GtToTonConv) %>%
  mutate(eu_nhb = (eu_nhb / pop_avg) * GtToTonConv) %>%
  mutate(eu_pcc_nhb = (eu_pcc_nhb / pop_avg) * GtToTonConv) %>%
  mutate(eu_epc_s = (eu_epc_s / pop_avg) * GtToTonConv)

mapResults <- mapResults %>% subset(select = c(-pop_21, -pop_50))

# set NUTS region as factor, and convert dataset to long format
mapResults$NUTS_ID <- as.factor(mapResults$NUTS_ID)
mapResults.m <- pivot_longer(mapResults, cols = -NUTS_ID)

# load the map shapefile
filename <- "./data/shpfile/NUTS2/NUTS_RG_20M_2016_3035.shp"
nuts2_map <- st_read(filename)

# check the coordinate system
crs(nuts2_map)

# combine the results with the blank map
results <- left_join(nuts2_map, mapResults.m)

# remove the weird territories that make the map awkward to display
results <- subset(results, NUTS_ID != 'ES70' &  NUTS_ID != "PT20" & 
                    NUTS_ID != "PT30" & NUTS_ID != "ES70" &
                    NUTS_ID != 'FRY1' &  NUTS_ID != "FRY2" & 
                    NUTS_ID != "FRY3" & NUTS_ID != "FRY4" & NUTS_ID != 'FRY5')

results$value[ grepl("PT|NL", results$NUTS_ID) ] <- NA

ggmap_epc_s <- ggplot(data = (filter(results, name == 'eu_epc_s')), 
                      aes(fill = value)) +
  geom_sf() + scale_fill_viridis_c() +
  labs(fill = "tons CO2 \nper capita") 


ggsave('./plots/nuts2_cbe_alloc_epc_s_noTitle_new.pdf', ggmap_epc_s,
       width = 90, height = 105, units = "mm")  

ggmap_epc_s <- ggplot(data = (filter(results, name == 'eu_epc_s')), 
                      aes(fill = value)) +
  geom_sf() + scale_fill_viridis_c() +
  labs(fill = "tons CO2 \nper capita") +
  theme(legend.position = 'NA')

ggsave('./plots/nuts2_cbe_alloc_epc_s_noLegend_new.pdf', ggmap_epc_s,
       width = 90, height = 105, units = "mm") 

# PBE - ES NUTS2 map ----

# create dataset of just results to map
es_by_nuts2 <- nuts2_es_pbe %>% group_by(NUTS_ID) %>% 
  summarise(es_b = sum(sect_b))

mapResults <- subset(es_by_nuts2, select = c(NUTS_ID, es_b))

pop21_missing <- nuts2_eu_cbe_final %>% subset(select = c(NUTS_ID, pop_21))

mapResults <- mapResults %>% left_join(pop21_missing) %>%
  mutate(es_b = (es_b / pop_21) * GtToTonConv) 

mapResults <- subset(mapResults, select = c(-pop_21))

# set NUTS region as factor, and convert dataset to long format
mapResults$NUTS_ID <- as.factor(mapResults$NUTS_ID)
mapResults.m <- pivot_longer(mapResults, cols = -NUTS_ID)

# load the map shapefile
filename <- "./data/shpfile/NUTS2/NUTS_RG_20M_2016_3035.shp"
nuts2_map <- st_read(filename)

# check the coordinate system
crs(nuts2_map)

# combine the results with the blank map
results <- merge(nuts2_map, mapResults.m)

# remove the weird territories that make the map awkward to display
results <- subset(results, NUTS_ID != 'ES70' &  NUTS_ID != "PT20" & 
                    NUTS_ID != "PT30" & NUTS_ID != "ES70" &
                    NUTS_ID != 'FRY1' &  NUTS_ID != "FRY2" & 
                    NUTS_ID != "FRY3" & NUTS_ID != "FRY4" & NUTS_ID != 'FRY5')

# initial plot
ggmap_test <- ggplot(data = results, aes(fill = value)) +
  geom_sf() +
  scale_fill_viridis_c() + 
  theme(legend.position = 'NA')
  labs(fill = "tons CO2e \nper capita")# +
  ggtitle("Effort sharing sector budgets \n(NUTS2-PBE; EU to National level via ESR estimate)")


ggsave('./plots/nuts2_es_pbe_alloc_noLegend.pdf', ggmap_test, 
       width = 90, height = 105, units = "mm")

# PBE - ETS NUTS2 map ----

# create dataset of just results to map
ets_by_nuts2 <- nuts2_ets_pbe %>% group_by(NUTS_ID) %>% 
  summarise(ets_b = sum(sect_b))

mapResults <- subset(ets_by_nuts2, select = c(NUTS_ID, ets_b))

pop21_missing <- nuts2_eu_cbe_final %>% subset(select = c(NUTS_ID, pop_21))

mapResults <- mapResults %>% left_join(pop21_missing) %>%
  mutate(ets_b = (ets_b / pop_21) * GtToTonConv) 

mapResults <- subset(mapResults, select = c(-pop_21))

# set NUTS region as factor, and convert dataset to long format
mapResults$NUTS_ID <- as.factor(mapResults$NUTS_ID)
mapResults.m <- pivot_longer(mapResults, cols = -NUTS_ID)

# load the map shapefile
filename <- "./data/shpfile/NUTS2/NUTS_RG_20M_2016_3035.shp"
nuts2_map <- st_read(filename)

# check the coordinate system
crs(nuts2_map)

# combine the results with the blank map
results <- merge(nuts2_map, mapResults.m)

# remove the weird territories that make the map awkward to display
results <- subset(results, NUTS_ID != 'ES70' &  NUTS_ID != "PT20" & 
                    NUTS_ID != "PT30" & NUTS_ID != "ES70" &
                    NUTS_ID != 'FRY1' &  NUTS_ID != "FRY2" & 
                    NUTS_ID != "FRY3" & NUTS_ID != "FRY4" & NUTS_ID != 'FRY5')

# initial plot
ggmap_test <- ggplot(data = results, aes(fill = value)) +
  geom_sf() +
  scale_fill_viridis_c() + 
  labs(fill = "tons CO2e \nper capita") +
  theme(legend.position = 'NA')# +
  ggtitle("ETS sector budgets (NUTS2-PBE) - EU budget direct to NUTS2") #+

ggsave('./plots/nuts2_ets_pbe_alloc_new_approach_direct_to_nuts2_noLegend.pdf', 
       ggmap_test, width = 90, height = 105, units = "mm")
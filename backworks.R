

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# World Population Prospects 2024 -----------------------------------------
new_names <- c("index", "variant", "region_name", "notes", "location_code", 
               "ISO3", "ISO2", "SDMX", "type", "parent_code", "year", "pop_jan_total", 
               "pop_jul_total", "pop_jul_male", "pop_jul_female", "pop_den", "sex_ratio", 
               "median_age", "natural_change", "RNC", "pop_change", "PGR", 
               "doubling_time", "births", "births_by_f1519", "CBR", "TFR", "NRR", 
               "mean_age_childbearing", "sex_ratio_birth", "deaths_total", 
               "deaths_male", "deaths_female", "CDR", "life_exp_total", 
               "life_exp_male", "life_exp_female", "life_exp_15_total", 
               "life_exp_15_male", "life_exp_15_female", "life_exp_65_total", 
               "life_exp_65_male", "life_exp_65_female", "life_exp_80_total", 
               "life_exp_80_male", "life_exp_80_female", "infant_deaths", 
               "IMR", "live_births", "under_five_deaths", "mort_under_five", 
               "mort_bf_40_total", "mort_bf_40_male", "mort_bf_40_female", "mort_bf_60_total", 
               "mort_bf_60_male", "mort_bf_60_female", "mort_bt_1550_total", 
               "mort_bt_1550_male", "mort_bt_1550_female", "mort_bt_1560_total", 
               "mort_bt_1560_male", "mort_bt_1560_female", "net_migrants", "NMR")
wpp_2024_estimates <- read_excel(
  "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
  sheet = "Estimates",
  skip = 17, 
  col_names = new_names,
  col_types = c(rep("guess", 3), "text", "guess", rep("text", 2), rep("guess", 58)),
  na = c("...", "")
)
wpp_2024_future <- read_excel(
  "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
  sheet = "Medium variant",
  skip = 17, 
  col_names = new_names,
  col_types = c(rep("guess", 3), "text", "guess", rep("text", 2), rep("guess", 58)),
  na = c("...", "")
)
wpp_2024 <- bind_rows(wpp_2024_estimates, wpp_2024_future)

wpp_2024 <- wpp_2024 |> 
  mutate(
    across(
      c(pop_jan_total, pop_jul_total, pop_jul_male, pop_jul_female, 
        natural_change, pop_change, births, deaths_total, 
        deaths_male, deaths_female, net_migrants), \(x) x * 1000
    )
  )

write_rds(wpp_2024, "wpp_2024.rds")


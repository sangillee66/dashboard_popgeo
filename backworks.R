

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(rnaturalearth)
library(sf)
library(tmap)

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

wpp_2024_new <- wpp_2024 |> 
  filter(
    type != "Label/Separator"
  ) |> 
  mutate(
    across(
      c(pop_jan_total, pop_jul_total, pop_jul_male, pop_jul_female, 
        natural_change, pop_change, births, deaths_total, 
        deaths_male, deaths_female, net_migrants), \(x) x * 1000
    )
  )

write_rds(wpp_2024_new, "wpp_2024.rds")


# 전세계 국가 참조 지도 ------------------------------------------------------------

countries.m <- st_read("D:/My R/World Data Manupulation/NaturalEarth/new_2_ne_50m_admin_0_countries.shp") 
# tiny.m <- ne_download(scale = "medium", type = "tiny_countries", category = "cultural", returnclass = "sf")

world.region.code <- read_excel("D:/My R/Population Geography/0 Population Data/World_Region_Code.xlsx", sheet = 1, col_names = TRUE)

countries.m.region <- countries.m |> 
  left_join(
    world.region.code, join_by(ISO_N3_CD == `M49 Code`)
  ) |> 
  mutate(
    New_New_Sub_SDG_NM = case_match(
      New_New_Sub_SDG_NM,
      "CENTRAL AND SOUTHERN ASIA" ~ "Central and Southern Asia",
      "EUROPE, NORTHERN AMERICA, AUSTRALIA, AND NEW ZEALAND" ~ "Europe, Northern America, Australia, and New Zealand",
      "ANTARCTICA" ~ "Antarctica",
      "NORTHERN AFRICA AND WESTERN ASIA" ~ "Northern Afraica and Western Asia",
      "OCEANIA (EXC AUSTRALIA AND NEW ZEALAND)" ~ "Oceania (Excluding Australia and New Zealand)",
      "SUB-SAHARAN AFRICA" ~ "Sub-Saharan Africa",
      "LATIN AMERICA AND THE CARIBBEAN" ~ "Latin America and THE Caribbean",
      "EASTERN AND SOUTH-EASTERN ASIA" ~ "Eastern and South-Eastern Asia" 
    )
  )

tmap_mode("view")
tm_shape(countries.m.region) + tm_polygons( 
  alpha = 0, border.alpha = 0, id = "NAME_KO", 
  popup.vars = c(
    "ISO3" = "ISO_A3", 
    "NAME" = "NAME_LONG",
    "UN Subregion" = "New_UN_All_Subregion_NM",
    "UN Region" = "New_UN_Region_NM",
    "SDG Region" = "New_New_Sub_SDG_NM"
    )
  ) 
  

# 우리나라 시군구 단위 -------------------------------------------------------------

SIGUNGU1.shp <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2023_2Q/NOT_MOVE/SIGUNGU1_NM_2023_2Q_GEN_0030.shp", options = "ENCODING=CP949")
SIDO_Polyline.shp <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2023_2Q/NOT_MOVE/SIDO_Polyline_NM_2023_2Q_GEN_0030.shp", options = "ENCODING=CP949")

SIGUNGU1.shp <- SIGUNGU1.shp |> 
  mutate(
    SGG1_CD = as.character(SGG1_CD)
  )

tmap_mode("view")
tm_shape(SIGUNGU1.shp) + tm_fill(
  col = "gray90", alpha = 0.2, id = "SGG1_FNM", 
  popup.vars = c(
    "영어 이름" = "Eng_NM",
    "코드번호" = "SGG1_CD" 
  ) 
  ) + tm_borders() +
  tm_shape(SIDO_Polyline.shp) + tm_lines(col = "black", lwd = 2)



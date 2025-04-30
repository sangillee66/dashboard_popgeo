

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


# 우리나라 인구 피라미드 애니메이션 ------------------------------------------------------

# 데이터
my.age.sex.data.one.80plus <- read_excel("D:/My R/Korea Census Data Manipulation/4 Population Projections/2022 기준/Nation_Age_Sex_Proj_One_80Plus_1960_2072.xlsx", sheet = 1)
my.age.sex.data.one.100plus <- read_excel("D:/My R/Korea Census Data Manipulation/4 Population Projections/2022 기준/Nation_Age_Sex_Proj_One_100Plus_2000_2072.xlsx", sheet = 1)

age_sex_data <- my.age.sex.data.one.100plus |> 
  pivot_longer(
    cols = `2000`:`2072`,
    names_to = "Year",
    values_to = "Pop"
  ) |> 
  mutate(
    Year = as.integer(Year),
    Ages = as.character(parse_number(Ages)),
    Gender = case_match(
      Gender,
      "전체"~"Total",
      "남자"~"Male",
      "여자"~"Female"
    ),
    Ages = ifelse(Ages == 100, "100+", Ages)
  ) |> 
  filter(
    Gender != "Total"
  )

# 기본 인구 피라미드

data_sel <- age_sex_data |> 
  filter(
    Year == 2025, 
    Gender != "Total"
  ) |> 
  mutate(
    Pop = Pop * 100/ sum(Pop),
    .by = Year
  )
  
ggplot(data = data_sel, aes(x = fct(Ages, levels = unique(Ages)), y = ifelse(Gender == "Male", -Pop, Pop), fill = fct(Gender, levels = c("Male", "Female")))) + 
  geom_bar(stat = "identity", alpha = 1) + 
  scale_x_discrete(breaks = unique(data_sel$Ages)[seq(1, 101, 5)], labels = unique(data_sel$Ages)[seq(1, 101, 5)]) +
  scale_y_continuous(labels = abs) +
  coord_flip() +
  scale_fill_manual(values = c("#80b1d3", "#fb8072")) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(x = "Ages", y = "Population (%)")

# gganimate 패키지를 활용하는 방법

data_sel <- age_sex_data |> 
  filter(
    Gender != "Total"
  ) |> 
  mutate(
    Pop = Pop * 100/ sum(Pop),
    .by = Year
  )

library(gganimate)

ggplot(data = data_sel, aes(x = fct(Ages, levels = unique(Ages)), y = ifelse(Gender == "Male", -Pop, Pop), fill = fct(Gender, levels = c("Male", "Female")))) + 
  geom_bar(stat = "identity", alpha = 1) + 
  scale_x_discrete(breaks = unique(data_sel$Ages)[seq(1, 101, 5)], labels = unique(data_sel$Ages)[seq(1, 101, 5)]) +
  scale_y_continuous(labels = abs) +
  coord_flip() +
  scale_fill_manual(values = c("#80b1d3", "#fb8072")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, hjust = 0.5),
    axis.title = element_text(size = 20, face = "plain"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "none",
    aspect.ratio = 1
  ) +
  labs(title = "Year: {frame_time}", x = "Ages", y = "Population (%)") + 
  transition_time(Year) +
  enter_fade() -> P

pop_pyramid_kr <- animate(P, rendere = gifski_renderer(loop=TRUE), width = 1200, height = 1200, dpi = 600)
anim_save("pop_pyramid_kr.gif", animation = pop_pyramid_kr)  

# plotly를 활용하는 방법

library(plotly)

data_sel <- age_sex_data |> 
  filter(
    # Year == 2025,
    Gender != "Total"
  ) |> 
  mutate(
    Pop = Pop * 100/ sum(Pop),
    .by = Year
  )

data_sel |> 
  mutate(
    Ages = fct(Ages, levels = unique(Ages)), 
    Gender = fct(Gender, levels = c("Male", "Female")),
    Pop = if_else(Gender == "Male", -Pop, Pop),
  ) |> 
  plot_ly(
    x = ~Pop, 
    y = ~Ages, 
    color = ~Gender, 
    frame = ~Year
  ) |> 
  add_bars(orientation = 'h', hoverinfo = 'text') |> 
  layout(
    bargap = 0.1,
    barmode = "relative",
    xaxis = list(
      tickvals = c(-1, -0.5, 0, 0.5, 1), 
      ticktext = as.character(abs(c(-1, 0.5, 0, 0.5, 1)))
    ),
    legend = list(
      x = 0.85, y = 0.95
    )
  ) |> 
  animation_slider(currentvalue = list(prefix = "Year: "))
  

# 전처리: 남성은 음수로
data_proc <- data_sel |> 
  mutate(
    Gender = factor(Gender, levels = c("Male", "Female")),
    Ages = fct(Ages, levels = unique(Ages)),
    Pop = if_else(Gender == "Male", -Pop, Pop)
  )

# 남성과 여성 데이터 분리
male_data <- filter(data_proc, Gender == "Male")
female_data <- filter(data_proc, Gender == "Female")

# plot 생성
plot_ly() %>%
  add_trace(
    data = male_data,
    x = ~Pop, y = ~Ages,
    type = "bar",
    name = "Male",
    orientation = "h",
    marker = list(color = "#8da0cb"),
    frame = ~Year,
    text = ~paste("Year:", Year, "<br>Age:", Ages, "<br>Male:", formatC(abs(Pop), format = "f", digits = 3, big.mark = ",")),
    hoverinfo = "text",
    textposition = "none" 
  ) |> 
  add_trace(
    data = female_data,
    x = ~Pop, y = ~Ages,
    type = "bar",
    name = "Female",
    orientation = "h",
    marker = list(color = "#e78ac3"),
    frame = ~Year,
    text = ~paste("Year:", Year, "<br>Age:", Ages, "<br>Female:", formatC(abs(Pop), format = "f", digits = 3, big.mark = ",")),
    hoverinfo = "text", 
    textposition = "none" 
  ) %>%
  layout(
    barmode = "relative",
    bargap = 0.1,
    hovermode = "y unified",
    legend = list(x = 0.85, y = 0.95),
    xaxis = list(
      title = "Population",
      tickvals = seq(-1, 1, by = 0.5),
      ticktext = as.character(abs(seq(-1, 1, by = 0.5)))
    ),
    yaxis = list(title = "Age Group")
  )


# tooltip 문자열 구성
data_wide <- data_sel %>%
  mutate(
    Gender = factor(Gender, levels = c("Male", "Female")),
    Pop = if_else(Gender == "Male", -Pop, Pop)
  ) %>%
  pivot_wider(names_from = Gender, values_from = Pop) %>%
  mutate(
    Ages = fct(Ages, levels = unique(Ages)),
    tooltip = paste0(
      "<span style='font-family:monospace'>",
      "Year:      ", Year, "<br>",
      "Age Group: ", Ages, "<br>",
      "Male:    ", formatC(abs(Male), format = "f", digits = 3, big.mark = ","), "<br>",
      "Female:  ", formatC(Female, format = "f", digits = 3, big.mark = ","),
      "</span>"
    )
  )

# 플롯 생성
plot_ly() %>%
  # 공통 툴팁 trace (보이지 않음)
  add_trace(
    data = data_wide,
    x = ~0, y = ~Ages,
    type = "bar",
    name = "tooltip",
    orientation = "h",
    marker = list(color = 'rgba(0,0,0,0)'),
    showlegend = FALSE,
    text = ~tooltip,
    hoverinfo = "text",
    frame = ~Year,
    textposition = "none"
  ) %>%
  # 남성 bar
  add_trace(
    data = data_wide,
    x = ~Male, y = ~Ages,
    type = "bar",
    name = "Male",
    orientation = "h",
    marker = list(color = "#8da0cb"),
    hoverinfo = "skip",
    frame = ~Year,
    textposition = "none"
  ) %>%
  # 여성 bar
  add_trace(
    data = data_wide,
    x = ~Female, y = ~Ages,
    type = "bar",
    name = "Female",
    orientation = "h",
    marker = list(color = "#e78ac3"),
    hoverinfo = "skip",
    frame = ~Year,
    textposition = "none"
  ) %>%
  layout(
    barmode = "relative",
    bargap = 0.1,
    hovermode = "y unified",
    xaxis = list(
      title = "Population",
      tickvals = seq(-1, 1, by = 0.5),
      ticktext = as.character(abs(seq(-1, 1, by = 0.5)))
    ),
    yaxis = list(title = "Age Group"),
    legend = list(x = 0.85, y = 0.95)
  ) %>%
  animation_opts(
    frame = 1000, transition = 500, redraw = FALSE
  ) %>%
  animation_slider(currentvalue = list(prefix = "Year: "))

# 혜민이 도움으로 완성

# plotly를 활용하는 방법

library(plotly)

data_sel <- age_sex_data |> 
  filter(
    Gender != "Total"
  ) |> 
  mutate(
    Pop_r = Pop * 100/ sum(Pop),
    .by = Year
  ) |> 
  mutate(
    Ages = if_else(Ages == "100+", "100", Ages)
  )

data_sel |> 
  mutate(
    Ages = fct(Ages, levels = unique(Ages)), 
    Gender = fct(Gender, levels = c("Male", "Female")),
    Pop_r = if_else(Gender == "Male", -Pop_r, Pop_r),
  ) |> 
  plot_ly(
    x = ~Pop_r, 
    y = ~Ages, 
    color = ~Gender, 
    frame = ~Year
  ) |> 
  add_bars(
    color = ~Gender, colors = c("#8da0cb", "#e78ac3"),
    orientation = 'h', 
    customdata = abs(data_sel$Pop),
    hovertemplate = paste(
      "%{customdata:,}"
    ) 
  ) |> 
  layout(
    bargap = 0.1,
    barmode = "relative",
    xaxis = list(
      title = "Population (%)",
      tickvals = c(-1, -0.5, 0, 0.5, 1),
      ticktext = as.character(abs(c(-1, 0.5, 0, 0.5, 1)))
    ),
    yaxis = list(
      title = "Ages",
      tickvals = unique(data_sel$Ages)[seq(1, 101, 5)],
      ticktext = c(unique(data_sel$Ages)[seq(1, 100, 5)], "100+")
    ),
    legend = list(
      x = 0.85, y = 0.95,
      traceorder = "reversed"
    ),
    margin = list(l = 75, r = 75, t = 75, b = 75),
    hovermode = "y unified",
    hoverlabel = list(bgcolor = "white", font = list(size = 12, family = "Pretendard Medium")),
    bargap = 0.1, barmode = "overlay"
  ) |> 
  config(
    displaylogo = FALSE,  # plotly 로고 제거
    modeBarButtonsToRemove = c(
      "zoom2d", "pan2d", "select2d", "lasso2d",
      "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines", "resetViews", "sendDataToCloud"
    )
  ) |> 
  animation_slider(currentvalue = list(prefix = "Year: "))


# Flowmapblue 사용 ----------------------------------------------------------

library(tidyverse)
library(sf)
library(flowmapblue)
library(htmlwidgets)
library(readxl)

OD.mig.SDGG.ori <- read_excel(
  "D:/My R/Korea Census Data Manipulation/9 Migration/DM_Comp_Code_Mig_2024_Sidogungu.xlsx"
  )
OD.mig.SDGG.ori <- OD.mig.SDGG.ori |> 
  filter(
    Ori_CD != 0 & Des_CD != 0
  ) |> 
  select(
    2:6
  ) |> 
  mutate(
    Ori_NM = if_else(str_detect(Ori_NM, "전특"), str_replace(Ori_NM, "전특", "전북"), Ori_NM),
    Des_NM = if_else(str_detect(Des_NM, "전특"), str_replace(Des_NM, "전특", "전북"), Des_NM)
  )

df_SD <- OD.mig.SDGG.ori |> 
  filter(
    Ori_CD < 100 & Des_CD < 100
  ) 
df_SGG1 <- OD.mig.SDGG.ori |> 
  filter(
    Ori_CD > 100 & Des_CD > 100
  ) 
df_SG1 <- OD.mig.SDGG.ori |> 
  filter(
    (Ori_CD > 30000 | Ori_CD < 30) & (Des_CD > 30000 | Des_CD < 30)
  ) 

df_SGG1_Seoul <- df_SGG1 |> 
  filter(
    Ori_CD < 20000 & Des_CD < 20000
  ) |> 
  mutate(
    Ori_NM = str_remove(Ori_NM, "서울 "), 
    Des_NM = str_remove(Des_NM, "서울 ")
  )

seoul <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2023_2Q/SEOUL_SIDO_2023_2Q_GEN_0030.shp", options = "ENCODING=CP949")
seoul_gu <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2023_2Q/SEOUL_GU_2023_2Q_GEN_0030.shp", options = "ENCODING=CP949")
SG1 <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2023_2Q/NOT_MOVE/SIGUN1_NM_2023_2Q_GEN_0030.shp", options = "ENCODING=CP949")
SGG1 <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2023_2Q/NOT_MOVE/SIGUNGU1_NM_2023_2Q_GEN_0030.shp", options = "ENCODING=CP949")

Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoic2xlZTExMDEiLCJhIjoiY20zaWJ2bXp5MGx1ZTJscXg5NWphcG4zaiJ9.QdFkfvzX1YN5baf7oFrxFg")

# 서울시 구간 인구이동

coords <- seoul_gu |> 
  st_transform(crs = 4326) |> 
  st_centroid() |> 
  st_coordinates()
locations <- seoul_gu |> 
  mutate(
    lon = coords[, 1],
    lat = coords[, 2],
    id = SGG1_CD, 
    name = SGG1_NM
  ) |> 
  st_drop_geometry() |> 
  select(
    id, name, lon, lat
  ) 

flows <- df_SGG1_Seoul |> 
  select(
    origin = Ori_CD,
    dest = Des_CD,
    count = Total_T
  )

flowmapblue_seoul_gu <- flowmapblue(
  locations = locations,
  flows = flows,
  mapboxAccessToken = Sys.getenv('MAPBOX_API_TOKEN'),
  clustering = TRUE,
  darkMode = TRUE,
  animation = FALSE
)

flowmapblue_seoul_gu

saveWidget(flowmapblue_seoul_gu, "flowmapblue_seoul_gu.html", selfcontained = TRUE)

# 우리나라 시군간 인구이동

coords <- SG1 |> 
  st_transform(crs = 4326) |> 
  st_centroid() |> 
  st_coordinates()
locations <- SG1 |> 
  mutate(
    lon = coords[, 1],
    lat = coords[, 2],
    id = SG1_CD, 
    name = SG1_NM
  ) |> 
  st_drop_geometry() |> 
  select(
    id, name, lon, lat
  ) 

flows <- df_SG1 |> 
  select(
    origin = Ori_CD,
    dest = Des_CD,
    count = Total_T
  )

flowmapblue_SG1 <- flowmapblue(
  locations = locations,
  flows = flows,
  mapboxAccessToken = Sys.getenv('MAPBOX_API_TOKEN'),
  clustering = TRUE,
  darkMode = TRUE,
  animation = FALSE
)

flowmapblue_SG1

saveWidget(flowmapblue_SG1, "flowmapblue_SG1.html", selfcontained = TRUE)

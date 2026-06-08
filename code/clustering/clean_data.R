library(dplyr)
library(tidyr)
library(slider)
library(dtwclust)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(here)
library(MMWRweek)
library(sf)
library(tidycensus)

# install.packages("here")



hsa1 <- read_xls("/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting/data/Health.Service.Areas.xls")
hsa <- hsa1 %>%
  dplyr::select(-`Health Service Area (NCI Modified) Description`)
hsa_clean <- hsa %>%
  rename(
    hsa_nci_id = `HSA # (NCI Modified)`,
    fips = FIPS
  ) %>%
  mutate(
    state  = str_extract(`State-county`, "^[A-Z]{2}"),
    county = str_extract(`State-county`, "(?<=: ).*(?= \\()"),
    county = str_remove(county, " County$")  
  ) %>%
  dplyr::select(hsa_nci_id, state, county, fips)

head(hsa_clean)
hsa_tx <- hsa_clean%>%
  filter(state == "TX")
root <- "/Users/dk29776/Dropbox/UTAustin/Spatial_clustering"  


df_all <- read.csv(file.path(root, "data/202201-202607_weekly_all.csv"))
df_flu <- read.csv(file.path(root, "data/202201-202607_weekly_flu.csv"))

df_all <- df_all %>% dplyr::select(-TX_UNKNOWN)
df_flu <- df_flu %>% dplyr::select(-TX_UNKNOWN)


week_to_date_mmwr <- function(week_str) {
  yr <- as.integer(substr(week_str, 1, 4))
  wk <- as.integer(substr(week_str, 6, 7))
  
  # MMWRweek2Date는 해당 주차의 '일요일' 날짜를 반환합니다.
  MMWRweek2Date(MMWRyear = yr, MMWRweek = wk)
}

df_all_long <- df_all %>%
  pivot_longer(
    cols = starts_with("TX_"),
    names_to = "county",
    values_to = "value"
  ) 

df_flu_long <- df_flu %>%
  pivot_longer(
    cols = starts_with("TX_"),
    names_to = "county",
    values_to = "value"
  ) 

df_all_long$Date <- week_to_date_mmwr(df_all_long$week)
df_flu_long$Date <- week_to_date_mmwr(df_flu_long$week)
df_all_long <- df_all_long %>%
  mutate(
    county = str_remove(county, "^TX_"),  # TX_ 제거
    county = str_replace_all(county, "\\.", " ")  # . → space
  )

df_flu_long <- df_flu_long %>%
  mutate(
    county = str_remove(county, "^TX_"),  # TX_ 제거
    county = str_replace_all(county, "\\.", " ")  # . → space
  )

head(df_flu_long)

df_all_long1 <- df_all_long %>%
  left_join(hsa_clean %>% 
              filter(state == "TX") %>%
              dplyr::select(-state, -fips), by = "county")


df_flu_long1 <- df_flu_long %>%
  left_join(hsa_clean %>% 
              filter(state == "TX") %>%
              dplyr::select(-state, -fips), by = "county")


tx_pop <- get_acs(
  geography = "county",
  state = "TX",
  variables = "B01003_001",
  year = 2023
) %>%
  mutate(
    county = sub(" County, Texas", "", NAME)
  ) %>%
  rename(population = estimate) %>%
  dplyr::select(county, population)


df_long <- df_all_long1 %>%
  left_join(df_flu_long1, by = c("week", "county", "Date", "hsa_nci_id"),
            suffix = c("_all", "_flu")) %>%
  mutate(
    yr = year(Date),
    mo = month(Date),
    season_start = if_else(mo >= 8, yr, yr - 1),
    season = paste0(season_start, "/", substr(season_start + 1, 3, 4)),
    value = value_flu/value_all
  ) %>% 
  dplyr::select(season, Date, hsa_nci_id, county, value_flu, value_all, value) %>%
  left_join(tx_pop, by = "county")

df_long$value[!is.finite(df_long$value)] <- 0         # NaN, Inf, -Inf, NA
write.csv(df_long, "/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/data/county_edvisits.csv",
          row.names = FALSE)

df_hsa <- df_long %>%
  group_by(season, Date, hsa_nci_id) %>%
  summarise(hsa_flu = sum(value_flu, na.rm = TRUE),
            hsa_all = sum(value_all, na.rm = TRUE),
            hsa_population = sum(population, na.rm = TRUE)) %>%
  mutate(value = hsa_flu/hsa_all) %>%
  rename(population = hsa_population)

df_hsa$value[!is.finite(df_hsa$value)] <- 0         # NaN, Inf, -Inf, NA

write.csv(df_hsa, "/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/data/hsa_edvisits.csv",
          row.names = FALSE)


df_hsa_county <- df_long %>%
  left_join(df_hsa, by = c("season", "Date", "hsa_nci_id"),
            suffix = c(".county", ".hsa"))

write.csv(df_hsa_county, "/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/data/county_hsa_edvisits.csv",
          row.names = FALSE)


library(tigris)
library(igraph)

options(tigris_use_cache = TRUE)

tx <- counties(state = "TX", cb = TRUE, year = 2023) %>%  # year는 너 데이터에 맞춰 대충
  st_transform(5070) %>%                                 # 면적/접촉 계산용 투영
  mutate(GEOID = as.character(GEOID))

tx_inc_hsa <- tx %>%
  left_join(hsa_tx, by = c("GEOID" = "fips",
                           "NAME" = "county",
                           "STUSPS" = "state"))

saveRDS(tx_inc_hsa, "/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/data/county_formap.RDS")

hsa_sf <- tx_inc_hsa %>%
  filter(!is.na(hsa_nci_id)) %>%
  group_by(hsa_nci_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

saveRDS(hsa_sf, "/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/data/hsa_formap.RDS")


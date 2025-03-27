# Load required libraries
library(tidyverse)
library(readr)
library(here)

# Load environmental data
env <- readRDS(here("data/processed", "environmentaldata.rds"))
env.month <- readRDS(here("data/processed", "environmentaldata.monthly.rds"))
cesar <- read.csv(here("data/processed","daymet_for_julia_2.csv"))
spei <- read.csv(here("data/processed", "daymet_for_julia_2.csv"))

# Standardize year format
env$year <- as.factor(as.character(env$year))
cesar$year <- as.factor(as.character(cesar$year))
spei$year <- as.factor(as.character(spei$year))

# Convert env.month to tibble and standardize factors
env.month <- as_tibble(env.month)
env.month$year <- as.factor(as.character(env.month$year))
env.month$month <- as.factor(as.character(env.month$month))

# Add elevation column
env$elevation_m <- env$elev
env.month$elevation_m <- env.month$elev

# Define function to standardize site names
standardize_site_names <- function(df, site_col = "site") {
  df[[site_col]] <- tolower(df[[site_col]])
  df[[site_col]] <- recode(df[[site_col]],
                           "pfeilerenquist" = "pfeiler",
                           "painterboy" = "pbm")
  return(df)
}

# Filter growing season: months with tmin > 0
env.month.subset <- env.month %>% 
  filter(tmin_c > 0) %>% 
  group_by(year, site, elev, elevation_m)

# Calculate growing season means

mean_climate_gs <- env.month.subset %>% 
  summarize(tmean_c_gs = mean(tmean_c),
            vpdmin_hPa_gs = mean(vpdmin_hPa),
            vpdmax_hPa_gs = mean(vpdmax_hPa),
            ppt_mm_gs = mean(ppt_mm))

# Merge climate growing season summaries
climate_data <- mean_tmean_gs
climate_data$elev <- NULL

# Join annual env data
env$elev <- NULL
climate_data <- left_join(climate_data, env, by=c("site", "year", "elevation_m"))

# Process Cesar´s climate data
cesar_subset <- cesar %>% 
  select(site, elevation, year, month, prcp, tmax, tmin, tave) %>%
  group_by(year, site, elevation)

mean_tave_cesar <- cesar_subset %>% summarize(tave = mean(tave))
mean_prcp_cesar <- cesar_subset %>% summarize(prcp = mean(prcp))
mean_tave_cesar$prcp <- mean_prcp_cesar$prcp

# Calculate growing season means from cesar
gs_cesar <- cesar_subset %>% filter(tmin > 0)
mean_tave_gs_cesar <- gs_cesar %>% summarize(tave_c_gs = mean(tave))
mean_prcp_gs_cesar <- gs_cesar %>% summarize(prcp_gs = mean(prcp))
mean_tave_gs_cesar$prcp_gs <- mean_prcp_gs_cesar$prcp_gs

climate_data_cesar <- mean_tave_gs_cesar
climate_data_cesar <- standardize_site_names(climate_data_cesar)

# Combine datasets
climate_data$elevation_m <- NULL
climate_data <- standardize_site_names(climate_data)
climate_data2 <- left_join(climate_data_cesar, climate_data, by=c("site", "year"))

mean_tave_cesar <- standardize_site_names(mean_tave_cesar)
climate_data3 <- left_join(climate_data2, mean_tave_cesar, by=c("site", "year", "elevation"))

# Process SPEI data
spei <- spei %>% 
  select(site, year, month, spei_01, spei_02, spei_03,
         spei_04, spei_05, spei_06, spei_07, spei_08,
         spei_09, spei_10, spei_11, spei_12) %>%
  standardize_site_names()

# Select June and July SPEI values
spei_6 <- spei %>% filter(month == "6")
spei_7 <- spei %>% filter(month == "7")

spei03_6 <- spei_6 %>% select(site, year, spei_03) %>% rename(spei_s03_m6 = spei_03)
spei01_6 <- spei_6 %>% select(site, year, spei_01) %>% rename(spei_s01_m6 = spei_01)
spei03_7 <- spei_7 %>% select(site, year, spei_03) %>% rename(spei_s03_m7 = spei_03)
spei04_7 <- spei_7 %>% select(site, year, spei_04) %>% rename(spei_s04_m7 = spei_04)

# Combine all climate and SPEI data
climate_data4 <- climate_data3 %>%
  left_join(spei03_6, by=c("site", "year")) %>%
  left_join(spei01_6, by=c("site", "year")) %>%
  left_join(spei03_7, by=c("site", "year")) %>%
  left_join(spei04_7, by=c("site", "year"))

# Save final dataset
saveRDS(climate_data4, file = "./data/processed/climate_data_full_2.rds")

# datos de julia: prism (da menos resolucion, pq el pixel es grande)
#
# datos climaticos de cesar: daymet
# prcp: precipitation (mm) ?
# short wave radiation
# short water equivalent
# temp. max
# temp. min
# temp. ave
# vp: vapor pressure
# ET_har:
# ET_thorn:
# par: 
# esT
# 

##

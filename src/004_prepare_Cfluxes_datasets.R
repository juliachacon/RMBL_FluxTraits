
## Preparing C Fluxes Datasets across RMBL gradient
## Original by Julia, improved March 24th 2025

# Load packages
library(tidyverse)
library(here)

# Set seed (for reproducibility, in case of future random operations)
set.seed(12345)

# Load elevation data
elevation_rmbl <- readRDS(here("data/processed", "elevation_rmbl.rds"))

# Source trait moments and climate scripts
source(here("scr", "start_here_moments.R"), echo = TRUE)
source(here("scr", "start_here_climate_3.R"), echo = TRUE)

# Load NEE data
load(here("data/processed", "nee_sum_curated.Rdata"))
nee <- as_tibble(nee_sum)

# Calculate mean NEE for each site, year, plot, and time
mean_nee <- nee %>%
  group_by(site, year, plot, time) %>%
  summarise(nee_lm_mean = mean(nee_lm, na.rm = TRUE), .groups = "drop")

# Split NEE, Reco_day, Reco_night datasets
NEE <- mean_nee %>% filter(time == "day") %>% rename(nee = nee_lm_mean)

dayresp <- mean_nee %>% filter(time == "dayresp") %>% rename(reco_day = nee_lm_mean)

nightresp <- mean_nee %>% filter(time == "nightresp") %>% rename(reco_night = nee_lm_mean)

# Calculate GPP (Gross Primary Production)
cfluxes_day <- left_join(dayresp, NEE, by = c("site", "year", "plot")) %>%
  mutate(gpp = nee - reco_day) %>%
  select(-time.x, -time.y)

cfluxes_night <- left_join(nightresp, NEE, by = c("site", "year", "plot")) %>%
  mutate(gpp = nee - reco_night) %>%
  select(-time.x, -time.y)

# Add elevation (merge elevation by site)
data_list <- list(NEE, cfluxes_day, cfluxes_night)
data_list <- lapply(data_list, function(df) left_join(df, elevation_rmbl, by = "site"))

NEE <- data_list[[1]]
cfluxes_day <- data_list[[2]]
cfluxes_night <- data_list[[3]]

# Check differences in years for further analysis.
# Moments and nee day early have been sampled in different years.
table(moments$year)
table(NEE$year)
table(cfluxes_night$year)
table(cfluxes_day$year)


# Filter unwanted years/sites
exclude_years <- c("2020", "2018", "2017", "2015", "2014", "2008", "2007", "2004", "2003")
filter_common <- function(df) {
  df %>% filter(site != "cinnamon", !year %in% exclude_years)
}

NEE <- filter_common(NEE)
cfluxes_night <- filter_common(cfluxes_night)
cfluxes_day <- filter_common(cfluxes_day)

# Remove positive respiration values in night data
cfluxes_night <- cfluxes_night %>% filter(reco_night < 0)

# Merge moments and climate data
cfluxes_day <- left_join(cfluxes_day, moments)
cfluxes_night <- left_join(cfluxes_night, moments)

cfluxes_day <- left_join(cfluxes_day, climate_data4)
cfluxes_night <- left_join(cfluxes_night, climate_data4)

# Remove rows with NA in key columns
cfluxes_night <- cfluxes_night %>% drop_na(percent_P, delta13C)

# Quick check
summary(cfluxes_night)

## Save outputs if needed
# saveRDS(cfluxes_night, here("clean data", "cfluxes_night_clean.rds"))
# saveRDS(cfluxes_day, here("clean data", "cfluxes_day_clean.rds"))

## checking de aqui para abajo:

## Before continuing: I am going to finally use cfluxes_night
# This database contains nee, reconight and gpp calculated with reconight
# So, now I am going to filter (remove) positive R eco values 

dim(cfluxes_night) # 220   7
head(cfluxes_night)
plot(cfluxes_night$reco_night)
abline(h=0)

cfluxes_night <- cfluxes_night %>% 
  filter(reco_night < 0)

dim(cfluxes_night) # 180   7
plot(cfluxes_night$reco_night)
abline(h=0)
plot(cfluxes_night$nee)
abline(h=0)

plot(cfluxes_night$gpp)
abline(h=0)

## Here we are averaging the fluxes measured at early + late + peak seasons
## We removed outliers from Reco, that is to say, all the positive respiration values. (L123)
## This code is coupled to 010a, 011a, and 012a


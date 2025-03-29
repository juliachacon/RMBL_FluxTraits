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
source(here("src", "start_here_moments.R"), echo = TRUE)
source(here("src", "start_here_climate.R"), echo = TRUE)

# Load NEE data
load(here("data/processed", "nee_sum_curated.Rdata"))
nee <- as_tibble(nee_sum)

# Calculate mean NEE for each site, year, plot, and time
mean_nee <- nee %>%
  group_by(site, year, plot, time) %>%
  summarise(nee_lm_mean = mean(nee_lm, na.rm = TRUE), .groups = "drop")

# Split into NEE, Reco_day, and Reco_night
day <- mean_nee %>% filter(time == "day") %>% rename(nee = nee_lm_mean)
dayresp <- mean_nee %>% filter(time == "dayresp") %>% rename(reco_day = nee_lm_mean)
nightresp <- mean_nee %>% filter(time == "nightresp") %>% rename(reco_night = nee_lm_mean)

# Calculate GPP
cfluxes_day <- left_join(dayresp, day, by = c("site", "year", "plot")) %>%
  mutate(gpp = nee - reco_day) %>%
  select(-time.x, -time.y)

cfluxes_night <- left_join(nightresp, day, by = c("site", "year", "plot")) %>%
  mutate(gpp = nee - reco_night) %>%
  select(-time.x, -time.y)

# Add elevation
data_list <- list(day, cfluxes_day, cfluxes_night)
data_list <- lapply(data_list, function(df) left_join(df, elevation_rmbl, by = "site"))

day <- data_list[[1]]
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

# Remove positive respiration values
cfluxes_night <- cfluxes_night %>% filter(reco_night < 0)

# Remove elevation_m to avoid column duplication during joins
moments_clean <- moments %>% select(-elevation_m)

# Merge with trait moments and climate data
cfluxes_day <- cfluxes_day %>%
  left_join(moments_clean, by = c("site", "year", "plot")) %>%
  left_join(climate_data4, by = c("site", "year"))

cfluxes_night <- cfluxes_night %>%
  left_join(moments_clean, by = c("site", "year", "plot")) %>%
  left_join(climate_data4, by = c("site", "year"))


# Remove rows with missing key trait values
cfluxes_night <- cfluxes_night %>% drop_na(percent_P, delta13C)

# Save outputs
saveRDS(cfluxes_night, here("data/processed", "cfluxes_night_clean.rds"))

saveRDS(cfluxes_day, here("data/processed", "cfluxes_day_clean.rds"))



## Before continuing: I am going to finally use cfluxes_night
# This database contains nee, reconight and gpp calculated with reconight
# So, now I am going to filter (remove) positive R eco values 

dim(cfluxes_night) # 220   7
head(cfluxes_night)
plot(cfluxes_night$reco_night)
abline(h=0)

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


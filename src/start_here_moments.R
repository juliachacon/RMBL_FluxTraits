# Load packages
library(tidyverse)
library(here)

# Load data
moments <- readRDS(here("data/processed", "rmbl_moments_updated.rds"))
elevation_rmbl <- readRDS(here("data/processed", "elevation_rmbl.rds"))

# Define target traits
target <- c("delta13C", "delta15N", "height_log", "LDMC_log",
            "leaf_area_log", "NC_ratio", "NP_ratio", "percent_C",                 
            "percent_N", "percent_P", "SLA", "leaf_thickness",
            "stomatal_cond", "stomatal_cond_log")

# Filter for target traits
moments <- moments %>% filter(traits %in% target)

# Standardize site names
standardize_site_names <- function(df, site_col = "site") {
  df[[site_col]] <- tolower(df[[site_col]])
  df[[site_col]] <- recode(df[[site_col]],
                           "pfeiler" = "pfeiler",
                           "pbm" = "pbm")
  return(df)
}

moments <- standardize_site_names(moments)
elevation_rmbl <- standardize_site_names(elevation_rmbl)

# Merge with elevation
moments <- left_join(moments, elevation_rmbl, by = "site") %>% as_tibble()

# Pivot to wide format
moments <- moments %>% 
  select(site, year, elevation_m, site_year, unique_block, traits, mean) %>% 
  pivot_wider(names_from = "traits", values_from = "mean")

# Remove Cinnamon site (elevation 3463 m)
moments <- moments %>% filter(elevation_m != 3463)

# Extract plot from unique_block
moments <- moments %>%
  mutate(plot = str_split(unique_block, "_", simplify = TRUE)[, 3])

# Extract plot from unique_block
# moments$plot <- sapply(X = moments$unique_block, 
                      # FUN = function(x){strsplit(x = x, split = "_")[[1]][3]})
# Save clean version
saveRDS(object = moments, file = here("data/processed", "rmbl_moments_updated_clean.rds"))


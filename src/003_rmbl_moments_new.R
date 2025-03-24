
# The script calculates community-weighted trait moments (mean, variance, etc.) across the RMBL (Rocky Mountain Biological Laboratory) gradient, using the traitstrap package. It handles missing trait data through hierarchical imputation and bootstrapping to get robust estimates.

## Code for calculating the moments for each site across the RMBL gradient
## Brian Maitner April 27th, 2020
## changes by Julia June 12nd, 2020
## more changes March 23rd 2021 Julia
## improvements March 24th 2025 

# Load required libraries
library(traitstrap)
library(tidyverse)
library(here)

# For reproducibility
set.seed(12345)

# Load data
load(here("data/processed", "trait_sbs_L.rds")) # trait_sbs_L
load(here("data/raw", "RMBL_2005_2019_MASTER.Rdata")) # com <- RMBL_2005_2019_MASTER

# Data cleaning and preparation
com <- RMBL_2005_2019_MASTER

## Convert block to character and clean plot column
trait_sbs_L <- trait_sbs_L %>% 
  mutate(block = as.character(block),
         plot = str_replace(block, "Block", ""))


## Remove NA values in trait values
trait_sbs_L <- trait_sbs_L %>%
  filter(!is.na(value))

## Create unique_block column
com <- com %>%
  mutate(unique_block = paste(site, year, plot, sep = "_"))

trait_sbs_L <- trait_sbs_L %>%
  mutate(unique_block = paste(site, year, plot, sep = "_"))

## Create site_year column
com <- com %>%
  mutate(site_year = paste(site, year, sep = "_"))

trait_sbs_L <- trait_sbs_L %>%
  mutate(site_year = paste(site, year, sep = "_"))

## Quick checks
glimpse(com)
glimpse(trait_sbs_L)

################################################################################
## Impute traits with scale_hierarchy "site"; "site_year"; unique_block
################################################################################

filled_traits <- traitstrap::trait_fill(comm = com,
                                           traits = trait_sbs_L,
                                           scale_hierarchy = c("site", "site_year", "unique_block"),
                                           taxon_col = "species",
                                           trait_col = "traits",
                                           value_col = "value",
                                           abundance_col = "abundance")

## Bootstrap trait moments
bootstrapped_moments <- traitstrap::trait_np_bootstrap(filled_traits = filled_traits, nrep = 200, sample_size = 1000)

## Summarize bootstrapped trait moments
rmbl_moments <- trait_summarise_boot_moments(bootstrap_moments = bootstrapped_moments)

## Extract year from site_year
rmbl_moments <- rmbl_moments %>%
  mutate(year = sapply(site_year, function(x) str_split(x, "_")[[1]][2]))

## Save outputs
saveRDS(object = rmbl_moments, file = "./clean data/rmbl_moments_updated.rds")
saveRDS(object = imputed_traits, file = "./clean data/rmbl_imputed_traits_updated.rds")

## Print session info for reproducibility
sessionInfo()

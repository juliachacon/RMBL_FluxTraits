
# RMBL trait data master
# RMBL trait data master cleaning and harmonization script

# Load libraries
library(tidyverse)
library(readr)

# Read master file
rmbl_trait <- read.csv("./data/raw/rmbl_trait_data_master.csv")
rmbl_trait <- as_tibble(rmbl_trait)

# Subset relevant columns, clean block factor, remove rows with NA species, replace block4 by Block4

traits_sbs <- rmbl_trait %>% 
  select(species, genus, site, block, year, year_2, elev, lat, long,
       leaf_area, SLA, LMA, LDMC, height, leaf_thickness,
       percent_C, percent_N, percent_P, delta13C, delta15N, 
       CN_ratio, NC_ratio, NP_ratio, 
       cond_corrected, cond) %>%
  filter(!is.na(species)) %>%
  mutate(block = as.factor(replace(block, block == "block4", "Block4")))

# Outlier removal and cleaning
# For example replace all values of SLA larger than 328 by NA
# I consider than a value larger than 320 is wrong or suspicious
# I replace values larger or lower than ** by NA, based on outlier detection

traits_sbs <- traits_sbs %>%
  mutate(
    SLA = replace(SLA, SLA > 328, NA),
    LMA = replace(LMA, LMA > 0.014, NA),
    LDMC = replace(LDMC, LDMC > 650, NA),
    leaf_thickness = replace(leaf_thickness, leaf_thickness > 0.41, NA),
    percent_C = replace(percent_C, percent_C > 50 | percent_C < 39, NA),
    percent_N = replace(percent_N, percent_N > 5 | percent_N < 0.5, NA),
    percent_P = replace(percent_P, percent_P > 0.41, NA),
    delta13C = replace(delta13C, delta13C > -24.2 | delta13C < -29.9, NA),
    delta15N = replace(delta15N, delta15N > 5 | delta15N < -2.5, NA),
    stomatal_cond = replace(cond_corrected, cond_corrected < 0, NA),
    
    # Log10 transformations
    leaf_area_log = log10(leaf_area),
    SLA_log = log10(SLA),
    LMA_log = log10(LMA),
    LDMC_log = log10(LDMC),
    height_log = log10(height),
    leaf_thickness_log = log10(leaf_thickness),
    stomatal_cond_log = log10(stomatal_cond),
    
    # Recalculate stoichiometric ratios after cleaning
    CN_ratio = percent_C / percent_N,
    NC_ratio = percent_N / percent_C,
    NP_ratio = percent_N / percent_P
  )

# Transform to long format
trait_sbs_L <- traits_sbs %>%
  select(species, genus, site, block, year, year_2, elev, lat, long,          leaf_area, leaf_area_log, 
         SLA, SLA_log,
         LMA, LMA_log,
         LDMC, LDMC_log,
         height, height_log, 
         leaf_thickness, leaf_thickness_log,
         stomatal_cond, stomatal_cond_log,
         percent_C, percent_N, percent_P, delta13C, delta15N, CN_ratio, NC_ratio, NP_ratio) %>%
  pivot_longer(cols = c("leaf_area", "leaf_area_log", 
                        "SLA", "SLA_log", "LMA", "LMA_log", "LDMC", "LDMC_log",
                        "height", "height_log", "leaf_thickness", "leaf_thickness_log",
                        "stomatal_cond", "stomatal_cond_log", "percent_C", "percent_N",
                        "percent_P", "delta13C", "delta15N", "CN_ratio", "NC_ratio",
                        "NP_ratio"), 
  names_to = "traits", values_to = "value") %>%
  mutate(trait_type = if_else(str_detect(traits, "_log$"), "log10_transformed", "raw"))

# Check output
glimpse(trait_sbs_L)
table(trait_sbs_L$traits)

#   SUMMARY OF WHAT I DID
# Data Cleaning and Processing Notes:
# Species Filtering :Removed species that are not present in the communities
# Height Data: Checked the distributions of species with height (h) > 100.
# Distributions appear reasonably normal.
# Decision: No transformation applied to height, and no outliers removed.
# Trait Transformation: Log10-transformed all traits, including SLA (Specific Leaf Area), after removing outliers.
# Stomatal Conductance: Removed values lower than 0.
# Stoichiometric Ratios: Recalculated C:N, N:C, and N:P ratios after outlier removal.
# Pending Decision: Flags. Still need to decide how to handle flagged data points



## Code for calculating the moments for each site across the RMBL gradient
## Brian Maitner
## April 27th, 2020
## changes by Julia June 12nd, 2020
## more changes March 23rd 2021 Julia

# install.packages("remotes")
#remotes::install_github("richardjtelford/traitstrap")

## need to update traitstrap
library(traitstrap)
library(tidyverse)

## load data
load("./clean data/trait_sbs_L.rds") #trait_sbs_L
load("./clean data/RMBL_2005_2019_MASTER.Rdata")
com <- RMBL_2005_2019_MASTER

## en trait_sbs_L renombrar los bloques
# cambiar block a character
trait_sbs_L <- trait_sbs_L %>% 
  mutate(block = as.character(block))

# create plot column with numbers of plots, remove this disgusting "Block1"
trait_sbs_L <- trait_sbs_L %>% 
mutate(plot = replace(block, block=="Block1", "1"))

## transform trait_sbs_L into a dataframe
trait_sbs_L <- as.data.frame(trait_sbs_L)
trait_sbs_L <- trait_sbs_L[which(!is.na(trait_sbs_L$value)),] # remove NA´s values in traits

#### create unique_block
com$unique_block <-
  paste(com$site,"_", com$year,"_", com$plot,sep = "")

trait_sbs_L$unique_block <- 
  paste(trait_sbs_L$site,"_",trait_sbs_L$year,"_",trait_sbs_L$plot,sep = "")

head(com)
head(trait_sbs_L)

#make unique site_year column
com$site_year <- paste(com$site,"_",com$year,sep = "")
trait_sbs_L$site_year <- paste(trait_sbs_L$site,"_",trait_sbs_L$year,sep = "")

head(com)
head(trait_sbs_L)

################################################################################
## imputed traits with scale_hierarchy "site"; "site_year"; unique_block
################################################################################

imputed_traits <- traitstrap::trait_impute(comm = com,
                traits = trait_sbs_L,
                scale_hierarchy = c("site", "site_year", "unique_block"),
                taxon_col = "species",
                trait_col = "traits",
                value_col = "value",
                abundance_col = "abundance")

imputed_traits

bootstrapped_moments <- traitstrap::trait_np_bootstrap(imputed_traits = imputed_traits, nrep = 200, sample_size = 1000)

rmbl_moments <- trait_summarise_boot_moments(bootstrap_moments = bootstrapped_moments)

#add year back in for convenience

rmbl_moments$year <- sapply(X = rmbl_moments$site_year, 
                        FUN = function(x){strsplit(x = x,split = "_")[[1]][2]})

#save output
saveRDS(object = rmbl_moments, file = "./clean data/rmbl_moments_updated.rds")
saveRDS(object = imputed_traits, file = "./clean data/rmbl_imputed_traits_updated.rds")


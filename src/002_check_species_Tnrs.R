# Reviewed: Validation and cleaning of species names using TNRS (taxize)

# Load necessary libraries
library(taxize)
library(dplyr)

# Load data
load("./data/raw/RMBL_2005_2019_MASTER.Rdata")
com <- RMBL_2005_2019_MASTER

# Read species dictionary
load("./data/raw/species_dictionary_RMBL_2005_2019.Rdata")

# List of unique species
# Note: make sure trait_sbs_L is loaded before this
taxaT <- unique(trait_sbs_L$species)
length(taxaT) # 240

# TNRS queries in blocks (complete if necessary)
# tnrs_out1 <- tnrs(query = taxaT[1:50], source = "iPlant_TNRS")[ , -c(5:7)]
# tnrs_out2 <- tnrs(query = taxaT[51:100], source = "iPlant_TNRS")[ , -c(5:7)]
# tnrs_out3 <- tnrs(query = taxaT[101:144], source = "iPlant_TNRS")[ , -c(5:7)]
# tnrs_out4 <- tnrs(query = taxaT[145:200], source = "iPlant_TNRS")[ , -c(5:7)]
# tnrs_out5 <- tnrs(query = taxaT[201:241], source = "iPlant_TNRS")[ , -c(5:7)]

# After the querry, just check for the differences between the submitted names and the accepted names.
# You have to do that once per querry

#setdiff(tnrs_out1$submittedname, tnrs_out1$acceptedname)

# Replace species names (simplified using case_when)
trait_sbs_L <- trait_sbs_L %>%
  mutate(species = case_when(
    species == "Juncus sp." ~ "Juncus",
    species == "Trifolium sp." ~ "Trifolium",
    species == "Cirsium sp." ~ "Cirsium",
    species == "Viola sp." ~ "Viola",
    species == "Ligusticum porteri" ~ "Ligusticum porteri",
    species == "Lupinus sp." ~ "Lupinus",
    species == "Rumex sp." ~ "Rumex",
    species == "Erigeron sp." ~ "Erigeron",
    species == "Senecio sp." ~ "Senecio",
    species == "Achnatherum sp." ~ "Achnatherum",
    species == "Phacelia sp." ~ "Phacelia",
    species == "Sedum roseum" ~ "Sedum roseum",
    species == "Bromus sp." ~ "Bromus",
    species == "Clematis hirsuta" ~ "Clematis hirsuta",
    species == "Penstemon sp." ~ "Penstemon",
    species == "Salix sp." ~ "Salix",
    species == "Gentiana sp." ~ "Gentiana",
    species == "Poa sp." ~ "Poa",
    species == "Sphaeralcea sp." ~ "Sphaeralcea",
    species == "Chrysothamnus sp." ~ "Chrysothamnus",
    species == "Potentilla sp." ~ "Potentilla",
    species == "Aquilegia sp." ~ "Aquilegia",
    species == "Mertensia sp." ~ "Mertensia",
    species == "Polygonum sp." ~ "Polygonum",
    species == "Veratrum sp." ~ "Veratrum",
    species == "Galium sp." ~ "Galium",
    species == "Achillea sp." ~ "Achillea",
    TRUE ~ species
  ))

# Filter species using species dictionary (make sure to define species_dictionary)
# species_dictionary <- c(...)  # Define or load this list
trait_sbs_L <- trait_sbs_L %>% filter(species %in% species_dictionary)

# Validation steps
taxaT <- unique(trait_sbs_L$species)
cat("Total species in traits: ", length(taxaT), "\n")  #163
cat("Total species in com: ", length(unique(com$species)), "\n")
cat("Common species in com and traits: ", length(intersect(com$species, trait_sbs_L$species)), "\n")  #163

# Save result
saveRDS(trait_sbs_L, file = "./data/processed/trait_sbs_L.rds")


# I have to re run the taxize again....
#just updated the names with the previous version

################################### check names with tnrs function from taxize package
#install.packages("taxize")
library(taxize)

load("./clean data/RMBL_2005_2019_MASTER.Rdata")
com <- RMBL_2005_2019_MASTER
taxaT <- unique(trait_sbs_L$species)
length(taxaT) # 240

# First make various querries, I am only selecting columns 5 to 7 CHECK THIS
# I made various querries with tnrs() to update the names of the traits database. 

tnrs_out1 <- tnrs(query = taxaT[1:50], source = "iPlant_TNRS")[ , -c(5:7)]
tnrs_out2 <- tnrs(query = taxaT[51:100], source = "iPlant_TNRS")[ , -c(5:7)]
tnrs_out3 <- tnrs(query = taxaT[101:144], source = "iPlant_TNRS")[ , -c(5:7)]

#tnrs_out4 <- tnrs(query = taxaT[151:200], source = "iPlant_TNRS")[ , -c(5:7)]
#tnrs_out5 <- tnrs(query = taxaT[201:241], source = "iPlant_TNRS")[ , -c(5:7)]

# After the querry, just check for thedifferences between the submitted names and the acceptednames.
# You have to do that once per querry

#setdiff(tnrs_out1$submittedname, tnrs_out1$acceptedname)

# Now replace the names in traitL$species
trait_sbs_L$species[which(trait_sbs_L$species == "Juncus sp.")] <- "Juncus" 
trait_sbs_L$species[which(trait_sbs_L$species == "Trifolium sp.")] <- "Trifolium" 
trait_sbs_L$species[which(trait_sbs_L$species == "Cirsium sp.")] <- "Cirsium" 
trait_sbs_L$species[which(trait_sbs_L$species == "Viola sp.")] <- "Viola" 
trait_sbs_L$species[which(trait_sbs_L$species == "Ligusticum porteri")] <- "Ligusticum porteri" 
trait_sbs_L$species[which(trait_sbs_L$species == "Lupinus sp.")] <- "Lupinus" 

# 
#setdiff(tnrs_out2$submittedname, tnrs_out2$acceptedname)

# 
trait_sbs_L$species[which(trait_sbs_L$species == "Rumex sp.")] <- "Rumex" 
trait_sbs_L$species[which(trait_sbs_L$species == "Erigeron sp.")] <- "Erigeron" 
trait_sbs_L$species[which(trait_sbs_L$species == "Senecio sp.")] <- "Senecio" 
trait_sbs_L$species[which(trait_sbs_L$species == "Achnatherum sp.")] <- "Achnatherum" 
trait_sbs_L$species[which(trait_sbs_L$species == "Phacelia sp.")] <- "Phacelia" 
trait_sbs_L$species[which(trait_sbs_L$species == "Sedum roseum")] <- "Sedum roseum" 

# 

#setdiff(tnrs_out3$submittedname, tnrs_out3$acceptedname)

# 
trait_sbs_L$species[which(trait_sbs_L$species == "Bromus sp.")] <- "Bromus" 
trait_sbs_L$species[which(trait_sbs_L$species == "Clematis hirsuta")] <- "Clematis hirsuta" 
trait_sbs_L$species[which(trait_sbs_L$species == "Penstemon sp.")] <- "Penstemon" 
trait_sbs_L$species[which(trait_sbs_L$species == "Salix sp.")] <- "Salix" 

# 
#setdiff(tnrs_out4$submittedname, tnrs_out4$acceptedname)

# 
trait_sbs_L$species[which(trait_sbs_L$species == "Gentiana sp.")] <- "Gentiana" 
trait_sbs_L$species[which(trait_sbs_L$species == "Poa sp.")] <- "Poa" 
trait_sbs_L$species[which(trait_sbs_L$species == "Sphaeralcea sp.")] <- "Sphaeralcea" 
trait_sbs_L$species[which(trait_sbs_L$species == "Chrysothamnus sp.")] <- "Chrysothamnus" 

# 
#setdiff(tnrs_out5$submittedname, tnrs_out5$acceptedname)

# 
trait_sbs_L$species[which(trait_sbs_L$species == "Potentilla sp.")] <- "Potentilla" 
trait_sbs_L$species[which(trait_sbs_L$species == "Aquilegia sp.")] <- "Aquilegia" 
trait_sbs_L$species[which(trait_sbs_L$species == "Mertensia sp.")] <- "Mertensia" 
trait_sbs_L$species[which(trait_sbs_L$species == "Polygonum sp.")] <- "Polygonum" 
trait_sbs_L$species[which(trait_sbs_L$species == "Veratrum sp.")] <- "Veratrum" 
trait_sbs_L$species[which(trait_sbs_L$species == "Galium sp.")] <- "Galium" 
trait_sbs_L$species[which(trait_sbs_L$species == "Achillea sp.")] <- "Achillea" 


################################################################################

# filter species in traits by species dictionary
trait_sbs_L <- trait_sbs_L[trait_sbs_L$species %in% species_dictionary,]
taxaT <- unique(trait_sbs_L$species)
length(taxaT) #163
length(unique(com$species))

# table(com$species[which(com$species == "Sphaeralcea")])
intersect(com$species, trait_sbs_L$species) # elements in common in the two data sets
# 163

save(trait_sbs_L, file="./clean data/trait_sbs_L.rds") 

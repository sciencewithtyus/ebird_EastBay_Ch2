library(readr)
require(tidyverse)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



highly_complete_holcs_aves_species_lists_2023_03_02 <- read_csv("working_data/highly_complete_holcs_aves_species_lists/highly_complete_holcs_aves_species_lists_2023-03-02.csv")  %>% mutate(
  holc_grade = substrRight( sub("_[^_]+$", "", id), 1)
)



splist_holc = unique(highly_complete_holcs_aves_species_lists_2023_03_02$species)

# Load in Avonet
# Change the loss of bird species at some point with synoynm list:
avonet_trait = read.csv('/Users/diegoellis/Downloads/16586228/ELEData/TraitData/AVONET1_BirdLife.csv')

mismatch_species = splist_holc[! splist_holc %in% avonet_trait$Species1]
# Find matching species name for GBIF and Birdlife



# Load in Avonet
# Change the loss of bird species at some point with synoynm list:
avonet_trait = read.csv('/Users/diegoellis/Downloads/16586228/ELEData/TraitData/AVONET1_BirdLife.csv') %>% filter(Species1 %in% unique(highly_complete_holcs_aves_species_lists_2023_03_02$species))

highly_complete_holcs_aves_species_lists_2023_03_02_genus = highly_complete_holcs_aves_species_lists_2023_03_02 %>% dplyr::select(holc_grade, genus)
highly_complete_holcs_aves_species_lists_2023_03_02_species  = highly_complete_holcs_aves_species_lists_2023_03_02 %>% dplyr::select(holc_grade, species) %>% distinct(holc_grade, species) %>% mutate(values = 1)


matrix_sites_species = highly_complete_holcs_aves_species_lists_2023_03_02_species %>% tidyr::pivot_wider(names_from = species, values_from = values, values_fill = 0)

# View(matrix_sites_species)
# Ok so there are slight diferences in species richness:
table(apply(matrix_sites_species, 2, function(a) length(unique(a))==1))


z_traits = scale(traits_birds, center = TRUE, scale = TRUE)
trait_distance = as.matrix(dist(z_traits))

birds_fric = fd_fric(z_traits, site_sp_birds)



## Download microclimate for 2 pumas: ####
# Get only the microclimate for the timestamp of the collected puma:
# store. 
#
# https://mrke.github.io/NicheMapR/inst/doc/microclimate-model-tutorial.html

# get daymet 

library(NicheMapR) 
longlat <- c(-89.40123, 43.07305) # Madison, Wisconsin, USA
micro <- micro_global(loc = longlat)


metout<-micro$metout # put the metout result into a variable 'metout'
head(metout,24) # show the first 24 rows, i.e. the first day

shadmet<-micro$shadmet # put the shadmet result into a variable 'shadmet'
head(shadmet,24) # show the first 24 rows, i.e. the first day


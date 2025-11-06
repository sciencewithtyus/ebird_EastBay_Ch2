
ebird_presence = fread('ebd_presence-2.csv')
ebird_checklists = fread('ebd_complete_checklists-2.csv')
avonet_trait = read.csv('AVONET1_BirdLife.csv')  |> select(Species1, Family1, Order1,
                                                                                                                  Wing.Length, Kipps.Distance,
                                                                                                                  Mass, Mass.Source, Habitat, Migration,
                                                                                                                  Habitat.Density, Trophic.Level,
                                                                                                                  Trophic.Niche, Primary.Lifestyle, Range.Size) |>
  mutate(taxa = Species1)
# Subset Avonet to trairs of interest:

# How many mismatched species for chceklists
# 17 mismatched species
mismatch_species_checklist = unique(ebird_checklists[! ebird_checklists$trip_comments %in% avonet_trait$taxa]$trip_comments)

ebird_checklists$taxa = ebird_checklists$trip_comments
# 13 mismatched species
mismatch_species_pres_ebird = unique(ebird_presence[! ebird_presence$common_name %in% avonet_trait$taxa]$common_name)
ebird_presence$taxa = ebird_presence$common_name
# Split by traits of interest:

# left_join
ebird_presence = ebird_presence |> left_join(avonet_trait, by = 'taxa')
ebird_checklists = ebird_checklists |> left_join(avonet_trait, by = 'taxa')

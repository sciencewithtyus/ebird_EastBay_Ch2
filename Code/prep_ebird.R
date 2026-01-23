# check species richness unique 

# Steps for Tyus: Harmonize missing data taxonomic mismatch avonet 
# could use taxize but likely not necessary since few speices
# add dist2coastline

# anno to cbd 


## ============================================================
## 0. Libraries
## ============================================================
require(tidycensus)
library(auk)
library(dplyr)
library(ggplot2)
library(auk)
library(tictoc)
require(data.table)
require(readr)
## ============================================================
## 1. Paths to raw county files ####
## ============================================================

# Skip if file exists:
base_dir <- "/Users/diegoellis/Desktop/Projects/Postdoc/Tyus"
combined_dir <- file.path(base_dir, "ebd_US-CA-001_013_smp_relSep-2025_v11")
f_ebd_filt <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_filtered_v11.txt")
f_smp_filt <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_filtered_sampling_v11.txt")

if(!file.exists(f_ebd_filt)){
  
  
  # Alameda
  dir_001   <- file.path(base_dir, "ebd_US-CA-001_smp_relSep-2025")
  f_ebd_001 <- file.path(dir_001, "ebd_US-CA-001_smp_relSep-2025.txt")
  f_smp_001 <- file.path(dir_001, "ebd_US-CA-001_smp_relSep-2025_sampling.txt")
  
  # Contra Costa
  dir_013   <- file.path(base_dir, "ebd_US-CA-013_smp_relSep-2025")
  f_ebd_013 <- file.path(dir_013, "ebd_US-CA-013_smp_relSep-2025.txt")
  f_smp_013 <- file.path(dir_013, "ebd_US-CA-013_smp_relSep-2025_sampling.txt")
  
  ## ============================================================
  ## 2. Helper to combine eBird txt files WITHOUT breaking format
  ## ============================================================
  
  # This keeps file1 exactly as-is (comments + header + data),
  # and appends only data rows (no comments/header) from file2.
  combine_ebird_txt <- function(file1, file2, outfile) {
    # read all lines of file1
    lines1 <- readLines(file1)
    
    # read all lines of file2
    lines2 <- readLines(file2)
    
    # find first non-comment line in file2 (the header row)
    header_idx2 <- which(!startsWith(lines2, "#"))[1]
    
    if (is.na(header_idx2)) {
      stop("Could not find header in second file: ", file2)
    }
    if (header_idx2 >= length(lines2)) {
      stop("No data rows found in second file: ", file2)
    }
    
    # data rows start AFTER the header
    data2 <- lines2[(header_idx2 + 1):length(lines2)]
    
    # write all of file1 + data rows from file2
    all_out <- c(lines1, data2)
    writeLines(all_out, outfile)
    
    invisible(outfile)
  }
  
  ## ============================================================
  ## 3. Create merged East Bay EBD + sampling files
  ## ============================================================
  
  # combined_dir <- file.path(base_dir, "ebd_US-CA-001_013_smp_relSep-2025_v11")
  dir.create(combined_dir, showWarnings = FALSE)
  
  f_ebd_merged <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_v11.txt")
  f_smp_merged <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_sampling_v11.txt")
  
  # IMPORTANT: this overwrites any broken merged files you created earlier
  combine_ebird_txt(f_ebd_001, f_ebd_013, f_ebd_merged)
  combine_ebird_txt(f_smp_001, f_smp_013, f_smp_merged)

  
  f_ebd_merged <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_v11.txt")
  f_smp_merged <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_sampling_v11.txt")
  
  
  ## ============================================================
  ## 4. Set up auk with the merged files
  ## ============================================================
  
  ebd_auk <- auk_ebd(f_ebd_merged, file_sampling = f_smp_merged)
  
  # Example filters: complete checklists + standard protocols
  ebd_auk_filt <- ebd_auk |>
    auk_complete() |>
    auk_protocol(c("Stationary", "Traveling", "Area"))
  # you can add more filters here:
  # |> auk_year(2022:2024)
  # |> auk_bbox(c(xmin, ymin, xmax, ymax))
  
  ## ============================================================
  ## 5. Apply filters on disk and read back in
  ## ============================================================
  
  # Output filtered files
  f_ebd_filt <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_filtered_v11.txt")
  f_smp_filt <- file.path(combined_dir, "ebd_US-CA-001_013_smp_relSep-2025_filtered_sampling_v11.txt")
  
  auk_filter(
    ebd_auk_filt,
    file          = f_ebd_filt,
    file_sampling = f_smp_filt,
    overwrite     = TRUE
  )
  
  }

# --- --- --- --- --- --- --- --- ---
# Load Filtered ebird data ####
# --- --- --- --- --- --- --- --- ---

# tic("Reading filtered eBird data with data.table")
if(!file.exists('../../2025_11_28_ebd_with_avonet.txt')){
ebd <- read_ebd(f_ebd_filt) # ~10 min
# # Read the filtered EBD file (tab-delimited)
# ebd <- fread(
#   f_ebd_filt,
#   sep = "\t",
#   quote = ""    # <<< IMPORTANT for eBird files
# )

# toc()  # end timing
# ebd <- janitor::clean_names(ebd)

# Load Avonet
avonet_trait = read.csv('../../AVONET1_BirdLife.csv') %>% 
  select(Species1, Family1, Order1, Wing.Length, Kipps.Distance, 
         Mass, Mass.Source, Habitat, Migration, Habitat.Density, Trophic.Level,
         Trophic.Niche, Primary.Lifestyle, Range.Size) %>% 
  mutate(taxa = Species1) # subset the avonet file for desired traits


# How many mismatched species for checklists
# 17 mismatched species

mismatch_species_checklist = unique(ebd$scientific_name[! ebd$scientific_name %in% avonet_trait$taxa])
ebd$taxa = ebd$scientific_name
# 13 mismatched species
mismatch_species_pres_ebird = unique(ebd$taxa[! ebd$taxa %in% avonet_trait$taxa])
# Split by traits of interest:

# left_join the avonet traits by the 'taxa' column
ebd_avo = ebd %>%  left_join(avonet_trait, by = 'taxa')
# ebd_complete = ebd_complete %>%  left_join(avonet_trait, by = 'taxa')
ebd_dt <- as.data.table(ebd_avo)

fwrite(
  ebd_dt,
  file = "../../2025_11_28_ebd_with_avonet.txt",
  sep = "\t",
  quote = FALSE,
  na = "",
  row.names = FALSE
)
# next we join the trait information to the main working ebird data frames
}

# --- --- --- --- --- --- --- --- ---
# Annotate socio-eco data to checklists ####
# --- --- --- --- --- --- --- --- ---

# same variables as before
acs_vars <- c(
  total_pop      = "B01003_001",  # total population
  med_hh_income  = "B19013_001",  # median household income
  housing_units  = "B25001_001"   # total housing units
)

# California boundary + centroid in lon/lat
ca <- tigris::states(year = 2022) |>
  filter(NAME == "California") |>
  st_transform(4326)
ca_centroid <- st_coordinates(st_centroid(ca))
lon0 <- ca_centroid[1]
lat0 <- ca_centroid[2]

laea_ca <- paste0(
  "+proj=laea ",
  " +lat_0=", lat0,
  " +lon_0=", lon0,
  " +x_0=0 +y_0=0 ",
  " +datum=WGS84 +units=m +no_defs"
)

bg_covs <- get_acs(
  geography = "block group",
  variables = acs_vars,
  state     = "CA",
  county    = c("Alameda", "Contra Costa"),
  year      = 2022,
  geometry  = TRUE
) |>
  # wide: one row per block group
  pivot_wider(
    id_cols   = c(GEOID, NAME, geometry),
    names_from  = variable,
    values_from = estimate
  ) |>
  # project to CA-centered LAEA for area calc
  st_transform(laea_ca) |>
  mutate(
    area_km2        = as.numeric(st_area(geometry)) / 1e6,
    pop_density     = total_pop / area_km2,
    housing_density = housing_units / area_km2
  ) |>
  # back to WGS84 for eBird points / auk
  st_transform(4326)

bg_v <- vect(bg_covs)

bio1 = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'bio1_mask_trim_crop_clip_wgs84')) # Load all .tif, not geodatabase
bio12 = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'bio12_mask_trim_crop_clip_wgs84')) # Load all .tif, not geodatabase
elevation = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'elev_mask_trim_crop_clip_wgs84')) # Load all .tif, not geodatabase
ndvi = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'ndvi_mask_trim_crop_clip_wgs84')) # Load all .tif, not geodatabase
imp_surf = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'nlcd_impervious_mask_trim_crop_clip_wgs84')) # Load all .tif, not geodatabase
nlcd_landcover = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'nlcd_landcover_mask_trim_crop_clip_wgs84')) # Load all .tif, not geodatabase
spring_migration = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'spring_stopover_2500_v9_clip_wgs84')) # Load all .tif, not geodatabase
viirs_mask_trim_crop_clip_wgs84 = rast(list.files('../../Reprojected_socio-eco-vars/', full.names = T, pattern = 'viirs_mask_trim_crop_clip_wgs84')) # Load all .tif, not geodatabase
walkability_sf <- st_read('../../WalkabilityIndex/Natl_WI.gdb') %>%st_transform(4326)
walkability_vect = vect(walkability_sf)

# walkability <- rast('../../WalkabilityIndex/Natl_WI.gdb')

# Annotate env vareiables, add walkability later
smp <- read_sampling(f_smp_filt)

smp_pts <- vect(
  smp,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"   # WGS84; matches your _wgs84 rasters
)

smp_pts$bio1 <- terra::extract(bio1, smp_pts)[, -1]
smp_pts$bio12 <- terra::extract(bio12, smp_pts)[, -1]
smp_pts$elevation <- terra::extract(elevation, smp_pts)[, -1]
smp_pts$ndvi <- terra::extract(ndvi, smp_pts)[, -1]
smp_pts$imp_surf <- terra::extract(imp_surf, smp_pts)[, -1]
smp_pts$nlcd_landcover <- terra::extract(nlcd_landcover, smp_pts)[, -1]
smp_pts$spring_migration <- terra::extract(spring_migration, smp_pts)[, -1]
smp_pts$nightlight <- terra::extract(viirs_mask_trim_crop_clip_wgs84, smp_pts)[, -1]

# --- --- --- --- --- --- --- --- ---
# Annotate eBird data to checklist ####
# --- --- --- --- --- --- --- --- ---

ebd_avo <- fread(
  "../../2025_11_28_ebd_with_avonet.txt",
  sep = "\t",
  header = TRUE,
  quote = "",
  na.strings = c("", "NA")
)

ebd2 <- ebd_avo %>%
  mutate(
    is_migrant = case_when(
      Migration %in% c(2, 3) ~ 1L,   # partial & full migrants
      Migration %in% c(1)    ~ 0L,   # residents
      TRUE ~ NA_integer_
    )
  )

checklist_rich <- ebd2 %>%
  group_by(checklist_id) %>%
  summarise(
    n_species           = n_distinct(scientific_name),
    n_migrant_species   = n_distinct(scientific_name[is_migrant == 1]),
    
    n_records           = n(),                     # total rows
    n_records_migrant   = sum(is_migrant == 1),    # rows where migrant
    
    .groups = "drop"
  )

bg_vals <- terra::extract(bg_v, smp_pts)
smp_pts <- cbind(smp_pts, bg_vals)
walk_vals <- terra::extract(walkability_vect, smp_pts)[, -1, drop = FALSE]
smp_pts   <- cbind(smp_pts, walk_vals)
# walk_vals <- terra::extract(vect(walkability_sf), smp_pts)[, -1, drop = FALSE]
# walk_vals <- terra::extract(vect(walkability_sf), smp_pts)[, -1, drop = FALSE]

dat <- smp_pts %>%
  left_join(checklist_rich, by = "checklist_id")
dat_df <- as.data.frame(dat)

# --- --- --- --- --- --- --- --- --- ---
# Store checklists annotated ####
# --- --- --- --- --- --- --- --- --- ---

fwrite(
  dat_df,
  file = "../../checklists_socio_eco_avo_richness_anno_smp.txt",
  sep = "\t",
  quote = FALSE,
  na = "",
  row.names = FALSE
)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Start a new script that is checklist socio eco but for census block group
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# End of annotations for sampling checklost objects
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Compute checklist-level behavioral metrics and join to smp_pts ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

bg_behavior <- dat |>
  # as.data.frame(geom = FALSE) |>
  group_by(GEOID) |>
  summarise(
    n_checklists      = n(),
    mean_duration     = mean(duration_minutes, na.rm=TRUE),
    mean_distance_km  = mean(effort_distance_km, na.rm=TRUE),
    mean_species      = mean(n_species, na.rm=TRUE),
    mean_migrant      = mean(n_migrant_species, na.rm=TRUE),
    mean_records      = mean(n_records, na.rm=TRUE),
    mean_n_checklist_migr = mean(n_records_migrant, na.rm=TRUE)
  )|> as_tibble()

# Adding ebird data
bg_covs = bg_covs |> left_join(bg_behavior, by='GEOID')

bg_covs$bio1 <- terra::extract(
  bio1,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]

bg_covs$bio12 <- terra::extract(
  bio12,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]


bg_covs$elevation <- terra::extract(
  elevation,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]

bg_covs$ndvi <- terra::extract(
  ndvi,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]

bg_covs$imp_surf <- terra::extract(
  imp_surf,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]

bg_covs$nlcd_landcover <- terra::extract(
  nlcd_landcover,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]

bg_covs$spring_migration <- terra::extract(
  spring_migration,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]

bg_covs$nightlights <- terra::extract(
  viirs_mask_trim_crop_clip_wgs84,
  bg_v,
  fun = mean,
  na.rm = TRUE
)[, -1]

smp_ids <- smp_pts |>
  as.data.frame() |>
  dplyr::select('sampling_event_identifier', 'GEOID') |>
  distinct()

ebd_with_geoid <- ebd2 %>%
  inner_join(
    smp_ids,
    by = "sampling_event_identifier"
  )

cbg_ebird_metrics <- ebd_with_geoid %>%
  group_by(GEOID) %>%
  summarise(
    # species richness at the block-group level
    cbg_species_richness         = n_distinct(scientific_name),
    
    # migrant species richness at the block-group level
    cbg_migrant_species_richness = n_distinct(scientific_name[is_migrant == 1]),
    
    # total eBird records (rows) in the block group
    cbg_n_records                = n(),
    
    # number of migrant records in the block group
    cbg_n_records_migrant        = sum(is_migrant == 1, na.rm = TRUE),
    
    # number of distinct checklists in the block group
    cbg_n_checklists             = n_distinct(sampling_event_identifier),
    
    .groups = "drop"
  )

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Write census block group annotated  .gpkg ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

bg_covs <- bg_covs %>%
  left_join(cbg_ebird_metrics, by = "GEOID")

bg_covs <- bg_covs %>%
  mutate(
    checklist_density_km2 = n_checklists / area_km2,
    record_density_km2    = cbg_n_records / area_km2
  )

# --- --- --- --- --- --- --- ---
# Store annotate cbg ####
# --- --- --- --- --- --- --- ---

st_write(
bg_covs,
"../../cbg_all_socioeco_ebird_anno.gpkg",
layer = "bg_covs"
)

options(tigris_use_cache = TRUE)

msa <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = c(pop = "B01001_001"),
  year = 2019,
  geometry = TRUE,
  output = "wide",
  moe_level = 95
) %>%
  rename(msa_GEOID = GEOID) %>%
  st_make_valid() %>%
  mutate(area_msa_km2 = as.double(st_area(.) / 1e6)) %>%
  separate(NAME, into = c("place", "rest"), sep = ", ", remove = FALSE) %>%
  separate(rest, into = c("states", "type"), sep = "\\s", remove = FALSE)

msa_ca <- msa %>%
  filter(str_detect(NAME, ", CA"))


msa_eastbay <- msa_ca %>%
  filter(NAME == "San Francisco-Oakland-Berkeley, CA Metro Area")
# ensure CRS match
msa_eastbay <- st_transform(msa_eastbay, st_crs(bg_covs))

# keep only block groups whose polygons fall inside the MSA polygon
bg_covs_eastbay_msa <- bg_covs[st_within(bg_covs, msa_eastbay, sparse = FALSE), ]

st_write(
  bg_covs_eastbay_msa,
  "../../bg_covs_eastbay_msa_all_socioeco_ebird_anno.gpkg",
  layer = "bg_covs_eastbay_msa"
)


ounties <- tigris::counties(state = "CA", year = 2020, cb = TRUE) %>%
  filter(NAME %in% c("Alameda", "Contra Costa"))

options(tigris_use_cache = TRUE)

# Get all US urban areas (2020 by default in recent tigris versions)
# ua <- urban_areas(cb = TRUE)  # cb = cartographic boundary (lighter)
ua = st_read('/Users/diegoellis/Downloads/2020_Adjusted_Urban_Area/2020_Adjusted_Urban_Area.shp')|>
  st_make_valid()

st_crs(ua)==st_crs(bg_covs_eastbay_msa)
# bg_covs_ua <- st_intersection(bg_covs_eastbay_msa, ua)


bg_covs_eastbay_msa = bg_covs_eastbay_msa |> st_make_valid()
sf_use_s2(FALSE)

ua_sub <- ua[st_intersects(ua, st_union(st_geometry(bg_covs_eastbay_msa)), sparse = FALSE), ]

plot(st_geometry(ua_sub), col = "lightblue")
plot(st_geometry(bg_covs_eastbay_msa), add = TRUE, border = "red")

# union of the CBGs (faster + simpler to intersect)
bg_union <- st_union(st_geometry(bg_covs_eastbay_msa))

# subset UA polygons that intersect the union of block groups
ua_sub <- ua[st_intersects(ua, bg_union, sparse = FALSE), ]

mapview(ua_sub) + mapview(bg_covs_eastbay_msa) + mapview(bg_covs)
ua_union <- st_make_valid(ua)
bg_fix   <- st_make_valid(bg_covs_eastbay_msa)

bg_covs_ua <- st_intersection(bg_fix, ua_sub)
plot(st_geometry(ua_sub), col="lightblue")
plot(st_geometry(bg_covs_ua), add=TRUE, border="red")

mapview(bg_covs_ua)+mapview(ua_sub) + mapview(bg_covs_eastbay_msa) + mapview(bg_covs)

st_write(
  bg_covs_ua,
  "../../bg_covs_eastbay_ua_only_all_socioeco_ebird_anno.gpkg",
  layer = "bg_covs_eastbay_msa"
)




ggplot(bg_covs) +
  geom_sf(aes(fill = cbg_species_richness), color = NA) +
  scale_fill_viridis_c(option = "C", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Mean checklist species richness",
    subtitle = "Census block groups, Alameda & Contra Costa",
    fill = "Mean\nrichness"
  )

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---







# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# Hasta aca llegue #####




# Make some cool plots with maps
l
# Next left_join number of checklists etc per census block group and left join back to census block group AND add county as random id?
# distance to coast line ! -> 

# table(ebd2$is_migrant)
# # Checklist-level summaries
# checklist_metrics <- ebd2 %>%
#   group_by(checklist_id) %>%
#   summarise(
#     n_species        = n_distinct(scientific_name),
#     n_migrant_species = n_distinct(scientific_name[is_migrant == 1]),
#     n_records        = n(),  # total observations on the checklist (rows)
#     .groups = "drop"
#   )

# smp_env = smp + bio1, bio12, elevation, ndvi, imp_surf, spring_migration, nightlight, etc.
# 
# ggplot(dat, aes(x = imp_surf, y = n_species)) +
#   # geom_point(alpha = 0.1, size = 1) +
#   geom_smooth() +
#   labs(
#     x = "Impervious surface (%)",
#     y = "Checklist species richness",
#     title = "Species richness declines along the urbanization gradient"
#   ) +
#   theme_minimal()
# 
# ggplot(dat, aes(x = imp_surf, y = n_species)) +
#   # geom_point(alpha = 0.1, size = 1) +
#   geom_smooth() +
#   labs(
#     x = "Impervious surface (%)",
#     y = "Checklist species richness",
#     title = "Species richness declines along the urbanization gradient"
#   ) +
#   theme_minimal()
# 
# 
# ggplot(dat, aes(x = imp_surf, y = n_migrant_species)) +
#   geom_smooth(method = "gam") +
#   labs(
#     x = "Impervious surface (%)",
#     y = "Migratory species per checklist",
#     title = "Migratory species richness vs urbanization"
#   ) +
#   theme_minimal()
# 
# 
# ggplot(dat, aes(x = imp_surf, y = n_migrant_species)) +
#   geom_smooth() +
#   labs(
#     x = "Impervious surface (%)",
#     y = "Migratory species per checklist",
#     title = "Migratory species richness vs urbanization"
#   ) +
#   theme_minimal()
# 
# library(sjPlot)
# library(performance)
# library(ggeffects)
# 
# library(MASS)
# 
# dat_clean <- dat %>%
#   mutate(
#     nightlight      = as.numeric(nightlight),
#     elevation       = as.numeric(elevation),
#     med_hh_income   = as.numeric(med_hh_income),
#     housing_units   = as.numeric(housing_units),
#     pop_density     = as.numeric(pop_density),
#     housing_density = as.numeric(housing_density),
#     NatWalkInd      = ifelse(NatWalkInd < 0, NA, NatWalkInd),
#     n_records_migrant = replace(n_records_migrant, is.na(n_records_migrant), 0)
#   )
# 
# m1 <- glm(
#   n_species ~ med_hh_income + NatWalkInd + ndvi + imp_surf +
#     nightlight + bio1 + bio12 + effort_distance_km + duration_minutes,
#   data = dat_clean,
#   family = "poisson"
# )
# 
# m1 <- gam(
#   n_species ~ med_hh_income + NatWalkInd + ndvi + imp_surf +
#     nightlight + bio1 + bio12 + effort_distance_km + duration_minutes,
#   data = dat_clean,
#   family = "poisson"
# )
# check_model(m1)
# summary(m1)
# 
# m1_nb <- MASS::glm.nb(
#   n_species ~ med_hh_income + NatWalkInd + ndvi + imp_surf +
#     nightlight + bio1 + bio12 + effort_distance_km + duration_minutes,
#   data = dat_clean
# )
# check_model(m1_nb)
# 
# plot_model(m1_nb, type = "est", show.values = TRUE) +
#   theme_minimal() +
#   ggtitle("Socio-ecological predictors of species richness")
# 
# plot_model(m1_nb, type = "pred", terms = "med_hh_income") +
#   theme_minimal() +
#   ggtitle("Luxury effect: Income vs. Species Richness")
# 
# plot_model(m1_nb, type = "pred", terms = "NatWalkInd") +
#   theme_minimal() +
#   ggtitle("Walkability effect on species richness")
# 
# plot_model(m1_nb, type = "pred", terms = "nightlight") +
#   theme_minimal() +
#   ggtitle("Light pollution and biodiversity")
# 
# 
# m2_nb <- MASS::glm.nb(
#   n_migrant_species ~ med_hh_income + NatWalkInd + ndvi +
#     imp_surf + bio1 + bio12 + nightlight + duration_minutes,
#   data = dat_clean
# )
# 
# plot_model(m2_nb, type="est", show.values = TRUE) +
#   ggtitle("Drivers of Migratory Species Richness")
# 
# 
# m3_nb <- MASS::glm.nb(
#   n_records ~ med_hh_income + NatWalkInd + pop_density +
#     housing_density + nightlight + bio1 + duration_minutes,
#   data = dat_clean
# )
# 
# plot_model(m3_nb, type="est", show.values=TRUE) +
#   ggtitle("What predicts birding effort (number of records)?")
# 
# ggplot(dat_clean, aes(med_hh_income, n_species)) +
#   geom_point(alpha=0.2) +
#   geom_smooth(method="lm") +
#   theme_minimal() +
#   labs(title="Species Richness vs. Income",
#        x="Median Household Income", y="Species Richness")
# 
# 
# ggplot(dat_clean, aes(nightlight, n_migrant_species)) +
#   geom_point(alpha=0.2) +
#   geom_smooth(method="lm") +
#   theme_minimal() +
#   labs(title="Migratory Species vs. Light Pollution")
# 
# 
# # Cut continuous variables into bins for plotting
# dat_plot <- dat %>%
#   mutate(
#     ndvi_bin      = cut(ndvi, breaks = quantile(ndvi, probs = seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE),
#     night_bin     = cut(nightlight, breaks = quantile(nightlight, probs = seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE)
#   )
# 
# ggplot(dat_plot, aes(x = ndvi_bin, y = night_bin, fill = n_species)) +
#   stat_summary_2d(fun = mean) +
#   scale_fill_viridis_c(option = "C") +
#   labs(
#     x = "NDVI quintiles (greenness)",
#     y = "Nightlight quintiles (anthropogenic light)",
#     fill = "Mean species richness",
#     title = "Interaction of greenness and nightlight on urban bird richness"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# 
# 
# 
# dat_year <- dat %>%
#   mutate(year = lubridate::year(observation_date)) %>%
#   group_by(year) %>%
#   summarise(
#     mean_richness         = mean(n_species, na.rm = TRUE),
#     mean_migrant_richness = mean(n_migrant_species, na.rm = TRUE),
#     n_checklists          = n(),
#     .groups = "drop"
#   )
# 
# ggplot(dat_year, aes(x = year, y = mean_richness)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     x = "Year",
#     y = "Mean checklist species richness",
#     title = "Temporal trend in checklist-level species richness"
#   ) +
#   theme_minimal()
# 
# 
# 
# ## ============================================================
# # raster extract and avonet the ebd data with terra: 
# ## ============================================================
# 
# library(parallel)
# library(terra)
# terraOptions(
#   threads  = max(1, detectCores() - 1),  # number of CPU threads
#   parallel = TRUE,                       # allow parallel processing where supported
#   progress = 3                           # nice progress bar for long ops
# )
# # make sure CRS matches and is projected (not lon/lat)
# bg_v   <- vect(bg_covs)  # polygons → SpatVector
# smp_v  <- vect(smp_sf)   # points   → SpatVector
# 
# tic("terra extract: checklists to block groups")
# # ex_df <- terra::extract(bg_v, smp_v, fun = mean, na.rm = TRUE)
# 
# # x = polygons, y = points
# smp_bg_v <- terra::extract(
#   x   = bg_v,   # polygons with econ vars
#   y   = smp_v  # checklist points
#   # ,  #
#   # bind = TRUE
#   # ,  # return a SpatVector of points with polygon attributes
#   # ID   = FALSE  # don't add the ID column
# )
# 
# toc()
# 
# smp_df <- as.data.frame(smp_v) %>%     # no geom= argument
#   mutate(id.y = dplyr::row_number())   # 1, 2, 3, ... 344112
# 
# # econ vars from extract()
# econ_df <- as.data.frame(smp_bg_v)
# 
# # join econ vars onto each checklist row
# smp_full <- smp_df %>%
#   left_join(econ_df, by = "id.y")
# 
# 
# # Here also add number of unique records 
# checklist_richness <- ebd %>%
#   group_by(sampling_event_identifier) %>%
#   summarise(
#     species_richness = n_distinct(scientific_name),
#     n_obs_per_checklist = n(),
#     .groups = "drop"
#   )
# 
# smp_full <- smp_full %>%
#   left_join(checklist_richness, by = "sampling_event_identifier")
# 
# # Hasta aca llegue:
# terraOptions(progress = 1)
# 
# 
# l_raw <- blackmarbler::bm_raster(
#   product_id = "VNP46A4",          # example product
#   start_date = "2022-01-01",
#   end_date   = "2022-12-31",
#   tiles      = "h08v05",           # adjust for your area
#   nighttime  = TRUE,
#   api_key    = "YOUR_API_KEY"
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ⃣ Summarise eBird info by census block group (GEOID)
# bg_summary <- smp_full %>%
#   group_by(GEOID) %>%
#   summarise(
#     n_checklists          = n(),
#     n_unique_observers    = n_distinct(observer_id),
#     mean_duration_min     = mean(duration_minutes, na.rm = TRUE),
#     median_duration_min   = median(duration_minutes, na.rm = TRUE),
#     mean_effort_km        = mean(effort_distance_km, na.rm = TRUE),
#     mean_num_observers    = mean(number_observers, na.rm = TRUE),
#     # if you added species_richness above:
#     mean_species_richness = mean(species_richness, na.rm = TRUE),
#     max_species_richness  = max(species_richness, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # You can tack on things like total sampling hours or checklist density per km² if you want, e.g.:
# 
# ggplot(smp_full, aes(x = housing_density, y = species_richness)) +
#   geom_point(alpha = 0.2, size = 1) +
#   geom_smooth(method = "gam", formula = y ~ s(x), color = "blue") +
#   scale_x_log10() +
#   labs(
#     x = "Housing Density (units per km², log scale)",
#     y = "Checklist Species Richness",
#     title = "Species Richness Declines with Increasing Urbanization"
#   ) +
#   theme_minimal()
# 
# ggplot(smp_full, aes(x = med_hh_income, y = species_richness)) +
#   geom_point(alpha = 0.15) +
#   geom_smooth(method = "gam", formula = y ~ s(x), color = "darkgreen") +
#   labs(
#     x = "Median Household Income ($)",
#     y = "Species Richness",
#     title = "The Urban 'Luxury Effect': Higher Richness in Wealthier Block Groups"
#   ) +
#   theme_minimal()
# 
# ggplot(smp_full, aes(x = duration_minutes, y = species_richness)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth(method = "gam") +
#   labs(
#     x = "Checklist Duration (minutes)",
#     y = "Species Richness",
#     title = "Longer Checklists Yield More Species"
#   ) +
#   theme_minimal()
# 
# ggplot(smp_full, aes(x = housing_density)) +
#   geom_histogram(binwidth = 0.1) +
#   scale_x_log10() +
#   labs(
#     x = "Housing Density (log scale)",
#     y = "Number of Checklists",
#     title = "Sampling Bias: More eBird Checklists in Denser Urban Areas"
#   ) +
#   theme_minimal()
# 
# ggplot(smp_full, aes(x = pop_density, y = species_richness)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth(method = "gam", formula = y ~ s(x)) +
#   scale_x_log10() +
#   labs(
#     x = "Population Density (people per km², log scale)",
#     y = "Species Richness",
#     title = "Relationship Between Urban Density and Bird Richness"
#   ) +
#   theme_minimal()
# 
# ggplot(smp_full, aes(x = effort_distance_km, y = species_richness)) +
#   geom_point(alpha = 0.15) +
#   geom_smooth(method = "loess", color = "purple") +
#   scale_x_log10() +
#   labs(
#     x = "Distance Traveled (km, log scale)",
#     y = "Species Richness",
#     title = "Sampling Distance Strongly Influences Species Counts"
#   ) +
#   theme_minimal()
# 
# ggplot(bg_summary, aes(x = med_hh_income, y = mean_species_richness)) +
#   geom_point(size = 2, alpha = 0.7) +
#   geom_smooth(method = "gam", formula = y ~ s(x), color = "orange") +
#   labs(
#     x = "Median Household Income",
#     y = "Mean Checklist Species Richness",
#     title = "Block-group Level Luxury Effect"
#   ) +
#   theme_minimal()
# 
# 
# 
# 
# ## ============================================================
# ## 6. Example: one-row-per-checklist + protocol barplot
# ## ============================================================
# 
# # collapse to one row per checklist
# chk <- ebd |>
#   distinct(
#     sampling_event_identifier,
#     latitude, longitude,
#     observation_date,
#     protocol_code,
#     .keep_all = TRUE
#   )
# 
# chk_proto <- chk |>
#   count(protocol_code, sort = TRUE)
# 
# dim(ebd)
# dim(smp)
# length(unique(ebd$sampling_event_identifier))
# length(unique(smp$sampling_event_identifier))
# 
# # Annotate these variables to both ebd and to census block gorups
# # Load get_social-ecological_vars.R
# 
# 
# smp_sf <- smp |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# 
# # smp_with_bg <- st_join(smp_sf, bg_covs, left = TRUE)
# 
# # spatial join: each checklist gets a block group + econ vars
# tic("Spatial join: checklists to block groups")
# 
# st_crs(smp_sf) == st_crs(bg_covs)
# 
# # tic("Spatial join: checklists to block groups (fast)")
# # smp_bg <- st_join(
# #   smp_sf,
# #   bg_covs,
# #   left    = TRUE,
# #   largest = TRUE  # in case a point is exactly on a boundary
# # )
# # toc()
# 
# 
# # smp_bg <- st_intersection(smp_sf,
# #                           bg_covs
# #                           # ,
# #                           # left = TRUE,
# #                           # largest = TRUE  # in case of boundary touches)
# # )
# # toc()
# 
# # smp_bg <- st_join(
# #   smp_sf,
# #   bg_covs,
# #   left = TRUE,
# #   largest = TRUE  # in case of boundary touches
# # )
# # toc()
# 
# # Annotate econ vars to all eBird records
# econ_by_checklist <- smp_bg |>
#   st_drop_geometry() 
# # |>
# #   select(
# #     sampling_event_identifier,
# #     GEOID,
# #     total_pop,
# #     med_hh_income,
# #     housing_units,
# #     pop_density,
# #     housing_density
# #   )
# 
# ebd_ej <- ebd |>
#   left_join(econ_by_checklist, by = "sampling_event_identifier")
# 
# # smp_bg = checklist-level + EJ variables
# # ebd_ej = every species record carries the block group + econ context
# 
# 
# # link each detection to a block group
# ebd_bg <- ebd_ej |>
#   left_join(
#     smp_bg |>
#       st_drop_geometry() |>
#       select(sampling_event_identifier, GEOID),
#     by = "sampling_event_identifier"
#   ) |>
#   filter(!is.na(GEOID))  # drop any checklists outside your bg_covs
# 
# # biodiversity summary per block group
# bg_bio_summary <- ebd_bg |>
#   group_by(GEOID) |>
#   summarise(
#     n_checklists      = n_distinct(sampling_event_identifier),
#     n_records         = n(),  # total species–checklist rows
#     species_richness  = n_distinct(scientific_name),
#     .groups = "drop"
#   )
# 
# # join back onto bg_covs to keep geometry + econ variables
# bg_ej <- bg_covs |>
#   left_join(bg_bio_summary, by = "GEOID")
# 
# 
# # bg_ej is now an sf object with:
# #   
# #   Econ: med_hh_income, housing_density, pop_density, etc.
# # 
# # Biodiv: n_checklists, n_records, species_richness
# 
# 
# 
# # Save annotated dataset
# 
# 
# 
# # ggplot(chk_proto, aes(x = reorder(protocol_code, n), y = n)) +
# #   geom_col() +
# #   coord_flip() +
# #   labs(
# #     x     = "Protocol",
# #     y     = "Number of checklists",
# #     title = "Checklists by protocol type (Alameda + Contra Costa)"
# #   )
# # 
# # 

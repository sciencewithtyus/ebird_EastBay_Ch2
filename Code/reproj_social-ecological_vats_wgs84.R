# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# The aim of this script is to reproject all remote sensing variables
# # reproject social-ecological_vars to wgs84
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
library(FedData)
library(raster)
library(blackmarbler)
library(tidyterra)
library(tidycensus)
library(dplyr)
library(sf)
library(tidyr)
library(geodata)
library(tidyr)
library(terra)
library(tictoc)
library(purrr)
terraOptions(progress = 1)


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

# --- --- --- --- --- --- --- --- ---
# Spring Migration:
# --- --- --- --- --- --- --- --- ---

spring_migrate <- rast('../../spring_stopover_2500_v9.tif')
bg_v_crs <- project(bg_v, spring_migrate)
spring_migrate_crop <- crop(spring_migrate, bg_v_crs)
spring_migrate_mask <- mask(spring_migrate_crop, bg_v_crs)
spring_migrate_mask_trim <- trim(spring_migrate_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
spring_migrate_mask_trim_crop <- project(
  spring_migrate_mask_trim,
  wgs84
  # ,method = "near"  # categorical raster (impervious % / classes)
)
dir.create('../../Reprojected_socio-eco-vars')
writeRaster(
  spring_migrate_mask_trim_crop,
  "../../Reprojected_socio-eco-vars/spring_stopover_2500_v9_clip_wgs84.tif",
  overwrite = TRUE
)

# --- --- --- --- --- --- --- --- ---
# Chelsa Bioclim: Temperature and Precipitation
# --- --- --- --- --- --- --- --- ---

bio1  <- rast('../../CHELSA_bio1_1981-2010_V.2.1.tif')  # annual mean temp
bg_v_crs <- project(bg_v, bio1)
bio1_crop <- crop(bio1, bg_v_crs)
bio1_mask <- mask(bio1_crop, bg_v_crs)
bio1_mask_trim <- trim(bio1_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
bio1_mask_trim_crop <- project(
  bio1_mask_trim,
  wgs84
  # ,method = "near"  # categorical raster (impervious % / classes)
)

writeRaster(
  bio1_mask_trim_crop,
  "../../Reprojected_socio-eco-vars/bio1_mask_trim_crop_clip_wgs84.tif",
  overwrite = TRUE
)


bio12 <- rast('../../CHELSA_bio12_1981-2010_V.2.1.tif') # annual precip
bg_v_crs <- project(bg_v, bio12)
bio12_crop <- crop(bio12, bg_v_crs)
bio12_mask <- mask(bio12_crop, bg_v_crs)
bio12_mask_trim <- trim(bio12_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
bio12_mask_trim_crop <- project(
  bio12_mask_trim,
  wgs84
  # ,method = "near"  # categorical raster (impervious % / classes)
)

writeRaster(
  bio12_mask_trim_crop,
  "../../Reprojected_socio-eco-vars/bio12_mask_trim_crop_clip_wgs84.tif",
  overwrite = TRUE
)

# --- --- --- --- --- --- --- --- ---
# Elevation
# --- --- --- --- --- --- --- --- ---
elev <- rast('../../output_SRTMGL1.tif')
bg_v_crs <- project(bg_v, elev)
elev_crop <- crop(elev, bg_v_crs)
elev_mask <- mask(elev_crop, bg_v_crs)
elev_mask_trim <- trim(elev_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
elev_mask_trim_crop <- project(
  elev_mask_trim,
  wgs84
  # ,method = "near"  # categorical raster (impervious % / classes)
)

writeRaster(
  elev_mask_trim_crop,
  "../../Reprojected_socio-eco-vars/elev_mask_trim_crop_clip_wgs84.tif",
  overwrite = TRUE
)

# --- --- --- --- --- --- --- --- ---
# NDVI
# --- --- --- --- --- --- --- --- ---
ndvi <- rast('../../SF_EastBay_NDVI_Sentinel_10_v4.tif')
bg_v_crs <- project(bg_v, ndvi)
ndvi_crop <- crop(ndvi, bg_v_crs)
ndvi_mask <- mask(ndvi_crop, bg_v_crs)
ndvi_mask_trim <- trim(ndvi_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
ndvi_mask_trim_crop <- project(
  ndvi_mask_trim,
  wgs84
  # ,method = "near"  # categorical raster (impervious % / classes)
)

writeRaster(
  ndvi_mask_trim_crop,
  "../../Reprojected_socio-eco-vars/ndvi_mask_trim_crop_clip_wgs84.tif",
  overwrite = TRUE
)

# --- --- --- --- --- --- --- --- ---
# Impervious surface
# --- --- --- --- --- --- --- --- ---
nlcd_impervious <- rast('/Users/diegoellis/Downloads/nlcd_2021_impervious_l48_20230630/nlcd_2021_impervious_l48_20230630.img')
bg_v_crs <- project(bg_v, nlcd_impervious)
nlcd_impervious_crop <- crop(nlcd_impervious, bg_v_crs)
nlcd_impervious_mask <- mask(nlcd_impervious_crop, bg_v_crs)
nlcd_impervious_mask_trim <- trim(nlcd_impervious_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
nlcd_impervious_mask_trim_crop <- project(
  nlcd_impervious_mask_trim,
  wgs84
  # ,method = "near"  # categorical raster (impervious % / classes)
)

writeRaster(
  nlcd_impervious_mask_trim_crop,
  "../../Reprojected_socio-eco-vars/nlcd_impervious_mask_trim_crop_clip_wgs84.tif",
  overwrite = TRUE
)


# --- --- --- --- --- --- --- --- ---
# NLCD Landcover
# --- --- --- --- --- --- --- --- ---
nlcd_landcover  <- rast('../../EastBay_NLCD2021Export.tif')
bg_v_crs <- project(bg_v, nlcd_landcover)
nlcd_landcover_crop <- crop(nlcd_landcover, bg_v_crs)
nlcd_landcover_mask <- mask(nlcd_landcover_crop, bg_v_crs)
nlcd_landcover_mask_trim <- trim(nlcd_landcover_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
nlcd_landcover_mask_trim_crop <- project(
  nlcd_landcover_mask_trim,
  wgs84,
  method = "near" #  Reproject to WGS84 using nearest neighbor (categorical)
  # ,method = "near"  # categorical raster (impervious % / classes)
)

writeRaster(
  nlcd_landcover_mask_trim_crop[[1]],
  "../../Reprojected_socio-eco-vars/nlcd_landcover_mask_trim_crop_clip_wgs84.tif",
  overwrite = TRUE
)


# --- --- --- --- --- --- --- --- ---
# Open Space Annotated to CBG
# --- --- --- --- --- --- --- --- ---



bg_sf <- bg_covs |> st_as_sf() |> st_transform(4326)
# Here we use bg_sf as the template, and ask specifically for the "Public_Access" layer. FedData will query the PAD-US web service and return only features that intersect your template area.
padus_list <- get_padus(
  template       = bg_sf,              # your East Bay block groups
  label          = "east_bay_bg",      # anything; used in cache folder name
  layer          = "Public_Access",    # <- key bit: public access layer
  extraction.dir = file.path("data", "FedData", "padus_east_bay"),
  force.redo     = FALSE               # TRUE if you want to re-download
)
names(padus_list)
padus_pa <- padus_list[["Public_Access"]]
# make sure it's WGS84 as well
padus_pa <- st_transform(padus_pa, 4326)

plot(st_geometry(bg_sf), border = 'grey')
plot(st_geometry(padus_pa), add = TRUE, col = rgb(0, 1, 0, 0.4))

# 3. Compute % open space per block group from PAD-US

bg_laea <- bg_sf |>
  st_transform(laea_ca) |>
  st_make_valid()


padus_laea <- padus_pa |>
  st_transform(laea_ca) |>
  st_make_valid()

## 2. Simple (slower) spatial intersection ----
# This intersects every PAD-US polygon with every block group
# and keeps only overlapping pieces

padus_bg <- st_intersection(bg_laea, padus_laea)

padus_bg_area <- padus_bg |>
  mutate(open_area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarise(open_area_km2 = sum(open_area_km2), .groups = "drop")

bg_covs_open <- bg_laea |>
  left_join(padus_bg_area, by = "GEOID") |>
  mutate(
    open_area_km2 = tidyr::replace_na(open_area_km2, 0),
    open_frac     = open_area_km2 / area_km2
  ) |>
  st_transform(4326)  # back to WGS84 to match smp_v / eBird

bg_v_with_open <- vect(bg_covs_open)
bg_v_with_open


writeVector(
  bg_v_with_open,
  filename = "../../Reprojected_socio-eco-vars/census_bg_v_with_open_space_terra_vect.gpkg",
  filetype = "GPKG",
  overwrite = TRUE
)

# --- --- --- --- --- --- --- --- ---
# # Nightlights 2022-2024 Black Marble
# --- --- --- --- --- --- --- --- ---
viirs = rast("../../EastBay_VIIRS_meanNightlights_2022_2024.tif")
bg_v_crs <- project(bg_v, viirs)
viirs_crop <- crop(viirs, bg_v_crs)
viirs_mask <- mask(viirs_crop, bg_v_crs)
viirs_mask_trim <- trim(viirs_mask)  # removes outer NA borders

wgs84 <- "EPSG:4326"
viirs_mask_trim_crop <- project(
  viirs_mask_trim,
  wgs84
  # ,method = "near"  # categorical raster (impervious % / classes)
)

writeRaster(
  viirs_mask_trim_crop,
  "../../Reprojected_socio-eco-vars/viirs_mask_trim_crop_clip_wgs84.tif",
  overwrite = TRUE
)

# # Define the NASA bearer token ###
# bearer <- get_nasa_token(username = "diego_ellis_soto",
#                          password = "Atelopus123!")
# 
# ### ROI
# # SpatVector -> sf
# bg_sf <- st_as_sf(bg_v_with_open)  # keeps geometry + attributes
# 
# # Dissolve to a single outline polygon for raster ROI
# bg_outline <- bg_sf |>
#   st_union() |>
#   st_as_sf() |>
#   st_make_valid()  # just in case
# require(lubridate)
# 
# dates_2022_2023 <- seq.Date(
#   from = ymd("2022-01-01"),
#   to   = ymd("2024-12-01"),
#   by   = "month"
# )
# 
# ntl_monthly_rast <- bm_raster(
#   roi_sf    = bg_outline,
#   product_id = "VNP46A3",
#   date       = dates_2022_2023,
#   bearer     = bearer
# )
# 
# bg_sf$ntl_mean_2022_2024 <- bm_extract(
#   roi_sf          = bg_sf,       # ⬅️ each block group polygon
#   product_id      = "VNP46A4",   # annual product
#   date            = 2022:2024,   # study years
#   aggregation_fun = "mean",
#   bearer          = bearer
# )
# 
# 
# # Define the region of interest (roi). Must be an sf polygon
# # Must also be in WGS 84 CRS (espg: 4326)
# # select region if it needs to be specific country using roi_sf = gadm
# roi_sf <- gadm(country)

# Making raster of nighttime lights

# # Add nightlights using black marble
# 
# bg_df <- bg_covs_open |>
#   st_drop_geometry()
# 
# require(ggplot2)
# gg_open_frac <- ggplot(bg_covs_open) +
#   geom_sf(aes(fill = open_frac), color = NA) +
#   scale_fill_viridis_c(option = "C", direction = 1, na.value = "grey80") +
#   labs(
#     title = "Fraction of Block Group in PAD-US Open Space",
#     fill  = "Open space\nfraction"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_line(color = NA),
#     axis.text = element_blank(),
#     axis.title = element_blank()
#   )
# 
# gg_open_frac
# 
# gg_income <- ggplot(bg_covs_open) +
#   geom_sf(aes(fill = med_hh_income), color = NA) +
#   scale_fill_viridis_c(option = "B", na.value = "grey80") +
#   labs(
#     title = "Median Household Income",
#     fill  = "Income (USD)"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_line(color = NA),
#     axis.text = element_blank(),
#     axis.title = element_blank()
#   )
# 
# gg_income
# 
# gg_housing_density <- ggplot(bg_covs_open) +
#   geom_sf(aes(fill = housing_density), color = NA) +
#   scale_fill_viridis_c(option = "D", trans = "log10", na.value = "grey80") +
#   labs(
#     title = "Housing Unit Density",
#     fill  = "Units per km²\n(log scale)"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_line(color = NA),
#     axis.text = element_blank(),
#     axis.title = element_blank()
#   )
# 
# gg_housing_density
# 
# library(patchwork)
# 
# (gg_open_frac | gg_income | gg_housing_density)
# 
# gg_open_income <- ggplot(bg_df, aes(x = med_hh_income, y = open_frac)) +
#   geom_point(alpha = 0.4, size = 1) +
#   geom_smooth(method = "loess", se = FALSE, color = "black") +
#   labs(
#     x = "Median household income (USD)",
#     y = "Fraction of block group in open space",
#     title = "Open Space vs Income"
#   ) +
#   theme_minimal()
# 
# gg_open_income
# 
# 
# # 
# # 
# # ## 4. Compute open-space area per block group (km²) ----
# # padus_bg_area <- padus_bg |>
# #   mutate(open_area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
# #   st_drop_geometry() |>
# #   group_by(GEOID) |>
# #   summarise(open_area_km2 = sum(open_area_km2), .groups = "drop")
# # 
# # ## 5. Join back to bg_laea and compute open fraction ----
# # bg_covs_open <- bg_laea |>
# #   left_join(padus_bg_area, by = "GEOID") |>
# #   mutate(
# #     open_area_km2 = replace_na(open_area_km2, 0),
# #     open_frac     = open_area_km2 / area_km2
# #   ) |>
# #   st_transform(4326)  # back to WGS84 to match smp_v, etc.
# # 
# # ## 6. Convert to terra SpatVector ----
# # bg_v_with_open <- vect(bg_covs_open)
# # bg_v_with_open
# # 
# # # 
# # # 
# # # # 
# # # # spring_migrate <- rast('../../spring_stopover_2500_v9.tif')
# # # # bg_v <- vect(bg_covs)
# # # # bg_v_crs <- project(bg_v, spring_migrate)
# # # # Bounding box (extent) of those points in the impervious CRS
# # # # bb_imp <- ext(bg_v_crs)
# # # 
# # # # spring_migrate_crop <- crop(spring_migrate, bg_v_crs)
# # # # store raster: 
# # # 
# # # writeRaster(
# # #   nlcd_imp_crop,
# # #   filename = "nlcd_impervious_wgs84_clip.tif",
# # #   overwrite = TRUE
# # # )
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # crop_reproject_to_bg <- function(r, # raster
# # #                                  bg_covs, # census block group as a spatvector
# # #                                  out_dir    = ".", # outdir
# # #                                  suffix     = "_wgs84_clip",
# # #                                  categorical = FALSE,
# # #                                  do_mask    = TRUE,
# # #                                  verbose    = TRUE) {
# # #   
# # #   # CRS
# # #   target_crs <- crs(bg_covs)  # usually WGS84
# # #   same_crs <- crs(r) == target_crs
# # #   # Path
# # #   in_path <- sources(spring_migrate)
# # #   
# # #   
# # #   if(same_crs){print('i')}else{
# # #     print('Different crs')
# # #     r_proj <- project(r, target_crs, method = method)
# # #     
# # #   }
# # #   
# # #   
# # # }
# # # 
# # # 
# # # 
# # # # Spring migration stopover suitability
# # # spring_migrate <- rast('../../spring_stopover_2500_v9.tif')
# # # r = spring_migrate
# # # 
# # # tools::file_path_sans_ext(basename(spring_migrate))
# # # 
# # # # CRS of impervious surface raster
# # # crs_imp <- crs(nlcd_impervious)
# # # 
# # # # Reproject checklist points into that CRS
# # # # Reproject cbg points into that CRS
# # # # smp_imp_crs <- project(smp_v, crs_imp)
# # # bg_v <- vect(bg_covs)
# # # bg_v_crs <- project(bg_v, crs_imp)
# # # 
# # # # Bounding box (extent) of those points in the impervious CRS
# # # # bb_imp <- ext(smp_imp_crs)
# # # bb_imp <- ext(bg_v_crs)
# # # 
# # # # nlcd_impervious_crop <- crop(nlcd_impervious, bb_imp)
# # # nlcd_impervious_crop <- crop(nlcd_impervious, bb_imp)
# # # 
# # # wgs84 <- "EPSG:4326"
# # # nlcd_imp_crop <- project(
# # #   nlcd_impervious_crop,
# # #   wgs84
# # #   # ,method = "near"  # categorical raster (impervious % / classes)
# # # )
# # # # store raster: 
# # # 
# # # writeRaster(
# # #   nlcd_imp_crop,
# # #   filename = "nlcd_impervious_wgs84_clip.tif",
# # #   overwrite = TRUE
# # # )
# # # 
# # # spring_migrate   <- project(spring_migrate,   wgs84)
# # # bio1_temp        <- project(bio1_temp,        wgs84)
# # # bio12_rain       <- project(bio12_rain,       wgs84)
# # # elev             <- project(elev,             wgs84)
# # # ndvi_wet         <- project(ndvi_wet,         wgs84)
# # # ndvi_dry         <- project(ndvi_dry,         wgs84)
# # # ndvi_full        <- project(ndvi_full,        wgs84)
# # # nlcd_landcover   <- project(nlcd_landcover,   wgs84)
# # # nlcd_impervious  <- project(nlcd_impervious,  wgs84)
# # # # Save the reprojected raster stack:
# # # 
# # # env_stack <- c(
# # #   spring_migrate,
# # #   bio1_temp,
# # #   bio12_rain,
# # #   elev,
# # #   ndvi_wet,
# # #   ndvi_dry,
# # #   ndvi_full,
# # #   nlcd_landcover,
# # #   nlcd_impervious,
# # #   nightlights
# # # )
# # # 
# # # names(env_stack) <- c(
# # #   "spring_migrate",
# # #   "bio1_temp",
# # #   "bio12_rain",
# # #   "elev",
# # #   "ndvi_wet",
# # #   "ndvi_dry",
# # #   "ndvi_full",
# # #   "nlcd_landcover",
# # #   "nlcd_impervious",
# # #   "nightlights"
# # # )

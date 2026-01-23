# Load the reprojected and cropped raster (much more memory efficient)
list.files('../../Reprojected_socio-eco-vars/')

# Annotate walkability and others 

# Add tic toc to the annotation 
# andprogress bars

# Avonet left_join all ebird
# Raster extract to all checklists
# Group_by summarize checklist info at the census block gorup

# Annotate at the:

# EBD AvoNet
# Checklist location socio-ecological anotation
# Census block group annotations




## --- Libraries -----------------------------------------------------------
library(raster)
library(blackmarbler)
library(tidyterra)
library(tidycensus)
library(dplyr)
library(sf)
library(tidyr)
library(geodata)
library(tidyr)

## --- Rasters -------------------------------------------------------------
# Spring migration stopover suitability
spring_migrate <- rast('../../spring_stopover_2500_v9.tif')
crs(spring_migrate)

# CHELSA bioclim
bio1_temp  <- rast('../../CHELSA_bio1_1981-2010_V.2.1.tif')  # annual mean temp
bio12_rain <- rast('../../CHELSA_bio12_1981-2010_V.2.1.tif') # annual precip

# Elevation
elev <- rast('../../output_SRTMGL1.tif')
crs(elev)

# NDVI (you can decide which to use in the model)
ndvi_wet <- rast('../../BayArea_NDVIExportWet2022_2023.tif')
ndvi_dry <- rast('../../BayArea_NDVIExportDry2022_2023.tif')
ndvi_full <- rast('../../SF_EastBay_NDVI_Sentinel_10_v4.tif')

# Land cover & impervious surface
nlcd_landcover  <- rast('../../EastBay_NLCD2021Export.tif')
nlcd_impervious <- rast('/Users/diegoellis/Downloads/nlcd_2021_impervious_l48_20230630/nlcd_2021_impervious_l48_20230630.img')


## --- Vector EJ layers ----------------------------------------------------
# CalEnviroScreen (traffic etc.)
calenviro_traffic <- read_sf('../../calenviroscreen.v4/CES4 Final Shapefile.shp') %>%
  filter(County %in% c("Alameda", "Contra Costa")) %>%
  st_transform(4326)

# National walkability index
# (you may need to specify layer= if sf prompts you)
walkability_sf <- st_read('../../WalkabilityIndex/Natl_WI.gdb') %>%
  st_transform(4326)


# vector of variables we want
acs_vars <- c(
  total_pop      = "B01003_001",  # total population
  med_hh_income  = "B19013_001",  # median household income (dollars)
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


# same variables as before
acs_vars <- c(
  total_pop      = "B01003_001",  # total population
  med_hh_income  = "B19013_001",  # median household income
  housing_units  = "B25001_001"   # total housing units
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



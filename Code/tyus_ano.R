# --- --- --- --- --- --- --- --- --- --- --- --- ---
# 
# --- --- --- --- --- --- --- --- --- --- --- --- ---

require(tidyverse)
require(sf)
require(terra)
require(raster)
require(sp)
library(FedData)
require(mapview)
setwd('/Users/diegoellis/Desktop/Projects/Postdoc/UrbanEcology_BayArea/')
source('Code/Functions/get_income_popden_race.R')

uwin = read.csv('/Users/diegoellis/Downloads/MSTR_UWIN_sites (1).csv') |> 
  dplyr::select(Site.Names, Lat, Long) |> rename(Name = Site.Names) |>
  dplyr::mutate(states_abbrev ='CA')
  
uwin_sf = uwin |> 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

uwin_sf_buf = uwin_sf |> st_buffer(1000) 

# mapview(uwin_sf)

# National Landocver 
nldc_landcover <- rast(
  "/Users/diegoellis/Desktop/Projects/Postdoc/OSM_for_Ecology/land_cover_2020v2_30m_tif/NA_NALCMS_landcover_2020v2_30m/data/NA_NALCMS_landcover_2020v2_30m.tif"
) 

uwin_sf_buf_sp_landcov <- as(st_transform(st_as_sf(uwin_sf_buf), crs(nldc_landcover)),'Spatial')
uwin_sf_buf_sp_landcov_vect = vect(uwin_sf_buf_sp_landcov)

uwin_sf_buf_sp_landcov_vect$nldc_landcover = terra::extract(nldc_landcover,
                                                              uwin_sf_buf_sp_landcov_vect)[,2]
p_landcover = as.tibble(uwin_sf_buf_sp_landcov_vect) |>
  dplyr::select(Name,
                nldc_landcover) |> 
  as.tibble() 


# Pop and housing density
uwin_sf_pop_hous_dens = pop_housing_density(uwin, 1000) |> dplyr::select(-buffer_size)

# Imp surf
ncld_imp_surf_2023 <- raster('/Users/diegoellis/Downloads/nlcd_2021_impervious_l48_20230630/nlcd_2021_impervious_l48_20230630.img')

uwin_sf_buf_tmp_sp <- as(st_transform(st_as_sf(uwin_sf_buf),
                                    crs(ncld_imp_surf_2023)),'Spatial')

uwin_sf_buf_tmp_sp$impervious_furface = raster::extract(ncld_imp_surf_2023,
                                                        uwin_sf_buf_tmp_sp,
                                                        fun=mean, na.rm = TRUE)[,1]

uwin_sf_buf_impervious_surface_percent = as.tibble(uwin_sf_buf_tmp_sp) |>
  dplyr::mutate(imp_surf = impervious_furface) |> 
  dplyr::select(Name, imp_surf) |> as.tibble() 

# left_join
uwin_anno_v1 = uwin |> 
  left_join(p_landcover) |>
  left_join(uwin_sf_pop_hous_dens) |>
  left_join(uwin_sf_buf_impervious_surface_percent)


# Bioclim 1
human_mod_americas_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/hmod_americas_masked.tif')
bio1_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/bio1_americas_masked.tif')
bio_precip = raster('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/CHELSA_pr_12_1981-2010_V.2.1.tif')




uwin_sf_buf$human_mod = raster::extract(human_mod_americas_masked,
                                                    uwin_sf_buf,
                                                    fun=mean, na.rm = TRUE)[,1]

uwin_sf_buf$bio_1 = raster::extract(bio1_masked,
                                    uwin_sf_buf,
                                    fun=mean, na.rm = TRUE)[,1]

uwin_sf_buf$bio_12 = extract(bio_precip, 
                             uwin_sf_buf,
                             fun=mean, na.rm = TRUE)[,1]

hum_mod_bio_1_bio12 = uwin_sf_buf |> as.tibble() |>
  dplyr::select(Name, bio_1, bio_12, human_mod) 

# left_join
uwin_anno_v2 = uwin_anno_v1 |> 
  left_join(hum_mod_bio_1_bio12)

elev = raster('/Users/diegoellis/Downloads/output_SRTMGL1.tif')

uwin_sf_buf$elev = raster::extract(elev,
                                        uwin_sf_buf,
                                        fun=mean, na.rm = TRUE)[,1]

elev_tmp = uwin_sf_buf

# left_join
uwin_anno_v3 = uwin_anno_v2 |> 
  left_join(elev_tmp[,c('Name','elev')]) |> as_tibble()

uwin_anno_v3_df = data.frame(uwin_anno_v3) |> 
  dplyr::select(- geometry)

write.csv(uwin_anno_v3_df, file = '/Users/diegoellis/Desktop/uwin_anno_tyus_20250328_v2.csv')

write.csv(uwin_anno_v3, file = '/Users/diegoellis/Desktop/uwin_anno_tyus.csv')

# ADD TO THE PRESIDIO !!!!

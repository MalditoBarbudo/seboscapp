## code to prepare `all_data` dataset goes here

# sources
source('data-raw/app_translations.R')

municipalities <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp'
) %>%
  dplyr::select(municipality_id = CODIMUNI, admin_municipality = NOMMUNI, geometry)
regions <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp'
) %>%
  dplyr::select(county_id = CODICOMAR, admin_region = NOMCOMAR, geometry)
provinces <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp'
) %>%
  dplyr::select(province_id = CODIPROV, admin_province = NOMPROV, geometry)

municipalities <- municipalities %>%
  rmapshaper::ms_simplify(0.1) %>%
  sf::st_transform(crs = '+proj=longlat +datum=WGS84')
regions <- regions %>%
  rmapshaper::ms_simplify(0.1) %>%
  sf::st_transform(crs = '+proj=longlat +datum=WGS84')
provinces <- provinces %>%
  rmapshaper::ms_simplify(0.1) %>%
  sf::st_transform(crs = '+proj=longlat +datum=WGS84')

usethis::use_data(
  # local scale
  municipalities, regions, provinces,
  # app_translations
  app_translations,

  internal = TRUE, overwrite = TRUE
)

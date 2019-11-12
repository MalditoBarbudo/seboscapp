## code to prepare `local_scale` dataset goes here
library(sf)
library(tidyverse)

# first, let's load the point data
p1_mushrooms_csv <- readr::read_csv('data-raw/local_scale/csv/P1mushrooms.csv') %>%
  sf::st_as_sf(
    coords = c('X1', 'Y2'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) %>%
  dplyr::rename(p1 = P1)
p2_water_csv <- readr::read_csv('data-raw/local_scale/csv/P2water.csv') %>%
  dplyr::filter(UTM_X > 0) %>%
  sf::st_as_sf(
    coords = c('UTM_X', 'UTM_Y'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) %>%
  dplyr::rename(p2 = P2)
r1_carbon_csv <- readr::read_csv('data-raw/local_scale/csv/R1Carbon.csv') %>%
  sf::st_as_sf(
    coords = c('UTM_X', 'UTM_Y'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) %>%
  dplyr::rename(r1 = R1)

# now the grid data, as points also
c1_animals <- stars::read_stars('data-raw/local_scale/c1/w001001.adf') %>%
  stars::as_tibble.stars() %>%
  sf::st_as_sf(
    coords = c('x', 'y'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) %>%
  dplyr::rename(c1 = w001001.adf) %>%
  dplyr::filter(!is.na(c1))

r2_soilc <- stars::read_stars('data-raw/local_scale/r2/w001001.adf') %>%
  stars::as_tibble.stars() %>%
  sf::st_as_sf(
    coords = c('x', 'y'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) %>%
  dplyr::rename(r2 = w001001.adf) %>%
  dplyr::filter(!is.na(r2))

r3_riparian <- stars::read_stars('data-raw/local_scale/r3/w001001.adf') %>%
  stars::as_tibble.stars() %>%
  sf::st_as_sf(
    coords = c('x', 'y'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) %>%
  dplyr::rename(r3 = w001001.adf) %>%
  dplyr::filter(!is.na(r3))

r4_erosion <- stars::read_stars('data-raw/local_scale/r4/w001001.adf') %>%
  stars::as_tibble.stars() %>%
  sf::st_as_sf(
    coords = c('x', 'y'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) %>%
  dplyr::rename(r4 = w001001.adf) %>%
  dplyr::filter(!is.na(r4))

municipalities <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp'
) %>%
  dplyr::select(municipality_id = CODIMUNI, municipality_name = NOMMUNI, geometry)
counties <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp'
) %>%
  dplyr::select(county_id = CODICOMAR, county_name = NOMCOMAR, geometry)
provinces <- sf::read_sf(
  '../../01_nfi_app/NFIappkg/data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp'
) %>%
  dplyr::select(province_id = CODIPROV, province_name = NOMPROV, geometry)

all_datasets_as_points <- list(
  c1 = c1_animals,
  p1 = p1_mushrooms_csv,
  p2 = p2_water_csv,
  r1 = r1_carbon_csv,
  r2 = r2_soilc,
  r3 = r3_riparian,
  r4 = r4_erosion
)

plot_to_poly_classificator <- function(plots_data, polygons_to_check, names_polys, name_var) {

  intersections_list <- polygons_to_check %>%
    sf::st_intersects(plots_data)
  names(intersections_list) <- names_polys
  plots_data[[name_var]] <- NA
  for (name in names_polys) {
    plots_data[intersections_list[[name]], name_var] <- name
  }

  plots_data
}

all_datasets_classified_by_admin_div <- all_datasets_as_points %>%
  purrr::map(
    .f = ~ plot_to_poly_classificator(
      .x, municipalities, municipalities$municipality_name, 'admin_municipality'
    )
  ) %>%
  purrr::map(
    .f = ~ plot_to_poly_classificator(
      .x, counties, counties$county_name, 'admin_region'
    )
  ) %>%
  purrr::map(
    .f = ~ plot_to_poly_classificator(
      .x, provinces, provinces$province_name, 'admin_province'
    )
  ) %>%
  purrr::map(
    .f = sf::st_transform, crs = '+proj=longlat +datum=WGS84'
  )

c1_data <- all_datasets_classified_by_admin_div[['c1']]
p1_data <- all_datasets_classified_by_admin_div[['p1']]
p2_data <- all_datasets_classified_by_admin_div[['p2']]
r1_data <- all_datasets_classified_by_admin_div[['r1']]
r2_data <- all_datasets_classified_by_admin_div[['r2']]
r3_data <- all_datasets_classified_by_admin_div[['r3']]
r4_data <- all_datasets_classified_by_admin_div[['r4']]

usethis::use_data(
  c1_data, p1_data, p2_data, r1_data, r2_data, r3_data, r4_data,

  internal = TRUE, overwrite = TRUE
)

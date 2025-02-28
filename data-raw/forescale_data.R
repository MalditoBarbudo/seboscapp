## code to prepare `local_scale` dataset goes here
library(sf)
library(stars)
library(tidyverse)

# first, let's load the point data
p1_mushrooms_csv <-
  readr::read_csv('data-raw/local_scale/csv/P1mushrooms.csv') |>
  dplyr::rename(mushrooms_production = P1, plot_id = ID, x = X1, y = Y2) |>
  dplyr::select(mushrooms_production, x, y)

p2_water_csv <- readr::read_csv('data-raw/local_scale/csv/P2water.csv') |>
  dplyr::filter(UTM_X > 0) |>
  dplyr::rename(exported_water = P2, plot_id = cod, x = UTM_X, y = UTM_Y) |>
  dplyr::select(exported_water, x, y)

r1_carbon_csv <- readr::read_csv('data-raw/local_scale/csv/R1Carbon.csv') |>
  dplyr::rename(
    carbon_sequestration = R1, plot_id = IDPARCELA, x = UTM_X, y = UTM_Y
  ) |>
  dplyr::select(carbon_sequestration, x, y)

# now the grid data, as points also
c1_animals <- terra::rast('data-raw/local_scale/c1/w001001.adf') |>
  terra::as.points() |>
  sf::st_as_sf(
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ',
    as_points = TRUE
  ) |>
  dplyr::rename(animals_presence = w001001) |>
  dplyr::filter(!is.na(animals_presence)) |>
  dplyr::mutate(plot_id = dplyr::row_number(animals_presence)) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    x = round(as.numeric(st_coordinates(geometry)[,1]), 0),
    y = round(as.numeric(st_coordinates(geometry)[,2]), 0)
  ) |>
  dplyr::select(animals_presence, x, y) |>
  dplyr::filter(animals_presence > 0)

r2_soilc <- terra::rast('data-raw/local_scale/r2/w001001.adf') |>
  terra::as.points() |>
  sf::st_as_sf(
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ',
    as_points = TRUE
  ) |>
  dplyr::rename(soil_organic_carbon = w001001) |>
  dplyr::filter(!is.na(soil_organic_carbon)) |>
  dplyr::mutate(plot_id = dplyr::row_number(soil_organic_carbon)) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    x = round(as.numeric(st_coordinates(geometry)[,1]), 0),
    y = round(as.numeric(st_coordinates(geometry)[,2]), 0)
  ) |>
  dplyr::select(soil_organic_carbon, x, y)

r3_riparian <- terra::rast('data-raw/local_scale/r3/w001001.adf') |>
  terra::resample(
    terra::rast('data-raw/local_scale/r2/w001001.adf')
  ) |>
  terra::as.points() |>
  sf::st_as_sf(
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ',
    as_points = TRUE
  ) |>
  dplyr::rename(riparian_forest_cover = w001001) |>
  dplyr::filter(!is.na(riparian_forest_cover)) |>
  dplyr::mutate(plot_id = dplyr::row_number(riparian_forest_cover)) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    x = round(as.numeric(st_coordinates(geometry)[,1]), 0),
    y = round(as.numeric(st_coordinates(geometry)[,2]), 0)
  ) |>
  dplyr::select(riparian_forest_cover, x, y)

r4_erosion <- terra::rast('data-raw/local_scale/r4/w001001.adf') |>
  terra::resample(
    terra::rast('data-raw/local_scale/r2/w001001.adf')
  ) |>
  terra::as.points() |>
  sf::st_as_sf(
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ',
    as_points = TRUE
  ) |>
  dplyr::rename(slope_forest_cover = w001001) |>
  dplyr::filter(!is.na(slope_forest_cover)) |>
  dplyr::mutate(plot_id = dplyr::row_number(slope_forest_cover)) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    x = round(as.numeric(st_coordinates(geometry)[,1]), 0),
    y = round(as.numeric(st_coordinates(geometry)[,2]), 0)
  ) |>
  dplyr::select(slope_forest_cover, x, y)

municipalities <- sf::read_sf(
  '../../01_nfi_app/nfiApp/data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp',
) |>
  dplyr::select(municipality_id = CODIMUNI, admin_municipality = NOMMUNI, geometry) |>
  sf::st_transform(crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ')
regions <- sf::read_sf(
  '../../01_nfi_app/nfiApp/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp'
) |>
  dplyr::select(county_id = CODICOMAR, admin_region = NOMCOMAR, geometry) |>
  sf::st_transform(crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ')
provinces <- sf::read_sf(
  '../../01_nfi_app/nfiApp/data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp'
) |>
  dplyr::select(province_id = CODIPROV, admin_province = NOMPROV, geometry) |>
  sf::st_transform(crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ')

# enpe_polygons
natural_interest_areas <-
  sf::read_sf('../../01_nfi_app/nfiApp/data-raw/shapefiles/enpe_2017.shp') |>
  # rmapshaper::ms_simplify(0.01) |>
  sf::st_transform(crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ') |>
  dplyr::select(admin_natural_interest_area = nom, geometry) |>
  dplyr::mutate(dummy = admin_natural_interest_area) |>
  dplyr::group_by(admin_natural_interest_area) |>
  dplyr::summarise(dummy = dplyr::first(dummy)) |>
  dplyr::select(-dummy) |>
  sf::st_cast('MULTIPOLYGON')

# peins
special_protection_natural_areas <-
  sf::read_sf('../../01_nfi_app/nfiApp/data-raw/shapefiles/pein_2017.shp') |>
  # rmapshaper::ms_simplify(0.01) |>
  sf::st_transform(crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ') |>
  dplyr::select(admin_special_protection_natural_area = nom, geometry) |>
  dplyr::mutate(dummy = admin_special_protection_natural_area) |>
  dplyr::group_by(admin_special_protection_natural_area) |>
  dplyr::summarise(dummy = dplyr::first(dummy)) |>
  dplyr::select(-dummy) |>
  sf::st_cast('MULTIPOLYGON')

# xn2000_polyogns
natura_network_2000s <-
  sf::read_sf('../../01_nfi_app/nfiApp/data-raw/shapefiles/xn2000_2017.shp') |>
  # rmapshaper::ms_simplify(0.01) |>
  sf::st_transform(crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs ') |>
  dplyr::select(admin_natura_network_2000 = nom_n2, geometry) |>
  dplyr::mutate(dummy = admin_natura_network_2000) |>
  dplyr::group_by(admin_natura_network_2000) |>
  dplyr::summarise(dummy = dplyr::first(dummy)) |>
  dplyr::select(-dummy) |>
  sf::st_cast('MULTIPOLYGON')

plot_to_poly_classificator <- function(plots_data, polygons_to_check, names_polys, name_var) {

  intersections_list <- polygons_to_check |>
    sf::st_intersects(plots_data)
  names(intersections_list) <- names_polys
  plots_data[[name_var]] <- NA_character_
  for (name in names_polys) {
    plots_data[intersections_list[[name]], name_var] <- name
  }

  plots_data
}

forescale_data <-
  list(
    c1 = c1_animals,
    p1 = p1_mushrooms_csv,
    p2 = p2_water_csv,
    r1 = r1_carbon_csv,
    r2 = r2_soilc,
    r3 = r3_riparian,
    r4 = r4_erosion
  ) |>
  purrr::reduce(full_join, by = c('x', 'y')) |>
  sf::st_as_sf(
    coords = c('x', 'y'),
    crs = '+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs '
  ) |>
  plot_to_poly_classificator(
    municipalities, municipalities$admin_municipality, 'admin_municipality'
  ) |>
  plot_to_poly_classificator(
    regions, regions$admin_region, 'admin_region'
  ) |>
  plot_to_poly_classificator(
    provinces, provinces$admin_province, 'admin_province'
  ) |>
  plot_to_poly_classificator(
    natural_interest_areas, natural_interest_areas$admin_natural_interest_area, 'admin_natural_interest_area'
  ) |>
  plot_to_poly_classificator(
    special_protection_natural_areas, special_protection_natural_areas$admin_special_protection_natural_area, 'admin_special_protection_natural_area'
  ) |>
  plot_to_poly_classificator(
    natura_network_2000s, natura_network_2000s$admin_natura_network_2000, 'admin_natura_network_2000'
  ) |>
  st_transform(crs = '+proj=longlat +datum=WGS84') |>
  dplyr::mutate(
    plot_id = paste0(
      "FES_",
      round(sf::st_coordinates(geometry)[,1], 6),
      "_",
      round(sf::st_coordinates(geometry)[,2], 6)
    ) |> stringr::str_remove_all('\\.')
  ) |>
  dplyr::select(plot_id, dplyr::everything())

municipalities <- municipalities |>
  rmapshaper::ms_simplify(0.1) |>
  sf::st_transform(crs = 4326)
regions <- regions |>
  rmapshaper::ms_simplify(0.1) |>
  sf::st_transform(crs = 4326)
provinces <- provinces |>
  rmapshaper::ms_simplify(0.1) |>
  sf::st_transform(crs = 4326)
natural_interest_areas <- natural_interest_areas |>
  rmapshaper::ms_simplify(0.1) |>
  sf::st_transform(crs = 4326)
special_protection_natural_areas <- special_protection_natural_areas |>
  rmapshaper::ms_simplify(0.1) |>
  sf::st_transform(crs = 4326)
natura_network_2000s <- natura_network_2000s |>
  rmapshaper::ms_simplify(0.1) |>
  sf::st_transform(crs = 4326)

library(tidyverse)
library(sf)
library(lfcdata)

nfidb <- nfi()
ifn_plots_data <-
  nfidb$get_data('plots', TRUE) %>%
  filter(presence_NFI_2, presence_NFI_3, presence_NFI_4) %>%
  select(
    plot_id,
    coords_utm_x_ETRS89, coords_utm_y_ETRS89,
    old_idparcela,
    admin_province, admin_region, admin_municipality
  )
dynamic_data <- read_delim('data-raw/completa5.txt', delim = '\t') %>%
  select(-AguaIFN2, -AguaIFN4)
erosion_water_data <- read_delim('data-raw/ErosionAgua.txt', delim = '\t')

# join all togheter, nfi plots to get the plot_id and also know which plots
# remain at the end
joined_data <-
  dynamic_data %>%
  full_join(erosion_water_data, by = c('PLOTS' = 'plot', 'X', 'Y')) %>%
  # remove the duplicated 251187 plots
  filter(!ID %in% c(2894, 2899)) %>%
  mutate(
    PLOTS = as.character(PLOTS),
    PLOTS = stringr::str_replace(PLOTS, '^8', '08')
  ) %>%
  rename(
    # coords_utm_x_ETRS89 = X,
    # coords_utm_y_ETRS89 = Y,
    old_idparcela = PLOTS,
    CarbonoIFN23 = CarbonoIFN2,
    CarbonoIFN34 = CarbonoIFN4,
    ErosIFN2 = Mitigada2,
    ErosIFN3 = Mitigada3,
    ErosIFN4 = Mitigada4,
    WaterIFN2 = Water2,
    WaterIFN3 = Water3,
    WaterIFN4 = Water4,
  ) %>%
  left_join(
    ifn_plots_data,
    by = c("old_idparcela"),
    # by = c("old_idparcela", 'coords_utm_x_ETRS89', "coords_utm_y_ETRS89")
  ) %>%
  select(
    plot_id, everything(), -ID, -old_idparcela,
    -coords_utm_x_ETRS89, -coords_utm_y_ETRS89, -X, -Y
  ) %>%
  # eliminamos aquellas de erosión que no están en los tres ifns
  filter(!is.na(plot_id))

# data treatment
all_together_data <-
  joined_data %>%
  pivot_longer(
    cols = SetasIFN2:WaterIFN4, names_to = 'serveiIFN', values_to = 'values'
  ) %>%
  separate(serveiIFN, c('Servei', 'IFN'), sep = 'I', remove = TRUE) %>%
  mutate(IFN = paste0('I', IFN)) %>%
  arrange(
    plot_id, Servei, IFN, values
  ) %>%
  select(
    plot_id, es_name = Servei, nfi = IFN, values,
    admin_province, admin_region, admin_municipality,
    geometry
  ) %>%
  mutate(
    nfi = case_when(
      nfi == 'IFN2' ~ 'NFI_2',
      nfi == 'IFN3' ~ 'NFI_3',
      nfi == 'IFN4' ~ 'NFI_4',
      nfi == 'IFN23' ~ 'NFI_2_NFI_3',
      nfi == 'IFN34' ~ 'NFI_3_NFI_4',
    ),
    es_name = case_when(
      es_name == 'Water' ~ 'exported_water',
      es_name == 'Carbono' ~ 'soil_organic_carbon',
      es_name == 'Eros' ~ 'erosion_control',
      es_name == 'Setas' ~ 'mushrooms_production',
      es_name == 'Madera' ~ 'wood'
    )
  )

nfi_2_data <-
  all_together_data %>%
  filter(nfi == 'NFI_2') %>%
  pivot_wider(names_from = es_name, values_from = values) %>%
  select(-nfi) %>%
  sf::st_as_sf(sf_column_name = 'geometry')


nfi_3_data <-
  all_together_data %>%
  filter(nfi == 'NFI_3') %>%
  pivot_wider(names_from = es_name, values_from = values) %>%
  select(-nfi) %>%
  sf::st_as_sf(sf_column_name = 'geometry')

nfi_4_data <-
  all_together_data %>%
  filter(nfi == 'NFI_4') %>%
  pivot_wider(names_from = es_name, values_from = values) %>%
  select(-nfi) %>%
  sf::st_as_sf(sf_column_name = 'geometry')

nfi_2_nfi_3_data <-
  all_together_data %>%
  filter(nfi == 'NFI_2_NFI_3') %>%
  pivot_wider(names_from = es_name, values_from = values) %>%
  select(-nfi) %>%
  sf::st_as_sf(sf_column_name = 'geometry')

nfi_3_nfi_4_data <-
  all_together_data %>%
  filter(nfi == 'NFI_3_NFI_4') %>%
  pivot_wider(names_from = es_name, values_from = values) %>%
  select(-nfi) %>%
  sf::st_as_sf(sf_column_name = 'geometry')

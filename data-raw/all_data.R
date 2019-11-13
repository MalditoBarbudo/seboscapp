## code to prepare `all_data` dataset goes here

# sources
source('data-raw/local_scale.R')
source('data-raw/app_translations.R')

usethis::use_data(
  # local scale
  c1_data, p1_data, p2_data, r1_data, r2_data, r3_data, r4_data,
  municipalities_simpl, counties_simpl, provinces_simpl,
  # app_translations
  app_translations,

  internal = TRUE, overwrite = TRUE
)

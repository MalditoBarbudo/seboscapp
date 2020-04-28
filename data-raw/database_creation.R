# database creation
library(RPostgres)
library(rpostgis)

# source data
source("data-raw/forescale_data.R")
source("data-raw/forestime_data.R")
source("data-raw/thesaurus.R")

# connection
# conn <- RPostgres::dbConnect(
#   RPostgres::Postgres(),
#   'ifn', 'laboratoriforestal.creaf.uab.cat', 5432, rstudioapi::askForPassword(),
#   'ifn'
# )
#
# create database and activate postgis
# sql_table_creation_1 <- glue::glue_sql(
#   .con = conn,
#   "
#   CREATE DATABASE forestecoserv;
#   "
# )
# sql_table_creation_2 <- glue::glue_sql(
#   .con = conn,
#   "
#   GRANT ALL PRIVILEGES ON DATABASE forestecoserv TO ifn;
#   "
# )
# sql_table_creation_3 <- glue::glue_sql(
#   .con = conn,
#   "
#   GRANT CONNECT ON DATABASE forestecoserv TO guest;
#   "
# )
#
# RPostgres::dbExecute(conn, sql_table_creation_1)
# RPostgres::dbExecute(conn, sql_table_creation_2)
# RPostgres::dbExecute(conn, sql_table_creation_3)
#
# # change conn
# RPostgres::dbDisconnect(conn)
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  'forestecoserv', 'laboratoriforestal.creaf.uab.cat',
  5432, rstudioapi::askForPassword(), 'ifn'
)
#
# sql_guest_activation_1 <- glue::glue_sql(
#   "
#   GRANT USAGE ON SCHEMA public TO guest;
#   "
# )
# sql_guest_activation_2 <- glue::glue_sql(
#   "
#   GRANT SELECT ON ALL TABLES IN SCHEMA public TO guest;
#   "
# )
# sql_guest_activation_3 <- glue::glue_sql(
#   "
#   ALTER DEFAULT PRIVILEGES IN SCHEMA public
#     GRANT SELECT ON TABLES TO guest;
#   "
# )
#
# RPostgres::dbExecute(conn, sql_guest_activation_1)
# RPostgres::dbExecute(conn, sql_guest_activation_2)
# RPostgres::dbExecute(conn, sql_guest_activation_3)
#
# # postgis extension
# rpostgis::pgPostGIS(conn, topology = TRUE, sfcgal = TRUE)

# data uploading
# forescale data
sf::st_write(forescale_data, conn, 'static', overwrite = TRUE)
# forestime data
sf::st_write(nfi_2_data, conn, 'plot_nfi_2_results', overwrite = TRUE)
sf::st_write(nfi_3_data, conn, 'plot_nfi_3_results', overwrite = TRUE)
sf::st_write(nfi_4_data, conn, 'plot_nfi_4_results', overwrite = TRUE)
sf::st_write(nfi_2_nfi_3_data, conn, 'plot_nfi2_nfi3_results', overwrite = TRUE)
sf::st_write(nfi_3_nfi_4_data, conn, 'plot_nfi3_nfi4_results', overwrite = TRUE)
sf::st_write(nfi_2_nfi_4_data, conn, 'plot_nfi2_nfi4_results', overwrite = TRUE)
# thesauruses
dplyr::copy_to(
  conn, variable_thesaurus, 'variables_thesaurus',
  overwrite = TRUE, temp = FALSE
)

RPostgres::dbDisconnect(conn)

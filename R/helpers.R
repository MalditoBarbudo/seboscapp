# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}


#' drawed_poly
#'
#' return the data calculated on-the-fly for the drawed poly from leaflet
drawed_poly <- function(custom_polygon, points_data, lang) {

  shiny::validate(
    shiny::need(
      custom_polygon, 'no custom poly'
    )
  )

  custom_poly_sf <- custom_polygon[['features']][[1]][['geometry']][['coordinates']] %>%
    purrr::flatten() %>%
    purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) %>%
    dplyr::bind_rows() %>%
    {list(as.matrix(.))} %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_sf(crs = "+proj=longlat +datum=WGS84")

  rows_to_maintain <- sf::st_contains(custom_poly_sf, points_data[['geometry']])

  points_data %>%
    dplyr::slice(purrr::flatten_int(rows_to_maintain)) %>%
    dplyr::mutate(poly_id = 'custom_polygon') %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry) %>%
    dplyr::mutate(geometry = custom_poly_sf[['geometry']])
}

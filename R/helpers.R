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

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {

  app_translations

  id %>%
    purrr::map_chr(
      ~ app_translations %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )

  # dplyr::tbl(db, 'app_translations_APP') %>%
  #   dplyr::filter(text_id %in% id) %>%
  #   dplyr::arrange(text_id) %>%
  #   dplyr::pull(!! rlang::sym(glue::glue("translation_{lang}")))
}

# cache_selected_choice
# logic is as follows:
#   - if the cached value is in choices provided, then return it, if not,
#     return the default value (first choice if not provided)
cache_selected_choice <- function(choices, cache, key, default = choices[1]) {
  cached_input <- cache$get(key, 'non_existent')
  if (all(cached_input %in% choices)) {
    return(cached_input)
  } else {
    return(default)
  }
}

# custom stats functions, capped to perform only with 3 or more
stat_capped <- function(x, .f, ...) {
  if (length(x[!is.na(x)]) < 3) {
    res <- NA_integer_
  } else {
    res <- .f(x, ...)
  }
  return(res)
}

# custom standar error function
se_custom <- function(x) {
  sd(x[!is.na(x)])/length(x[!is.na(x)])
}

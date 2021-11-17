# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form
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
            message(glue::glue("{.x} not found in app thesaurus"))
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )
}

#' translate variable function
#'
#' translate the variable based on the lang selected
translate_var <- function(id, version, scale, lang, variables_thesaurus) {

  if (scale != 'local') {
    stat <- stringr::str_extract(id, '_mean$|_se$|_min$|_max$|_n$|_q05$|_q95$')
    id <- stringr::str_remove(id, '_mean$|_se$|_min$|_max$|_n$|_q05$|_q95$')
  } else {
    stat <- rep('', length(id))
  }

  id_translation <-
    id %>%
    purrr::map_chr(
      ~ variables_thesaurus %>%
        dplyr::filter(var_id == .x, var_table == version) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            message(glue::glue("{.x} not found in variable thesaurus"))
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue('translation_{lang}')))
          }
        }
    )
  id_units <-
    id %>%
    purrr::map_chr(
      ~ variables_thesaurus %>%
        dplyr::filter(var_id == .x, var_table == version) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            ''
          } else {
            dplyr::pull(data_filtered, var_units)
          }
        }
    ) %>%
    stringr::str_replace('-', '') %>%
    purrr::map_chr(
      function(x) {
        if (x == '') {
          x
        } else {
          glue::glue(" [{x}]")
        }
      }
    )

  stat <- dplyr::case_when(
    is.na(stat) ~ '',
    TRUE ~ stat
  )

  translation <-
    list(id = id_translation, units = id_units, stat = stat) %>%
    purrr::pmap_chr(
      function(id, units, stat) {
        if (stringr::str_detect(stat, '_se$|_min$|_max$|_n$|_q05$|_q95$')) {
          glue::glue("{id}{translate_app(stat, lang)}")
        } else {
          glue::glue("{id}{translate_app(stat, lang)}{units}")
        }
      }
    )

  return(translation)

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

# raw data grouping, for preset polys or custom ones
raw_data_grouping <- function(raw_data, data_scale, custom_polygon) {

  # if the scale is one of the presets, group by that and return it
  if (!data_scale %in% c('file', 'drawn_polygon')) {
    res <- raw_data %>%
      dplyr::as_tibble() %>%
      dplyr::select(-geometry) %>%
      dplyr::group_by(!! rlang::sym(data_scale))
    return(res)
  }

  # if scale is given by the user (file or drawn poly) then we need to
  # make the intersection of the data and the polygons
  #
  # get the custom polygon with the reactive and validate it
  custom_poly <- custom_polygon()
  shiny::validate(shiny::need(custom_poly, 'no custom poly'))

  # get only the plots inside the polygons supplied
  # The logic is as follows:
  #   - get the indexes of the intersection between them
  #   - use that indexes to extract the poly_id from the custom poly
  #   - create a new column in the main data with the poly_id to summarise
  #     later
  indexes <- sf::st_intersects(raw_data, custom_poly) %>%
    as.numeric()
  polys_names <- custom_poly %>%
    dplyr::pull(poly_id) %>%
    as.character() %>%
    magrittr::extract(indexes)

  res <- raw_data %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry) %>%
    dplyr::mutate(poly_id = polys_names) %>%
    dplyr::filter(!is.na(poly_id)) %>%
    dplyr::group_by(poly_id)
}


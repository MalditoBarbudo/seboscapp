# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  navbar[[4]][[1]]$children[[1]][[1]]$children[[1]][[3]][[2]] <-
    htmltools::tagAppendChild(navbar[[4]][[1]]$children[[1]][[1]]$children[[1]][[3]][[2]], form)

  return(navbar)
}


#' file_poly
#'
#' return the data calculated on-the-fly for the file loaded
#'
file_poly <- function(file, lang) {

  shiny::validate(
    shiny::need(file, 'no file yet')
  )

  # check for input file format (csv (wkt) not working as it does not store the
  # crs)
  if (stringr::str_detect(file$type, 'zip')) {
    # shapefile
    tmp_folder <- tempdir()
    utils::unzip(file$datapath, exdir = tmp_folder)

    user_polygons <- sf::st_read(
      list.files(tmp_folder, '.shp', recursive = TRUE, full.names = TRUE),
      as_tibble = TRUE
    ) |>
      sf::st_transform(crs = 4326)

  } else {
    # gpkg
    user_polygons <- sf::st_read(file$datapath, as_tibble = TRUE) |>
      sf::st_transform(crs = 4326)
  }

  # check for poly_id
  if (!"poly_id" %in% names(user_polygons)) {
    warning('No poly_id variable found in spatial file, using first variable found as id')
    user_polygons$poly_id <- as.character(user_polygons[[1]])

    shiny::showNotification(
      ui = shiny::tagList(
        shiny::h4(translate_app("poly_id_missing_title", lang))
      ),
      action = shiny::tagList(
        translate_app("poly_id_missing_message", lang)
      ),
      duration = 15,
      type = "warning"
    )

  } else {
    # ensure polygon id is character (factors fuck it all)
    user_polygons$poly_id <- as.character(user_polygons$poly_id)
  }


  return(user_polygons)
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {

  # recursive call for vectors
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_app(.id, lang)
      }
    )
    return(res)
  }

  # get id translations
  id_row <- app_translations |>
    dplyr::filter(text_id == id)

  # return raw id if no matching id found
  if (nrow(id_row) < 1) {
    return(id)
  }

  # get the lang translation
  return(dplyr::pull(id_row, glue::glue("translation_{lang}")))
}

#' translate variable function
#'
#' translate the variable based on the lang selected
translate_var <- function(id, version, scale, lang, variables_thesaurus) {

  # recursive call for vectors
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_var(.id, version, scale, lang, variables_thesaurus)
      }
    )
    return(res)
  }

  if (scale != 'local') {
    stat <- stringr::str_extract(id, '_mean$|_se$|_min$|_max$|_n$|_q05$|_q95$')
    id <- stringr::str_remove(id, '_mean$|_se$|_min$|_max$|_n$|_q05$|_q95$')
  } else {
    stat <- rep('', length(id))
  }

  # get id translations
  id_row <- variables_thesaurus |>
    dplyr::filter(var_id == id, var_table == version)

  # default values
  id_translation <- id
  id_units <- ""

  # if there is id, modify default values to translations
  if (nrow(id_row) > 0) {
    id_translation <- id_row |>
      dplyr::pull(glue::glue("translation_{lang}"))

    id_units <- id_row |>
      dplyr::pull("var_units") |>
      stringr::str_replace("-", "")
    if (id_units != "") {
      id_units <- glue::glue(" [{id_units}]")
    }

    stat <- dplyr::case_when(
      is.na(stat) ~ '',
      TRUE ~ stat
    )
  }

  return(glue::glue("{id_translation}{translate_app(stat, lang)}{id_units}"))

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
    res <- raw_data |>
      dplyr::as_tibble() |>
      dplyr::select(-geometry) |>
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
  indexes <- sf::st_intersects(raw_data, custom_poly) |>
    as.numeric()
  polys_names <- custom_poly |>
    dplyr::pull(poly_id) |>
    as.character() |>
    magrittr::extract(indexes)

  res <- raw_data |>
    dplyr::as_tibble() |>
    dplyr::select(-geometry) |>
    dplyr::mutate(poly_id = polys_names) |>
    dplyr::filter(!is.na(poly_id)) |>
    dplyr::group_by(poly_id)
}


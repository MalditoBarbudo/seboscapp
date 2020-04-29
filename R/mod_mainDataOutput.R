#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data as tbl_sql
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_mainData server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,map_reactives reactives from modules
#' @param fesdb object to access the fes db
#' @param lang lang selected
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  data_reactives, map_reactives,
  fesdb, lang
) {

  # custom poly
  # custom polygon ####
  # we need to check if custom polygon, to retrieve it and build the data later
  custom_polygon <- shiny::reactive({

    shiny::validate(
      shiny::need(data_reactives$data_scale, 'no inputs yet')
    )

    data_scale <- data_reactives$data_scale
    path_to_file <- data_reactives$user_file_sel$datapath

    # file
    if (data_scale == 'file') {
      # check if there is user file
      if (is.null(path_to_file)) {
        user_file_polygons <- NULL
      } else {
        # check if zip (shapefile) or gpkg to load the data
        if (stringr::str_detect(path_to_file, 'zip')) {
          tmp_folder <- tempdir()
          utils::unzip(path_to_file, exdir = tmp_folder)

          user_file_polygons <- sf::st_read(
            list.files(tmp_folder, '.shp', recursive = TRUE, full.names = TRUE),
            as_tibble = TRUE
          ) %>%
            sf::st_transform(4326)
        } else {
          # gpkg
          user_file_polygons <- sf::st_read(path_to_file) %>%
            sf::st_transform(4326)
        }
      }

      shiny::validate(
        shiny::need(user_file_polygons, 'no file provided')
      )

      # rename the poly_id
      names(user_file_polygons)[1] <- 'poly_id'

      return(user_file_polygons)
    }

    if (data_scale == 'drawn_polygon') {
      # validation
      drawn_polygon <- map_reactives$fes_map_draw_all_features
      # When removing the features (custom polygon) the
      # input$map_draw_new_feature is not cleared, so is always filtering the
      # sites, even after removing. For that we need to control when the removed
      # feature equals the new, that's it, when we removed the last one
      shiny::validate(
        shiny::need(drawn_polygon, 'no draw polys yet'),
        shiny::need(length(drawn_polygon[['features']]) != 0, 'removed poly')
      )

      res <-
        drawn_polygon[['features']][[1]][['geometry']][['coordinates']] %>%
        purrr::flatten() %>%
        purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) %>%
        dplyr::bind_rows() %>%
        {list(as.matrix(.))} %>%
        sf::st_polygon() %>%
        sf::st_sfc() %>%
        sf::st_sf(crs = 4326) %>%
        dplyr::mutate(poly_id = 'drawn_polygon')
      return(res)
    }
  })

  # raw data
  raw_data <- shiny::reactive({

    shiny::validate(
      shiny::need(data_reactives$data_version, 'no inputs yet')
    )

    # data version
    data_version <- data_reactives$data_version
    # table
    res <- fesdb$get_data(data_version, TRUE)
    return(res)
  })

  # summarised data
  summ_data <- shiny::reactive({

    shiny::validate(
      shiny::need(data_reactives$data_scale, 'no inputs yet'),
      shiny::need(raw_data(), 'no raw data yet')
    )

    # scale
    data_scale <- data_reactives$data_scale

    if (data_scale == 'local') {
      return(raw_data())
    }

    # set a progress
    progress <- shiny::Progress$new(session, min = 25, max = 100)
    on.exit(progress$close())
    progress$set(
      message = translate_app("progress_message", lang()),
      # 'Calculation in progress',
      detail = translate_app("progress_detail_initial", lang())
      # 'This may take a while...'
    )

    summ_data <- raw_data() %>%
      raw_data_grouping(data_scale, custom_polygon) %>%
      purrr::walk(
        ~ progress$set(value = 35)
      ) %>%
      dplyr::summarise_if(
        is.numeric,
        .funs = list(
          mean = ~ stat_capped(., mean, na.rm = TRUE),
          se = ~ stat_capped(., se_custom),
          min = ~ stat_capped(., min, na.rm = TRUE),
          max = ~ stat_capped(., max, na.rm = TRUE),
          q95 = ~ stat_capped(., quantile, prob = 0.95, na.rm = TRUE),
          q05 = ~ stat_capped(., quantile, prob = 0.05, na.rm = TRUE),
          n = ~ n() - sum(is.na(.))
        )
      )

    progress$set(value = 95)

    return(summ_data)
  })

  # reactive to return
  main_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    main_data_reactives$raw_data <- raw_data()
    main_data_reactives$summ_data <- summ_data()
    main_data_reactives$custom_polygon <- custom_polygon()
  })
  return(main_data_reactives)
}

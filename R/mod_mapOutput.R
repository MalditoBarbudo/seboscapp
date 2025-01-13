#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
mod_mapOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    mapdeck::mapdeckOutput(ns("fes_map"), height = 600),
    shiny::uiOutput(ns('map_container'))
  )
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,viz_reactives,main_data_reactives reactives
#' @param lang lang selected
#' @param var_thes thesauruses
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  data_reactives, viz_reactives, main_data_reactives,
  lang, var_thes
) {

  ## renderUI ####
  output$map_container <- shiny::renderUI({

    # ns
    ns <- session$ns
    shiny::tags$div(
      id = 'cite',
      translate_app('cite_div', lang())
    )
  }) # end of renderUI

  ## mapdeck output (empty map) ####
  output$fes_map <- mapdeck::renderMapdeck({
    mapdeck::mapdeck(
      # style = mapdeck::mapdeck_style('dark'),
      style = "https://raw.githubusercontent.com/CartoDB/basemap-styles/refs/heads/master/mapboxgl/dark-matter-nolabels.json",
      location = c(1.744, 41.726), zoom = 7, pitch = 0
    )
  }) # end of mapdeck output (empty map)

  ## reactives ####
  # reactive to buid the map data
  map_data <- shiny::reactive({

    shiny::validate(
      shiny::need(main_data_reactives$raw_data, 'no raw data'),
      shiny::need(main_data_reactives$summ_data, 'no summ data')
    )

    # get the scale and the data
    data_scale <- shiny::isolate(data_reactives$data_scale)
    raw_data <- main_data_reactives$raw_data
    summ_data <- main_data_reactives$summ_data
    custom_polygon <- main_data_reactives$custom_polygon

    # if local scale, then the raw data is ok
    if (data_scale == 'local') {
      res <- raw_data
    } else {

      # if scale different from local, the summ data is what we want.
      # Also we need the polygons
      polygon_data <- switch(
        data_scale,
        'admin_municipality' = municipalities,
        'admin_region' = regions,
        'admin_province' = provinces,
        "admin_natural_interest_area" = natural_interest_areas,
        "admin_special_protection_natural_area" = special_protection_natural_areas,
        "admin_natura_network_2000" = natura_network_2000s,
        'file' = custom_polygon,
        'drawn_polygon' = custom_polygon
      )

      geom_column <- attr(polygon_data, 'sf_column')

      join_by <- switch(
        data_scale,
        'admin_municipality' = 'admin_municipality',
        'admin_region' = 'admin_region',
        'admin_province' = 'admin_province',
        "admin_natural_interest_area" = "admin_natural_interest_area",
        "admin_special_protection_natural_area" = "admin_special_protection_natural_area",
        "admin_natura_network_2000" = "admin_natura_network_2000",
        'file' = 'poly_id',
        'drawn_polygon' = 'poly_id'
      )
      res <- summ_data |>
        dplyr::left_join(polygon_data, by = join_by) |>
        sf::st_as_sf(sf_column_name = geom_column) |>
        sf::st_cast("MULTIPOLYGON")
    }
    return(res)
  })

  ## observers ####
  # observer to update the map
  shiny::observe({

    shiny::validate(
      shiny::need(map_data(), 'no data yet'),
      shiny::need(viz_reactives$viz_color, 'no viz yet')
      # shiny::need(
      #   viz_reactives$viz_color %in% names(map_data()), 'no updated viz'
      # )
    )

    # scale
    data_scale <- shiny::isolate(data_reactives$data_scale)
    if (data_scale == 'local') {
      viz_color <- viz_reactives$viz_color
    } else {
      viz_color <-
        glue::glue("{viz_reactives$viz_color}_{viz_reactives$viz_statistic}")
        # glue::glue("{viz_reactives$viz_color}_mean")
    }
    data_version <- shiny::isolate(data_reactives$data_version)

    # validate the viz color is in concordance with the data
    shiny::validate(
      shiny::need(
        viz_color %in% names(map_data()), 'no updated viz'
      )
    )

    # data (remove NAs and Inf)
    map_data_ready <- map_data() |>
      dplyr::filter(
        !is.na(!! rlang::sym(viz_color)),
        # min and max creates Inf when all vector values are NAs, remove them
        !is.infinite(!! rlang::sym(viz_color))
      )

    # custom polys and files 0 rows check. It can happen that a custom polygon
    # or a file can try to scale an area with less than 3 plots. In this case
    # the stat_capped functions will return NA. Check if this has happen and
    # give a nice warning to the user
    if (nrow(map_data_ready) < 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = translate_app(
          'stats_unavailable_title', lang()
        ),
        text = translate_app(
          'stats_unavailable', lang()
        )
      )
    }

    # validation
    shiny::validate(
      shiny::need(nrow(map_data_ready) > 0, 'custom scale too restrictive')
    )

    # palette configuration
    color_vector <- map_data_ready |>
      dplyr::pull(!! rlang::sym(viz_color))

    if (length(color_vector) < 2) {
      color_vector_legend <- c(
        color_vector - (color_vector * 0.05),
        color_vector,
        color_vector + (color_vector * 0.05)
      )
    } else {
      color_vector_legend <- color_vector
    }

    color_palette <- switch(
      viz_reactives$viz_pal_config,
      "low" = scales::col_numeric(
        scales::gradient_n_pal(
          hcl.colors(9, "ag_GrnYl", alpha = ifelse(data_scale == "local", 0.8, 1)),
          c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
        ),
        c(min(color_vector, na.rm = TRUE), max(color_vector, na.rm = TRUE)),
        na.color = "#FFFFFF00", reverse = !viz_reactives$viz_pal_reverse, alpha = TRUE
      ),
      "high" = scales::col_numeric(
        scales::gradient_n_pal(
          hcl.colors(9, "ag_GrnYl", alpha = ifelse(data_scale == "local", 0.8, 1)),
          c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        c(min(color_vector, na.rm = TRUE), max(color_vector, na.rm = TRUE)),
        na.color = "#FFFFFF00", reverse = !viz_reactives$viz_pal_reverse, alpha = TRUE
      ),
      "normal" = scales::col_numeric(
        hcl.colors(256, "ag_GrnYl", alpha = ifelse(data_scale == "local", 0.8, 1)),
        c(min(color_vector, na.rm = TRUE), max(color_vector, na.rm = TRUE)),
        na.color = "#FFFFFF00", reverse = !viz_reactives$viz_pal_reverse, alpha = TRUE
      )
    )

    color_palette_legend <- switch(
      viz_reactives$viz_pal_config,
      "low" = scales::col_numeric(
        scales::gradient_n_pal(
          hcl.colors(9, "ag_GrnYl", alpha = ifelse(data_scale == "local", 0.8, 1)),
          c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
        ),
        c(min(color_vector, na.rm = TRUE), max(color_vector, na.rm = TRUE)),
        na.color = "#FFFFFF00", reverse = viz_reactives$viz_pal_reverse, alpha = TRUE
      ),
      "high" = scales::col_numeric(
        scales::gradient_n_pal(
          hcl.colors(9, "ag_GrnYl", alpha = ifelse(data_scale == "local", 0.8, 1)),
          c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        c(min(color_vector, na.rm = TRUE), max(color_vector, na.rm = TRUE)),
        na.color = "#FFFFFF00", reverse = viz_reactives$viz_pal_reverse, alpha = TRUE
      ),
      "normal" = scales::col_numeric(
        hcl.colors(256, "ag_GrnYl", alpha = ifelse(data_scale == "local", 0.8, 1)),
        c(min(color_vector, na.rm = TRUE), max(color_vector, na.rm = TRUE)),
        na.color = "#FFFFFF00", reverse = viz_reactives$viz_pal_reverse, alpha = TRUE
      )
    )
    # labels
    if (data_scale %in% c('file', 'drawn_polygon')) {
      polygon_label <- as.formula("~poly_id")
    } else {
      polygon_label <- as.formula(glue::glue("~{data_scale}"))
    }

    # custom legend (to be able to show in natural order, high values up)
    legend_js <- mapdeck::legend_element(
      variables = rev(round(seq(
        min(color_vector, na.rm = TRUE),
        max(color_vector, na.rm = TRUE),
        length.out = 10
      ), 3)),
      colours = color_palette_legend(seq(
        min(color_vector, na.rm = TRUE),
        max(color_vector, na.rm = TRUE),
        length.out = 10
      )),
      colour_type = "fill", variable_type = "gradient",
      title = translate_var(
        viz_color, data_version, data_scale, lang(), var_thes
      )
    ) |>
      mapdeck::mapdeck_legend()
    
    if (data_scale == "local") {
      # create mapdeck vars needed
      map_data_ready <- map_data_ready |>
        dplyr::mutate(
          hex = color_palette(color_vector),
          tooltip = paste0(
            "<p>", plot_id, ": ", round(.data[[viz_color]], 2), "</p>"
          )
        )

      mapdeck::mapdeck_update(map_id = session$ns("fes_map")) |>
        mapdeck::clear_polygon(layer_id = "polys") |>
        mapdeck::clear_scatterplot(layer_id = "plots") |>
        mapdeck::add_scatterplot(
          data = map_data_ready,
          fill_colour = "hex",
          stroke_colour = "hex",
          id = "plot_id", layer_id = "plots",
          update_view = FALSE, focus_layer = FALSE,
          tooltip = "tooltip",
          legend = legend_js,
          radius = 750
        )
    } else {
      # create mapdeck vars needed
      map_data_ready <- map_data_ready |>
        dplyr::select(dplyr::all_of(c(viz_color, labels(terms(polygon_label))))) |>
        dplyr::mutate(
          hex = color_palette(color_vector),
          tooltip = paste0(
            "<p>", .data[[labels(terms(polygon_label))]], ": ", round(.data[[viz_color]], 2), "</p>"
          ),
          fake_elevation = 20000 * color_vector / max(color_vector, na.rm = TRUE)
        )
      mapdeck::mapdeck_update(map_id = session$ns("fes_map")) |>
        mapdeck::clear_polygon(layer_id = "polys") |>
        mapdeck::clear_scatterplot(layer_id = "plots") |>
        mapdeck::add_polygon(
          data = map_data_ready,
          fill_colour = "hex", fill_opacity = 1,
          id = labels(terms(polygon_label)), layer_id = "polys",
          update_view = FALSE, focus_layer = FALSE,
          tooltip = "tooltip",
          elevation = "fake_elevation", elevation_scale = 1,
          legend = legend_js
        )
    }
  })

  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$map_data <- map_data()
    # map_reactives$fes_map_shape_click <- input$fes_map_shape_click
    map_reactives$fes_map_plot_click <- input$fes_map_scatterplot_click
    map_reactives$fes_map_poly_click <- input$fes_map_polygon_click
    # map_reactives$fes_map_draw_all_features <- input$fes_map_draw_all_features
  })
  return(map_reactives)
}

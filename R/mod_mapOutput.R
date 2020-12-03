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
    leaflet::leafletOutput(ns("fes_map"), height = 600),
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

  ## leaflet output (empty map) ####
  output$fes_map <- leaflet::renderLeaflet({

    # we need data, and we need color var at least
    leaflet::leaflet() %>%
      leaflet::setView(1.72, 41.70, zoom = 8) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief, group = 'Relief'
      ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery, group = 'Imaginery'
      ) %>%
      leaflet::addMapPane('admin_divs', zIndex = 410) %>%
      leaflet::addMapPane('plots', zIndex = 420) %>%
      leaflet::addLayersControl(
        baseGroups = c('Relief', 'Imaginery'),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      # leaflet.extras plugins
      leaflet.extras::addDrawToolbar(
        targetGroup = 'drawn_polygon',
        position = 'topleft',
        polylineOptions = FALSE, circleOptions = FALSE,
        rectangleOptions = FALSE, markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = leaflet.extras::drawPolygonOptions(
          shapeOptions = leaflet.extras::drawShapeOptions()
        ),
        editOptions = leaflet.extras::editToolbarOptions(
          edit = TRUE, remove = TRUE
        ),
        singleFeature = TRUE
      )
  }) # end of leaflet output (empty map)

  ## reactives ####
  # zoom-size transformation. Logic is as follows:
  #   - In closer zooms (10) go to the base size of 750. In far zooms increase
  #     accordingly, until zoom 7 and further, with a max size of 1500
  base_size <- shiny::reactive({
    current_zoom <- input$fes_map_zoom
    if (current_zoom <= 7) {
      current_zoom <- 7
    }
    if (current_zoom >= 10) {
      current_zoom <- 10
    }

    size_transformed <- 750 + ((10 - current_zoom) * 250)

    return(size_transformed)
  })

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
      res <- summ_data %>%
        dplyr::left_join(polygon_data, by = join_by) %>%
        sf::st_as_sf(sf_column_name = geom_column)
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
    map_data_ready <- map_data() %>%
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
    color_vector <- map_data_ready %>%
      dplyr::pull(!! rlang::sym(viz_color))

    if (length(color_vector) < 2) {
      color_vector_legend <- c(
        color_vector - (color_vector*0.05),
        color_vector,
        color_vector + (color_vector*0.05)
      )
    } else {
      color_vector_legend <- color_vector
    }

    color_palette <- switch(
      viz_reactives$viz_pal_config,
      "low" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          viridis::plasma(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
        ),
        color_vector, reverse = viz_reactives$viz_pal_reverse,
        na.color = 'black'
      ),
      "high" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          viridis::plasma(9), c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        color_vector, reverse = viz_reactives$viz_pal_reverse,
        na.color = 'black'
      ),
      "normal" = leaflet::colorNumeric(
        'plasma', color_vector, reverse = viz_reactives$viz_pal_reverse,
        na.color = 'black'
      )
    )

    color_palette_legend <- switch(
      viz_reactives$viz_pal_config,
      "low" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          viridis::plasma(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
        ),
        color_vector_legend, reverse = !viz_reactives$viz_pal_reverse,
        na.color = 'black'
      ),
      "high" = leaflet::colorNumeric(
        scales::gradient_n_pal(
          viridis::plasma(9), c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        ),
        color_vector_legend, reverse = !viz_reactives$viz_pal_reverse,
        na.color = 'black'
      ),
      "normal" = leaflet::colorNumeric(
        'plasma', color_vector_legend, reverse = !viz_reactives$viz_pal_reverse,
        na.color = 'black'
      )
    )
    # labels
    if (data_scale %in% c('file', 'drawn_polygon')) {
      polygon_label <- as.formula("~poly_id")
    } else {
      polygon_label <- as.formula(glue::glue("~{data_scale}"))
    }

    # message(sf::st_crs(map_data_ready))

    leaflet::leafletProxy('fes_map') %>%
      leaflet::clearGroup('plots') %>%
      leaflet::clearGroup('polys') %>%
      leaflet::clearGroup('drawn_polygon') %>%
      {
        temp <- .
        if (data_scale == 'local') {
          temp %>%
            leaflet::addCircles(
              data = map_data_ready,
              group = 'plots', label = ~plot_id, layerId = ~plot_id,
              stroke = FALSE, fillOpacity = 0.7,
              fillColor = color_palette(color_vector),
              radius = base_size(),
              options = leaflet::pathOptions(pane = 'plots')
            )
        } else {
          temp %>%
            leaflet::addPolygons(
              data = map_data_ready,
              group = 'polys',
              label = polygon_label,
              layerId = polygon_label,
              weight = 1, smoothFactor = 1,
              opacity = 1.0, fill = TRUE,
              color = '#6C7A89FF',
              fillColor = color_palette(color_vector),
              fillOpacity = 0.7,
              highlightOptions = leaflet::highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = FALSE
              ),
              options = leaflet::pathOptions(
                pane = 'admin_divs'
              )
            )
        }
      } %>%
      leaflet::addLegend(
        position = 'bottomright', pal = color_palette_legend,
        values = color_vector_legend,
        title = translate_var(
          viz_color, data_version, data_scale, lang(), var_thes
        ),
        layerId = 'color_legend', opacity = 1,
        na.label = '', className = 'info legend na_out',
        labFormat = leaflet::labelFormat(
          transform = function(x) {sort(x, decreasing = TRUE)}
        )
      )
  })

  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$map_data <- map_data()
    map_reactives$fes_map_shape_click <- input$fes_map_shape_click
    map_reactives$fes_map_draw_all_features <- input$fes_map_draw_all_features
  })
  return(map_reactives)
}

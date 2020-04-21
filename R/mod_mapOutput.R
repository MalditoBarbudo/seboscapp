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
      leaflet::setView(1.1, 41.70, zoom = 8) %>%
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
      ) #%>%
      # leaflet.extras plugins
      # leaflet.extras::addDrawToolbar(
      #   targetGroup = 'drawn_poly',
      #   position = 'topleft',
      #   polylineOptions = FALSE, circleOptions = FALSE,
      #   rectangleOptions = FALSE, markerOptions = FALSE,
      #   circleMarkerOptions = FALSE,
      #   polygonOptions = leaflet.extras::drawPolygonOptions(
      #     shapeOptions = leaflet.extras::drawShapeOptions()
      #   ),
      #   editOptions = leaflet.extras::editToolbarOptions(
      #     edit = TRUE, remove = TRUE
      #   ),
      #   singleFeature = TRUE
      # )
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

    # get the scale
    data_scale <- shiny::isolate(data_reactives$data_scale)
    raw_data <- main_data_reactives$raw_data
    summ_data <- main_data_reactives$summ_data

    # if local scale, then the raw data is ok
    if (data_scale == 'local') {
      raw_data <- raw_data
      return(raw_data)
    }

    # if scale diferent from local, the summ data is what we want.
    # Also we need the polygons
    polygon_data <- switch(
      data_scale,
      'admin_municipality' = municipalities,
      'admin_region' = regions,
      'admin_province' = provinces

    )
    summ_data <- summ_data %>%
      dplyr::left_join(polygon_data, by = data_scale) %>%
      sf::st_as_sf(sf_column_name = 'geometry')

    return(summ_data)
  })

  ## observers ####
  # observer to update the map
  shiny::observe({

    shiny::validate(
      shiny::need(map_data(), 'no data yet'),
      shiny::need(viz_reactives$viz_color, 'no viz yet'),
      shiny::need(
        viz_reactives$viz_color %in% names(map_data()), 'no updated viz'
      )
    )

    foo <- map_data()

    browser()
    # data and scale
    data_scale <- shiny::isolate(data_reactives$data_scale)
    map_data_ready <- map_data() %>%
      # remove the nas
      dplyr::filter(!is.na(!! rlang::sym(viz_reactives$viz_color)))
    # palette configuration
    color_vector <- map_data_ready %>%
      dplyr::pull(!! rlang::sym(viz_reactives$viz_color))
    color_palette <- leaflet::colorNumeric(
      'plasma', color_vector, reverse = FALSE,
      na.color = 'black'
    )
    color_palette_legend <- leaflet::colorNumeric(
      'plasma', color_vector, reverse = TRUE,
      na.color = 'black'
    )
    # labels
    polygon_label <- as.formula(glue::glue("~{data_scale}"))

    leaflet::leafletProxy('fes_map') %>%
      leaflet::clearGroup('plots') %>%
      leaflet::clearGroup('polys') %>%
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
        values = color_vector,
        title = names(
          translate_app(
            viz_reactives$viz_color, lang()
          )
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

#' function to launch the lidar app
#'
#' @importFrom magrittr %>%
#'
#' @export
seboscapp <- function() {

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'seboscapp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  ## UI ####
  ui <- shiny::tagList(
    # shinyjs
    shinyjs::useShinyjs(),

    # css
    shiny::tags$head(
      # custom css
      shiny::includeCSS(
        system.file('resources', 'seboscapp.css', package = 'seboscapp')
      ),
      # corporative image css
      shiny::includeCSS(
        system.file('resources', 'corp_image.css', package = 'seboscapp')
      )
    ),

    navbarPageWithInputs(
      # opts
      title = 'Forest Ecosystem Services of Catalunya App',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # navbarPage contents
      shiny::tabPanel(
        title = 'Fixed data',
        ########################################################### debug ####
        # shiny::absolutePanel(
        #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
        #   draggable = TRUE, width = 640, height = 'auto',
        #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
        #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
        #   top = 60, left = 'auto', right = 50, bottom = 'auto',
        #
        #   shiny::textOutput('debug1'),
        #   shiny::textOutput('debug2'),
        #   shiny::textOutput('debug3')
        # ),
        ####################################################### end debug ####

        # we need an UI beacuse we need to translate based on the lang input from the
        # navbar
        shiny::uiOutput('fixed_ui')

      ), # end of tabPanel "Fixed data"

      shiny::tabPanel(
        title = 'Dynamic data',
        ########################################################### debug ####
        # shiny::absolutePanel(
        #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
        #   draggable = TRUE, width = 640, height = 'auto',
        #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
        #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
        #   top = 60, left = 'auto', right = 50, bottom = 'auto',
        #
        #   shiny::textOutput('debug1'),
        #   shiny::textOutput('debug2'),
        #   shiny::textOutput('debug3')
        # ),
        ####################################################### end debug ####

        # we need an UI beacuse we need to translate based on the lang input from the
        # navbar
        shiny::uiOutput('dynamic_ui')

      ) # end of tabPanel "Dynamic data"
    ) # end of navbarwithinputs
  ) # end of ui (tagList)

  ## SERVER ####
  server <- function(input, output, session) {
    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   input$
    # })
    # output$debug2 <- shiny::renderPrint({
    #   input$
    # })
    # output$debug3 <- shiny::renderPrint({
    #   input$
    # })

    ## lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    ## fixed UI (to use lang) ####
    output$fixed_ui <- shiny::renderUI({

      # lang
      lang_declared <- lang()

      # proper UI ####
      shiny::fluidPage(
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            # title
            # shiny::h4(translate_app('sidebar_h4_title', lang_declared)),
            shiny::h4('Settings'),

            shiny::selectInput(
              'fixed_var_sel', 'Select the variable',
              choices = c('c1', 'p1', 'p2', 'r1', 'r2', 'r3', 'r4')
            ),

            shiny::selectInput(
              'fixed_scale', 'Select the scale',
              choices = c('local', 'municipalities', 'counties', 'provinces')
            ),

            shinyjs::hidden(
              shiny::selectInput(
                'fixed_metric', 'Select the summary metric',
                choices = c('mean', 'min', 'max', 'n')
              )
            )
          ), # end of sidebar panel

          mainPanel = shiny::mainPanel(
            width = 9,
            shiny::tabsetPanel(
              shiny::tabPanel(
                title = 'Map',
                leaflet::leafletOutput('fixed_map')
              ), # end of fixed map tab
              shiny::tabPanel(
                title = 'Table',
                DT::DTOutput('fixed_table')
              ) # end of fixed table tab
            )
          ) # end of main panel
        ) # end of layout
      ) # end of fluidPage
    }) # end of fixed_ui

    ## dynamic UI (to use lang) ####

    ## data_reactive ####
    data_fixed <- shiny::reactive({

      shiny::validate(
        shiny::need(input$fixed_var_sel, 'no inputs'),
        shiny::need(input$fixed_scale, 'no inputs')
      )

      dataset_fixed <- switch(
        input$fixed_var_sel,
        'c1' = c1_data,
        'p1' = p1_data,
        'p2' = p2_data,
        'r1' = r1_data,
        'r2' = r2_data,
        'r3' = r3_data,
        'r4' = r4_data
      )

      scale_sel <- input$fixed_scale

      # if scale is local, return the data as is
      if (scale_sel == 'local') {
        return(dataset_fixed)
      } else {
        admin_var <- switch(
          scale_sel,
          'municipalities' = 'admin_municipality',
          'counties' = 'admin_region',
          'provinces' = 'admin_province'
        )

        admin_polys <- switch(
          scale_sel,
          'municipalities' = municipalities_simpl,
          'counties' = counties_simpl,
          'provinces' = provinces_simpl
        )

        # calculate the scale
        summarise_fixed <- dataset_fixed %>%
          dplyr::group_by(.data[[admin_var]]) %>%
          dplyr::summarise(
            mean = mean(.data[[input$fixed_var_sel]], na.rm = TRUE),
            max = max(.data[[input$fixed_var_sel]], na.rm = TRUE),
            min = min(.data[[input$fixed_var_sel]], na.rm = TRUE),
            n = n()
          ) %>%
          tibble::as_tibble() %>%
          dplyr::select(-geometry) %>%
          dplyr::left_join(admin_polys, by = c(admin_var)) %>%
          sf::st_as_sf(sf_column_name = 'geometry')

        return(summarise_fixed)
      }

    })

    ## table fixed output ####
    output$fixed_table <- DT::renderDT({data_fixed()})

    ## fixed map output ####
    output$fixed_map <- leaflet::renderLeaflet({

      leaflet::leaflet() %>%
        leaflet::setView(1.744, 41.726, zoom = 8) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldShadedRelief,
          group = 'Relief',
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = 'Imaginery',
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c('Relief', 'Imaginery'),
          options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
        ) %>%
        # leaflet.extras plugins
        leaflet.extras::addDrawToolbar(
          targetGroup = 'poly',
          position = 'topleft',
          polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
          markerOptions = FALSE, circleMarkerOptions = FALSE,
          polygonOptions = leaflet.extras::drawPolygonOptions(
            shapeOptions = leaflet.extras::drawShapeOptions()
          ),
          editOptions = leaflet.extras::editToolbarOptions(
            edit = TRUE, remove = TRUE
          ),
          singleFeature = TRUE
        )
    })

    ## show the metric when scale is not local ####
    shiny::observeEvent(
      eventExpr = input$fixed_scale,
      handlerExpr = {
        if (input$fixed_scale == 'local') {
          shinyjs::hideElement('fixed_metric')
        } else {
          shinyjs::showElement('fixed_metric')
        }
      }
    )

    ## leaflet proxy scale ####
    shiny::observe({

      # needed inputs
      shiny::validate(
        shiny::need(input$fixed_scale, 'no inputs'),
        shiny::need(input$fixed_var_sel, 'no inputs'),
        shiny::need(input$fixed_metric, 'no inputs')
      )

      # triggers observer (inputs)
      data_sel <- data_fixed()
      scale_sel <- input$fixed_scale
      var_sel <- input$fixed_var_sel
      metric_sel <- input$fixed_metric

      # local scale, markers
      if (scale_sel == 'local') {

        # palettes
        palette_map <- leaflet::colorBin(
          palette = 'viridis',
          domain = c(
            min(data_sel[[var_sel]], na.rm = TRUE),
            max(data_sel[[var_sel]], na.rm = TRUE)
          ),
          bins = 6
        )

        leaflet::leafletProxy('fixed_map', session, data = data_sel) %>%
          leaflet::clearGroup('poly') %>%
          leaflet::clearGroup('plot') %>%
          leaflet::addCircleMarkers(
            group = 'plot', label = as.character(data_sel[[var_sel]]),
            stroke = FALSE, fillOpacity = 0.7,
            fillColor = ~palette_map(data_sel[[var_sel]]), radius = 5
          ) %>%
          leaflet::addLegend(
            position = 'bottomright', pal = palette_map, values = data_sel[[var_sel]],
            layerId = 'color_palette', title = var_sel
          )
      } else {
        # bigger scale, polygons
        admin_var <- switch(
          scale_sel,
          'municipalities' = 'admin_municipality',
          'counties' = 'admin_region',
          'provinces' = 'admin_province'
        )

        # palettes
        palette_map <- leaflet::colorBin(
          palette = 'viridis',
          domain = c(
            min(data_sel[[metric_sel]], na.rm = TRUE),
            max(data_sel[[metric_sel]], na.rm = TRUE)
          ),
          bins = 6
        )

        leaflet::leafletProxy('fixed_map', session, data = data_sel) %>%
          leaflet::clearGroup('poly') %>%
          leaflet::clearGroup('plot') %>%
          leaflet::addPolygons(
            group = 'poly',
            label = glue::glue("{data_sel[[admin_var]]} - {data_sel[[metric_sel]]}"),
            fillColor = ~palette_map(data_sel[[metric_sel]]),
            fillOpacity = 0.9, stroke = FALSE,
            color = ~palette_map(data_sel[[metric_sel]])
          ) %>%
          leaflet::addLegend(
            position = 'bottomright', pal = palette_map, values = data_sel[[metric_sel]],
            layerId = 'color_palette', title = var_sel
          )
      }
    })

  } # end of server function

  # Run the application
  seboscapp <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {
      # nothing to see here
    }
  )

  # shiny::runApp(nfi_app)
  return(seboscapp)

}

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
        title = shiny::uiOutput('fixed_data_tabtitle'),
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
        title = shiny::uiOutput('dynamic_data_tabtitle'),
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

    output$fixed_data_tabtitle <- shiny::renderText({translate_app('static', lang())})
    output$dynamic_data_tabtitle <- shiny::renderText({translate_app('dynamic', lang())})

    ## fixed UI (to use lang) ####
    output$fixed_ui <- shiny::renderUI({

      # lang
      lang_declared <- lang()

      shiny::fluidPage(
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            # title
            # shiny::h4(translate_app('sidebar_h4_title', lang_declared)),
            shiny::h4('fixed_sidebar_h4_title' %>% translate_app(lang_declared)),

            shiny::selectInput(
              'fixed_var_sel',
              'fixed_var_sel' %>% translate_app(lang_declared),
              choices = list(
                'provisioning' = c('p1', 'p2') %>% purrr::set_names(nm = translate_app(., lang_declared)),
                'cultural' = c('c1') %>% purrr::set_names(nm = translate_app(., lang_declared)),
                'regulation' = c('r1', 'r2', 'r3', 'r4') %>% purrr::set_names(nm = translate_app(., lang_declared))
              ) %>% purrr::set_names(nm = translate_app(c('provisioning', 'cultural', 'regulation'), lang_declared))
            ),

            shiny::selectInput(
              'fixed_scale',
              'fixed_scale' %>% translate_app(lang_declared),
              choices = c('local', 'municipalities', 'counties', 'provinces', 'drawed_poly') %>%
                purrr::set_names(nm = translate_app(., lang_declared))
            ),

            shinyjs::hidden(
              shiny::selectInput(
                'fixed_metric',
                'fixed_metric' %>% translate_app(lang_declared),
                choices = c('mean', 'min', 'max', 'n') %>%
                  purrr::set_names(nm = translate_app(., lang_declared))
              )
            ),

            # little spaces
            shiny::br(),
            shiny::br(),

            # download
            shiny::actionButton(
              'fixed_download_dialogue', 'download' %>% translate_app(lang_declared)
            )
          ), # end of sidebar panel

          mainPanel = shiny::mainPanel(
            width = 9,
            shiny::tabsetPanel(
              shiny::tabPanel(
                title = 'map' %>% translate_app(lang_declared),
                leaflet::leafletOutput('fixed_map', height = '70vh')
              ), # end of fixed map tab
              shiny::tabPanel(
                title = 'table' %>% translate_app(lang_declared),
                DT::DTOutput('fixed_table', height = '70vh')
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
        if (scale_sel %in% c('municipalities', 'counties', 'provinces')) {
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
            dplyr::filter(n > 2) %>%
            tibble::as_tibble() %>%
            dplyr::select(-geometry) %>%
            dplyr::left_join(admin_polys, by = c(admin_var)) %>%
            sf::st_as_sf(sf_column_name = 'geometry')

          return(summarise_fixed)
        } else {

          shiny::validate(
            shiny::need(input$fixed_map_draw_all_features, 'no drawed poly'),
            shiny::need(
              !rlang::is_empty(input$fixed_map_draw_all_features[['features']]),
              'removed poly'
            )
          )

          # here the data for custom poly
          custom_poly_data <- drawed_poly(
            custom_polygon = input$fixed_map_draw_all_features,
            points_data = dataset_fixed,
            lang()
          ) %>%
            dplyr::group_by(poly_id) %>%
            dplyr::summarise(
              mean = mean(.data[[input$fixed_var_sel]], na.rm = TRUE),
              max = max(.data[[input$fixed_var_sel]], na.rm = TRUE),
              min = min(.data[[input$fixed_var_sel]], na.rm = TRUE),
              n = n(),
              geometry = unique(geometry)
            ) %>%
            sf::st_as_sf(sf_column_name = 'geometry')

          return(custom_poly_data)
        }
      }

    })

    ## table fixed output ####
    output$fixed_table <- DT::renderDT({

      data_sel <- data_fixed()
      columns_to_round <- names(data_sel)[
        names(data_sel) %in% c(
          'c1', 'p1', 'p2', 'r1', 'r2', 'r3',
          'r4', 'mean', 'min', 'max'
        )
      ]

      data_sel %>%
        tibble::as_tibble() %>%
        dplyr::select(
          dplyr::starts_with('admin_'),
          dplyr::one_of(c('poly_id', 'c1', 'p1', 'p2', 'r1', 'r2', 'r3', 'r4')),
          dplyr::one_of(c('mean', 'min', 'max', 'n'))
        ) %>%
        {
          DT::datatable(
            .,
            rownames = FALSE,
            colnames = names(.) %>% purrr::set_names(., nm = translate_app(., lang())),
            class = 'hover order-column stripe nowrap',
            filter = list(position = 'top', clear = FALSE, plain = FALSE),
            options = list(
              pageLength = 15,
              dom = 'tip',
              autoWidth = FALSE,
              initComplete = DT::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
                "$(this.api().table().body()).css({'font-family': 'Hacker'});",
                "}"
              )
            )
          )
        } %>%
        DT::formatRound(
          columns = columns_to_round %>% translate_app(lang()),
          digits = 2
        )

    })

    ## fixed map output ####
    output$fixed_map <- leaflet::renderLeaflet({

      lang_declared <- lang()

      leaflet::leaflet() %>%
        leaflet::setView(1.744, 41.726, zoom = 8) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldShadedRelief,
          group = 'Relief' %>% translate_app(lang_declared),
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = 'Imaginery' %>% translate_app(lang_declared),
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c('Relief', 'Imaginery') %>% translate_app(lang_declared),
          options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
        ) %>%
        # leaflet.extras plugins
        leaflet.extras::addDrawToolbar(
          targetGroup = 'custom_poly',
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

    ## leaflet proxy map ####
    shiny::observe({

      # needed inputs
      shiny::validate(
        shiny::need(input$fixed_scale, 'no inputs'),
        shiny::need(input$fixed_var_sel, 'no inputs'),
        shiny::need(input$fixed_metric, 'no inputs'),
        shiny::need(lang(), 'no language')
      )

      # triggers observer (inputs)
      data_sel <- data_fixed()
      scale_sel <- input$fixed_scale
      var_sel <- input$fixed_var_sel
      metric_sel <- input$fixed_metric
      lang_declared <- lang()

      # local scale, markers
      if (scale_sel == 'local') {

        # palettes
        palette_map <- leaflet::colorBin(
          palette = 'plasma',
          domain = c(
            min(data_sel[[var_sel]], na.rm = TRUE),
            max(data_sel[[var_sel]], na.rm = TRUE)
          ),
          bins = 6
        )

        leaflet::leafletProxy('fixed_map', session, data = data_sel) %>%
          leaflet::clearGroup('poly') %>%
          leaflet::clearGroup('plot') %>%
          leaflet::clearGroup('custom_poly') %>%
          leaflet::addCircles(
            group = 'plot', label = as.character(data_sel[[var_sel]]),
            stroke = FALSE, fillOpacity = 0.7,
            fillColor = ~palette_map(data_sel[[var_sel]]), radius = 750
          ) %>%
          leaflet::addLegend(
            position = 'bottomright', pal = palette_map, values = data_sel[[var_sel]],
            layerId = 'color_palette', opacity = 0.7,
            title = var_sel %>% translate_app(lang_declared)
          )
      } else {
        # bigger scale, polygons
        admin_var <- switch(
          scale_sel,
          'municipalities' = 'admin_municipality',
          'counties' = 'admin_region',
          'provinces' = 'admin_province',
          'drawed_poly' = 'poly_id'
        )

        # palettes
        palette_map <- leaflet::colorBin(
          palette = 'plasma',
          domain = c(
            min(data_sel[[metric_sel]], na.rm = TRUE),
            max(data_sel[[metric_sel]], na.rm = TRUE)
          ),
          bins = 6
        )

        leaflet::leafletProxy('fixed_map', session, data = data_sel) %>%
          leaflet::clearGroup('poly') %>%
          leaflet::clearGroup('plot') %>%
          leaflet::clearGroup('custom_poly') %>%
          leaflet::addPolygons(
            group = if (admin_var == 'drawed_poly') {'custom_poly'} else {'poly'},
            label = glue::glue("{data_sel[[admin_var]]} - {data_sel[[metric_sel]]}"),
            fillColor = ~palette_map(data_sel[[metric_sel]]),
            fillOpacity = 0.9, stroke = TRUE, weight = 2,
            color = ~palette_map(data_sel[[metric_sel]]),
            highlightOptions = leaflet::highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE
            ),
          ) %>%
          leaflet::addLegend(
            position = 'bottomright', pal = palette_map, values = data_sel[[metric_sel]],
            layerId = 'color_palette', opacity = 0.9,
            title = var_sel %>% translate_app(lang_declared)
          )
      }
    })

    ## download handlers ####
    # modal for saving the raster data
    shiny::observeEvent(
      eventExpr = input$fixed_download_dialogue,
      handlerExpr = {

        lang_declared = lang()

        shiny::showModal(
          ui = shiny::modalDialog(
            shiny::tagList(

              shiny::fluidRow(
                shiny::column(
                  12,
                  # format options
                  shiny::selectInput(
                    'fixed_data_format',
                    'Select the format',
                    choices = list(
                      'Map' = c('shp', 'gpkg'),
                      'Table' = c('excel', 'csv')
                    ),
                    selected = 'gpkg'
                  )
                )
              )
            ),
            easyClose = TRUE,
            footer = shiny::tagList(
              # shiny::modalButton(translate_app('modal_dismiss_label', lang_declared)),
              shiny::modalButton('Dismiss'),
              shiny::downloadButton(
                'fixed_download',
                label = 'Download',
                class = 'btn-success'
              )
            )
          )
        )
      }
    ) # end of download dialog handler

    output$fixed_download <- shiny::downloadHandler(
      filename = function() {
        file_name <- switch(
          input$fixed_data_format,
          'shp' = glue::glue("{input$fixed_var_sel}_{input$fixed_scale}_static.zip"),
          'gpkg' = glue::glue("{input$fixed_var_sel}_{input$fixed_scale}_static.gpkg"),
          'excel' = glue::glue("{input$fixed_var_sel}_{input$fixed_scale}_static.xlsx"),
          'csv' = glue::glue("{input$fixed_var_sel}_{input$fixed_scale}_static.csv")
        )
        return(file_name)
      },
      content = function(file) {

        if (input$fixed_data_format == 'gpkg') {
          sf::st_write(data_fixed(), dsn = file)
        } else {
          if (input$fixed_data_format == 'xlsx') {
            writexl::write_xlsx(data_fixed(), path = file)
          } else {
            if (input$fixed_data_format ==  'csv') {
              readr::write_csv(data_fixed(), path = file)
            } else {
              tmp_dir <- tempdir()
              sf::st_write(
                data_fixed(),
                file.path(
                  tmp_dir,
                  glue::glue("{input$fixed_var_sel}_{input$fixed_scale}_static.shp")
                )
              )
              shp_files <- list.files(
                tmp_dir,
                glue::glue("{input$fixed_var_sel}_{input$fixed_scale}_static"),
                full.names = TRUE
              )
              utils::zip(
                file.path(tmp_dir, 'shp_files.zip'),
                shp_files
              )
              file.copy(file.path(tmp_dir, 'shp_files.zip'), file)
              file.remove(file.path(tmp_dir, 'shp_files.zip'), shp_files)
            }
          }
        }
      }
    )

    ## summarising banner ####
    shiny::observeEvent(
      eventExpr = input$fixed_scale,
      handlerExpr = {
        if (input$fixed_scale %in% c('municipalities', 'counties', 'provinces')) {
          shiny::showModal(
            shiny::modalDialog(
              shiny::p('fixed_scale_summ_warning' %>% translate_app(lang())),
              title = 'fixed_scale_summ_warning_title' %>% translate_app(lang()),
              footer = NULL, size = 'm', easyClose = TRUE
            )
          )
        }
      }
    )

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

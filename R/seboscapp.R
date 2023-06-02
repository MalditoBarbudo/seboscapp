#' function to launch the fes app
#'
#' @export
fes_app <- function() {

  ### DB access ################################################################
  fesdb <- lfcdata::fes()

  ### thesauruses ##############################################################
  var_thes <- fesdb$get_data('variables_thesaurus')

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'seboscapp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue(
      "<img class='flag-image' src='images/cat.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/spa.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/eng.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    )
  )

  ## JS code needed ############################################################
  keep_alive_script <- shiny::HTML(
    "var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 10000);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});"
  )

  ## UI ########################################################################
  ui <- shiny::tagList(

    # use shinyjs
    shinyjs::useShinyjs(),

    # use waiter and waitress
    waiter::use_waiter(),
    waiter::use_hostess(),

    # navbar with inputs (custom function, see helpers.R)
    navbarPageWithInputs(
      # opts
      title = 'FES app',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for
      # the lang selector
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

      # footer
      footer = shiny::tags$footer(
        shiny::fluidRow(
          shiny::column(
            width = 12, align = "right",
            shiny::HTML(glue::glue(
              '<img src="images/emf_white_logo.svg" width="120px" class="d-inline-block" alt="" loading="lazy">
              <img src="images/creaf_white_logo.svg" width="135px" class="d-inline-block" alt="" loading="lazy">
              <span>({lubridate::year(Sys.Date())})</span>'
            ))
          )
        )
      ),

      # main tab
      shiny::tabPanel(
        title = mod_tab_translateOutput('main_tab_translation'),
        # css
        shiny::tags$head(
          # js script,
          shiny::tags$script(keep_alive_script),
          # corporative image css
          shiny::includeCSS(
            system.file('apps_css', 'corp_image.css', package = 'lfcdata')
          ),
          # custom css
          shiny::includeCSS(
            system.file('apps_css', 'seboscapp.css', package = 'lfcdata')
          )
        ),
        # Sidebar layout
        shiny::sidebarLayout(
          ## options
          position = 'left', fluid = TRUE,
          ## sidebar panel
          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # this is gonna be a tabsetPanel, for data selection, save and help.
            # tabset panel
            shiny::tabsetPanel(
              id = 'sidebar_tabset', type = 'pills',
              # data tab
              shiny::tabPanel(
                title = mod_tab_translateOutput('data_translation'),
                # 'data',
                value = 'data_inputs_panel',
                mod_dataInput('mod_dataInput'),
                mod_vizInput('mod_vizInput')
              ), # end of data tab
              shiny::tabPanel(
                title = mod_tab_translateOutput('save_translation'),
                # 'save',
                value = 'save_panel',
                mod_saveUI('mod_saveUI')
              ), # end fo save panel
              # help panel
              shiny::tabPanel(
                title = mod_tab_translateOutput('help_translation'),
                # 'help',
                value = 'help_panel',
                mod_helpUI('mod_helpUI')
              )
            ) # end of sidebar tabsetPanel
          ),
          ## main panel
          mainPanel = shiny::mainPanel(
            width = 8,
            shiny::tabsetPanel(
              id = 'main_panel_tabset', type = 'pills',
              shiny::tabPanel(
                title = mod_tab_translateOutput('map_translation'),
                # 'map',
                value = 'map_panel',
                mod_mapOutput('mod_mapOutput')
              ),
              shiny::tabPanel(
                title = mod_tab_translateOutput('table_translation'),
                # 'table',
                value = 'table_panel',
                mod_dataTableOutput('mod_dataTableOutput')
              )
            )
          )
        ) # end sidebar layout
      ), # end of tabPanel
      shiny::tabPanel(
        title = mod_tab_translateOutput('tech_specs_translation'),
        value = 'tech_spec_panel',
        mod_techSpecsOutput('mod_techSpecsOutput')
      )
    ) # end NavBarWithInputs

  ) # end of UI

  ## server ####
  server <- function(input, output, session) {

    # lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    # cache ####
    viz_cache <- cachem::cache_mem(evict = 'fifo')

    # modules ####
    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput', lang
    )
    # main data
    main_data_reactives <- shiny::callModule(
      mod_mainData, 'mod_mainDataOutput',
      data_reactives, map_reactives,
      fesdb, lang
    )
    # viz
    viz_reactives <- shiny::callModule(
      mod_viz, 'mod_vizInput',
      data_reactives, var_thes, lang, viz_cache
    )
    # table
    table_reactives <- shiny::callModule(
      mod_dataTable, 'mod_dataTableOutput',
      main_data_reactives, data_reactives,
      var_thes, lang
    )
    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapOutput',
      data_reactives, viz_reactives, main_data_reactives,
      lang, var_thes
    )
    # info
    shiny::callModule(
      mod_info, 'mod_infoUI',
      map_reactives, data_reactives, viz_reactives,
      var_thes, lang
    )
    # save
    shiny::callModule(
      mod_save, 'mod_saveUI',
      map_reactives, table_reactives, main_data_reactives,
      lang
    )
    # help
    shiny::callModule(
      mod_help, 'mod_helpUI',
      data_reactives, viz_reactives,
      var_thes, lang
    )
    # technical specifications module
    shiny::callModule(
      mod_techSpecs, 'mod_techSpecsOutput',
      lang
    )

    ## tab translations ####
    shiny::callModule(
      mod_tab_translate, 'main_tab_translation',
      'main_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'data_translation',
      'data_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'viz_translation',
      'viz_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'save_translation',
      'save_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'help_translation',
      'help_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'map_translation',
      'map_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'table_translation',
      'table_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'tech_specs_translation',
      'tech_specs_translation', lang
    )

    ## observers ####
    # modal observer
    shiny::observeEvent(
      eventExpr = map_reactives$fes_map_shape_click,
      handlerExpr = {

        shiny::showModal(
          shiny::modalDialog(
            mod_infoUI('mod_infoUI'),
            footer = shiny::modalButton(
              translate_app('dismiss', lang())
            ),
            size = 'm', easyClose = TRUE
          )
        )
      }
    )

  } # end of server

  # Run the application
  fes_app_res <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(fes_app)
  return(fes_app_res)

}

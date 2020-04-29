#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container')
    )
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @export
mod_data <- function(
  input, output, session,
  lang
) {

  # renderUI ####
  output$mod_data_container <- shiny::renderUI({
    ns <- session$ns

    ## preacalculated choices
    version_choices <- list(
      'static' %>%
        magrittr::set_names(translate_app(., lang())),
      c(
        # base_data
        'plot_nfi_2_results', 'plot_nfi_3_results', 'plot_nfi_4_results',
        # comparisions
        'plot_nfi2_nfi3_results', 'plot_nfi3_nfi4_results',
        'plot_nfi2_nfi4_results'
      ) %>%
        magrittr::set_names(translate_app(., lang()))
    ) %>%
      magrittr::set_names(translate_app(c('static', 'dynamic'), lang()))

    scale_choices <- c(
      # preset scales
      'local', 'admin_municipality', 'admin_region', 'admin_province',
      "admin_natural_interest_area","admin_special_protection_natural_area",
      "admin_natura_network_2000",
      # custom scales
      'drawn_polygon', 'file'
    ) %>%
      magrittr::set_names(translate_app(., lang()))

    # tagList
    shiny::tagList(
      # data version and admin row
      shiny::h4(translate_app('h4_data_version', lang())),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shinyWidgets::pickerInput(
            ns('data_version'),
            label = translate_app('data_version', lang()),
            choices = version_choices,
            selected = 'static'
          )
        ),
        shiny::column(
          6,
          shinyWidgets::pickerInput(
            ns('data_scale'), translate_app('data_scale', lang()),
            scale_choices, selected = 'local'
          )
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns('file_upload_panel'),
          shiny::fluidRow(
            shiny::column(
              7, align = 'center',
              shiny::fileInput(
                ns('user_file_sel'),
                translate_app('user_file_sel_label', lang()),
                accept = c('zip', 'gpkg'),
                buttonLabel = translate_app(
                  'user_file_sel_buttonLabel', lang()
                ),
                placeholder = translate_app(
                  'user_file_sel_placeholder', lang()
                )
              )
            ),
            shiny::column(
              5, align = 'center',
              shiny::p(translate_app('file_text', lang()))
            )
          )
        )
      ) # end of hidden file selector
    ) # end of tagList
  }) # end of renderUI

  ## observers ####
  # observer to show the file upload panel if needed
  shiny::observe({

    shiny::validate(
      shiny::need(input$data_scale, 'no div')
    )
    data_scale <- input$data_scale

    if (data_scale == 'file') {
      shinyjs::show('file_upload_panel')
    } else {
      shinyjs::hide('file_upload_panel')
    }
  })

  ## returning inputs ####
  # reactive values to return and use in other modules
  data_reactives <- shiny::reactiveValues()

  shiny::observe({
    data_reactives$data_version <- input$data_version
    data_reactives$data_scale <- input$data_scale
    data_reactives$user_file_sel <- input$user_file_sel
  })

  return(data_reactives)
}

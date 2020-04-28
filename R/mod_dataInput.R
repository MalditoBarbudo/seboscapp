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
      'local', 'admin_municipality', 'admin_region', 'admin_province'
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
      )
    ) # end of tagList
  }) # end of renderUI

  ## returning inputs ####
  # reactive values to return and use in other modules
  data_reactives <- shiny::reactiveValues()

  shiny::observe({
    data_reactives$data_version <- input$data_version
    data_reactives$data_scale <- input$data_scale
  })

  return(data_reactives)
}

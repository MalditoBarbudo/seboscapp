#' @title mod_saveUI and mod_save
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_saveUI <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # ui
  shiny::uiOutput(ns("save_container"))
}

#' mod_save
#' @param input internal
#' @param output internal
#' @param session internal
#' @param map_reactives,table_reactives,main_data_reactives reactives needed
#' @param lang language selected
#'
#' @export
#'
#' @rdname mod_saveUI
mod_save <- function(
  input, output, session,
  map_reactives, table_reactives, main_data_reactives,
  lang
) {

  # renderUI ####
  output$save_container <- shiny::renderUI({
    ns <- session$ns
    shiny::tagList(
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          6, align = 'center',
          shiny::downloadButton(
            ns('save_map_btn'),
            label = translate_app('save_map_btn', lang())
          )
        ),
        shiny::column(
          6, align = 'center',
          shiny::downloadButton(
            ns('save_table_btn'),
            label = translate_app('save_table_btn', lang())
          )
        )
      ), # end of buttons row
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          6, offset = 6, align = 'center',
          shinyWidgets::prettyRadioButtons(
            ns('table_output_options'),
            label = translate_app('table_output_options_input', lang()),
            choices = c('csv', 'xlsx') |>
              purrr::set_names(translate_app(c('csv', 'xlsx'), lang())),
            status = 'info', fill = TRUE, shape = 'round'
          )
        )
      )
    )
  }) # end of renderUI

  # download handlers ####
  # map
  output$save_map_btn <- shiny::downloadHandler(
    filename = function() {
      glue::glue("{stringr::str_remove_all(Sys.Date(), '-')}_fes_map.gpkg")
    },
    content = function(filename) {
      sf::st_write(map_reactives$map_data, filename)
    }
  )
  # table
  output$save_table_btn <- shiny::downloadHandler(
    filename = function() {
      glue::glue(
        "{stringr::str_remove_all(Sys.Date(), '-')}_nfi_data.",
        "{input$table_output_options}"
      )
    },
    content = function(filename) {
      data_to_write <- table_reactives$table_data

      if (input$table_output_options == 'csv') {
        readr::write_csv(data_to_write, filename)
      } else {
        writexl::write_xlsx(data_to_write, filename)
      }
    }
  )
}

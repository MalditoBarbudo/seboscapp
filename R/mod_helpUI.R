#' @title mod_helpUI and mod_help
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_helpUI <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # ui
  shiny::uiOutput(ns("help_container"))
}

#' mod_help
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data_reactives,viz_reactives reactives needed
#' @param var_thes thesauruses
#' @param lang language selected
#'
#' @export
#'
#' @rdname mod_helpUI
mod_help <- function(
  input, output, session,
  data_reactives, viz_reactives,
  var_thes, lang
) {

  ## renderUI ####
  output$help_container <- shiny::renderUI({

    ns <- session$ns
    data_version <- shiny::isolate(data_reactives$data_version)
    data_scale <- shiny::isolate(data_reactives$data_scale)

    var_choices <- var_thes %>%
      dplyr::filter(var_table == data_version) %>%
      dplyr::pull(var_id) %>%
      magrittr::set_names(translate_var(
        ., data_version, data_scale, lang(), var_thes
      ))
    selected_choice <- viz_reactives$viz_color

    # tagList
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          8, #align = 'center',
          shiny::br(),
          shinyWidgets::pickerInput(
            ns('glossary_var'),
            translate_app('glossary_var_input', lang()),
            choices = var_choices, width = '100%',
            selected = selected_choice,
            options = list(
              `size` = 10,
              `live-search` = TRUE,
              `action-box` = FALSE
            )
          ),
          shiny::br(),
          shiny::tags$strong(translate_app("var_description_title", lang())),
          shiny::textOutput(ns('var_description_panel')),
          shiny::tags$strong(translate_app("var_units_title", lang())),
          shiny::textOutput(ns('var_units_panel'))
        ),
        shiny::column(
          4, align = 'center',
          shiny::br(),
          shiny::tags$a(
            translate_app('link_to_tutorials_text', lang()),
            href = "http://laboratoriforestal.creaf.uab.cat/tutorial/fes_app/"
          )
        )
      )
    ) # end of tagList
  }) # end of renderUI

  output$var_description_panel <- shiny::renderText({
    shiny::validate(
      shiny::need(input$glossary_var, 'no var selected yet')
    )

    data_version <- shiny::isolate(data_reactives$data_version)

    var_description <- var_thes %>%
      dplyr::filter(
        var_id == input$glossary_var,
        var_table == data_version
      ) %>%
      dplyr::select(tidyselect::any_of(
        c(glue::glue("var_description_{lang()}"))
      )) %>%
      purrr::flatten_chr() %>%
      unique()

    return(var_description)
  })

  output$var_units_panel <- shiny::renderText({
    shiny::validate(
      shiny::need(input$glossary_var, 'no var selected yet')
    )

    data_version <- shiny::isolate(data_reactives$data_version)

    var_units <- var_thes %>%
      dplyr::filter(
        var_id == input$glossary_var,
        var_table == data_version
      ) %>%
      dplyr::select(tidyselect::any_of(c(
        glue::glue("var_units")
      ))) %>%
      purrr::flatten_chr() %>%
      unique()
    if (length(var_units) < 1) {
      var_units <- '-'
    }

    return(var_units)
  })

}

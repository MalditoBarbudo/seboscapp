#' mod_vizInput and mod_viz
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#'
#' @export
mod_vizInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(ns('mod_viz_panel'))
  )
}

#' mod_viz server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives reactives needed
#' @param var_thes thesauruses
#' @param lang lang value
#' @param cache memoryCache object to store the selected col
#'
#' @export
#'
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  data_reactives,
  var_thes, lang, cache
) {

  ## renderUI ####
  output$mod_viz_panel <- shiny::renderUI({

    shiny::validate(
      shiny::need(data_reactives$data_version, 'no inputs yet')
    )

    ns <- session$ns

    # precalculated choices
    color_choices <- var_thes %>%
      dplyr::filter(var_table == data_reactives$data_version) %>%
      dplyr::pull(var_id) %>%
      magrittr::extract(
        stringr::str_detect(., pattern = '^admin_|^plot_', negate = TRUE)
      ) %>%
      magrittr::set_names(translate_app(., lang()))
    selected_color <- cache_selected_choice(
      color_choices, cache, 'selectedcol'
    )

    statistic_choices <- c(
      'mean', 'se', 'min', 'max', 'n'
    ) %>%
      magrittr::set_names(translate_app(., lang()))
    selected_statistic <- cache_selected_choice(
      statistic_choices, cache, 'selectedstatistic', 'mean'
    )

    # tagList ####
    shiny::tagList(
      shiny::h4(translate_app('h4_viz', lang())),
      shiny::fluidRow(
        shiny::column(
          8,
          shinyWidgets::pickerInput(
            ns('viz_color'),
            translate_app('viz_color_input', lang()),
            choices = color_choices,
            selected = selected_color,
            options = shinyWidgets::pickerOptions(
              actionsBox = FALSE,
              noneSelectedText = translate_app(
                'deselect-all-text', lang()
              ),
              selectAllText = translate_app(
                'select-all-text', lang()
              ),
              selectedTextFormat =  'count',
              countSelectedText = translate_app(
                'count-selected-text-value', lang()
              ),
              size = 10,
              liveSearch = TRUE,
              tickIcon = 'glyphicon-tree-deciduous'
            )
          ),
          {
            if (data_reactives$data_scale == 'local') {
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_statistic'),
                  translate_app('viz_color_input', lang()),
                  choices = statistic_choices,
                  selected = selected_statistic,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = FALSE,
                    noneSelectedText = translate_app(
                      'deselect-all-text', lang()
                    ),
                    selectAllText = translate_app(
                      'select-all-text', lang()
                    ),
                    selectedTextFormat =  'count',
                    countSelectedText = translate_app(
                      'count-selected-text-value', lang()
                    ),
                    size = 10,
                    liveSearch = TRUE,
                    tickIcon = 'glyphicon-tree-deciduous'
                  )
                )
              )
            } else {
              shinyWidgets::pickerInput(
                ns('viz_statistic'),
                translate_app('viz_color_input', lang()),
                choices = statistic_choices,
                selected = selected_statistic,
                options = shinyWidgets::pickerOptions(
                  actionsBox = FALSE,
                  noneSelectedText = translate_app(
                    'deselect-all-text', lang()
                  ),
                  selectAllText = translate_app(
                    'select-all-text', lang()
                  ),
                  selectedTextFormat =  'count',
                  countSelectedText = translate_app(
                    'count-selected-text-value', lang()
                  ),
                  size = 10,
                  liveSearch = TRUE,
                  tickIcon = 'glyphicon-tree-deciduous'
                )
              )
            }
          }
        )
      ) # end of fluidRow
    ) # end of tagList
  }) # end of renderUI

  # observers ####
  # make visible statistic selecctor if needed
  shiny::observe({

    shiny::validate(
      shiny::need(data_reactives$data_scale, 'no inputs yet')
    )

    if (data_reactives$data_scale == 'local') {
      shinyjs::reset('viz_statistic')
      shinyjs::disable('viz_statistic')
      shinyjs::hide('viz_statistic')
    } else {
      shinyjs::enable('viz_statistic')
      shinyjs::show('viz_statistic')
    }
  })
  # update cache
  shiny::observe({
    shiny::validate(shiny::need(input$viz_color, 'no input yet'))
    selected_color <- input$viz_color
    cache$set('selectedcol', selected_color)
  })

  shiny::observe({
    shiny::validate(shiny::need(input$viz_statistic, 'no input yet'))
    selected_statistic <- input$viz_statistic
    cache$set('selectedstatistic', selected_statistic)
  })

  # return the viz inputs
  viz_reactives <- shiny::reactiveValues()
  shiny::observe({
    viz_reactives$viz_color <- input$viz_color
    viz_reactives$viz_statistic <- input$viz_statistic
  })
  return(viz_reactives)
}
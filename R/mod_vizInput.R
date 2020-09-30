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

    data_version <- data_reactives$data_version
    data_scale <- data_reactives$data_scale

    # precalculated choices
    color_choices <- var_thes %>%
      dplyr::filter(var_table == data_version) %>%
      dplyr::pull(var_id) %>%
      magrittr::extract(
        stringr::str_detect(., pattern = '^admin_|^plot_', negate = TRUE)
      ) %>%
      magrittr::set_names(
        translate_var(., data_version, 'local', lang(), var_thes)
      )
    selected_color <- cache_selected_choice(
      color_choices, cache, 'selectedcol', 'mushrooms_production'
    )

    statistic_choices <- c(
      'mean', 'se', 'min', 'max', 'q05', 'q95', 'n'
    ) %>%
      magrittr::set_names(translate_app(., lang()))
    selected_statistic <- cache_selected_choice(
      statistic_choices, cache, 'selectedstatistic', 'mean'
    )

    selected_pal_config <- cache$get('selectedpalconfig', 'normal')
    selected_pal_reverse <- cache$get('selectedpalreverse', FALSE)

    # tagList ####
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          8,
          shiny::h4(translate_app('h4_servei', lang())),
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
          )
        )
      ),
      # visual aid for "differences of differences" services
      shinyjs::hidden(
        shiny::div(
          id = ns('diff_of_diffs'),
          shiny::fluidRow(
            shiny::column(
              8,
              translate_app('diff_of_diffs', lang())
            )
          )
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          8, shiny::h4(translate_app('h4_viz', lang()))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          {
            if (data_scale == 'local') {
              shinyjs::disabled(
                shinyWidgets::pickerInput(
                  ns('viz_statistic'),
                  translate_app('viz_statistic_input', lang()),
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
                translate_app('viz_statistic_input', lang()),
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
        ),
        shiny::column(
          6, align = 'center',
          # low, normal or high palette
          shinyWidgets::radioGroupButtons(
            ns('viz_pal_config'),
            translate_app('viz_pal_config_input', lang()),
            size = 'sm',
            choices = c('high', 'normal', 'low') %>%
              magrittr::set_names(c(
                translate_app('pal_high', lang()),
                translate_app('pal_normal', lang()),
                translate_app('pal_low', lang())
              )),
            selected = selected_pal_config, direction = 'vertical',
            checkIcon = list(
              yes = shiny::icon('tree-deciduous', lib = 'glyphicon')
            ),
            status = 'lfc_radiogroupbuttons'
          ),
          # reverse palette
          shinyWidgets::awesomeCheckbox(
            ns('viz_pal_reverse'),
            label = translate_app('viz_pal_reverse_input', lang()),
            value = selected_pal_reverse, status = 'info'
          )
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
      # shinyjs::hide('viz_statistic')
    } else {
      shinyjs::enable('viz_statistic')
      # shinyjs::show('viz_statistic')
    }
  })
  # make visible diff of diffs visual aid
  shiny::observe({

    shiny::validate(
      shiny::need(data_reactives$data_version, 'no inputs yet')
    )

    if (
      input$viz_color %in% c('carbon_sequestration', 'wood') &&
      data_reactives$data_version %in% c('plot_nfi3_nfi4_results')
    ) {
      shinyjs::show('diff_of_diffs')
    } else {
      shinyjs::hide('diff_of_diffs')
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

  shiny::observe({
    shiny::validate(shiny::need(input$viz_pal_config, 'no_input_yet'))
    selected_pal_config <- input$viz_pal_config
    cache$set('selectedpalconfig', selected_pal_config)
  })

  shiny::observe({
    shiny::validate(shiny::need(input$viz_pal_reverse, 'no_input_yet'))
    selected_pal_reverse <- input$viz_pal_reverse
    cache$set('selectedpalreverse', selected_pal_reverse)
  })

  # return the viz inputs
  viz_reactives <- shiny::reactiveValues()
  shiny::observe({
    viz_reactives$viz_color <- input$viz_color
    viz_reactives$viz_statistic <- input$viz_statistic
    viz_reactives$viz_pal_config <- input$viz_pal_config
    viz_reactives$viz_pal_reverse <- input$viz_pal_reverse
  })
  return(viz_reactives)
}

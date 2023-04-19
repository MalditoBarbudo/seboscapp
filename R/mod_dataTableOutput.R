#' mod_dataTableOutput and mod_dataTable
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataTableOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    DT::DTOutput(ns('main_table'))
  )
}

#' mod_dataTable server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param main_data_reactives,data_reactives reactives from other modules
#' @param var_thes thesauruses
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_dataTableOutput
mod_dataTable <- function(
  input, output, session,
  main_data_reactives, data_reactives,
  var_thes, lang
) {

  # data reactive
  table_data <- shiny::reactive({

    shiny::validate(
      shiny::need(main_data_reactives$raw_data, 'no raw data'),
      shiny::need(main_data_reactives$summ_data, 'no summ data')
    )

    # get the scale
    data_scale <- shiny::isolate(data_reactives$data_scale)

    # if local scale, then the raw data is ok
    if (data_scale == 'local') {
      res <- main_data_reactives$raw_data |>
        dplyr::as_tibble() |>
        dplyr::select(-geometry)
    } else {
      res <- main_data_reactives$summ_data
    }

    return(res)
  })

  # table output
  output$main_table <- DT::renderDT({
    # validation
    shiny::validate(
      shiny::need(table_data(), 'no data yet')
    )

    data_version <- shiny::isolate(data_reactives$data_version)
    data_scale <- shiny::isolate(data_reactives$data_scale)

    # DT
    table_data() |>
      dplyr::mutate_if(
        is.numeric, round, 3
      ) |>
      DT::datatable(
        rownames = FALSE,
        colnames = translate_var(
          names(table_data()), data_version, data_scale, lang(), var_thes
        ),
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        # extensions = 'Buttons',
        options = list(
          pageLength = 15,
          dom = 'tip',
          # buttons = I('colvis'),
          autoWidth = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
            "$(this.api().table().body()).css({'font-family': 'Montserrat'});",
            "}"
          )
        )
      )
  })

  # reactives to return
  table_reactives <- shiny::reactiveValues()
  shiny::observe({
    table_reactives$table_data <- table_data()
  })
  return(table_reactives)

}

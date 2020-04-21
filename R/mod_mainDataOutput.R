#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data as tbl_sql
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_mainData server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives reactives from modules
#' @param fesdb object to access the fes db
#' @param lang lang selected
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  data_reactives, fesdb, lang
) {

  # raw data
  raw_data <- shiny::reactive({

    shiny::validate(
      shiny::need(data_reactives$data_version, 'no inputs yet')
    )

    # data version
    data_version <- data_reactives$data_version
    # table
    res <- fesdb$get_data(data_version, TRUE)
    return(res)
  })

  # summarised data
  summ_data <- shiny::reactive({

    shiny::validate(
      shiny::need(data_reactives$data_scale, 'no inputs yet'),
      shiny::need(raw_data(), 'no raw data yet')
    )

    # scale
    data_scale <- data_reactives$data_scale

    if (data_scale == 'local') {
      return(raw_data())
    }

    summ_data <- raw_data() %>%
      dplyr::as_tibble() %>%
      dplyr::select(-geometry) %>%
      dplyr::group_by(!! rlang::sym(data_scale)) %>%
      dplyr::summarise_if(
        is.numeric,
        .funs = list(
          mean = ~ mean(., na.rm = TRUE),
          se = ~ sd(., na.rm = TRUE)/sqrt(n()),
          min = ~ min(., na.rm = TRUE),
          max = ~ max(., na.rm = TRUE),
          n = ~ n()
        )
      )

    return(summ_data)
  })

  # reactive to return
  main_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    main_data_reactives$raw_data <- raw_data()
    main_data_reactives$summ_data <- summ_data()
  })
  return(main_data_reactives)
}

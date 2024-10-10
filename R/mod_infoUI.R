#' @title mod_infoUI and mod_info
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_infoUI <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # ui skeleton (rows)
  shiny::tagList(
    shiny::fluidRow(
      shiny::br(),
      shiny::h4(shiny::textOutput(ns('plot_title'))),
      shiny::plotOutput(ns("info_plot"))
    )
  )
}

#' mod_info server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param map_reactives,data_reactives,viz_reactives reactives
#' @param var_thes thesauruses
#' @param lang lang reactive
#'
#' @export
mod_info <- function(
  input, output, session,
  map_reactives, data_reactives, viz_reactives,
  var_thes, lang, type = c("plot", "poly")
) {

  ns <- session$ns

  waiter_plot <- waiter::Waiter$new(
    id = ns('info_plot'),
    html = waiter::spin_timer(),
    color = "#444444"
  )

  ## outputs ####
  # info plot output
  output$info_plot <- shiny::renderPlot({

    waiter_plot$show()

    # data, scale and color variable
    data_scale <- data_reactives$data_scale
    data_version <- data_reactives$data_version
    viz_color <- viz_reactives$viz_color
    # necessary changes when local or not
    if (data_scale == 'local') {
      data_scale <- 'plot_id'
    } else {
      viz_color <- glue::glue("{viz_color}_{viz_reactives$viz_statistic}")
      if (data_scale %in% c('file', 'drawn_polygon')) {
        data_scale <- 'poly_id'
      }
    }
    map_data <- map_reactives$map_data |>
      dplyr::filter(
        !is.na(!! rlang::sym(viz_color)),
        # min and max creates Inf when all vector values are NAs, remove them
        !is.infinite(!! rlang::sym(viz_color))
      )
    # click info
    if (type == "plot") {
      # remember that deckgl is 0 indexed, so we add 1 to the declared index
      fes_map_shape_click <-
        jsonlite::fromJSON(shiny::req(map_reactives$fes_map_plot_click))$index + 1
    } else {
      fes_map_shape_click <- which(
        map_data[[data_scale]] == jsonlite::fromJSON(shiny::req(map_reactives$fes_map_poly_click))$object$properties$id
      )
    }

    temp_plot <- map_data |>
      dplyr::rename(
        y_var = !! rlang::sym(viz_color),
        label_var = !! rlang::sym(data_scale)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = 0, y = y_var))

    # case 1 row, for gray points to not appear. Like for example file or
    # drawn polygon with only one element
    if (nrow(map_data) > 1) {
      temp_plot <- temp_plot +
        ggplot2::geom_point(
          data = ~ dplyr::slice(.x, -fes_map_shape_click),
          colour = '#606060', size = 4, alpha = 0.5,
          position = ggplot2::position_jitter(
            width = .2, height = 0, seed = 25
          )
        )
    }
    # case less than 3 rows, not violin plots because it fails. Like for example
    # when file or custom polygon use less than 3 plots
    if (nrow(map_data) > 2) {
      temp_plot <- temp_plot +
        ggplot2::geom_violin(fill = 'transparent')
    }

    temp_plot +
      ggplot2::geom_point(
        data = ~ dplyr::slice(.x, fes_map_shape_click),
        colour = '#22B0C6', size = 6
      ) +
        ggplot2::scale_x_continuous(breaks = NULL) +
        ggplot2::labs(
          x = '',
          y = translate_var(
            viz_color, data_version, data_scale, lang(), var_thes
          )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 14, color = '#606060'),
        axis.text = ggplot2::element_text(color = '#606060'),
        strip.text = ggplot2::element_text(color = '#606060'),
        panel.background = ggplot2::element_rect(
          fill = '#F8F9FA', colour = NA
        ),
        plot.background = ggplot2::element_rect(
          fill = '#F8F9FA', colour = NA
        ),
        strip.background = ggplot2::element_rect(
          fill = '#F8F9FA', colour = NA
        ),
        panel.grid = ggplot2::element_line(colour = '#606060'),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          size = ggplot2::rel(0.5), colour = '#606060'
        )
      )
  })

  output$plot_title <- shiny::renderText({

    data_scale <- data_reactives$data_scale
    data_version <- data_reactives$data_version
    viz_color <- viz_reactives$viz_color
    # necessary changes when local or not
    if (data_scale == 'local') {
      data_scale <- 'plot_id'
    } else {
      viz_color <- glue::glue("{viz_color}_{viz_reactives$viz_statistic}")
      if (data_scale %in% c('file', 'drawn_polygon')) {
        data_scale <- 'poly_id'
      }
    }

    glue::glue(
      "{translate_var(viz_color, data_version, data_scale, lang(), var_thes)} ",
      translate_app(glue::glue("{data_scale}_info_plot_title"), lang())
    )
  })

}

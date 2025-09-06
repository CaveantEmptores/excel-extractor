# file: R/plot_bar_line.R

#' Combo: vertical bars (single/aggregated) + line (sec-axis supported)
#'
#' Bars are summed per `X` if multiple series are present. Lines can be multiple series.
#'
#' @inheritParams plot_cbar_line
#' @export
#' @importFrom dplyr filter group_by summarise mutate n_distinct
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point labs theme scale_y_continuous scale_x_discrete element_text
#' @importFrom ggplot2 expansion sec_axis
#' @importFrom stringr str_wrap
#' @importFrom scales label_percent label_number
#' @importFrom rlang .data
plot_bar_line <- function(df, width = 8, height = 5, base_size = 12,
                          out_file = NULL, th = brand_theme_defaults,
                          palette_line = NULL,
                          show_bar_labels = "auto",
                          bar_label_format = scales::label_number(accuracy = 0.1),
                          show_line_labels = "auto",
                          line_label_format_percent = scales::label_percent(accuracy = 1),
                          line_label_format_number  = scales::label_number(accuracy = 0.1),
                          line_size = 1.2, line_point_size = 3.0,
                          title = NULL, subtitle = NULL, annotations = NULL) {

  use_brand_fonts(th$family_text, "Montserrat")
  df <- dplyr::filter(df, is_visible(.data$IsActive))

  has_lbl <- "Point_HasDataLabel_Visible" %in% names(df)
  bars <- df %>%
    dplyr::filter(.data$Series_ElementType %in% c("Column","Bar")) %>%
    dplyr::group_by(.data$X) %>%
    dplyr::summarise(
      Y = sum(.data$Y, na.rm = TRUE),
      Point_HasDataLabel_Visible = if (has_lbl) any(.data$Point_HasDataLabel_Visible, na.rm = TRUE) else NA,
      .groups = "drop"
    ) %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = unique(.data$X)))

  lines <- df %>%
    dplyr::filter(.data$Series_ElementType == "Line") %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = unique(.data$X)))

  if (nrow(bars) == 0 || nrow(lines) == 0) rlang::abort("plot_bar_line() needs \u22651 bar and \u22651 line series.", call = NULL)

  ttl <- wrap_to_lines(title %||% (df$Chart_Title %>% unique() %>% .[!is.na(.)] %>% utils::head(1)),
                       wrap_chars(th$wrap_title_chars_base, width))
  sub <- wrap_to_lines(subtitle, wrap_chars(th$wrap_subtitle_chars_base, width))

  p_rng <- compute_axis_ranges(df, .data$AxisGroup == 1)
  s_rng <- compute_axis_ranges(df, .data$AxisGroup == 2 & .data$Series_ElementType == "Line")
  use_secondary <- is.finite(s_rng$min) && is.finite(s_rng$max) && (s_rng$max - s_rng$min) > 0
  if (use_secondary) lines <- dplyr::mutate(lines, Y_rescaled = rescale_to_primary(.data$Y, p_rng$min, p_rng$max, s_rng$min, s_rng$max))
  else               lines <- dplyr::mutate(lines, Y_rescaled = .data$Y)

  if (is.null(palette_line)) {
    pal <- auto_palettes_for_chart(df, "bar_line")
    palette_line <- pal$color
  }

  fill_col <- COL_FOCUS_GREEN

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(data = bars, ggplot2::aes(x = .data$X_cat, y = .data$Y),
                      width = th$bar_width, fill = fill_col) +
    ggplot2::geom_line(data = lines, ggplot2::aes(x = .data$X_cat, y = .data$Y_rescaled, group = .data$Series, color = .data$Series),
                       linewidth = line_size) +
    ggplot2::geom_point(data = lines, ggplot2::aes(x = .data$X_cat, y = .data$Y_rescaled, color = .data$Series),
                        size = line_point_size) +
    ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = NULL) +
    theme_article_responsive(th, base_size, width, height) +
    scale_brand_color(palette_line) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_chars(th$wrap_x_labels_chars_base, width)))

  if (use_secondary) {
    p <- p + ggplot2::scale_y_continuous(
      expand   = ggplot2::expansion(mult = c(0, 0.15)),
      sec.axis = ggplot2::sec_axis(~ (. - p_rng$min) / (p_rng$max - p_rng$min) * (s_rng$max - s_rng$min) + s_rng$min, name = NULL)
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
  }

  if (!identical(show_bar_labels, "off")) {
    show <- if (!all(is.na(bars$Point_HasDataLabel_Visible))) isTRUE(bars$Point_HasDataLabel_Visible) else TRUE
    labs_vec <- ifelse(show | identical(show_bar_labels, "on"), bar_label_format(bars$Y), "")
    p <- p + ggplot2::geom_text(
      data = bars,
      ggplot2::aes(x = .data$X_cat, y = .data$Y, label = labs_vec),
      vjust = -0.5, size = pt_to_mm(th$size_annot), color = th$axis_color,
      family = th$family_text
    )
  }

  if (!identical(show_line_labels, "off")) {
    is_rate <- max(abs(lines$Y), na.rm = TRUE) <= 1.05
    lf <- if (is_rate) line_label_format_percent else line_label_format_number
    lines$._lbl_show <- if ("Point_HasDataLabel_Visible" %in% names(lines)) isTRUE(lines$Point_HasDataLabel_Visible) else TRUE
    if (identical(show_line_labels, "on") || any(lines$._lbl_show, na.rm = TRUE)) {
      p <- p + ggplot2::geom_text(
        data = dplyr::mutate(lines, .lab = ifelse(.data$._lbl_show, lf(.data$Y), "")),
        ggplot2::aes(x = .data$X_cat, y = .data$Y_rescaled, label = .data$.lab, color = .data$Series),
        vjust = -1.0, size = pt_to_mm(th$size_annot), fontface = "bold",
        family = th$family_text, show.legend = FALSE
      )
    }
  }

  y_top_guess <- max(c(bars$Y, lines$Y_rescaled), na.rm = TRUE) * 1.02
  p <- apply_annotations(p, annotations, y_top_guess = y_top_guess, color = th$axis_color)

  if (!is.null(out_file)) save_svg(p, out_file, width, height)
  print(p); invisible(p)
}

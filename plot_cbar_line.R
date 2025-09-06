# file: R/plot_cbar_line.R

#' Combo: clustered columns + line (optional secondary axis)
#' @export
#' @inheritParams plot_cbar
#' @param palette_line Optional named palette for lines.
#' @param show_bar_labels,bar_label_format As in [plot_cbar()].
#' @param show_line_labels "auto"|"on"|"off".
#' @param line_label_format_percent,line_label_format_number formatters for line labels.
#' @param line_size,line_point_size numeric sizes.
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point labs theme guides guide_legend
#' @importFrom ggplot2 sec_axis scale_y_continuous scale_x_discrete element_text expansion
#' @importFrom dplyr filter mutate n_distinct
#' @importFrom stringr str_wrap
#' @importFrom scales label_percent label_number
#' @importFrom rlang .data
plot_cbar_line <- function(df, width = 8, height = 5, base_size = 12,
                           out_file = NULL, th = brand_theme_defaults,
                           palette_fill = NULL, palette_line = NULL,
                           show_bar_labels = "auto",
                           bar_label_format = scales::label_number(accuracy = 0.1),
                           show_line_labels = "auto",
                           line_label_format_percent = scales::label_percent(accuracy = 1),
                           line_label_format_number  = scales::label_number(accuracy = 0.1),
                           line_size = 1.2, line_point_size = 3.0,
                           title = NULL, subtitle = NULL, annotations = NULL) {

  use_brand_fonts(th$family_text, "Montserrat")
  df <- dplyr::filter(df, is_visible(.data$IsActive))

  bars  <- df %>% dplyr::filter(.data$Series_ElementType %in% c("Column","Bar")) %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = unique(.data$X)))
  lines <- df %>% dplyr::filter(.data$Series_ElementType == "Line") %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = unique(.data$X)))
  if (nrow(bars) == 0 || nrow(lines) == 0) rlang::abort("plot_cbar_line() needs \u22651 bar and \u22651 line series.", call = NULL)

  ttl <- wrap_to_lines(title %||% (df$Chart_Title %>% unique() %>% .[!is.na(.)] %>% utils::head(1)),
                       wrap_chars(th$wrap_title_chars_base, width))
  sub <- wrap_to_lines(subtitle, wrap_chars(th$wrap_subtitle_chars_base, width))

  p_rng <- compute_axis_ranges(df, AxisGroup == 1)
  s_rng <- compute_axis_ranges(df, AxisGroup == 2 & Series_ElementType == "Line")
  use_secondary <- is.finite(s_rng$min) && is.finite(s_rng$max) && (s_rng$max - s_rng$min) > 0
  if (use_secondary) lines <- dplyr::mutate(lines, Y_rescaled = rescale_to_primary(.data$Y, p_rng$min, p_rng$max, s_rng$min, s_rng$max))
  else               lines <- dplyr::mutate(lines, Y_rescaled = .data$Y)

  if (is.null(palette_fill) || is.null(palette_line)) {
    pal <- auto_palettes_for_chart(df, "cbar_line")
    palette_fill <- palette_fill %||% pal$fill
    palette_line <- palette_line %||% pal$color
  }

  n_leg <- dplyr::n_distinct(c(bars$Series, lines$Series))
  leg   <- legend_layout(n_leg, width, height)
  inside <- leg$position == "inside" && !leg$bottom
  extra_top <- if (inside) (leg$rows * 0.9) else 0

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(data = bars, ggplot2::aes(x = .data$X_cat, y = .data$Y, fill = .data$Series),
                      position = ggplot2::position_dodge2(preserve = "single", padding = 0.2),
                      width = th$bar_width) +
    ggplot2::geom_line(data = lines, ggplot2::aes(x = .data$X_cat, y = .data$Y_rescaled, group = .data$Series, color = .data$Series),
                       linewidth = line_size) +
    ggplot2::geom_point(data = lines, ggplot2::aes(x = .data$X_cat, y = .data$Y_rescaled, color = .data$Series),
                        size = line_point_size) +
    ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = NULL) +
    theme_article_responsive(th, base_size, width, height, extra_top_lines = extra_top) +
    scale_brand_fill(palette_fill) + scale_brand_color(palette_line) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_chars(th$wrap_x_labels_chars_base, width)))

  if (use_secondary) {
    p <- p + ggplot2::scale_y_continuous(
      expand   = ggplot2::expansion(mult = c(0, 0.15)),
      sec.axis = ggplot2::sec_axis(~ (. - p_rng$min) / (p_rng$max - p_rng$min) * (s_rng$max - s_rng$min) + s_rng$min, name = NULL)
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
  }

  if (needs_tilt(levels(bars$X_cat), th$x_tilt_threshold_chars)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = th$x_tilt_angle_deg, hjust = 1))
  }

  if (inside) {
    p <- p + ggplot2::theme(legend.position = th$legend_xy, legend.justification = th$legend_just) +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE),
             fill  = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  } else {
    p <- p + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE),
             fill  = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  }

  if (!identical(show_bar_labels, "off")) {
    labs_vec <- make_bar_labels(bars, label_format = bar_label_format)
    if (identical(show_bar_labels, "on") || any(nzchar(labs_vec))) {
      p <- p + ggplot2::geom_text(data = bars,
               ggplot2::aes(x = .data$X_cat, y = .data$Y, label = labs_vec, group = .data$Series),
               position = ggplot2::position_dodge2(preserve = "single", padding = 0.2),
               vjust = -0.5, size = pt_to_mm(th$size_annot), color = th$axis_color,
               family = th$family_text)
    }
  }

  if (!identical(show_line_labels, "off")) {
    is_rate <- max(abs(lines$Y), na.rm = TRUE) <= 1.05
    lf <- if (is_rate) line_label_format_percent else line_label_format_number
    if ("Point_HasDataLabel_Visible" %in% names(lines)) {
      lines$._lbl_show <- isTRUE(lines$Point_HasDataLabel_Visible)
    } else lines$._lbl_show <- TRUE
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

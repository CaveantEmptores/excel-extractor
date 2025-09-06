# file: R/plot_line.R

#' Simple line chart
#' @export
#' @inheritParams plot_cbar
#' @param palette_line Optional named palette for lines.
#' @param line_size,point_size numeric sizes.
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme guides guide_legend scale_x_discrete element_text
#' @importFrom dplyr filter mutate n_distinct
#' @importFrom stringr str_wrap
#' @importFrom rlang .data
plot_line <- function(df, width = 8, height = 5, base_size = 12,
                      out_file = NULL, th = brand_theme_defaults,
                      palette_line = NULL,
                      line_size = 1.2, point_size = 3.0,
                      title = NULL, subtitle = NULL, annotations = NULL) {
  use_brand_fonts(th$family_text, "Montserrat")
  df <- dplyr::filter(df, is_visible(.data$IsActive))
  df <- df %>% dplyr::filter(.data$Series_ElementType == "Line") %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = unique(.data$X)))

  ttl <- wrap_to_lines(title %||% (df$Chart_Title %>% unique() %>% .[!is.na(.)] %>% utils::head(1)),
                       wrap_chars(th$wrap_title_chars_base, width))
  sub <- wrap_to_lines(subtitle, wrap_chars(th$wrap_subtitle_chars_base, width))

  if (is.null(palette_line)) palette_line <- auto_palettes_for_chart(df, "line")$color

  n_leg <- dplyr::n_distinct(df$Series)
  leg   <- legend_layout(n_leg, width, height)
  inside <- leg$position == "inside" && !leg$bottom
  extra_top <- if (inside) (leg$rows * 0.9) else 0

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$X_cat, y = .data$Y, group = .data$Series, color = .data$Series)) +
    ggplot2::geom_line(linewidth = line_size) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = NULL) +
    theme_article_responsive(th, base_size, width, height, extra_top_lines = extra_top) +
    scale_brand_color(palette_line) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_chars(th$wrap_x_labels_chars_base, width)))

  if (needs_tilt(levels(df$X_cat), th$x_tilt_threshold_chars)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = th$x_tilt_angle_deg, hjust = 1))
  }

  if (inside) {
    p <- p + ggplot2::theme(legend.position = th$legend_xy, legend.justification = th$legend_just) +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  } else {
    p <- p + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  }

  y_top_guess <- max(df$Y, na.rm = TRUE) * 1.02
  p <- apply_annotations(p, annotations, y_top_guess = y_top_guess, color = th$axis_color)

  if (!is.null(out_file)) save_svg(p, out_file, width, height)
  print(p); invisible(p)
}

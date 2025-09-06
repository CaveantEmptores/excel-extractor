# file: R/plot_cbar_stack.R

#' Stacked vertical columns (including 100%)
#' @export
#' @inheritParams plot_cbar
#' @param stack100 logical; if TRUE use 100% stacks.
#' @param label_threshold minimum segment percent to show label (0-1).
#' @param label_format format function for percentages.
#' @importFrom ggplot2 ggplot aes geom_col labs theme guides guide_legend position_stack scale_x_discrete element_text
#' @importFrom ggplot2 expansion
#' @importFrom dplyr filter mutate n_distinct group_by summarise pull
#' @importFrom stringr str_wrap
#' @importFrom rlang .data
plot_cbar_stack <- function(df, width = 8, height = 5, base_size = 12,
                            out_file = NULL, th = brand_theme_defaults,
                            palette_fill = NULL,
                            stack100 = FALSE, label_threshold = 0.07,
                            label_format = scales::label_percent(accuracy = 1),
                            title = NULL, subtitle = NULL, annotations = NULL) {
  use_brand_fonts(th$family_text, "Montserrat")
  df <- dplyr::filter(df, is_visible(.data$IsActive))
  df_bar <- df %>% dplyr::filter(.data$Series_ElementType %in% c("Column","Bar")) %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = unique(.data$X)))

  ttl <- wrap_to_lines(title %||% (df$Chart_Title %>% unique() %>% .[!is.na(.)] %>% utils::head(1)),
                       wrap_chars(th$wrap_title_chars_base, width))
  sub <- wrap_to_lines(subtitle, wrap_chars(th$wrap_subtitle_chars_base, width))

  if (is.null(palette_fill)) {
    pal_code <- if (stack100) "cbar_stack100" else "cbar_stack"
    palette_fill <- auto_palettes_for_chart(df, pal_code)$fill
  }

  n_leg <- dplyr::n_distinct(df_bar$Series)
  leg   <- legend_layout(n_leg, width, height)
  inside <- leg$position == "inside" && !leg$bottom
  extra_top <- if (inside) (leg$rows * 0.9) else 0

  p <- ggplot2::ggplot(df_bar, ggplot2::aes(x = .data$X_cat, y = .data$Y, fill = .data$Series)) +
    { if (isTRUE(stack100)) ggplot2::geom_col(position = "fill", width = th$bar_width)
      else ggplot2::geom_col(position = "stack", width = th$bar_width) } +
    ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = NULL) +
    theme_article_responsive(th, base_size, width, height, extra_top_lines = extra_top) +
    scale_brand_fill(palette_fill) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_chars(th$wrap_x_labels_chars_base, width)))

  if (needs_tilt(levels(df_bar$X_cat), th$x_tilt_threshold_chars)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = th$x_tilt_angle_deg, hjust = 1))
  }

  if (inside) {
    p <- p + ggplot2::theme(legend.position = th$legend_xy, legend.justification = th$legend_just) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  } else {
    p <- p + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  }

  labs_vec <- df_bar %>%
    dplyr::mutate(.lbl = make_stacked_labels(dplyr::cur_data(), label_threshold = label_threshold,
                                             label_format = label_format)) %>% dplyr::pull(.data$.lbl)

  if (any(nzchar(labs_vec))) {
    p <- p + ggplot2::geom_text(
      data = df_bar,
      mapping = ggplot2::aes(x = .data$X_cat, y = .data$Y, label = labs_vec, group = .data$Series),
      position = ggplot2::position_stack(vjust = 0.5),
      size = pt_to_mm(th$size_annot), color = th$axis_color, family = th$family_text
    )
  }

  y_top_guess <- df_bar %>% dplyr::group_by(.data$X_cat) %>% dplyr::summarise(tt = sum(.data$Y, na.rm = TRUE), .groups = "drop") %>% dplyr::pull(.data$tt) %>% max(na.rm = TRUE) * 1.02
  p <- apply_annotations(p, annotations, y_top_guess = y_top_guess, color = th$axis_color)

  if (!is.null(out_file)) save_svg(p, out_file, width, height)
  print(p); invisible(p)
}

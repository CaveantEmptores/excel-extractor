# file: R/plot_chbar.R

#' Clustered horizontal bars (aka 'chbar')
#' @export
#' @inheritParams plot_cbar
#' @importFrom ggplot2 ggplot aes geom_col position_dodge2 labs scale_x_continuous theme guides guide_legend expansion
#' @importFrom dplyr filter mutate n_distinct
#' @importFrom rlang .data
plot_hbar <- function(df, width = 8, height = 5, base_size = 12,
                      out_file = NULL, th = brand_theme_defaults,
                      palette_fill = NULL,
                      show_labels = "auto", label_format = scales::label_number(accuracy = 0.1),
                      title = NULL, subtitle = NULL, annotations = NULL) {
  use_brand_fonts(th$family_text, "Montserrat")
  df <- dplyr::filter(df, is_visible(.data$IsActive))
  df_bar <- df %>% dplyr::filter(.data$Series_ElementType %in% c("Column","Bar")) %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = rev(unique(.data$X))))

  ttl <- wrap_to_lines(title %||% (df$Chart_Title %>% unique() %>% .[!is.na(.)] %>% utils::head(1)),
                       wrap_chars(th$wrap_title_chars_base, width))
  sub <- wrap_to_lines(subtitle, wrap_chars(th$wrap_subtitle_chars_base, width))

  if (is.null(palette_fill)) palette_fill <- auto_palettes_for_chart(df, "chbar")$fill

  n_leg <- dplyr::n_distinct(df_bar$Series)
  leg   <- legend_layout(n_leg, width, height)
  inside <- leg$position == "inside" && !leg$bottom
  extra_top <- if (inside) (leg$rows * 0.9) else 0

  p <- ggplot2::ggplot(df_bar, ggplot2::aes(y = .data$X_cat, x = .data$Y, fill = .data$Series)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single", padding = 0.2),
                      width = th$bar_width) +
    ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = NULL) +
    theme_article_responsive(th, base_size, width, height, extra_top_lines = extra_top) +
    scale_brand_fill(palette_fill) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))

  if (inside) {
    p <- p + ggplot2::theme(legend.position = th$legend_xy, legend.justification = th$legend_just) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  } else {
    p <- p + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg$ncol, byrow = TRUE))
  }

  if (!identical(show_labels, "off")) {
    labs_vec <- make_bar_labels(df_bar, label_format = label_format)
    if (identical(show_labels, "on") || any(nzchar(labs_vec))) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = labs_vec),
               position = ggplot2::position_dodge2(preserve = "single", padding = 0.2),
               hjust = -0.1, size = pt_to_mm(th$size_annot), color = th$axis_color,
               family = th$family_text)
    }
  }

  y_top_guess <- max(df_bar$Y, na.rm = TRUE) * 1.02
  p <- apply_annotations(p, annotations, y_top_guess = y_top_guess, color = th$axis_color)

  if (!is.null(out_file)) save_svg(p, out_file, width, height)
  print(p); invisible(p)
}

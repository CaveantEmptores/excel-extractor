# file: R/plot_hbar_simple.R

#' Horizontal bar chart (single-series; aggregates if multiple)
#'
#' If multiple bar/column series are present, values are summed per `X`.
#' Legend is suppressed (single fill). Uses brand theme & annotations.
#'
#' @inheritParams plot_bar
#' @export
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col labs scale_x_continuous theme
#' @importFrom ggplot2 expansion
#' @importFrom scales label_number
#' @importFrom rlang .data
plot_hbar_simple <- function(df, width = 8, height = 5, base_size = 12,
                             out_file = NULL, th = brand_theme_defaults,
                             palette_fill = NULL,
                             show_labels = "auto", label_format = scales::label_number(accuracy = 0.1),
                             title = NULL, subtitle = NULL, annotations = NULL) {

  use_brand_fonts(th$family_text, "Montserrat")
  df <- dplyr::filter(df, is_visible(.data$IsActive))

  has_lbl <- "Point_HasDataLabel_Visible" %in% names(df)
  df_bar <- df %>%
    dplyr::filter(.data$Series_ElementType %in% c("Column", "Bar")) %>%
    dplyr::group_by(.data$X) %>%
    dplyr::summarise(
      Y = sum(.data$Y, na.rm = TRUE),
      Point_HasDataLabel_Visible = if (has_lbl) any(.data$Point_HasDataLabel_Visible, na.rm = TRUE) else NA,
      .groups = "drop"
    ) %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = rev(unique(.data$X))))

  ttl <- wrap_to_lines(
    title %||% (df$Chart_Title %>% unique() %>% .[!is.na(.)] %>% utils::head(1)),
    wrap_chars(th$wrap_title_chars_base, width)
  )
  sub <- wrap_to_lines(subtitle, wrap_chars(th$wrap_subtitle_chars_base, width))

  fill_col <- if (is.null(palette_fill)) COL_FOCUS_GREEN else unname(palette_fill[1])

  p <- ggplot2::ggplot(df_bar, ggplot2::aes(y = .data$X_cat, x = .data$Y)) +
    ggplot2::geom_col(width = th$bar_width, fill = fill_col) +
    ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = NULL) +
    theme_article_responsive(th, base_size, width, height) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme(legend.position = "none")

  if (!identical(show_labels, "off")) {
    show <- if (!all(is.na(df_bar$Point_HasDataLabel_Visible))) isTRUE(df_bar$Point_HasDataLabel_Visible) else TRUE
    labs_vec <- ifelse(show | identical(show_labels, "on"), label_format(df_bar$Y), "")
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = labs_vec), hjust = -0.1,
      size = pt_to_mm(th$size_annot), color = th$axis_color,
      family = th$family_text
    )
  }

  y_top_guess <- max(df_bar$Y, na.rm = TRUE) * 1.02
  p <- apply_annotations(p, annotations, y_top_guess = y_top_guess, color = th$axis_color)

  if (!is.null(out_file)) save_svg(p, out_file, width, height)
  print(p); invisible(p)
}

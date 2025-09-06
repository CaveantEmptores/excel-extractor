# file: R/plot_cbar_smmult.R

#' Clustered vertical bars as small multiples
#'
#' Facets the chart into small multiples. By default facets by `Series`;
#' override the facet variable with `facet_var` if your data includes an
#' alternative (e.g., "Group", "Region", etc.).
#'
#' @inheritParams plot_cbar
#' @param facet_var character column name to facet by (defaults to "Series", or
#'   the nearest available among `facet_var`, "Group", "Series_Group").
#' @param ncol integer passed to [ggplot2::facet_wrap()].
#' @export
#' @importFrom dplyr filter mutate group_by summarise pull
#' @importFrom ggplot2 ggplot aes geom_col labs facet_wrap vars scale_y_continuous scale_x_discrete theme element_text position_dodge2
#' @importFrom ggplot2 expansion
#' @importFrom stringr str_wrap
#' @importFrom rlang sym .data
plot_cbar_smmult <- function(df, width = 8, height = 5, base_size = 12,
                             out_file = NULL, th = brand_theme_defaults,
                             palette_fill = NULL,
                             title = NULL, subtitle = NULL, annotations = NULL,
                             facet_var = "Series", ncol = NULL) {

  use_brand_fonts(th$family_text, "Montserrat")
  df <- dplyr::filter(df, is_visible(.data$IsActive))
  df_bar <- df %>%
    dplyr::filter(.data$Series_ElementType %in% c("Column","Bar")) %>%
    dplyr::mutate(X_cat = factor(.data$X, levels = unique(.data$X)))

  # choose facet variable with graceful fallback
  fv <- if (facet_var %in% names(df_bar)) {
    facet_var
  } else if ("Group" %in% names(df_bar)) {
    "Group"
  } else if ("Series_Group" %in% names(df_bar)) {
    "Series_Group"
  } else {
    "Series"
  }

  ttl <- wrap_to_lines(title %||% (df$Chart_Title %>% unique() %>% .[!is.na(.)] %>% utils::head(1)),
                       wrap_chars(th$wrap_title_chars_base, width))
  sub <- wrap_to_lines(subtitle, wrap_chars(th$wrap_subtitle_chars_base, width))

  if (is.null(palette_fill)) palette_fill <- auto_palettes_for_chart(df_bar, "cbar")$fill

  p <- ggplot2::ggplot(df_bar, ggplot2::aes(x = .data$X_cat, y = .data$Y, fill = .data$Series)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single", padding = 0.2),
                      width = th$bar_width) +
    ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = NULL) +
    theme_article_responsive(th, base_size, width, height) +
    scale_brand_fill(palette_fill) +
    ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(fv)), scales = "free_y", ncol = ncol) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_chars(th$wrap_x_labels_chars_base, width))) +
    ggplot2::theme(legend.position = "bottom")

  if (needs_tilt(levels(df_bar$X_cat), th$x_tilt_threshold_chars)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = th$x_tilt_angle_deg, hjust = 1))
  }

  y_top_guess <- df_bar %>%
    dplyr::group_by(.data$X_cat) %>%
    dplyr::summarise(tt = sum(.data$Y, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(.data$tt) %>%
    max(na.rm = TRUE) * 1.02

  p <- apply_annotations(p, annotations, y_top_guess = y_top_guess, color = th$axis_color)

  if (!is.null(out_file)) save_svg(p, out_file, width, height)
  print(p); invisible(p)
}

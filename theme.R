# file: R/theme.R

#' Brand theme defaults
#'
#' Default theme settings used across plotters.
#' @export
brand_theme_defaults <- list(
  family_title  = "mont",
  family_text   = "mont",
  background    = "#F8F6F2",
  grid_color    = "#E0E0E0",
  grid_size     = 0.5,
  axis_color    = "#333333",
  size_title    = 26,
  size_sub      = 18,
  size_axis     = 11,
  size_caption  = 12,
  size_annot    = 11,
  bar_width     = 0.6,
  legend_inside = TRUE,
  legend_xy     = c(0.98, 0.98),
  legend_just   = c(0, 0),
  margin_top    = 3.0,
  margin_right  = 1.4,
  margin_bottom = 1.8,
  margin_left   = 1.4,
  wrap_title_chars_base    = 38,
  wrap_subtitle_chars_base = 60,
  wrap_x_labels_chars_base = 14,
  x_tilt_threshold_chars = 14,
  x_tilt_angle_deg       = 45
)

#' Load Google Montserrat via showtext (optional)
#'
#' Falls back silently if showtext/sysfonts not installed.
#'
#' @param family Internal family alias.
#' @param google_family Google Fonts family name.
#' @param regular_wt,bold_wt numeric weight codes.
#' @return logical invisible; TRUE if attempt made.
#' @export
use_brand_fonts <- function(family = "mont", google_family = "Montserrat",
                            regular_wt = 400, bold_wt = 800) {
  if (!requireNamespace("showtext", quietly = TRUE) ||
      !requireNamespace("sysfonts", quietly = TRUE)) {
    message("showtext/sysfonts not available; using system fonts.")
    return(invisible(FALSE))
  }
  try({
    sysfonts::font_add_google(google_family, family,
                              regular.wt = regular_wt, bold.wt = bold_wt)
    showtext::showtext_auto()
    message("Loaded Google font '", google_family, "' as '", family, "'.")
  }, silent = TRUE)
  invisible(TRUE)
}

#' Responsive article-style theme
#'
#' @param th list of theme defaults (see [brand_theme_defaults]).
#' @param base_size numeric base size in pts.
#' @param width_in,height_in device size in inches.
#' @param extra_top_lines numeric extra lines to reserve (e.g., for legends).
#' @export
#' @importFrom ggplot2 theme_minimal theme element_rect element_blank element_line
#' @importFrom ggplot2 element_text margin
#' @importFrom grid unit
theme_article_responsive <- function(th = brand_theme_defaults,
                                     base_size = 12, width_in = 8, height_in = 5,
                                     extra_top_lines = 0) {
  rel  <- base_size / 12
  area <- sqrt(width_in * height_in) / sqrt(8 * 5)
  top  <- (th$margin_top + extra_top_lines) * rel * area
  m <- ggplot2::margin(top,
    th$margin_right  * rel * area,
    th$margin_bottom * rel * area,
    th$margin_left   * rel * area, unit = "lines"
  )

  ggplot2::theme_minimal(base_size = base_size, base_family = th$family_text) +
    ggplot2::theme(
      plot.background       = ggplot2::element_rect(fill = th$background, color = NA),
      panel.background      = ggplot2::element_rect(fill = th$background, color = NA),
      panel.grid.minor      = ggplot2::element_blank(),
      panel.grid.major.x    = ggplot2::element_blank(),
      panel.grid.major.y    = ggplot2::element_line(color = th$grid_color, linewidth = th$grid_size),
      axis.title            = ggplot2::element_blank(),
      axis.line.x           = ggplot2::element_line(color = th$axis_color, linewidth = 0.7),
      axis.ticks            = ggplot2::element_blank(),
      axis.text             = ggplot2::element_text(size = th$size_axis, color = th$axis_color),
      plot.title            = ggplot2::element_text(family = th$family_title, face = "bold",
                               size = th$size_title, hjust = 0, lineheight = 1.1,
                               margin = ggplot2::margin(b = base_size * 0.6)),
      plot.subtitle         = ggplot2::element_text(size = th$size_sub, hjust = 0, lineheight = 1.1,
                               margin = ggplot2::margin(b = base_size * 1.5)),
      plot.caption          = ggplot2::element_text(size = th$size_caption, hjust = 0.5,
                               margin = ggplot2::margin(t = base_size * 1.2)),
      legend.background     = ggplot2::element_blank(),
      legend.title          = ggplot2::element_blank(),
      legend.text           = ggplot2::element_text(size = th$size_axis),
      legend.spacing.y      = grid::unit(0.15, "cm"),
      plot.margin           = m
    )
}

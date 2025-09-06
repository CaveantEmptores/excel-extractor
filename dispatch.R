# file: R/dispatch.R

#' Dispatch to the correct plotter and optionally save
#'
#' @inheritParams plot_cbar
#' @param df_chart rows for a single chart (unique Workbook+Sheet+Chart).
#' @param palette_line Optional named palette for lines (for combo/line).
#' @param codes_col Column name holding a chart code; inferred if absent.
#' @return The ggplot object produced; invisibly.
#' @export
#' @importFrom dplyr distinct
#' @importFrom rlang .data
plot_and_save_dispatch <- function(df_chart,
                                   width = 8, height = 5, base_size = 12,
                                   out_file = NULL,
                                   th = brand_theme_defaults,
                                   palette_fill = NULL, palette_line = NULL,
                                   codes_col = "Chart_Code",
                                   title = NULL, subtitle = NULL,
                                   annotations = NULL) {

  keys <- dplyr::distinct(df_chart, .data$Workbook, .data$Sheet, .data$Chart)
  if (nrow(keys) != 1) rlang::abort("df_chart must contain a single chart's rows.", call = NULL)

  if (!(codes_col %in% names(df_chart))) {
    df_chart <- add_chart_codes(df_chart); codes_col <- "Chart_Code"
  }
  code <- df_chart[[codes_col]][1] %||% infer_chart_code_one(df_chart)
  message("Dispatching chart code: ", code)

  default_file <- paste0(
    sanitize(keys$Workbook[1]), "__",
    sanitize(keys$Sheet[1]), "__",
    sanitize(keys$Chart[1]), "__", code, ".svg"
  )
  ofile <- out_file %||% default_file

  switch(code,
    # single-series bars
    bar           = plot_bar(df_chart,  width, height, base_size, ofile, th,
                             title = title, subtitle = subtitle, annotations = annotations),
    hbar          = plot_hbar_simple(df_chart, width, height, base_size, ofile, th,
                                     title = title, subtitle = subtitle, annotations = annotations),

    # clustered bars
    cbar          = plot_cbar(df_chart, width, height, base_size, ofile, th, palette_fill,
                              title = title, subtitle = subtitle, annotations = annotations),
    chbar         = plot_chbar(df_chart, width, height, base_size, ofile, th, palette_fill,
                              title = title, subtitle = subtitle, annotations = annotations),

    # stacked bars
    sbar          = plot_cbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, FALSE,
                                    title = title, subtitle = subtitle, annotations = annotations),
    shbar         = plot_hbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, FALSE,
                                    title = title, subtitle = subtitle, annotations = annotations),
    sbar100       = plot_cbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, TRUE,
                                    title = title, subtitle = subtitle, annotations = annotations),
    shbar100      = plot_hbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, TRUE,
                                    title = title, subtitle = subtitle, annotations = annotations),

    # combos
    bar_line      = plot_bar_line(df_chart,   width, height, base_size, ofile, th,
                                  palette_line = palette_line,
                                  title = title, subtitle = subtitle, annotations = annotations),
    cbar_line     = plot_cbar_line(df_chart,  width, height, base_size, ofile, th, palette_fill, palette_line,
                                   title = title, subtitle = subtitle, annotations = annotations),
    sbar_line     = plot_sbar_line(df_chart,  width, height, base_size, ofile, th, palette_fill, palette_line,
                                   title = title, subtitle = subtitle, annotations = annotations),

    # small multiples
    cbar_smmult   = plot_cbar_smmult(df_chart, width, height, base_size, ofile, th,
                                     title = title, subtitle = subtitle, annotations = annotations),

    # existing
    line          = plot_line(df_chart, width, height, base_size, ofile, th, palette_line,
                              title = title, subtitle = subtitle, annotations = annotations),

    # legacy synonyms remain
    cbar_stack    = plot_cbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, FALSE,
                                    title = title, subtitle = subtitle, annotations = annotations),
    cbar_stack100 = plot_cbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, TRUE,
                                    title = title, subtitle = subtitle, annotations = annotations),
    hbar_stack    = plot_hbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, FALSE,
                                    title = title, subtitle = subtitle, annotations = annotations),
    hbar_stack100 = plot_hbar_stack(df_chart, width, height, base_size, ofile, th, palette_fill, TRUE,
                                    title = title, subtitle = subtitle, annotations = annotations),

    {
      warning("Chart code '", code, "' not explicitly supported. Falling back to clustered column.")
      plot_cbar(df_chart, width, height, base_size, ofile, th, palette_fill,
                title = title, subtitle = subtitle, annotations = annotations)
    }
  )
}

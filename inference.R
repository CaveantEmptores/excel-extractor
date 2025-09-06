# file: R/inference.R

#' Infer a chart code from a single chart's rows
#'
#' @param df Data for one chart (same Workbook/Sheet/Chart).
#' @return character scalar code (e.g., 'bar', 'hbar', 'cbar', 'chbar', 'sbar', 'shbar', 'bar_line',
#'   'cbar_line', 'sbar_line', etc.)
#' @export
#' @importFrom dplyr distinct n_distinct filter
#' @importFrom rlang .data
infer_chart_code_one <- function(df) {
  s <- dplyr::distinct(
    df,
    .data$Series, .data$Series_Chart_Type_Name,
    .data$Series_ElementType, .data$Chart_Type, .data$Chart_Type_ID
  )
  nm <- tolower(paste0(s$Series_Chart_Type_Name, " ", s$Series_ElementType))

  has_col       <- any(grepl("column|\\bcol\\b", nm))
  has_bar_orient<- any(grepl("\\bbar\\b", nm))  # horizontal orientation signal
  has_line      <- any(grepl("line", nm))

  is_stacked100 <- any(grepl("stacked100", nm))
  is_stacked    <- any(grepl("stacked(?!100)", nm, perl = TRUE)) || is_stacked100

  # number of bar/column series
  n_bar_series <- df %>%
    dplyr::filter(.data$Series_ElementType %in% c("Column", "Bar")) %>%
    dplyr::n_distinct(.data$Series)

  # choose orientation: explicit "Bar" (horizontal) beats Column
  orient_h <- has_bar_orient && !has_col

  # combos (bar + line)
  if (has_line && (has_col || has_bar_orient)) {
    if (is_stacked) return("sbar_line")
    if (n_bar_series <= 1) return("bar_line")
    return("cbar_line")
  }

  # stacked (incl. 100%)
  if (is_stacked100) {
    if (orient_h) return("shbar100") else return("sbar100")
  }
  if (is_stacked) {
    if (orient_h) return("shbar") else return("sbar")
  }

  # plain bars
  if (has_col || has_bar_orient) {
    if (orient_h) {
      if (n_bar_series <= 1) return("hbar") else return("chbar")
    } else {
      if (n_bar_series <= 1) return("bar") else return("cbar")
    }
  }

  # lines only
  if (has_line && !(has_col || has_bar_orient)) return("line")

  "other"
}

#' Add inferred chart codes to a long dataset
#'
#' @param long A long-format dataset containing at least Workbook/Sheet/Chart columns.
#' @return `long` with an added `Chart_Code` column.
#' @export
#' @importFrom dplyr group_by group_modify ungroup left_join across
#' @importFrom rlang .data
add_chart_codes <- function(long) {
  keys <- c("Workbook", "Sheet", "Chart")
  codes <- long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::group_modify(~ tibble::tibble(Chart_Code = infer_chart_code_one(.x))) %>%
    dplyr::ungroup()
  dplyr::left_join(long, codes, by = keys)
}

#' Ensure a chart code exists for a specific chart subset
#'
#' @param df_chart Rows for a single chart.
#' @return length-1 character code.
#' @export
ensure_chart_code <- function(df_chart) {
  code <- df_chart$Chart_Code
  if (!length(code) || all(is.na(code))) infer_chart_code_one(df_chart) else code[1]
}

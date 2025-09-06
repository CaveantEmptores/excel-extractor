# file: R/labels.R

#' Label helpers
#' @keywords internal
NULL

#' Make bar labels honoring Point_HasDataLabel_Visible when present
#' @keywords internal
#' @importFrom scales label_number
make_bar_labels <- function(df, label_format = scales::label_number(accuracy = 0.1)) {
  if (!"Point_HasDataLabel_Visible" %in% names(df)) return(rep("", nrow(df)))
  show <- isTRUE(df$Point_HasDataLabel_Visible)
  out <- rep("", nrow(df)); out[show] <- label_format(df$Y[show]); out
}

#' Make stacked labels with threshold by segment percent
#' @keywords internal
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom scales label_percent
#' @importFrom rlang .data
make_stacked_labels <- function(df, label_threshold = 0.07, label_format = scales::label_percent(accuracy = 1)) {
  df %>%
    dplyr::group_by(.data$X) %>%
    dplyr::mutate(.stack_sum = sum(.data$Y, na.rm = TRUE),
                  .seg_pct   = ifelse(.data$.stack_sum > 0, .data$Y / .data$.stack_sum, 0),
                  .show      = .data$.seg_pct >= label_threshold &
                    (!"Point_HasDataLabel_Visible" %in% names(df) | isTRUE(.data$Point_HasDataLabel_Visible))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.label = ifelse(.data$.show, label_format(.data$.seg_pct), "")) %>%
    dplyr::pull(.data$.label)
}

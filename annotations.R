# file: R/annotations.R

#' Apply text and optional vertical line annotations
#'
#' @param p ggplot object.
#' @param annotations_df data.frame with columns x, text; optional y, vline, vline_ymax, hjust, vjust, nudge_y.
#' @param y_top_guess numeric top y for defaults.
#' @param color character color.
#' @return ggplot object
#' @export
#' @importFrom ggplot2 geom_text aes geom_segment
#' @importFrom rlang .data
apply_annotations <- function(p, annotations_df, y_top_guess = NULL, color = "#333333") {
  if (is.null(annotations_df) || !nrow(annotations_df)) return(p)
  if (!all(c("x", "text") %in% names(annotations_df))) return(p)

  ann <- annotations_df
  if (!"y"          %in% names(ann)) ann$y          <- NA_real_
  if (!"hjust"      %in% names(ann)) ann$hjust      <- 0
  if (!"vjust"      %in% names(ann)) ann$vjust      <- 1
  if (!"nudge_y"    %in% names(ann)) ann$nudge_y    <- 0
  if (!"vline"      %in% names(ann)) ann$vline      <- FALSE
  if (!"vline_ymax" %in% names(ann)) ann$vline_ymax <- NA_real_

  ann$y_use <- ifelse(is.na(ann$y), y_top_guess, ann$y)
  ann$y_seg <- ifelse(is.na(ann$vline_ymax), y_top_guess, ann$vline_ymax)

  p <- p + ggplot2::geom_text(
    data = ann,
    mapping = ggplot2::aes(x = .data$x, y = .data$y_use, label = .data$text, hjust = .data$hjust, vjust = .data$vjust),
    nudge_y = ann$nudge_y,
    color = color, size = pt_to_mm(3.6),
    inherit.aes = FALSE
  )

  ann_v <- ann[isTRUE(ann$vline), , drop = FALSE]
  if (nrow(ann_v)) {
    p <- p + ggplot2::geom_segment(
      data = ann_v,
      mapping = ggplot2::aes(x = .data$x, xend = .data$x, y = 0, yend = .data$y_seg),
      linetype = "dashed", linewidth = 0.4, color = color,
      inherit.aes = FALSE
    )
  }
  p
}

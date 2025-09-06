# file: R/axes.R

#' Axis range helpers
#' @keywords internal
NULL

#' Compute axis ranges from value columns with fallback to Y
#' @keywords internal
#' @importFrom dplyr filter
compute_axis_ranges <- function(df, filter_expr) {
  d <- dplyr::filter(df, {{ filter_expr }})
  rng <- list(
    min = suppressWarnings(min(d$ValueAxis_Min, na.rm = TRUE)),
    max = suppressWarnings(max(d$ValueAxis_Max, na.rm = TRUE))
  )
  if (is.infinite(rng$min) || is.infinite(rng$max) || is.na(rng$min) || is.na(rng$max)) {
    vmin <- suppressWarnings(min(d$Y, na.rm = TRUE))
    vmax <- suppressWarnings(max(d$Y, na.rm = TRUE))
    rng$min <- if (is.finite(vmin)) vmin else 0
    rng$max <- if (is.finite(vmax)) vmax else 1
  }
  rng
}

#' Rescale secondary series to primary range
#' @keywords internal
rescale_to_primary <- function(y, pmin, pmax, smin, smax) {
  if (any(!is.finite(c(pmin, pmax, smin, smax))) || (pmax - pmin) == 0 || (smax - smin) == 0) return(y)
  (y - smin) / (smax - smin) * (pmax - pmin) + pmin
}

# file: R/utils.R

#' Internal utilities
#' @keywords internal
#' @importFrom rlang abort
NULL

#' Safe-null infix
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a) || a == "") b else a

#' Sanitize a string for filenames
#' @param x character
#' @return character
#' @keywords internal
sanitize <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

#' Check visibility flag, treating NA as visible
#' @keywords internal
is_visible <- function(x) !(isFALSE(x))

#' Convert typographic points to millimeters
#' @param pt numeric points
#' @keywords internal
pt_to_mm <- function(pt) {
  # points per millimeter (72.27 pt per inch / 25.4 mm per inch)
  pt_per_mm <- 72.27 / 25.4
  pt / pt_per_mm
}

#' Wrap-length scaler given device width
#' @keywords internal
wrap_chars <- function(base_chars, width_in, ref_width = 8) {
  max(10, floor(base_chars * (width_in / ref_width)))
}

#' Wrap text into hard line breaks
#' @keywords internal
wrap_to_lines <- function(txt, chars) {
  if (is.null(txt) || is.na(txt) || !nzchar(txt)) return(NULL)
  paste(strwrap(txt, width = chars), collapse = "\n")
}

#' Should we tilt x labels?
#' @keywords internal
needs_tilt <- function(labels, threshold_chars = 14) {
  if (length(labels) == 0) return(FALSE)
  max(nchar(as.character(labels)), na.rm = TRUE) > threshold_chars
}

#' Legend layout heuristic
#' @keywords internal
legend_layout <- function(n_items, width_in, height_in) {
  if (n_items <= 3L) {
    list(position = "inside", ncol = 1L, rows = n_items, bottom = FALSE)
  } else if (n_items <= 6L) {
    list(position = "inside", ncol = 2L, rows = ceiling(n_items / 2), bottom = FALSE)
  } else if (n_items <= 9L) {
    list(position = "inside", ncol = 3L, rows = ceiling(n_items / 3), bottom = FALSE)
  } else {
    list(position = "bottom", ncol = min(4L, ceiling(n_items / 2)),
         rows = ceiling(n_items / min(4L, ceiling(n_items / 2))), bottom = TRUE)
  }
}

#' Optional dependency guard
#' @keywords internal
.require_or_stop <- function(pkg, why = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Package '", pkg, "' is required",
                  if (!is.null(why)) paste0(" (", why, ")"), ". ",
                  "Please run install.packages('", pkg, "').")
    rlang::abort(msg, call = NULL)
  }
}

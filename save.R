# file: R/save.R

#' Save a ggplot to SVG with sensible defaults
#'
#' @param p ggplot object.
#' @param file output path.
#' @param width,height dimensions in inches.
#' @param dpi numeric dots per inch.
#' @param device Device function; defaults to `svglite::svglite` if available.
#' @export
#' @examples
#' \dontrun{
#' save_svg(ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) + ggplot2::geom_point(), "p.svg")
#' }
save_svg <- function(p, file, width = 8, height = 5, dpi = 300, device = NULL) {
  if (is.null(device)) {
    .require_or_stop("svglite", "to save SVG")
    device <- svglite::svglite
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename = file, plot = p, device = device,
                  width = width, height = height, units = "in", dpi = dpi)
  message("Saved SVG: ", normalizePath(file, winslash = "/"))
}

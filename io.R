# file: R/io.R

#' Load long/tall chart data from CSV/TSV/XLS/XLSX
#'
#' Applies light normalization and type coercion for known columns.
#'
#' @param path file path.
#' @param sheet Sheet name or index when reading Excel; ignored for CSV/TSV.
#' @return tibble/data.frame
#' @export
#' @examples
#' \dontrun{
#' long <- load_long_data("charts.xlsx", sheet = "Data")
#' }
#' @importFrom rlang abort
#' @importFrom dplyr rename_with all_of
load_long_data <- function(path, sheet = NULL) {
  stopifnot(is.character(path), length(path) == 1L)
  if (!file.exists(path)) rlang::abort(paste0("File not found: '", path, "'"), call = NULL)
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv", "tsv")) {
    .require_or_stop("readr", "for reading CSV/TSV")
    delim <- if (ext == "tsv") "\t" else ","
    df <- readr::read_delim(path, delim = delim, show_col_types = FALSE, guess_max = 100000)
  } else if (ext %in% c("xlsx", "xls")) {
    .require_or_stop("readxl", "for reading Excel files")
    df <- if (is.null(sheet)) readxl::read_excel(path) else readxl::read_excel(path, sheet = sheet)
  } else {
    rlang::abort(paste0("Unsupported file type: .", ext,
                        ". Use one of: .csv, .tsv, .xlsx, .xls."), call = NULL)
  }

  std_names <- c(
    "Workbook","Sheet","Chart","Chart_Title","Chart_Type","Chart_Type_ID",
    "Series","Series_Chart_Type_Name","Series_Chart_Type_ID","Series_ElementType",
    "AxisGroup","IsActive",
    "ValueAxis_Min","ValueAxis_Max","ValueAxis_MajorUnit","ValueAxis_ScaleType","ValueAxis_LogBase",
    "Point","X","Y","Point_HasDataLabel_Visible","Extracted_On","Source"
  )
  df <- dplyr::rename_with(df, ~ .x, dplyr::all_of(intersect(std_names, names(df))))

  num_cols <- intersect(c("AxisGroup","Chart_Type_ID","Series_Chart_Type_ID",
                          "ValueAxis_Min","ValueAxis_Max",
                          "ValueAxis_MajorUnit","ValueAxis_LogBase","Y","Point"),
                        names(df))
  df[num_cols] <- lapply(df[num_cols], function(v) suppressWarnings(as.numeric(v)))
  if ("IsActive" %in% names(df)) df$IsActive <- as.logical(df$IsActive)
  if ("Point_HasDataLabel_Visible" %in% names(df)) df$Point_HasDataLabel_Visible <- as.logical(df$Point_HasDataLabel_Visible)
  df
}

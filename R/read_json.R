#' Reads JSON from an input uri (file, url, ...) and returns a
#' \code{\link{tbl_json}} object
#'
#' @param path to some json data
#' @param format
#'   If \code{"json"}, process the data like one large JSON record.
#'   If \code{"jsonl"}, process the data one JSON record per line (json lines
#'   format).
#'   If \code{"infer"}, the format is the suffix of the given filepath.
#' @return a \code{\link{tbl_json}} object
#' @export
read_json <- function(path, format = c("json", "jsonl", "infer")) {

  if (length(format) > 1 || identical(format,"infer")) {
    format <- tail(strsplit(path, "[.]")[[1]], 1)
  }
  if (format == "json") {
    data <- readChar(path, nchars = file.info(path)$size)
  } else if (format == "jsonl") {
    data <- readLines(path)
  } else {
    stop(sprintf("Unrecognized json format: %s", format))
  }
  data %>% as.tbl_json
}

#' Reads JSON from an input uri (file, url, ...) and returns a tbl_json
#'
#' @param path to some json data
#' @param format
#'   If "json", process the data like one large JSON record.
#'   If "jsonl", process the data one JSON record per line (json lines format)
#'   If "infer", the format is the suffix of the given filepath.
#' @return tbl_json instance
#' @export
read_json <- function(path, format = c("json", "jsonl", "infer")) {

  if (format == "infer" || length(format) > 1) {
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

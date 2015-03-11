#' Reads JSON from an input uri (file, url, ...) and returns a tbl_json
#'
#' @param path to some json data
#' @param json.lines
#'   If True, process the data one JSON record per line.
#'   If False, process the data like one large JSON record.
#'   If infer, evaluate True only if the suffix of the filepath ends in .jsonl
#' @return tbl_json instance
#' @export
read_json <- function(path, json.lines="infer") {

  if (json.lines == "infer") {
    if (any(grep(".[jJ][sS][oO][nN][lL]$", path))) {
      json.lines <- T
    } else {
      json.lines <- F
    }
  }

  return (
    if (!json.lines) {
      readChar(path, nchars = file.info(path)$size)
    } else {
      readLines(path)
    }
  ) %>% as.tbl_json
}

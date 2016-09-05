#' Add a column that contains the complexity (recursively unlisted length) of the JSON data
#'
#' When investigating complex JSON data it can be helpful to identify the
#' complexity of deeply nested documents. The json_complexity() function adds a
#' column (default name "complexity") that contains the 'complexity' of the JSON
#' associated with each row. Essentially, every on-null scalar value is found in the
#' object by recursively stripping away all objects or arrays, and the complexity
#' is the count of these scalar values. Note that 'null' has complexity 0.
#'
#' @param .x a json string or tbl_json object
#' @param column.name the name to specify for the length column
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#' c('[1, 2, [3, 4]]', '{"k1": 1, "k2": [2, [3, 4]]}', '1', {}) %>%
#'   json_lengths %>% json_complexity
json_complexity <- function(.x, column.name = "complexity") {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Extract json
  json <- attr(.x, "JSON")

  # Determine lengths
  lengths <- json %>% map(unlist, recursive = TRUE) %>% map_int(length)

  # Add as a column to x
  .x[column.name] <- lengths

  tbl_json(.x, json)

}

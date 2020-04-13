#' Compute the complexity (recursively unlisted length) of JSON data
#'
#' When investigating complex JSON data it can be helpful to identify the
#' complexity of deeply nested documents. The \code{json_complexity} function
#' adds a column (default name \code{"complexity"}) that contains the
#' 'complexity' of the JSON associated with each row. Essentially, every on-null
#' scalar value is found in the object by recursively stripping away all objects
#' or arrays, and the complexity is the count of these scalar values. Note that
#' 'null' has complexity 0, as do empty objects and arrays.
#'
#' @seealso \code{\link{json_lengths}} to compute the length of each value
#' @param .x a json string or tbl_json object
#' @param column.name the name to specify for the length column
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # A simple example
#' json <- c('[1, 2, [3, 4]]', '{"k1": 1, "k2": [2, [3, 4]]}', '1', 'null')
#'
#' # Complexity is larger than length for nested objects
#' json %>% json_lengths %>% json_complexity
#'
#' # Worldbank has complexity ranging from 8 to 17
#' library(magrittr)
#' worldbank %>% json_complexity %$% table(complexity)
#'
#' # Commits are much more regular
#' commits %>% gather_array %>% json_complexity %$% table(complexity)
json_complexity <- function(.x, column.name = "complexity") {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Extract json
  json <- .x[["..JSON"]]

  # Determine lengths
  lengths <- json %>% purrr::map(unlist, recursive = TRUE) %>% purrr::map_int(length)

  # Add as a column to x
  .x[column.name] <- lengths

  tbl_json(.x, json)

}

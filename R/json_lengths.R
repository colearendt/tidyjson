#' Compute the length of JSON data
#'
#' When investigating JSON data it can be helpful to identify the lengths of the
#' JSON objects or arrays, especialy when they are 'ragged' across documents.
#' The \code{json_lengths} function adds a column (default name \code{"length"})
#' that contains the 'length' of the JSON associated with each row. For objects,
#' this will be equal to the number of name-value pairs. For arrays, this will
#' be equal to the length of the array. All scalar values will be of length 1,
#' and null will have length 0.
#'
#' @seealso \code{\link{json_complexity}} to compute the recursive length of
#'          each value
#' @param .x a json string or \code{\link{tbl_json}} object
#' @param column.name the name to specify for the length column
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # A simple example
#' json <- c('[1, 2, 3]', '{"k1": 1, "k2": 2}', '1', 'null')
#'
#' # Complexity is larger than length for nested objects
#' json %>% json_lengths
#'
#' # Worldbank objcts are either length 7 or 8
#' library(magrittr)
#' worldbank %>% json_lengths %$% table(length)
#'
#' # All commits are length 8
#' commits %>% gather_array %>% json_lengths %$% table(length)
json_lengths <- function(.x, column.name = "length") {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Extract json
  json <- json_get(.x)

  # Determine lengths
  lengths <- purrr::map_int(json, length)

  # Add as a column to x
  .x[column.name] <- lengths

  tbl_json(.x, json)

}

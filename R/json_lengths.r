#' Add a column that contains the length of the JSON data
#' 
#' When investigating JSON data it can be helpful to identify the lengths of the
#' JSON objects or arrays, especialy when they are 'ragged' across documents. The
#' json_lengths() function adds a column (default name "length") that contains
#' the 'length' of the JSON associated with each row. For objects, this will
#' be equal to the number of keys. For arrays, this will be equal to the length
#' of the array. All scalar values will be of length 1.
#' 
#' @param x a tbl_json object
#' @param column.name the name to specify for the length column
#' @return a tbl_json object with column.name column that tells the length
#' @export
#' @examples 
#' library(magrittr)  # for %>% 
#' c('[1, 2, 3]', '{"k1": 1, "k2": 2}', '1', {}) %>% json_lengths
json_lengths <- function(x, column.name = "length") {
  
  if (!is.tbl_json(x)) x <- as.tbl_json(x)
  
  # Extract json 
  json <- attr(x, "JSON")
  
  # Determine lengths
  lengths <- vapply(json, length, integer(1))
  
  # Add as a column to x
  x[column.name] <- lengths
  
  tbl_json(x, json)
  
}
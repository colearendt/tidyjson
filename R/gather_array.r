#' Stack a JSON array
#'
#' Given a JSON array, such as [1, 2, 3], gather_array will "stack" the array in 
#' the tbl_json data.frame, by replicating each row of the data.frame by the
#' length of the corresponding JSON array. A new column (by default called 
#' "array.index") will be added to keep track of the referenced position in the
#' array for each row of the resuling data.frame.
#' 
#' JSON can contain arrays of data, which can be simple vectors (fixed or varying 
#' length integer, character or logical vectors). But they also often contain 
#' lists of other objects (like a list of purchases for a user). The function 
#' gather_array() takes JSON arrays and duplicates the rows in the data.frame to 
#' correspond to the indices of the array, and puts the elements of 
#' the array into the JSON attribute. This is equivalent to "stacking" the array 
#' in the data.frame, and lets you continue to manipulate the remaining JSON 
#' in the elements of the array. For simple arrays, use append_values_* to 
#' capture all of the values of the array. For more complex arrays (where the
#' values are themselves objects or arrays), continue using other tidyjson
#' functions to structure the data as needed.
#' 
#' @param x a tbl_json whose JSON attribute should always be an array
#' @param column.name the name to give to the array index column created
#' @return a tbl_json with a new column (column.name) that captures the array
#'   index and JSON attribute extracted from the array
#' @export
#' @examples
#' library(magrittr)  # for %>%  
#' '[1, "a", {"k": "v"}]' %>% gather_array %>% json_types
gather_array <- function(x, column.name = "array.index") {
  
  if (!is.tbl_json(x)) x <- as.tbl_json(x)
  
  # Get JSON
  json <- attr(x, "JSON")
  
  # Handle the case where json is just an empty list
  if (identical(json, list())) {
    # Drop any rows
    y <- x[integer(0), , drop = FALSE]
    # Setup 
    y[column.name] <- integer(0) 
    return(tbl_json(y, list()))
  }
  
  # Determine types
  types <- determine_types(json)
  
  # Check if not arrays
  not_arrays <- types != "array"
  if (any(not_arrays))
    stop(sprintf("%s records are values not arrays", sum(not_arrays)))
  
  # Get array lengths
  lengths <- vapply(json, length, integer(1))
  
  # Compute indices
  indices <- rep(seq_along(json), lengths)
  
  # Expand x
  y <- x[indices, , drop = FALSE]
  
  # Add sequence column
  y[column.name] <- sequence(lengths)
  
  # Unwind JSON one level
  json <- unlist(json, recursive = FALSE)
  
  # Construct tbl_json
  tbl_json(y, json)
  
}

#' Add a column that tells the 'length' of the data in the root of the JSON
#' 
#' @param x a tbl_json object
#' @param column.name the name to specify for the length column
#' @return a tbl_json object with column.name column that tells the length
#' @export
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
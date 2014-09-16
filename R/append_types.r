
#' Append a column that tells the 'type' of the data in the root of the JSON
#' 
#' @param x a tbl_json object
#' @param column.name the name to specify for the type column
#' @return a tbl_json object with column.name column that tells the type
#' @export
append_types <- function(x, column.name = "type") {
  
  assert_that(is.tbl_json(x))
  
  # Extract json 
  json <- attr(x, "JSON")
  
  # Determine types
  types <- determine_types(json)
  
  # Add as a column to x
  x[column.name] <- types
  
  tbl_json(x, json)
  
}

#' Fundamental JSON types from http://json.org/, where I collapse 'true' and
#' 'false' into 'logical'
json_types <- c("object", "array", "string", "number", "logical", "null")

#' Determines the types of a list of parsed JSON
#' @param json_list a list of parsed JSON
#' @return a factor with levels json_types
determine_types <- function(json_list) {
  
  # Get classes
  classes <- vapply(json_list, class, character(1))
  
  # Check existence of names
  names <- vapply(json_list, function(x) !is.null(attr(x, "names")), logical(1))
  
  # Check for length
  lengths <- vapply(json_list, length, integer(1))
  
  # Fix class names
  classes[classes == "list" & names] <- "object"
  classes[classes == "list" & !names] <- "array"
  classes[lengths > 1] <- "array"
  classes[classes == "character"] <- "string"
  classes[classes == "numeric"] <- "number"
  classes[classes == "NULL"] <- "null"

  # Turn into a factor
  factor(classes, levels = json_types)
  
}
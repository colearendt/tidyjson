#' Add a column that tells the 'type' of the data in the root of the JSON
#'
#' The function json_types() inspects the JSON associated with 
#' each row of the tbl_json data.frame, and adds a new column ("type" by 
#' default) that identifies the type according to the 
#' JSON standard at http://json.org/.
#'
#' This is particularly useful for inspecting your JSON data types, and can added
#' after gather_array() (or gather_keys()) to inspect the types of the elements
#' (or values) in arrays (or objects).
#'
#' @param x a tbl_json object
#' @param column.name the name to specify for the type column
#' @return a tbl_json object with column.name column that tells the type
#' @export
#' @examples 
#' library(magrittr)  # for %>%
#' c('{"a": 1}', '[1, 2]', '"a"', '1', 'true', 'null') %>% json_types
json_types <- function(x, column.name = "type") {
  
  if (!is.tbl_json(x)) x <- as.tbl_json(x)
  
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
allowed_json_types <- 
  c("object", "array", "string", "number", "logical", "null")

#' Determines the types of a list of parsed JSON
#' @param json_list a list of parsed JSON
#' @return a factor with levels json_types
determine_types <- function(json_list) {
  
  # Get classes
  classes <- vapply(json_list, class, character(1))
  
  # Check existence of names
  names <- vapply(json_list, function(x) !is.null(attr(x, "names")), logical(1))
  
  # Fix class names
  classes[classes == "list" & names] <- "object"
  classes[classes == "list" & !names] <- "array"
  classes[classes == "character"] <- "string"
  classes[classes == "integer"] <- "number"
  classes[classes == "numeric"] <- "number"
  classes[classes == "NULL"] <- "null"

  # Turn into a factor
  factor(classes, levels = allowed_json_types)
  
}

#' Append keys to a new column
#' 
#' @name append_values
NULL

#' Creates the append_values_* functions
append_values_factory <- function(type, na_value) {
  
  function(x, column.name = type) {
    
    assert_that(is.tbl_json(x))
  
    # Extract json 
    json <- attr(x, "JSON")
    
    # Determine types
    types <- determine_types(json)
    
    # Initialize column to NAs
    column <- rep(na_value, nrow(x))
    
    # Identify correct types
    correct <- types == type
    
    # Set correct to data
    column[correct] <- unlist(json[correct])
    
    # Add as a column to x
    x[column.name] <- column
    
    tbl_json(x, json)
    
  }
}
  

#' @export
#' @rdname append_values
append_values_string <- append_values_factory("string", NA_character_)

#' @export
#' @rdname append_values
append_values_number <- append_values_factory("number", NA_real_)

#' @export
#' @rdname append_values
append_values_logical <- append_values_factory("logical", NA)

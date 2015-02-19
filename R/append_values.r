#' Append keys to a new column
#' 
#' @name append_values
#' @param x a tbl_json object
#' @param column.name the column.name to append the values into the data.frame
#'   under
NULL

#' Creates the append_values_* functions
#' @param type the JSON type that will be appended
#' @param na_value the NA value that will be used if the type is incorrect
#' @param force parameter that determines if the type should be determined or not
#'        if force is FALSE, then the function takes more memory
append_values_factory <- function(type, na_value, blank_value, force=TRUE) {
  
  function(x, column.name = type, force=TRUE) {
    
    assert_that(is.tbl_json(x))
  
    # Extract json 
    json <- attr(x, "JSON")

    assert_that(length(json) == nrow(x))
    
    # if json is empty, return empty
    if (length(json) == 0) {
       x[column.name] <- blank_value
       return(tbl_json(x, json))
     }
   
    if (!force) { 
       x[column.name] <- append_values_type(json, type, na_value)
    } else {
       unlist_json <- unlist(json)
       # need this because of the way unlist handles NULL
       if (length(unlist_json) == nrow(x)) {
           x[column.name] <- unlist_json
       } else { 
           x[column.name] <- unlist(lapply(json, 
                                           function(a) 
                                             ifelse(length(a) == 0, na_value, a)))
       }
    }
     
    tbl_json(x, json)
    
  }
}

#' get list of values from json
#' @param json extracted using attributes
#' @param type input type (numeric, string, etc)
#' @param na_value default null value
append_values_type <- function(json, type, na_value) {

   # Determine type
   types <- determine_types(json)
    
   # Initialize column to NAs
   column <- rep(na_value, length(json))
    
   # Identify correct types
   correct <- types == type
   
   # Set correct to data
   column[correct] <- unlist(json[correct])

   column
    
}

#' @export
#' @rdname append_values
append_values_string <- append_values_factory("string", NA_character_, 
                                              character(0), force=TRUE)

#' @export
#' @rdname append_values
append_values_number <- append_values_factory("number", NA_real_, 
                                              numeric(0), force=TRUE)

#' @export
#' @rdname append_values
append_values_logical <- append_values_factory("logical", NA, 
                                               logical(0), force=TRUE)

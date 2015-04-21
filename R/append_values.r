#' Appends all values with a specified type as a new column
#' 
#' The append_values_X functions let you take any remaining JSON and add it as
#' a column X (for X in "string", "number", "logical") insofar as it is of the
#' JSON type specified.
#'
#' Any values that do not conform to the type specified will be NA in the resulting
#' column. This includes other scalar types (e.g., numbers or logicals if you are
#' using append_values_string) and *also* any rows where the JSON is still an
#' object or an array.
#' 
#' @name append_values
#' @param x a tbl_json object
#' @param column.name the column.name to append the values into the data.frame
#'   under
#' @param force parameter that determines if the variable type should be computed or not
#'        if force is FALSE, then the function may take more memory
#' @examples
#' library(magrittr)  # for %>%
#' '{"first": "bob", "last": "jones"}' %>% 
#'   gather_keys() %>%
#'   append_values_string()
NULL

#' Creates the append_values_* functions
#' @param type the JSON type that will be appended
#' @param as.value function to force coercion to numeric, string, or logical
append_values_factory <- function(type, as.value) {
  
  function(x, column.name = type, force=TRUE) {
    
    if (!is.tbl_json(x)) x <- as.tbl_json(x)
  
    # Extract json 
    json <- attr(x, "JSON")

    assert_that(length(json) == nrow(x))
    
    # if json is empty, return empty
    if (length(json) == 0) {
       x[column.name] <- as.value(NULL)
       return(tbl_json(x, json))
     }
  
    # if force is FALSE, then check type of the elements 
    if (!force) { 
       x[column.name] <- append_values_type(json, type) %>% as.value
    } else {
       new_val <- my_unlist(json) %>% as.value
       assert_that(length(new_val) == nrow(x))
       x[column.name] <- new_val
    }
  
    # return as appropriate class type 
    tbl_json(x, json)
    
  }
}

#' Unlists while preserving NULLs and only unlisting lists with one value
#' @param l a list that we want to unlist
my_unlist <- function(l) {
  nulls <- vapply(l, length, 1L) != 1
  l[nulls] <- NA
  unlist(l, recursive = FALSE)
}

#' get list of values from json
#' @param json extracted using attributes
#' @param type input type (numeric, string, etc)
append_values_type <- function(json, type) {

   # Determine type
   types <- determine_types(json)
    
   # Initialize column to NAs
   column <- rep(NA, length(json))
    
   # Identify correct types
   correct <- types == type
   
   # Set correct to data
   column[correct] <- unlist(json[correct])

   column
    
}

#' @export
#' @rdname append_values
append_values_string <- append_values_factory("string", function(x) as.character(x))

#' @export
#' @rdname append_values
append_values_number <- append_values_factory("number", function(x) as.numeric(x))

#' @export
#' @rdname append_values
append_values_logical <- append_values_factory("logical", function(x) as.logical(x))

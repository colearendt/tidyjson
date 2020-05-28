#' Appends all JSON values with a specified type as a new column
#'
#' The \code{append_values} functions let you take any scalar JSON values
#' of a given type ("string", "number", "logical") and add them as a new
#' column named \code{column.name}. This is particularly useful after using
#' \code{\link{gather_object}} to gather an object.
#'
#' Any values that can not be converted to the specified will be \code{NA} in
#' the resulting column. This includes other scalar types (e.g., numbers or
#' logicals if you are using \code{append_values_string}) and *also* any rows
#' where the JSON is NULL or an object or array.
#'
#' Note that the \code{append_values} functions do not alter the JSON
#' attribute of the \code{tbl_json} object in any way.
#'
#' @name append_values
#' @seealso \code{\link{gather_object}} to gather an object first,
#'    \code{\link{spread_all}} to spread values into new columns,
#'    \code{\link{json_get_column}}
#' @param .x a json string or \code{\link{tbl_json}} object
#' @param column.name the name of the column to append values as
#' @param force should values be coerced to the appropriate type
#'        when possible, otherwise, types are checked first (requires more
#'        memory)
#' @param recursive logical indicating whether to recurisvely extract a single
#'        value from a nested object. Only used when \code{force = TRUE}. If
#'        \code{force = FALSE}, and \code{recursive = TRUE}, throws an error.
#' @return a \code{\link{tbl_json}} object
#' @examples
#'
#' # Stack names
#' '{"first": "bob", "last": "jones"}' %>%
#'   gather_object %>%
#'   append_values_string
#'
#' # This is most useful when data is stored in name-value pairs
#' # For example, tags in recipes:
#' recipes <- c('{"name": "pie", "tags": {"apple": 10, "pie": 2, "flour": 5}}',
#'              '{"name": "cookie", "tags": {"chocolate": 2, "cookie": 1}}')
#' recipes %>%
#'   spread_values(name = jstring(name)) %>%
#'   enter_object(tags) %>%
#'   gather_object("tag") %>%
#'   append_values_number("count")
NULL

#' Creates the append_values_* functions
#' @param type the JSON type that will be appended
#' @param as.value function to force coercion to numeric, string, or logical
#' @keywords internal
append_values_factory <- function(type, as.value) {

  function(.x, column.name = type, force = TRUE, recursive = FALSE) {

    if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

    if (force == FALSE) assertthat::assert_that(recursive == FALSE)

    # Extract json
    json <- json_get(.x)

    assertthat::assert_that(length(json) == nrow(.x))

    # if json is empty, return empty
    if (length(json) == 0) {
       .x[column.name] <- as.value(NULL)
       return(tbl_json(.x, json))
     }

    # if force is FALSE, then check type of the elements
    if (!force) {
       .x[column.name] <- append_values_type(json, type) %>% as.value
    } else {
       new_val <- my_unlist(json, recursive)

       # if new_val is a list and recursive = FALSE, then
       # need to identify values with a name and change to NA
       if (is.list(new_val) && !recursive) {
         classof <- purrr::map_chr(new_val, class)
         loc <- classof == "list"
         #loc <- names(new_val) != ""
         new_val[loc] <- NA
       }
       new_val <- new_val %>% as.value()
       assertthat::assert_that(length(new_val) == nrow(.x))
       .x <- dplyr::mutate(.x, !!column.name := new_val)
    }

    # return as appropriate class type
    tbl_json(.x, json)

  }
}

#' Unlists while preserving NULLs and only unlisting lists with one value
#' @param l a list that we want to unlist
#' @param recursive logical indicating whether to unlist nested lists
#' @keywords internal
my_unlist <- function(l, recursive = FALSE) {
  nulls <- purrr::map_int(l, length) != 1
  l[nulls] <- NA
  unlist(l, recursive = recursive)
}

#' get list of values from json
#' @param json extracted using attributes
#' @param type input type (numeric, string, etc)
#' @keywords internal
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
append_values_string <- append_values_factory("string", as.character)

#' @export
#' @rdname append_values
append_values_number <- append_values_factory("number", as.numeric)

#' @export
#' @rdname append_values
append_values_logical <- append_values_factory("logical", as.logical)

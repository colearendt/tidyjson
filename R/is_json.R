#' Factory to create \code{is_json} functions
#' @param desired.types character vector of types we want to check for
#' @return a function
is_json_factory <- function(desired.types) {

  function(.x) {
    types <- .x %>% json_types %>% extract2("type")
    types %in% desired.types
  }

}

#' Predicates to test for specific JSON types in \code{\link{tbl_json}} objects
#'
#' These functions are often useful with \code{\link[dplyr]{filter}} to
#' filter complex JSON by type before applying \code{\link{gather_keys}} or
#' \code{\link{gather_array}}.
#'
#' @seealso \code{\link{json_types}} for creating a new column to identify the
#'          type of every JSON document
#' @name is-json
#' @param .x a json string or \code{\link{tbl_json}} object
#' @return a logical vector
#' @examples
#'
#' # Test a simple example
#' json <- '[1, "string", true, [1, 2], {"key": "value"}, null]' %>% gather_array
#' json %>% is_json_number
#' json %>% is_json_array
#' json %>% is_json_scalar
#'
#' # Use with filter
#' library(dplyr)
#' json %>% filter(is_json_object(.))
#'
#' # Combine with filter in advance of using gather_array
#' companies[1:5] %>% gather_keys %>% filter(is_json_array(.))
#' companies[1:5] %>% gather_keys %>% filter(is_json_array(.)) %>% gather_array
#'
#' # Combine with filter in advance of using gather_keys
#' companies[1:5] %>% gather_keys %>% filter(is_json_object(.))
#' companies[1:5] %>% gather_keys %>% filter(is_json_object(.)) %>% gather_keys("key2")
NULL

#' @rdname is-json
#' @export
is_json_string  <- is_json_factory("string")

#' @rdname is-json
#' @export
is_json_number  <- is_json_factory("number")

#' @rdname is-json
#' @export
is_json_logical <- is_json_factory("logical")

#' @rdname is-json
#' @export
is_json_null    <- is_json_factory("null")

#' @rdname is-json
#' @export
is_json_array   <- is_json_factory("array")

#' @rdname is-json
#' @export
is_json_object  <- is_json_factory("object")

#' @rdname is-json
#' @export
is_json_scalar  <- is_json_factory(c("string", "number", "logical"))
#' Enter into a specific object and discard all other JSON data
#'
#' JSON can contain nested objects, such as
#' \code{'{"key1": {"key2": [1, 2, 3]}}'}. \code{enter_object} can be used to
#' access the array nested under \code{"key1"} and \code{"key2"}. After using
#' \code{enter_object}, all further tidyjson calls happen inside the referenced
#' object (all other JSON data outside the object is discarded). If the object
#' doesn't exist for a given row / index, then that row will be discarded.
#'
#' This is useful when you want to limit your data to just information found in
#' a specific key. In pipelines, \code{enter_object} is often preceded by
#' \code{gather_keys} and followed by \code{gather_array}
#' if the key contains an array, or \code{spread_all} if the key contains an
#' object.
#'
#' @seealso \code{\link{gather_keys}} to access keys that could be entered
#'    into, \code{\link{gather_array}} to gather an array in an object and
#'    \code{\link{spread_all}} to spread values in an object.
#' @param .x a json string or tbl_json object
#' @param ... path to filter
#' @export
#' @examples
#'
#' # Let's start with a simple example of parents and children
#' json <- c('{"parent": "bob",  "children": ["sally", "george"]}',
#'           '{"parent": "fred", "children": ["billy"]}',
#'           '{"parent": "anne"}')
#'
#' # We can see the keys and types in each
#' json %>% gather_keys %>% json_types
#'
#' # Let's capture the parent first and then enter in the children object
#' json %>% spread_all %>% enter_object("children")
#'
#' # Notice that "anne" was discarded, as she has no children
#'
#' # We can now use gather array to stack the array
#' json %>% spread_all %>% enter_object("children") %>%
#'   gather_array("child.num")
#'
#' # And append_values_string to add the children names
#' json %>% spread_all %>% enter_object("children") %>%
#'   gather_array("child.num") %>%
#'   append_values_string("child")
#'
#' # A more realistc example with companies data
#' library(dplyr)
#' companies %>%
#'   enter_object("acquisitions") %>%
#'   gather_array %>%
#'   spread_all %>%
#'   glimpse
enter_object <- function(.x, ...) {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Prepare path
  path <- list(...)

  # Extract json
  json <- attr(.x, "JSON")

  # Access path
  json <- map(json, path)

  tbl_json(.x, json, drop.null.json = TRUE)

}

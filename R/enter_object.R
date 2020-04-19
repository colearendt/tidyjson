#' Enter into a specific object and discard all other JSON data
#'
#' When manipulating a JSON object, \code{enter_object} lets you navigate to
#' a specific value of the object by referencing it's name. JSON can contain
#' nested objects, and you can pass in more than one character string into
#' \code{enter_object} to navigate through multiple objects simultaneously.
#'
#' After using \code{enter_object}, all further tidyjson calls happen inside the
#' referenced object (all other JSON data outside the object is discarded).
#' If the object doesn't exist for a given row / index, then that row will be
#' discarded.
#'
#' In pipelines, \code{enter_object} is often preceded by \code{gather_object}
#' and followed by \code{gather_array} if the value is an array, or
#' \code{spread_all} if the value is an object.
#'
#' @seealso \code{\link{gather_object}} to find sub-objects that could be
#'    entered into, \code{\link{gather_array}} to gather an array in an object
#'    and \code{\link{spread_all}} or \code{\link{spread_values}} to spread values in an object.
#' @param .x a json string or tbl_json object
#' @param ... a quoted or unquoted sequence of strings designating the object
#'            name or sequences of names you wish to enter
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # Let's start with a simple example of parents and children
#' json <- c('{"parent": "bob",  "children": ["sally", "george"]}',
#'           '{"parent": "fred", "children": ["billy"]}',
#'           '{"parent": "anne"}')
#'
#' # We can see the names and types in each
#' json %>% gather_object %>% json_types
#'
#' # Let's capture the parent first and then enter in the children object
#' json %>% spread_all %>% enter_object(children)
#'
#' # Also works with quotes
#' json %>% spread_all %>% enter_object("children")
#'
#' # Notice that "anne" was discarded, as she has no children
#'
#' # We can now use gather array to stack the array
#' json %>% spread_all %>% enter_object(children) %>%
#'   gather_array("child.num")
#'
#' # And append_values_string to add the children names
#' json %>% spread_all %>% enter_object(children) %>%
#'   gather_array("child.num") %>%
#'   append_values_string("child")
#'
#' # The path can be comma delimited to go deep into a nested object
#' json <- '{"name": "bob", "attributes": {"age": 32, "gender": "male"}}'
#' json %>% enter_object(attributes, age)
#'
#' # A more realistc example with companies data
#' library(dplyr)
#' companies %>%
#'   enter_object(acquisitions) %>%
#'   gather_array %>%
#'   spread_all %>%
#'   glimpse
enter_object <- function(.x, ...) {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Prepare path
  path <- path(...)

  # Extract json
  json <- json_get(.x)

  # Access path
  json <- purrr::map(json, path %>% as.list)

  tbl_json(.x, json, drop.null.json = TRUE)

}

#' Dive into a specific object "key"
#' 
#' JSON can contain nested objects, such as {"key1": {"key2": [1, 2, 3]}}. The
#' function enter_object() can be used to access the array nested under "key1"
#' and "key2". After using enter_object(), all further tidyjson calls happen 
#' inside the referenced object (all other JSON data outside the object 
#' is discarded). If the object doesn't exist for a given row / index, then that 
#' data.frame row will be discarded.
#' 
#' This is useful when you want to limit your data to just information found in
#' a specific key. Use the ... to specific a sequence of keys that you want to
#' enter into. Keep in mind that any rows with JSON that do not contain the key
#' will be discarded by this function.
#' 
#' @param x a tbl_json object
#' @param ... path to filter
#' @export
#' @examples
#' library(magrittr)  # for %>%
#' c('{"name": "bob", "children": ["sally", "george"]}', '{"name": "anne"}') %>%
#'   spread_values(parent.name = jstring("name")) %>%
#'   enter_object("children") %>% 
#'   gather_array %>% 
#'   append_values_string("children")
enter_object <- function(x, ...) {
  
  if (!is.tbl_json(x)) x <- as.tbl_json(x)
  
  # Prepare path
  path <- prep_path(...)
  
  # Extract json
  json <- attr(x, "JSON")
  
  # Access path
  json <- list_path(json, path)
  
  tbl_json(x, json, drop.null.json = TRUE)
  
}
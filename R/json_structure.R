#' Recursively structures arbitrary JSON data into a single data frame
#'
#' Returns a \code{\link{tbl_json}} object where each row corresponds to a leaf
#' in the JSON structure. The first row corresponds to the JSON document as
#' a whole. If the document is a scalar value (JSON string, number, logical
#' or null), then there will only be 1 row. If instead it is an object or
#' an array, then subsequent rows will recursively correspond to the elements
#' (and their children) of the object or array.
#'
#' The columns in the \code{\link{tbl_json}} returend are defined as
#'
#' \itemize{
#'   \item \code{document.id} 1L if \code{.x} is a single JSON string, otherwise
#'         the index of \code{.x}.
#'
#'   \item \code{parent.id} the string identifier of the parent node for this
#'         child.
#'
#'   \item \code{level} what level of the hierarchy this child resides at,
#'         starting at \code{0L} for the root and incrementing for each level
#'         of nested array or object.
#'
#'   \item \code{index} what index of the parent object / array this child
#'         resides at (from \code{gather_array} for arrays).
#'
#'   \item \code{child.id} a unique ID for this leaf in this document,
#'         represented as <parent>.<index> where <parent> is the ID for the
#'         parent and <index> is this index.
#'
#'   \item \code{seq} the sequence of names / indices that led to this child
#'         (parents that are arrays are excluded) as a list, where character
#'         strings denote objects and integers denote array positions
#'
#'   \item \code{name} if this is the value of an object, what was the name that
#'         it is listed under (from \code{\link{gather_object}}).
#'
#'   \item \code{type} the type of this object (from \code{\link{json_types}}).
#'
#'   \item \code{length} the length of this object (from
#'         \code{\link{json_lengths}}).
#' }
#'
#' @seealso \code{\link{json_schema}} to create a schema for a JSON document or
#'          collection
#' @param .x a json string or tbl_json object
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # A simple string
#' '"string"' %>% json_structure
#'
#' # A simple object
#' '{"name": "value"}' %>% json_structure
#'
#' # A complex array
#' '[{"a": 1}, [1, 2], "a", 1, true, null]' %>% json_structure
#'
#' # A sample of structure rows from a company
#' library(dplyr)
#' companies[1] %>% json_structure %>% sample_n(5)
json_structure <- function(.x) {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Create initial structure for top level
  structure <- json_structure_init(.x)

  this_level <- 0L
  while(structure %>% should_json_structure_expand_more(this_level)) {

    structure <- bind_rows(
      structure,
      json_structure_level(structure %>% dplyr::filter(level == this_level))
    )

    this_level <- this_level + 1L

  }

  structure

}

json_structure_init <- function(x) {

  if (!'document.id' %in% names(x)) {
    x <- x %>% dplyr::mutate(
      document.id=row_number()
    )
  }
  x %>%
    dplyr::mutate(
      parent.id = NA_character_,
      level = 0L,
      index = 1L,
      child.id = "1",
      seq = replicate(n(), list()),
      name = NA_character_
    ) %>%
    json_types %>%
    json_lengths

}

should_json_structure_expand_more <- function(s, this.level) {

  s %>%
    dplyr::filter(level == this.level) %>%
    json_lengths %>%
    dplyr::filter(type %in% c("object", "array") & length > 0) %>%
    nrow %>%
    magrittr::is_greater_than(0L)

}

json_structure_empty <- function() {

  tbl_json(
    dplyr::tibble(
      document.id = integer(0),
      parent.id = character(0),
      level = integer(0),
      index = integer(0),
      child.id = character(0),
      seq = list(),
      name = character(0),
      type = factor(character(0), levels = allowed_json_types),
      length = integer(0)
    ),
    list()
  )

}

json_structure_level <- function(s) {

  new_s <- json_structure_empty()
  cols_select <- names(s)[names(s) %in% names(new_s)]
  cols_select <- setdiff(cols_select, "..JSON")
  new_s <- new_s %>% dplyr::select(!!!cols_select)

  # Expand any objects
  if (any(s$type == "object")) {
    new_s <- bind_rows(
      new_s,
      s %>% json_structure_objects
    )
  }

  # Expand any arrays
  if (any(s$type == "array")) {
    new_s <- bind_rows(
      new_s,
      s %>% json_structure_arrays
    )
  }

  new_s

}

json_structure_objects <- function(s) {
  
  expand_s <- s %>%
    dplyr::filter(type == "object") %>%
    dplyr::transmute(
      document.id
      , parent.id=child.id
      , seq
      , level=level + 1L
    ) %>%
    gather_object %>%
    json_types %>%
    json_lengths

  # Create rest of data frame
  df_s <- expand_s %>%
    dplyr::group_by(parent.id) %>%
    dplyr::mutate(index = 1L:n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      child.id = paste(parent.id, index, sep = "."),
      seq = purrr::map2(seq, name, c)
    ) %>%
    dplyr::select(
      document.id
      , parent.id
      , level
      , index
      , child.id
      , seq
      , name
      , type
      , length
    )

  # Reconstruct tbl_json object
  tbl_json(df_s, json_get(expand_s))

}

json_structure_arrays <- function(s) {
  
  s <- s %>%
    dplyr::filter(type == "array") %>%
    dplyr::transmute(
      document.id
      , parent.id=child.id
      , seq
      , level=level + 1L
    ) %>%
    gather_array("index") %>%
    json_types %>%
    json_lengths %>%
    dplyr::mutate(
      child.id = paste(parent.id, index, sep = "."),
      seq = purrr::map2(seq, index, c)
    ) %>%
    dplyr::transmute(
      document.id
      , parent.id
      , level
      , index
      , child.id
      , seq
      , name=NA_character_
      , type
      , length
    )

}

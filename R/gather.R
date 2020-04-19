#' Factory to create gather functions
#'
#' @param default.column.name the desired name of the default added column
#' @param default.column.empty the value to use when the default column should
#'   be empty because the JSON has length 0
#' @param expand.fun a function applied to the JSON that will expand the rows
#'   in the tbl_df
#' @param required.type the json_types type that must be present in every
#'   element of the JSON for this to succeed
#' @keywords internal
gather_factory <- function(default.column.name, default.column.empty,
                           expand.fun, required.type) {

  function(.x, column.name = default.column.name) {

    assertthat::assert_that(!("..name" %in% names(.x)))
    assertthat::assert_that(!("..json" %in% names(.x)))

    if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

    # Get JSON
    json <- json_get(.x)

    # Handle the case where json is just an empty list
    if (identical(json, list())) {
      y <- .x[integer(0), , drop = FALSE]
      y[column.name] <- default.column.empty
      return(tbl_json(y, list()))
    }

    # Determine types
    types <- determine_types(json)

    # Check if not correct type
    bad_type <- types != required.type
    if (any(bad_type))
      stop(sprintf("%s records are not %ss", sum(bad_type), required.type))

    y <- .x %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        ..name = json %>% purrr::map(expand.fun),
        ..json = json %>%
          purrr::map(~dplyr::tibble(..json = as.list(.)))
      ) %>%
      tidyr::unnest(c(..name, ..json))

    # Check to see if column.name exists, otherwise, increment until not
    if (column.name %in% names(y)) {
      new_col <- column.name
      suffix <- 2L
      while (new_col %in% names(y)) {
        new_col <- paste(column.name, suffix, sep = ".")
        suffix <- suffix + 1L
      }
      warning("%s column name already exists, changing to %s" %>%
                sprintf(column.name, new_col))
      column.name <- new_col
    }

    # Rename
    y <- y %>% dplyr::rename(!!!setNames("..name", column.name))

    # hotfix ..json names
    # https://github.com/tidyverse/tidyr/issues/802
    json_out <- y$..json
    if (
      !is.null(names(json_out)) &&
      (
        all(
          is.na(nchar(names(json_out))) |
          nchar(names(json_out)) == 0
        ) || length(names(json_out)) == 0
      )
      ) names(json_out) <- NULL
    # Construct tbl_json
    tbl_json(y %>% dplyr::select(-..json), json_out)

  }

}

#' Gather a JSON object into name-value pairs
#'
#' \code{gather_object} collapses a JSON object into name-value pairs, creating
#' a new column \code{'name'} to store the pair names, and storing the
#' values in the \code{'JSON'} attribute for further tidyjson manipulation.
#' All other columns are duplicated as necessary. This allows you to access the
#' names of the object pairs just like \code{\link{gather_array}} lets you
#' access the values of an array.
#'
#' \code{gather_object} is often followed by \code{\link{enter_object}} to enter
#' into a value that is an object, by \code{\link{append_values}} to append all
#' scalar values as a new column or \code{\link{json_types}} to determine the
#' types of the values.
#'
#' @seealso \code{\link{gather_array}} to gather a JSON array,
#'          \code{\link{enter_object}} to enter into an object,
#'          \code{\link[tidyr]{gather}} to gather name-value pairs in a data
#'          frame
#' @param .x a JSON string or \code{\link{tbl_json}} object whose JSON attribute
#'        should always be an object
#' @param column.name the name to give to the column of pair names created
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # Let's start with a very simple example
#' json <- '{"name": "bob", "age": 32, "gender": "male"}'
#'
#' # Check that this is an object
#' json %>% json_types
#'
#' # Gather object and check types
#' json %>% gather_object %>% json_types
#'
#' # Sometimes data is stored in object pair names
#' json <- '{"2014": 32, "2015": 56, "2016": 14}'
#'
#' # Then we can use the column.name argument to change the column name
#' json %>% gather_object("year")
#'
#' # We can also use append_values_number to capture the values, since they are
#' # all of the same type
#' json %>% gather_object("year") %>% append_values_number("count")
#'
#' # This can even work with a more complex, nested example
#' json <- '{"2015": {"1": 10, "3": 1, "11": 5}, "2016": {"2": 3, "5": 15}}'
#' json %>% gather_object("year") %>% gather_object("month") %>%
#'   append_values_number("count")
#'
#' # Most JSON starts out as an object (or an array of objects), and
#' # gather_object can be used to inspect the top level (or 2nd level) objects
#' library(dplyr)
#' worldbank %>% gather_object %>% json_types %>% count(name, type)
gather_object <- gather_factory("name", character(0), names, "object")

#' @rdname gather_object
#' @export
#' @usage NULL
gather_keys <- function(.x, column.name = "key") {
  .Deprecated("gather_object")
  f <- gather_factory("key", character(0), names, "object")
  f(.x, column.name)
}

#' Gather a JSON array into index-value pairs
#'
#' \code{gather_array} collapses a JSON array into index-value pairs, creating
#' a new column \code{'array.index'} to store the index of the array, and
#' storing values in the \code{'JSON'} attribute for further tidyjson
#' manipulation. All other columns are duplicated as necessary. This allows you
#' to access the values of the array just like \code{\link{gather_object}} lets
#' you access the values of an object.
#'
#' JSON arrays can be simple vectors (fixed or varying length number, string
#' or logical vectors with or without null values). But they also often contain
#' lists of other objects (like a list of purchases for a user). Thus, the
#' best analogy in R for a JSON array is an unnamed list.
#'
#' \code{gather_array} is often preceded by \code{\link{enter_object}} when the
#' array is nested under a JSON object, and is often followed by
#' \code{\link{gather_object}} or \code{\link{enter_object}} if the array values
#' are objects, or by \code{\link{append_values}} to append all scalar values
#' as a new column or \code{\link{json_types}} to determine the types of the
#' array elements (JSON does not guarantee they are the same type).
#'
#' @seealso \code{\link{gather_object}} to gather a JSON object,
#'          \code{\link{enter_object}} to enter into an object,
#'          \code{\link[tidyr]{gather}} to gather name-value pairs in a data
#'          frame
#' @param .x a json string or tbl_json object whose JSON attribute should always
#'        be an array
#' @param column.name the name to give to the array index column created
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # A simple character array example
#' json <- '["a", "b", "c"]'
#'
#' # Check that this is an array
#' json %>% json_types
#'
#' # Gather array and check types
#' json %>% gather_array %>% json_types
#'
#' # Extract string values
#' json %>% gather_array %>% append_values_string
#'
#' # A more complex mixed type example
#' json <- '["a", 1, true, null, {"name": "value"}]'
#'
#' # Then we can use the column.name argument to change the name column
#' json %>% gather_array %>% json_types
#'
#' # A nested array
#' json <- '[["a", "b", "c"], ["a", "d"], ["b", "c"]]'
#'
#' # Extract both levels
#' json %>% gather_array("index.1") %>% gather_array("index.2") %>%
#'   append_values_string
#'
#' # Some JSON begins as an array
#' commits %>% gather_array
#'
#' # We can use spread_all to capture all values
#' # (recursive = FALSE to limit to the top level object)
#' library(dplyr)
#' commits %>% gather_array %>% spread_all(recursive = FALSE) %>% glimpse
gather_array <- gather_factory("array.index", integer(0), seq_along, "array")

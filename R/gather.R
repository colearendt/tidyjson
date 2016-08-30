#' Factory to create gather functions
#'
#' @param default.column.name the desired name of the default added column
#' @param default.column.empty the value to use when the default column should
#'   be empty because the JSON has length 0
#' @param expand.fun a function applied to the JSON that will expand the rows
#'   in the tbl_df
#' @param required.type the json_types type that must be present in every
#'   element of the JSON for this to succeed
gather_factory <- function(default.column.name, default.column.empty,
                           expand.fun, required.type) {

  function(x, column.name = default.column.name) {

    assert_that(!("..key" %in% names(x)))
    assert_that(!("..json" %in% names(x)))

    if (!is.tbl_json(x)) x <- as.tbl_json(x)

    # Get JSON
    json <- attr(x, "JSON")

    # Handle the case where json is just an empty list
    if (identical(json, list())) {
      y <- x[integer(0), , drop = FALSE]
      y[column.name] <- default.column.empty
      return(tbl_json(y, list()))
    }

    # Determine types
    types <- determine_types(json)

    # Check if not correct type
    bad_type <- types != required.type
    if (any(bad_type))
      stop(sprintf("%s records are not %ss", sum(bad_type), required.type))

    y <- x %>%
      tbl_df %>%
      mutate(
        ..key = json %>% map(expand.fun),
        ..json = json %>%
          map(~data_frame(..json = as.list(.)))
      ) %>%
      unnest(..key, ..json, .drop = FALSE) %>%
      rename_(.dots = setNames("..key", column.name))

    # Construct tbl_json
    tbl_json(y %>% select(-..json), y$..json)

  }

}

#' Stack a JSON {"key": value} object
#'
#' Given a JSON key value structure, like {"key1": 1, "key2": 2}, the
#' gather_keys() function duplicates the rows of the tbl_json data.frame for
#' every key, adds a new column (default name "key") to capture the key names,
#' and then dives into the JSON values to enable further manipulation with
#' downstream tidyjson functions.
#'
#' This allows you to *enter into* the keys of the objects just like `gather_array`
#' let you enter elements of the array.
#'
#' @param x a tbl_json whose JSON attribute should always be an object
#' @param column.name the name to give to the column of key names created
#' @return a tbl_json with a new column (column.name) that captures the keys
#'   and JSON attribute of the associated value data
#' @export
#' @examples
#' '{"name": "bob", "age": 32}' %>% gather_keys %>% json_types
gather_keys <- gather_factory("key", character(0), names, "object")

#' Stack a JSON array
#'
#' Given a JSON array, such as [1, 2, 3], gather_array will "stack" the array in
#' the tbl_json data.frame, by replicating each row of the data.frame by the
#' length of the corresponding JSON array. A new column (by default called
#' "array.index") will be added to keep track of the referenced position in the
#' array for each row of the resuling data.frame.
#'
#' JSON can contain arrays of data, which can be simple vectors (fixed or varying
#' length integer, character or logical vectors). But they also often contain
#' lists of other objects (like a list of purchases for a user). The function
#' gather_array() takes JSON arrays and duplicates the rows in the data.frame to
#' correspond to the indices of the array, and puts the elements of
#' the array into the JSON attribute. This is equivalent to "stacking" the array
#' in the data.frame, and lets you continue to manipulate the remaining JSON
#' in the elements of the array. For simple arrays, use append_values_* to
#' capture all of the values of the array. For more complex arrays (where the
#' values are themselves objects or arrays), continue using other tidyjson
#' functions to structure the data as needed.
#'
#' @param x a tbl_json whose JSON attribute should always be an array
#' @param column.name the name to give to the array index column created
#' @return a tbl_json with a new column (column.name) that captures the array
#'   index and JSON attribute extracted from the array
#' @export
#' @examples
#' '[1, "a", {"k": "v"}]' %>% gather_array %>% json_types
gather_array <- gather_factory("array.index", integer(0), seq_along, "array")

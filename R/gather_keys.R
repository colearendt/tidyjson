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
gather_keys <- function(x, column.name = "key") {

  assert_that(!("..key" %in% names(x)))
  assert_that(!("..json" %in% names(x)))

  if (!is.tbl_json(x)) x <- as.tbl_json(x)

  # Get JSON
  json <- attr(x, "JSON")

  # Handle the case where json is just an empty list
  if (identical(json, list())) {
    y <- x[integer(0), , drop = FALSE]
    y[column.name] <- character(0)
    return(tbl_json(y, list()))
  }

  # unnest keys
  tryCatch({

    y <- x %>%
      tbl_df %>%
      mutate(
        ..key = json %>% map(names),
        ..json = json %>%
          map(~data_frame(..json = as.list(.)))
      ) %>%
      unnest(..key, ..json) %>%
      rename_(.dots = setNames("..key", column.name))

    },
    error = function(e) {

      # Determine types
      types <- determine_types(json)

      # Check if not objects
      not_objects <- types != "object"
      if (any(not_objects)) {
        stop(sprintf("%s records are values not objects", sum(not_objects)))
      } else {
        # Throw whatever other error we found
        stop(e$message)
      }

    }
  )

  # Construct tbl_json
  tbl_json(y %>% select(-..json), y$..json)

}

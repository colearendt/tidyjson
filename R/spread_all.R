#' Spreads all scalar values of a JSON object into new columns
#'
#' Like the \code{\link[tidyr]{spread}} function in \code{tidyr} but for JSON,
#' this function spreads out any JSON objects that are scalars into new columns.
#' If objects are nested, then the recursive flag will expand scalar values of
#' nested objects out with a compound column name based on the sequences of
#' nested keys concatenated with the \code{sep} character.
#'
#' Note that arrays are ignored by this function, use \code{\link{gather_array}}
#' to gather the array first, and then use \code{spread_all} if the array
#' contains objects or use one of the \code{\link{append_values}} functions to
#' capture the array values if they are scalars.
#'
#' Note that scalar JSON values (e.g., a JSON string like '1') are also
#' ignored, as they have no keys to create column names with.
#'
#' The order of columns is determined by the order they are encountered in the
#' JSON document, with nested objects placed at the end.
#'
#' This function does not change the value of the JSON attribute of the
#' \code{\link{tbl_json}} object in any way.
#'
#' @seealso \code{\link{spread_values}} to specific which specific values
#'          to spread along with their types,
#'          \code{\link[tidyr]{spread}} for spreading data frames
#' @param .x a json string or \code{\link{tbl_json}} object
#' @param recursive whether or not to recursively spread nested objects
#' @param sep character used to separate nested object keys when resursive
#'   is \code{TRUE}
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # A simple example
#' json <- c('{"a": "x", "b": 1, "c": true}',
#'           '{"a": "y", "c": false}',
#'           '{"a": null, "d": "z"}')
#' json %>% spread_all
#'
#' # A more complex example
#' worldbank %>% spread_all
spread_all <- function(.x, recursive = TRUE, sep = ".") {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  reserved_cols <- c("..id", "..key", "..key2", "..type", "..value")
  assert_that(!(any(reserved_cols %in% names(.x))))

  # Return .x if no rows
  if (nrow(.x) == 0)
    return(.x)

  # Check if any objects
  unq_types <- .x %>% json_types("..type") %>% extract2("..type") %>% unique
  if (!("object" %in% unq_types)) {
    warning("no JSON records are objects, returning .x")
    return(.x)
  }

  # Get JSON
  json <- attr(.x, "JSON")

  # Create a new identifier
  .x <- .x %>% mutate(..id = seq_len(n()))

  # gather types
  y <- .x %>%
    gather_keys("..key") %>%
    json_types("..type")

  if (recursive)
    while(any(y$..type == "object"))
      y <- rbind_tbl_json(
        y %>% filter(..type != "object"),
        recursive_gather(y, sep)
      )

  key_order <- y %>%
    filter(..type %in% c("string", "number", "logical", "null")) %>%
    extract2("..key") %>%
    unique

  y_string  <- spread_type(y, "string",  append_values_string)
  y_number  <- spread_type(y, "number",  append_values_number)
  y_logical <- spread_type(y, "logical", append_values_logical)

  z <- .x %>%
    left_join(y_string,  by = "..id") %>%
    left_join(y_number,  by = "..id") %>%
    left_join(y_logical, by = "..id")

  all_null <- y %>%
    group_by(..key) %>%
    summarize(all.null = all(..type == "null")) %>%
    filter(all.null)

  if (nrow(all_null) > 0) {
    null_keys <- all_null %>% extract2("..key")
    z[, null_keys] <- NA
  }

  final_columns <- names(.x) %>%
    setdiff("..id") %>%
    c(key_order)

  z[, final_columns, drop = FALSE] %>%
    tbl_json(json)

}

# Recursively gathers keys
recursive_gather <- function(.x, sep) {

  .x %>%
    filter(..type == "object") %>%
    rename(..key1 = ..key) %>%
    gather_keys("..key2") %>%
    mutate(..key = paste(..key1, ..key2, sep = sep)) %>%
    select(-..type, -..key1, -..key2) %>%
    json_types("..type")

}

# Spreads keys of a specific type
spread_type <- function(.x, this.type, append.fun) {

  any_type <- any(.x$..type == this.type)

  if (!any_type)
    return(data_frame(..id = integer(0)))

  .x %>%
    filter(..type == this.type) %>%
    append.fun("..value") %>%
    tbl_df %>%
    select(..id, ..key, ..value) %>%
    spread(..key, ..value)

}

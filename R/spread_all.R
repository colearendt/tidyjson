#' Spreads all scalar values of a JSON object into new columns
#'
#' Like the \code{\link[tidyr]{spread}} function in \code{tidyr} but for JSON,
#' this function spreads out any JSON objects that are scalars into new columns.
#' If objects are nested, then the recursive flag will expand scalar values of
#' nested objects out with a compound column name based on the sequences of
#' nested object names concatenated with the \code{sep} character.
#'
#' Note that arrays are ignored by this function, use \code{\link{gather_array}}
#' to gather the array first, and then use \code{spread_all} if the array
#' contains objects or use one of the \code{\link{append_values}} functions to
#' capture the array values if they are scalars.
#'
#' Note that scalar JSON values (e.g., a JSON string like '1') are also
#' ignored, as they have no names to create column names with.
#'
#' The order of columns is determined by the order they are encountered in the
#' JSON document, with nested objects placed at the end.
#'
#' If an objects have name-value pairs with names that are duplicates, then
#' \code{".n"} is appended for n incrementing from 2 on to ensure that columns
#' are unique. This also happens if \code{.x} already has a column with the
#' name of a name-value pair.
#'
#' This function does not change the value of the JSON attribute of the
#' \code{\link{tbl_json}} object in any way.
#'
#' @seealso \code{\link{spread_values}} to specific which specific values
#'          to spread along with their types,
#'          \code{\link[tidyr]{spread}} for spreading data frames
#' @param .x a json string or \code{\link{tbl_json}} object
#' @param recursive whether or not to recursively spread nested objects
#' @param sep character used to separate nested object names when resursive
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
#'
#' \dontrun{
#'   # Resolving duplicate column names
#'   json <- '{"a": "x", "a": "y"}'
#'   json %>% spread_all
#' }
spread_all <- function(.x, recursive = TRUE, sep = ".") {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  reserved_cols <- c("..id", "..name1", "..name2", "..type", "..value",
                     "..suffix")
  assertthat::assert_that(!(any(reserved_cols %in% names(.x))))

  # Return .x if no rows
  if (nrow(.x) == 0)
    return(.x)

  # Check if any objects
  unq_types <- .x %>% json_types("..type") %>% magrittr::extract2("..type") %>% unique
  if (!("object" %in% unq_types)) {
    warning("no JSON records are objects, returning .x")
    return(.x)
  }

  # Get existing column names
  exist_cols <- names(.x)

  # Get JSON
  json <- json_get(.x)

  # Create a new identifier
  .x <- .x %>% dplyr::mutate(..id = seq_len(n()))

  # gather types
  y <- .x %>%
    gather_object("..name1") %>%
    json_types("..type")

  if (recursive) {
    while(any(y$..type == "object"))
      y <- dplyr::bind_rows(
        y %>% dplyr::filter(..type != "object"),
        recursive_gather(y, sep)
      )
  } else {
    y <- y %>% dplyr::filter(..type != 'object')
  }
    

  # Look for duplicate keys
  key_freq <- y %>% dplyr::group_by(..id, ..name1) %>% dplyr::tally()

  while (any(key_freq$n > 1) || any(key_freq$..name1 %in% exist_cols)) {

    warning("results in duplicate column names, appending .# for uniqueness")

    # Deal with duplicate keys
    y_dedupe <- y %>%
      dplyr::group_by(..id, ..name1) %>%
      dplyr::mutate(..suffix = 1L:n()) %>%
      dplyr::mutate(..suffix = ..suffix + ifelse(..name1 %in% exist_cols, 1L, 0L)) %>%
      dplyr::mutate(..suffix = ifelse(..suffix == 1L, "", paste0(".", ..suffix))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(..name1 = paste0(..name1, ..suffix)) %>%
      dplyr::select(-..suffix)

    # Re-attach JSON
    y <- tbl_json(y_dedupe, json_get(y))

    key_freq <- y %>% dplyr::group_by(..id, ..name1) %>% dplyr::tally()
  }

  name_order <- y %>%
    dplyr::filter(..type %in% c("string", "number", "logical", "null")) %>%
    magrittr::extract2("..name1") %>%
    unique

  y_string  <- spread_type(y, "string",  append_values_string)
  y_number  <- spread_type(y, "number",  append_values_number)
  y_logical <- spread_type(y, "logical", append_values_logical)

  ## Build tibble component
  z <- dplyr::as_tibble(.x) %>%
    dplyr::left_join(y_string,  by = "..id") %>%
    dplyr::left_join(y_number,  by = "..id") %>%
    dplyr::left_join(y_logical, by = "..id")

  all_null <- y %>%
    dplyr::group_by(..name1) %>%
    dplyr::summarize(all.null = all(..type == "null")) %>%
    dplyr::filter(all.null)

  if (nrow(all_null) > 0) {
    null_names <- all_null %>% magrittr::extract2("..name1")
    z[, null_names] <- NA
  }

  final_columns <- names(.x) %>%
    dplyr::setdiff(c("..id", "..JSON")) %>%
    c(name_order)

  z[, final_columns, drop = FALSE] %>%
    tbl_json(json)

}

# Recursively gathers names
recursive_gather <- function(.x, sep) {

  .x %>%
    dplyr::filter(..type == "object") %>%
    gather_object("..name2") %>%
    dplyr::mutate(..name1 = paste(..name1, ..name2, sep = sep)) %>%
    dplyr::select(-..type, -..name2) %>%
    json_types("..type")

}

# Spreads names of a specific type
spread_type <- function(.x, this.type, append.fun) {

  any_type <- any(.x$..type == this.type)

  if (!any_type)
    return(dplyr::tibble(..id = integer(0)))

  .x %>%
    dplyr::filter(..type == this.type) %>%
    append.fun("..value") %>%
    dplyr::as_tibble() %>%
    dplyr::select(..id, ..name1, ..value) %>%
    tidyr::spread(..name1, ..value)

}

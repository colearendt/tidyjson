#' Combines structured JSON (as a data.frame) with remaining JSON
#'
#' @name tbl_json
NULL

#' \code{tbl_json} constructor
#'
#' Constructs a \code{tbl_json} object, for further downstream manipulation
#' by other tidyjson functions. Methods exist to convert JSON stored in
#' character strings without any other associated data, as a separate
#' character string and associated data frame, or as a single data frame
#' with a specified character string JSON column.
#'
#' Most tidyjson functions accept a \code{tbl_json} object as the first
#' argument, and return a \code{tbl_json} object unless otherwise specified.
#' tidyjson functions will attempt to convert an object that isn't a
#' \code{tbl_json} object first, and so explicit construction of \code{tidyjson}
#' objects is rarely needed.
#'
#' \code{tbl_json} objects consist of a data frame along with it's associated
#' JSON, where each row of the data frame corresponds to a single JSON
#' document. The JSON is stored in a \code{"JSON"} attribute.
#'
#' Note that \code{json.list} must have the same length as \code{nrow(df)}, and
#' if \code{json.list} has any \code{NULL} elements, the corresponding rows will
#' be removed from \code{df}. Also note that \code{"..JSON"} is a reserved
#' column name used internally for filtering tbl_json objects, and so is not
#' allowed in the names of \code{df}.
#'
#' @seealso \code{read_json} for reading json from files
#' @param df data.frame
#' @param json.list list of json lists parsed with
#'                  \code{\link[jsonlite]{fromJSON}}
#' @param drop.null.json drop \code{NULL} json entries from \code{df} and
#'                       \code{json.list}
#' @param .x an object to convert into a \code{tbl_json} object
#' @param json.column the name of the json column of data in \code{.x}, if
#'                    \code{.x} is a data frame
#' @param ... other arguments
#' @return a \code{\link{tbl_json}} object
#' @rdname tbl_json
#' @export
#' @examples
#'
#' # Construct a tbl_json object using a charater string of JSON
#' json <- '{"animal": "cat", "count": 2}'
#' json %>% as.tbl_json
#'
#' # access the "JSON" argument
#' json %>% as.tbl_json %>% attr("JSON")
#'
#' # Construct a tbl_json object using multiple documents
#' json <- c('{"animal": "cat", "count": 2}', '{"animal": "parrot", "count": 1}')
#' json %>% as.tbl_json
#'
#' # Construct a tbl_json object from a data.frame with a JSON colum
#' library(tibble)
#' farms <- tribble(
#'   ~farm, ~animals,
#'   1L,    '[{"animal": "pig", "count": 50}, {"animal": "cow", "count": 10}]',
#'   2L,    '[{"animal": "chicken", "count": 20}]'
#' )
#' farms %>% as.tbl_json(json.column = "animals")
#' # tidy the farms
#' farms %>% as.tbl_json(json.column = "animals") %>%
#'   gather_array %>% spread_all
tbl_json <- function(df, json.list, drop.null.json = FALSE) {

  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.list(json.list) || is.vector(json.list))
  assertthat::assert_that(nrow(df) == length(json.list))
  assertthat::assert_that(!("..JSON" %in% names(df)))

  # Remove any row.names
  row.names(df) <- NULL

  # Remove any rows of df where json.list is NULL
  if (drop.null.json) {
    nulls <- purrr::map_lgl(json.list, is.null)
    df <- df[!nulls, , drop = FALSE]
    json.list <- json.list[!nulls]
  }

  structure(df, JSON = json.list, class = c("tbl_json", "tbl_df", "tbl", "data.frame"))
}

#' @export
#' @rdname tbl_json
as.tbl_json <- function(.x, ...) UseMethod("as.tbl_json")


#' @export
#' @rdname tbl_json
as.tbl_json.tbl_json <- function(.x, ...) .x

#' @export
#' @rdname tbl_json
as.tbl_json.character <- function(.x, ...) {

  # Parse the json
  json <- purrr::map(.x, jsonlite::fromJSON, simplifyVector = FALSE)

  # Setup document ids
  ids <- data.frame(document.id = seq_along(json))

  # Construct tbl_json
  tbl_json(ids, json)
}

#' @export
#' @rdname tbl_json
as.tbl_json.data.frame <- function(.x, json.column, ...) {

  assertthat::assert_that(is.character(json.column))
  assertthat::assert_that(json.column %in% names(.x))

  # Parse the json
  json <- purrr::map(.x[[json.column]], jsonlite::fromJSON, simplifyVector = FALSE)

  # Remove json column
  .x <- .x[, setdiff(names(.x), json.column), drop = FALSE]

  # Construct tbl_json
  tbl_json(.x, json)

}

#' @export
#' @rdname tbl_json
is.tbl_json <- function(.x) inherits(.x, "tbl_json")

#' Extract subsets of a tbl_json object (not replace)
#'
#' Extends `[.data.frame` to work with tbl_json objects, so that row filtering
#' of the underlying data.frame also filters the associated JSON.
#'
#' @param .x a tbl_json object
#' @param i row elements to extract
#' @param j column elements to extract
#' @param drop whether or not to simplify results
#' @return a \code{\link{tbl_json}} object
#' @export
`[.tbl_json` <- function(.x, i, j,
  drop = FALSE) {

  # Extract JSON to subset later
  json <- attr(.x, "JSON")
  
  # Subset x
  .x <- NextMethod('[')

  # If i is not missing, subset json as well
  if (!missing(i)) {
    json <- json[i]
  }

  tbl_json(.x, json)
}

#' Wrapper for extending dplyr verbs to tbl_json objects
#' @param dplyr.verb a dplyr::verb such as filter, arrange
wrap_dplyr_verb <- function(dplyr.verb) {

  function(.data, ...) {

    # Check if reserved ..JSON name already in data.frame
    if ("..JSON" %in% names(.data))
      stop("'..JSON' in the column names of tbl_json object being filtered")

    # Assign JSON to the data.frame so it is treated as any other column
    .data$..JSON <- attr(.data, "JSON")

    # Apply the transformation
    y <- dplyr.verb(dplyr::as_tibble(.data), ...)

    # Reconstruct tbl_json without ..JSON column
    tbl_json(dplyr::select(y, -..JSON), y$..JSON)

  }
}

#' @export
filter_.tbl_json <- wrap_dplyr_verb(dplyr::filter_)

#' @export
#' @method filter tbl_json
filter.tbl_json <- wrap_dplyr_verb(dplyr::filter)

#' @export
arrange_.tbl_json <- wrap_dplyr_verb(dplyr::arrange_)

#' @export
#' @method arrange tbl_json
arrange.tbl_json <- wrap_dplyr_verb(dplyr::arrange)

#' @export
mutate_.tbl_json <- wrap_dplyr_verb(dplyr::mutate_)

#' @export
#' @method mutate tbl_json
mutate.tbl_json <- wrap_dplyr_verb(dplyr::mutate)

#' @export
slice_.tbl_json <- wrap_dplyr_verb(dplyr::slice_)

#' @export
#' @method slice tbl_json
slice.tbl_json <- wrap_dplyr_verb(dplyr::slice)

#' 
#' Bind Rows (tidyjson)
#' 
#' Since bind_rows is not currently an s3 method, this function
#' is meant to mask dplyr::bind_rows (although it is called directly).
#' 
#' @return If all parameters are `tbl_json` objects, then the JSON attributes
#' will be stacked and a `tbl_json` will be returned.  Otherwise, 
#' `dplyr::bind_rows` is used, a message is displayed,
#' and a `tbl_df` is returned.
#' 
#' @seealso [Related dplyr issue](https://github.com/tidyverse/dplyr/issues/2457)
#' @seealso \code{\link[dplyr]{bind_rows}}
#'  
#' @param ... Values passed on to dplyr::bind_rows
#' 
#' @examples 
#' 
#' ## Simple example
#' a <- as.tbl_json('{"a": 1, "b": 2}')
#' b <- as.tbl_json('{"a": 3, "b": 4}')
#' 
#' bind_rows(a,b) %>% spread_values(a=jnumber(a),b=jnumber(b))
#' 
#' ## as a list
#' bind_rows(list(a,b)) %>% spread_all()
#' 
#' @export
#' 
bind_rows <- function(...) {
  r <- dplyr::bind_rows(...)
  
  d <- list_or_dots(...)
  if (all(unlist(lapply(d,is.tbl_json)))) {
    j <- unlist(lapply(d, attr, 'JSON'), recursive=FALSE)
    return(tbl_json(r,j))
  } else {
    message('Some non-tbl_json objects.  Reverting to dplyr::bind_rows')
    return(dplyr::as_tibble(r))
  }
}

#' Convert the JSON in an tbl_json object back to a JSON string
#'
#' @param x a tbl_json object
#' @param ... not used (\code{\link[purrr]{map_chr}} used instead)
#' @return a character vector of formatted JSON
#' @export
as.character.tbl_json <- function(x, ...) {

  json <- attr(x, "JSON")
  if (is.null(json)) {
    warning("attr(.,'JSON') has been removed from this tbl_json object")
    json <- list()
  }
  json %>% purrr::map_chr(jsonlite::toJSON,
                   null = "null",
                   auto_unbox = TRUE)

}

#' Convert a tbl_json back to a tbl_df
#' 
#' Drops the JSON attribute and the tbl_json class, so that
#' we are back to a pure tbl_df.  Useful for some internals.  Also useful
#' when you are done processing the JSON portion of your data and are
#' ready to move on to other tools.
#' 
#' Note that as.tbl calls tbl_df under the covers, which in turn
#' calls as_tibble.  As a result, this should take care of all cases.
#' 
#' @param x a tbl_json object
#' @param ... additional parameters
#' @return a tbl_df object (with no tbl_json component)
#' 
#' @export
as_tibble.tbl_json <- function(x, ...) {
  attr(x,'JSON') <- NULL
  as_tibble(
    structure(x, class = class(tibble::tibble()))
  )
}

#' @rdname as_tibble.tbl_json
#' 
#' @export
as_data_frame.tbl_json <- function(x, ...) {
  as_tibble.tbl_json(x,...)
}



#' Print a tbl_json object
#'
#' @param x a \code{\link{tbl_json}} object
#' @param ... other arguments into \code{\link[tibble]{print.tbl_df}}
#' @param json.n number of json records to add (...) otherwise
#' @param json.width number of json characters to print
#' @export
print.tbl_json <- function(x, ..., json.n = 20, json.width = 15) {

  # Extract json
  json <- x %>% as.character
  json <- json[seq_len(min(json.n, nrow(x)))]

  # Truncate json
  lengths <- dplyr::coalesce(json %>% nchar,0L)
  json <- json %>% strtrim(json.width)
  json[lengths > json.width] <- paste0(json[lengths > json.width], "...")

  # Add the json
  .y <- dplyr::as_tibble(x)
  json_name <- 'attr(., "JSON")'
  .y[json_name] <- rep("...", nrow(x))
  .y[[json_name]][seq_len(length(json))] <- json

  # Re-arrange columns
  ncol_y <- ncol(.y)
  .y <- .y[, c(ncol_y, seq_len(ncol_y - 1))]

  # Print trunc_mat version
  out <- capture.output(print(.y))
  out <- gsub("^\\# A tibble:.*",
              "\\# A tbl_json: %s x %s tibble with a \"JSON\" attribute" %>%
                sprintf(nrow(x) %>% format(big.mark = ','),
                        ncol(x) %>% format(big.mark = ',')),
              out)
  writeLines(out)

  invisible(x)
}

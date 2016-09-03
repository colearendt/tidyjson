#' Spreads all non-array keys into new columns
#'
#' @param .x a json string or tbl_json object
#' @param sep what to separate nested object keys with
#' @export
spread_all <- function(.x, sep = ".") {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  reserved_cols <- c("..id", "..key", "..type", "..value")
  assert_that(!(any(reserved_cols %in% names(.x))))

  # Get JSON
  json <- attr(.x, "JSON")

  # Create a new identifier
  .x <- .x %>% mutate(..id = seq_len(n()))

  # gather types
  y <- .x %>%
    gather_keys("..key") %>%
    json_types("..type")

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

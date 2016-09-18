#' Startup company information for 1,000 companies
#'
#' From: http://jsonstudio.com/resources/
#'
#' @docType data
#' @name companies
#' @usage companies
#' @format JSON
#' @examples
#'
#' library(dplyr)
#'
#' # Companies is a long character vector
#' companies %>% str
#'
#' # Work with a small sample
#' co_samp <- companies[1:5]
#'
#' # Gather top level values and glimpse
#' co_samp %>% spread_all %>% glimpse
#'
#' # Get the key employees data for the first 100 companies
#' key_employees <- companies[1:100] %>%
#'   spread_all %>%
#'   select(name) %>%
#'   enter_object(relationships) %>%
#'   gather_array() %>%
#'   spread_all
#'
#' key_employees %>% glimpse
#'
#' # Show the top 10 titles
#' key_employees %>%
#'   filter(!is_past) %>%
#'   count(title) %>%
#'   arrange(desc(n)) %>%
#'   top_n(10)
NULL

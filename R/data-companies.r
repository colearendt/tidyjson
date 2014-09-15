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
#' # Print the first record
#' companies[[1]] %>% writeLines
#' 
#' # Get the key employees data
#' key_employees <- companies %>%
#'   as.tbl_json %>% 
#'   spread_values(
#'     name = jstring("name")
#'   ) %>% 
#'   enter_object("relationships") %>%
#'   gather_array("relationship.index") %>%
#'   spread_values(
#'     is.past = jlogical("is_past"),
#'     name = jstring("person", "permalink"),
#'     title = jstring("title")
#'   )
#' 
#' # Show the top 10 titles
#' key_employees %>%
#'   filter(!is.past) %>%
#'   group_by(title) %>%
#'   tally() %>%
#'   arrange(desc(n)) %>%
#'   top_n(10)
NULL


#' Issue data for the dplyr repo from github API
#' 
#' @docType data
#' @name issues
#' @usage issues
#' @format JSON
#' @examples
#' 
#' library(dplyr)
#' 
#' # Show first 200 characters of JSON
#' issues %>% substr(1, 200) %>% writeLines
#' 
#' # Extract metadata for every issue
#' issues %>%          # single json document of github issues from dplyr 
#'   gather_array %>%  # stack as an array
#'   spread_values(
#'     id          = jnumber("id"),
#'     number      = jnumber("number"),
#'     title       = jstring("title"),
#'     user.login  = jstring("user", "login"),
#'     sate        = jstring("state"),
#'     locked      = jlogical("locked"),
#'     comments    = jnumber("comments")
#'   ) %>% head
#' 
#' # Extract label content for issues with labels
#' issues %>%          # single json document of github issues from dplyr 
#'   gather_array %>%  # stack as an array
#'   spread_values(id = jnumber("id")) %>% # capture issue id for relational purposes
#'   enter_object("labels") %>%            # filter just those with labels
#'   gather_array("label.index") %>%       # stack labels
#'   spread_values(
#'     url   = jstring("url"),
#'     name  = jstring("name"),
#'     color = jstring("color")
#'   ) %>% head
#' 
#' # Get all URLs at the top level of the JSON
#' issues %>% 
#'   gather_array %>%
#'   gather_keys %>%
#'   append_values_string() %>%
#'   filter(grepl("url", key)) %>%
#'   head
NULL



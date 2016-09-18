#' Commit data for the dplyr repo from github API
#'
#' @docType data
#' @name commits
#' @usage commits
#' @format JSON
#' @examples
#'
#' library(dplyr)
#'
#' # Commits is a long character string
#' commits %>% nchar
#'
#' # Let's make it a tbl_json object
#' commits %>% as.tbl_json
#'
#' # It begins as an array, so let's gather that
#' commits %>% gather_array
#'
#' # Now let's spread all the top level values
#' commits %>% gather_array %>% spread_all %>% glimpse
#'
#' # Are there any top level objects or arrays?
#' commits %>% gather_array %>% gather_object %>% json_types %>%
#'   count(name, type)
#'
#' # Let's look at the parents array
#' commits %>% gather_array("commit") %>%
#'   enter_object(parents) %>% gather_array("parent") %>%
#'   spread_all %>% glimpse
NULL

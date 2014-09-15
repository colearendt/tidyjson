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
#' # Show first 2k characters of JSON
#' commits %>% substr(1, 2000) %>% writeLines
#' 
#' # Extract metadata for every commit
#' commits %>%   # single json document of github commits from dplyr 
#'   as.tbl_json %>%  # turn into a 'tbl_json'
#'   gather_array %>%  # stack as an array
#'   spread_values(
#'     sha         = jstring("sha"),
#'     author      = jstring("commit", "author", "name"),
#'     author.date = jstring("commit", "author", "date")
#'   )
NULL
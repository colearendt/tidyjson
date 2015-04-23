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
#' # Show first 200 characters of JSON
#' commits %>% substr(1, 200) %>% writeLines
#' 
#' # Extract metadata for every commit
#' commits %>%   # single json document of github commits from dplyr 
#'   gather_array %>%  # stack as an array
#'   spread_values(
#'     sha         = jstring("sha"),
#'     author      = jstring("commit", "author", "name"),
#'     author.date = jstring("commit", "author", "date")
#'   ) %>% head
NULL
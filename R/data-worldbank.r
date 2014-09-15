#' Projects funded by the World Bank
#'
#' From: http://jsonstudio.com/resources/
#' 
#' @docType data
#' @name worldbank
#' @usage worldbank
#' @format JSON
#' @examples
#' 
#' library(dplyr)
#' 
#' # Print the first record
#' worldbank[[1]] %>% writeLines
#' 
#' # Get the top 10 sectors by funded project in Africa
#' worldbank %>%   # 500 Projects funded by the world bank
#'   as.jdf %>%    # turn into a jdf object
#'   jvalue(
#'     name = jstring("project_name"), # Gather name 
#'     region = jstring("regionname")  # Gather region
#'   ) %>% 
#'   jfilter("sector") %>%             # Select the 'sector' object
#'   jarray("sector.index") %>%        # Expand the array
#'   jvalue(sector = jstring("Name")) %>% # Capture the sector name
#'   filter(region == "Africa") %>%    # Filter to just Africa
#'   group_by(sector) %>%              # Group by sector
#'   tally() %>%                       # Count funded projects
#'   arrange(desc(n)) %>%              # Arrange descending
#'   top_n(10)                         # Take the top 10  
NULL

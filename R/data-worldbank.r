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
#' library(jsonlite)
#' 
#' # Print the first record
#' worldbank[[1]] %>% prettify
#' 
#' # Get the top 10 sectors by funded project in Africa
#' wb_sectors <- worldbank %>%   # 500 Projects funded by the world bank
#'   spread_values(
#'     name = jstring("project_name"), # Spread name 
#'     region = jstring("regionname")  # Spread region
#'   ) %>% 
#'   enter_object("majorsector_percent") %>%              # Enter the 'sector' object
#'   gather_array("sector.index") %>%        # Gather the array
#'   spread_values(sector = jstring("Name")) # Spread the sector name
#' 
#' # Examine the structured data
#' wb_sectors %>% head
#' 
#' # Get the top 10 sectors by funded project in Africa
#' wb_sectors %>%
#'   filter(region == "Africa") %>% # Filter to just Africa
#'   group_by(sector) %>%           # Group by sector
#'   tally() %>%                    # Count funded projects
#'   arrange(desc(n)) %>%           # Arrange descending
#'   top_n(10)                      # Take the top 10
NULL

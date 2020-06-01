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
#' \dontrun{
#' library(dplyr)
#'
#' # worldbank is a 500 length character vector of JSON
#' length(worldbank)
#'
#' # Let's look at top level values
#' worldbank %>% spread_all %>% glimpse
#'
#' # Are there any arrays?
#' worldbank %>% gather_object %>% json_types %>% count(name, type)
#'
#' # Get the top 10 sectors by funded project in Africa
#' wb_sectors <- worldbank %>%   # 500 Projects funded by the world bank
#'   spread_all %>%
#'   select(project_name, regionname) %>%
#'   enter_object(majorsector_percent) %>% # Enter the 'sector' object
#'   gather_array("sector.index") %>%      # Gather the array
#'   spread_values(sector = jstring(Name)) # Spread the sector name
#'
#' # Examine the structured data
#' wb_sectors %>% glimpse
#'
#' # Get the top 10 sectors by funded project in Africa
#' wb_sectors %>%
#'   filter(regionname == "Africa") %>% # Filter to just Africa
#'   count(sector) %>%              # Count by sector
#'   arrange(desc(n)) %>%           # Arrange descending
#'   top_n(10)                      # Take the top 10
#' }
NULL

# Script to get github commit json data
#
# Notes:
#  1) this script must be run interactively (e.g., source("file.r"))
#  2) you must set JPLYR_* environment variables used below

require(httr)

#' Gets github data from API
#'
#' @param url the github API url to GET
#' @param token the oauth2.0 token
get_github_data <- function(url, token) {
  
  # Get request
  request <- with_config(config(token = token), GET(url))

  # Convert to JSON
  readBin(request$content, character())

}

# Setup app
github_app <- oauth_app("jplyr_demo",
  key = Sys.getenv("JPLYR_GITHUB_KEY"),
  secret = Sys.getenv("JPLYR_GITHUB_SECRET")
)

# Get endpoing
github_endpoint <- oauth_endpoints("github")

# Get credentials
github_token <- oauth2.0_token(github_endpoint, github_app)

# Get commits in a repo
commits <- get_github_data(
  "https://api.github.com/repos/hadley/dplyr/commits",
  github_token)
save(commits, file = "../data/commits.rda")

# Get dplyr issues
issues <- get_github_data(
  "https://api.github.com/repos/hadley/dplyr/issues",
  github_token)
save(issues, file = "../data/issues.rda")

  





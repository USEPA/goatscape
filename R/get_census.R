#' get_census
#'
#' This function ...
#'
#' @param year Which census year do you want to use? In format YYYY.  Currently the package supports census years 2000 and 2010.
#' @param spatial keep the spatial data? Default = FALSE
#' @param api_key #Your Census API key. Obtain one at http://api.census.gov/data/key_signup.html
#'
#' @export
#' @examples
#' get_census()
get_census <- function(year = 2010, spatial = FALSE,
                     api_key = Sys.getenv("CENSUS_API_KEY")){
  
  #browser()
  # check for census api key (modified from tidycensus::get_decennial)
  if (Sys.getenv("CENSUS_API_KEY") != "") {
    api_key<-Sys.getenv("CENSUS_API_KEY")
  } else if (is.null(api_key)) {
    stop("A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html.")
  }
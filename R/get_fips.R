#' get_fips
#'
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon.returns the total population for that area.
#' Currently the function returns the state and county fips codes for years 1990, 2000, and 2010
#' Additional years may be added later.
#'
#' @param landscape A spatial polygon of class "sf" (see package [sf]). Note: the object must be projected (i.e. has a coordinate reference system)
#' @param year Which census year do you want to use? In format YYYY.  Currently the package supports census years 1990, 2000, and 2010.
#'
#' @export
#' @examples
#' get_fips()
get_fips <- function(landscape, year = 2010){

 #check that landscape is an sf object
  if (class(landscape)[1] != "sf") stop("The landscape input must be an [sf] object")

  #check that the landscape is projected
  if(is.na(sf::st_crs(landscape)$epsg) | is.na(sf::st_crs(landscape)$proj4string)) stop("The landscape object is not projected; check [st_crs]")

  # check year == 1990 or year == 2000 or year == 2010
    # load the tiger spatial data for counties
    # reproject county to Albers
  if(year == 1990) {
    if(!exists("county1990low")) { #check to see if data are already in memory
      cty<-tigris::counties(cb=TRUE, year = 1990) #get the data 
      cty<-sf::st_transform(sf::st_as_sf(cty), "+init=ESRI:102008") #convert to sf & reproject to Albers
      assign("county1990low", cty, envir=globalenv()) #save cty file to memory in case so it cdfsaan be reused
    }
      county<-county1990low 
    } else if(year == 2000) {
        if(!exists("county2000low")) {
          cty<-tigris::counties(cb=TRUE, year = 2000) 
          cty<-sf::st_transform(sf::st_as_sf(cty), "+init=ESRI:102008")
          assign("county2000low", cty, envir=globalenv())
        }
          county<-county2000low
    } else if(year == 2010) {
            if(!exists("county2010low")) {
              cty<-tigris::counties(cb=TRUE, year = 2010) 
              cty<-sf::st_transform(sf::st_as_sf(cty), "+init=ESRI:102008")
              assign("county2010low", cty, envir=globalenv())
            }
            county<-county2010low
    } else {
        stop("Year must == 1990, 2000 or 2010")
  }

  #reproject input landscape to albers (even if already in albers: just as fast)
    landscape<-sf::st_transform(landscape, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)

  #get counties that landscape intersects
    fips<-as.data.frame(dplyr::slice(county, unlist(sf::st_intersects(landscape, county))))
    
  #check that fips has data.  If nrow == 0 then input polygon does not overlap tiger files (i.e. not in US) or CRS wrong.
    if (nrow(fips) == 0) stop("The input polygon does not overlap USA Census boundaries or projection information incorrect")
    
  #rename fields
    if(year == 1990) {
      fips<-dplyr::select(fips, state = ST, county = CO)
    } else {
      fips<-dplyr::select(fips, state = STATE, county = COUNTY) 
    }
    
  #create fips string for get_census
    fips$regionin<-paste("state:", fips$county, "+county:", fips$state, sep = "")

  return(fips)
}

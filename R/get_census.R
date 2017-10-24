#' get_census
#'
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon and returns the total population for that area for a given USA decadal census.
#' The input polygon must be projected and must overlap a USA census area
#' Data are returned for years 1990, 2000, or 2010; The default is 2010
#'
#' @param landscape A spatial polygon of class "sf" (see package [sf]). Note: the object must be projected (i.e. has a coordinate reference system and must overlap a USA census area)
#' @param year Which census year do you want to use? In format YYYY.  Currently the package supports census years 1990, 2000, and 2010.
#' @param spatial keep the spatial data? Default = FALSE
#' @param api_key #Your Census API key. Obtain one at http://api.census.gov/data/key_signup.html
#'
#' @export
#' @examples
#' get_census()
get_census <- function(landscape, year = 2010, spatial = FALSE,
                     api_key = Sys.getenv("CENSUS_API_KEY")){

# check for census api key (modified from tidycensus::get_decennial)
  if (Sys.getenv("CENSUS_API_KEY") != "") {
    api_key<-Sys.getenv("CENSUS_API_KEY")
  } else if (is.null(api_key)) {
    stop("A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html.")
  }
  
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
  fips$regionin<-paste("state:", fips$state, "+county:", fips$county, sep = "")
  
#set up censusapi and tigris::blocks calls
  if(year == 1990) {
    name<-"sf3"
    vars<-"POP100"
    region<-"block group:*"
    func<-tigris::block_groups
    cb<-TRUE  #download the generalized block_groups
  } else if(year == 2000) {
      name<-"sf3"
      vars<-"P001001"
      region<-"block group:*"
      func<-tigris::block_groups
      cb<-FALSE  #download the detailed block_groups
  } else {
      name<-"sf1"
      vars<-"P0010001"
      region<-"block:*"
      func<-tigris::blocks
      cb<-FALSE  #download the detailed blocks
  }
  
#get the census data for total pop
  pop<-list()
  for(i in c(1:nrow(fips))){
    pop[[i]]<-censusapi::getCensus(name = name, vintage = year, vars = vars, region = region, regionin=fips$regionin[i])
  }

#rbind the pop data
  tot_pop<-pop[[1]]
    if(length(pop)>1) for(i in c(2:length(pop))) tot_pop<-rbind(tot_pop, pop[[i]])  
  names(tot_pop)[5]<-'tot_pop'
  
#get the census blocks/block_groups for state(s) and county(s)-each year is slightly different
  #year = 1990
  if(year == 1990) {
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-sf::st_as_sf(tigris::block_groups(state = as.numeric(fips$state[i]), 
                                          county = as.numeric(fips$county[i]), 
                                          year = year, cb = TRUE))
      blk[[i]]<-dplyr::select(blk[[i]], state = ST, county = CO, tract = TRACT, block.group = BG, 
                              area_land = AREALAND, area_water = AREAWAT, geometry)
    }
  } 
  
  #year = 2000
  if(year == 2000) {
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-sf::st_as_sf(tigris::block_groups(state = as.numeric(fips$state[i]), 
                                                  county = as.numeric(fips$county[i]), 
                                                  year = year, cb = FALSE))
      blk[[i]]<-dplyr::select(blk[[i]], state = STATEFP, county = COUNTYFP, tract = TRACTCE00, block.group = BLKGRPCE00, 
                              area_land = ALAND00, area_water = AWATER00, geometry)
    }
  } 
  
  #year = 2010
  if(year == 2010) {
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-sf::st_as_sf(tigris::blocks(state = as.numeric(fips$state[i]), 
                                                  county = as.numeric(fips$county[i]), year = year))
      blk[[i]]<-dplyr::select(blk[[i]], state = STATEFP, county = COUNTYFP, tract = TRACTCE10, block = BLOCKCE10, 
                              area_land = ALAND10, area_water = AWATER10, geometry)
    }
  } 

#rbind the blk
  blocks<-blk[[1]]
    if(length(blk)>1) for(i in c(2:length(blk))) blocks<-rbind(blocks,blk[[i]])
  
#convert area_land and area_water to numeric
  blocks$area_land<-as.numeric(blocks$area_land)
  blocks$area_water<-as.numeric(blocks$area_water)
  
#join blocks and tot_pop
  dim(blocks)
  dim(tot_pop)
  blocks<-dplyr::left_join(blocks, tot_pop)
  
#reproject blocks
  blocks<-sf::st_transform(blocks,"+init=ESRI:102008")
    
#calc area of blocks-add to attributes
  blocks$blk_area<-as.numeric(sf::st_area(blocks))
  
#create intersection of landscape and blocks for export
  int<-sf::st_intersection(landscape,blocks)
  
#calc area of intersection polygons
  int$int_area<-as.numeric(sf::st_area(int))
  
#calc proportion of original block included in int polygons
  int$pro_blk<-round(int$int_area / int$blk_area, 4)
  
#calc proportion of original landscape included in intersection
  #note: landscapes that cross international borders or have missing data will have pro_ls < 1
  int$pro_ls<-round(sum(int$int_area) / as.numeric(sf::st_area(landscape)), 2)
    
#if(spatial==False) convert int to data.frame for output
    if(!spatial) out<-as.data.frame(int)
    
    #return the output
    return(int)
}


#head(get_census(lake, 1990))  
    
    
 
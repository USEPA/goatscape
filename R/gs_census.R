#' gs_census
#'
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon and returns the total population for that area for a given USA decadal census.
#' The input polygon must be projected and must overlap a USA census area
#' Data are returned for years 2000, or 2010; The default is 2010
#' For 2000 only the census block group data are available.  For 2010 the block level information
#' is available but can take a long time to download for larger polygons.  By default the function
#' downloads the block group level for 2010 but you can request the more detailed block data.
#' NOTE: highly recommended that you load the [tigris] package and set [options(tigris_use_cache = TRUE)]
#'
#' @param landscape A spatial polygon of class "sf" (see package [sf]). Note: the object must be projected (i.e. has a coordinate reference system) and must overlap a USA census area.
#' @param year Which census year do you want to use? In format YYYY.  Currently the package supports census years 2000, 
#' and 2010 (default).
#' @param spatial keep the spatial data? Default = TRUE
#' @param level Choose the census reporting level: "block_group" (default) or "block".  NOTE: for year = 2000 only 
#' "block_group" level is available; Both levels available for year = 2010. 
#' @param api_key Your Census API key. Obtain one at http://api.census.gov/data/key_signup.html; For more information see: https://hrecht.github.io/censusapi/index.html#api-key-setup.
#'
#' @export
#' @examples
#' # NOTE: this function requires a Census API key. Obtain one at http://api.census.gov/data/key_signup.html
#' # For more information see: https://hrecht.github.io/censusapi/index.html#api-key-setup
#' 
#' gs_census(buf20, year = 2010, spatial = TRUE, level = "block_group")

gs_census<-function(landscape, year = 2010, spatial = TRUE, level = "block_group",
                     api_key = Sys.getenv("CENSUS_API_KEY")){

# check for census api key (modified from tidycensus::get_decennial)
  if (Sys.getenv("CENSUS_API_KEY") != "") {
    api_key<-Sys.getenv("CENSUS_API_KEY")
  } else if (is.null(api_key)) {
    stop("A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html. 
         For more information see: https://hrecht.github.io/censusapi/index.html#api-key-setup")
  }
  
# check that level = "block_group" for year == 2000
  if (year==2000 & level == "block") stop("Block level data not available for 2000 use: level = 'block_group'")
    
#check that landscape is an sf polygon object
  if (class(landscape)[1] != 'sf') stop("The landscape input must be an [sf] polygon object")
  
#check that the landscape is projected
  if(is.na(sf::st_crs(landscape)$epsg) & is.na(sf::st_crs(landscape)$proj4string)) stop("The landscape object is not projected; check [st_crs]")

# check year year == 2000 or year == 2010
  # load the tiger spatial data for counties
  # reproject county to Albers
  if(year == 2000) {
    if(!exists("county2000low")) {
      cty<-tigris::counties(cb=TRUE, year = 2000, class = 'sf') 
      cty<-sf::st_transform(cty, crs_alb)
      assign("county2000low", cty, envir=globalenv())
    }
    county<-county2000low
  } else if(year == 2010) {
    if(!exists("county2010low")) {
      cty<-tigris::counties(cb=TRUE, year = 2010, class = 'sf') 
      cty<-sf::st_transform(cty, crs_alb)
      assign("county2010low", cty, envir=globalenv())
    }
    county<-county2010low
  } else {
    stop("Year must == 2000 or 2010")
  }
  
#save input crs for output
  input_crs<-sf::st_crs(landscape)
  
#reproject input landscape to albers (even if already in albers: just as fast)
  landscape<-sf::st_transform(landscape, crs_alb) #reproject to the Albers Equal Area Conic projection used by NLCD
  
#get counties that landscape intersects
  fips<-as.data.frame(dplyr::slice(county, unlist(sf::st_intersects(landscape, county))))
  
#check that fips has data.  If nrow == 0 then input polygon does not overlap tiger files (i.e. not in US) or CRS wrong.
  if (nrow(fips) == 0) stop("The input polygon does not overlap USA Census boundaries or projection information incorrect")
  
#rename fields
  fips<-dplyr::select(fips, state = STATE, county = COUNTY) 

#create fips string for gs_census
  fips$regionin<-paste("state:", fips$state, "+county:", fips$county, sep = "")
  
#set up censusapi and tigris calls
  if(year == 2000) {
      name<-"sf1"
      vars<-"P001001"
  }
  
  if(year == 2010) {    
      name<-"sf1"
      vars<-"P0010001"
  }

  if(level == "block") region <- "block:*"
  
  if(level == "block_group") region <- "block group:*"
  
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
  #year = 2000
  if(year == 2000) {
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-tigris::block_groups(state = as.numeric(fips$state[i]), 
                                                  county = as.numeric(fips$county[i]),year = year, 
                                                  class='sf')
      blk[[i]]<-dplyr::select(blk[[i]], state = STATEFP, county = COUNTYFP, tract = TRACTCE00, block.group = BLKGRPCE00, 
                              area_land = ALAND00, area_water = AWATER00, geometry)
    }
  } 
  
#year == 2010 & level == 'block_group'
  if(year == 2010 & level == 'block_group') {
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-tigris::block_groups(state = as.numeric(fips$state[i]), 
                                     county = as.numeric(fips$county[i]), year = year, 
                                     class='sf')
      blk[[i]]<-dplyr::select(blk[[i]], state = STATEFP, county = COUNTYFP, tract = TRACTCE10, block.group = BLKGRPCE10, 
                              area_land = ALAND10, area_water = AWATER10, geometry)
    }
  } 

#year == 2010 & level == 'block'
  if(year == 2010 & level == 'block') {
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-tigris::blocks(state = as.numeric(fips$state[i]), 
                                            county = as.numeric(fips$county[i]), year = year, 
                                            class='sf')
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
  blocks<-merge(blocks, tot_pop, by=names(blocks)[1:4], all.x=TRUE)
  
#reproject blocks
  blocks<-sf::st_transform(blocks, crs_alb)
    
#calc area of blocks-add to attributes
  blocks$blk_area<-as.numeric(sf::st_area(blocks))
  
#create intersection of landscape and blocks 
  int<-sf::st_intersection(landscape,blocks)
  
#calc area of intersection polygons
  int$int_area<-as.numeric(sf::st_area(int))
  
#calc proportion of original block included in int polygons
  int$pro_blk<-round(int$int_area / int$blk_area, 4)
  
#calc proportional population by block/block_group
  int$pro_pop<-round(int$tot_pop * int$pro_blk, 2)
  
#prepare output list
  out<-list()
  
#if spatial = TRUE transform intersection to input_crs and add
  if(spatial) {
    out$landscape<-sf::st_transform(landscape, input_crs)
    out$census_raw<-sf::st_transform(blocks, input_crs)
    out$census_data<-sf::st_transform(int, input_crs)
  } else {
    out$census_data<-as.data.frame(int)
  }
  
#calc estimated pop for the input landscape
  out$est_population<-round(sum(int$pro_pop), 2)
  
#calc input area in m2
  out$input_area_km2<-round(as.numeric(sf::st_area(landscape) / 1000000), 2)
  
#calc pop density
  out$est_pop_per_km2<-round(out$est_population / out$input_area_km2, 1)
  
#percent overlap between input landscape and census area
  out$percent_overlap<-round(sum(int$int_area) / as.numeric(sf::st_area(landscape)) * 100, 1)
  
#add overlap warning
  if(out$percent_overlap !=100) {
    out$warning<-"percent_overlap not equal 100%; input landscape partially outside USA census boundaries" 
    warning('percent_overlap not equal 100%; input landscape partially outside USA census boundaries')
  } 

  #return the output
  return(out)
}



    
    
 
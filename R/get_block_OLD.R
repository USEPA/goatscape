#' get_fips
#'
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon.returns the total population for that area.
#' Currently the function returns total population for the 2000 and 2010 censuses.
#' Additional years may be added later.
#'
#' @param landscape A spatial polygon of class "sf" (see package [sf]). Note: the object must be projected (i.e. has a coordinate reference system)
#' @param year Which census year do you want to use? In format YYYY.  Currently the package supports census years 2000 and 2010.
#' @param spatial keep the spatial data? Default = FALSE
#'
#' @export
#' @examples
#' get_census()
get_block <- function(landscape, year = 2010, spatial = FALSE){

 #check that landscape is an sf object
  if (class(landscape)[1] != "sf") stop("The landscape input must be an [sf] object")

  #check that the landscape is projected
  if(is.na(sf::st_crs(landscape)$epsg) | is.na(sf::st_crs(landscape)$proj4string)) stop("The landscape object is not projected; check [st_crs]")

  #check year == 2000 or year == 2010
  if(year != 2000 & year != 2010) stop("Year must == 2000 or 2010")

  # load the tiger spatial data for counties
  if(year == 2000) {
    if(!exists("tiger2000_county20m")) { 
      load(here::here('data/tiger2000_county20m.rda'), envir = globalenv())
    }
    county<-tiger2000_county20m
  } else {
    if(!exists("tiger2010_county20m")) { 
      load(here::here('data/tiger2010_county20m.rda'), envir = globalenv())
    }
    county<-tiger2010_county20m
  }

  #reproject input landscape to albers (even if already in albers: just as fast)
    landscape<-sf::st_transform(landscape, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)

  #calc area of landscape-add to attributes
    landscape$ls_area<-sum(as.numeric(sf::st_area(landscape)), na.rm=TRUE)

    #get counties that lake intersects
    fips<-as.data.frame(dplyr::slice(county, unlist(sf::st_intersects(landscape, county))))

  #get the census blocks for state(s) and county(s)
    blk<-list()
    for(i in c(1:nrow(fips))){
      blk[[i]]<-sf::st_as_sf(tigris::blocks(state=as.numeric(fips$STATEFP[i]),county=as.numeric(fips$COUNTYFP[i]),year=year))
    }

    #rbind the blocks
    blocks<-blk[[1]]
    if(length(blk)>1) for(i in c(2:length(blk))) blocks<-rbind(blocks,blk[[i]])

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
    int$pro_ls<-round(sum(int$int_area) / int$ls_area[1], 2)


  # create output sf object "out"
    #select and rename attribute names
    #convert to data.frame select and rename
      if(year==2000) out<-dplyr::select(as.data.frame(int), geoid = BLKIDFP00, aland = ALAND00,
                                       awater = AWATER00, ls_area, blk_area, int_area, pro_blk, pro_ls)
      if(year==2010) out<-dplyr::select(as.data.frame(int), geoid = GEOID10, aland = ALAND10,
                                       awater = AWATER10, ls_area, blk_area, int_area, pro_blk, pro_ls)

    #convert aland and awater to numeric
      out<-dplyr::mutate(out, aland=as.numeric(aland), awater=as.numeric(awater))

    #if(spatial==TRUE) recombine out and geom to create sf object "out"
      if(spatial) out<-sf::st_as_sf(cbind(out,sf::st_geometry(int)))

  #return the output
    return(out)
}

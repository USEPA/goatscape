#' gs_nlcd
#'
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon and returns the NLCD landscape, canopy, or impervious cover for the polygon.
#' The input polygon must be projected and must overlap a USA NLCD area
#' Data are returned for years 2001, 2006, or 2011; The default is 2011
#'
#' @param landscape A spatial polygon of class "sf" (see package [sf]). Note: the object must be projected (i.e. has a coordinate reference system and must overlap the NLCD area)
#' @param spatial keep the spatial data? Default = TRUE 
#' @inheritParams FedData::get_nlcd
#'
#' @export
#' @examples
#' gs_nlcd()
gs_nlcd<-function(landscape, spatial = TRUE, label, year = 2011, dataset = "landcover",  
                  raw.dir = "./RAW/NLCD", extraction.dir = paste0("./EXTRACTIONS/",
                  "/NLCD"), raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9",
                  "INTERLEAVE=BAND"), force.redo = F) {
  
# define albers projection to use for analysis
  crs_alb<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0
                +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# reproject landscape to the Albers projection used by NLCD
landscape_alb<-sf::st_transform(landscape, crs_alb)  

# for the FedData package need to convert to [sp]
landscape_alb<-sf::as_Spatial(sf::st_geometry(sf::st_zm(landscape_alb)))

# Get the NLCD 
nlcd <- FedData::get_nlcd(template = landscape_alb,
                          year = year,
                          dataset = dataset,  # could also be "canopy" or "impervious"
                          label = label)

# to limit the NLCD data to the input landscape [mask] is used to assign the values outside the landscape to NA
nlcd<-raster::mask(nlcd, landscape_alb)  

# calculate areas and proportions of each NLCD class 
nlcd_freq<-raster::freq(nlcd)

# remove the NA values and convert to df
nlcd_freq<-as.data.frame(nlcd_freq[!is.na(nlcd_freq[,1]),])

#calculate the proportions in each class
nlcd_freq$proportion<-round(nlcd_freq$count/sum(nlcd_freq$count),3)

# calculate the total area of each class based on 30m x 30m grid cell size
nlcd_freq$areaM2<-nlcd_freq$count * 30 * 30

#add the label
nlcd_freq$label<-label

# add NLCD descriptors and legend info for dataset == "landcover"
if(dataset == "landcover") { 
  load(here::here('data/nlcd_landuse_info.rda'))                                ############NOTE: use package data later
  nlcd_freq <- merge(nlcd_freq, nlcd_landuse_info, by = 'value', all.x = TRUE)
    #add the 
  nlcd@legend@values <- nlcd_landuse_info$value  
  nlcd@legend@names <- nlcd_landuse_info$description
}

#prepare output list
out<-list()

#if spatial = TRUE add spatial data to list "out"
if(spatial) {
  out$landscape <- landscape
  out$landscape_alb <- sf::st_as_sf(landscape_alb)
  out$nlcd <- nlcd
} 

# add the nlcd_freq data to out
out$nlcd_freq <- nlcd_freq

# percent overlap between input landscape and census area

# get the area of the landscape
out$percent_overlap<-round(100 * sum(nlcd_freq$areaM2, na.rm = TRUE) / as.numeric(sf::st_area(out$landscape)), 1)

#add overlap warning
if(out$percent_overlap !=100) out$warning<-"percent_overlap not equal 100%; small differences may be due to intersecting landscape with NLCD raster; large difference suggest input landscape partially outside USA NLCD boundaries"



###########dataset == 'landscape'

# finally, add the labels
lulc<-merge(nlcd_info,lulc,by.x='code',by.y='value',all.y=TRUE)



#update legend info for nlcd landuse raster
# slotNames(nlcd)
# slotNames(nlcd@legend)
load(here::here('data/nlcd_info.rda'))      #############NOTE: change this to package data
nlcd@legend@values <- nlcd_info$code  
nlcd@legend@names <- nlcd_info$label

#format output
#if spatial = TRUE return list with raster "nlcd" and d.f. "lulc"
if(spatial) {
  out<-list()
  out$nlcd<-nlcd
  out$landscape<-landscape
  out$lulc<-lulc
} else {
  out<-lulc
}

return(out)
}

############TODO
# determine percent overlap of landscape and NLCD data
  # for the impervious this should be the NA values
#  Fix the ?gs_nlcd add #' @inheritParams FedData::get_nlcd

#' gs_nlcd
#'
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon and returns the NLCD landscape, canopy, or impervious cover for the polygon.
#' The input polygon must be projected and must overlap a USA NLCD area
#' Data are returned for years 2001, 2006, or 2011; The default is 2011
#'
#' @param landscape A spatial polygon of class "sf" (see package [sf]) to serve as a template for cropping the NLCD tile(s). Note: the object must be projected (i.e. has a coordinate reference system and must overlap the NLCD area).
#' @param spatial keep the spatial data? Default = TRUE. 
#' @param label A character string naming the study area. If no label is supplied the function will use the name of the input landscape as the label.  A unique label for each input landscape is required if force.redo = FALSE.
#' @param year An integer representing the year of desired NLCD product. Acceptable values are 2011 (default), 2006, and 2001.
#' @param dataset A character string representing type of the NLCD product. Acceptable values are landcover' (default), 'impervious', and 'canopy'. NOTE: canopy data not currently available for year = 2006 and sometimes year = 2001; appears to work for year = 2011.
#' @param raw.dir	A character string indicating where raw downloaded files should be put. The directory will be created if missing. Defaults to './nlcd_data/raw/'.
#' @param extraction.dir	A character string indicating where the extracted and cropped NLCD tile should be put. The directory will be created if missing. Defaults to './nlcd_data/cropped/'.
#' @param raster.options	A vector of options for raster::writeRaster (see also FedData::get_nlcd).
#' @param force.redo	If an extraction for this template and label already exists, should a new one be created?
#'
#' @export
#' @examples
#' gs_nlcd()
gs_nlcd<-function(landscape, spatial = TRUE, label = NA, year = 2011, dataset = "landcover", force.redo = TRUE, 
                  raw.dir = "./nlcd_data/raw", extraction.dir = "./nlcd_data/cropped", 
                  raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9")) {
  
#check that landscape is an sf polygon object
if (class(landscape)[1] != 'sf') stop("The landscape input must be an [sf] polygon object")
  
#check that the landscape is projected
if(is.na(sf::st_crs(landscape)$epsg) & is.na(sf::st_crs(landscape)$proj4string)) stop("The landscape object is not projected; check [st_crs]")
  
# if is.na(label) set label to the name of the input landscape
  if(is.na(label)) label <- deparse(substitute(landscape))
  
#save input crs for output
input_crs<-sf::st_crs(landscape)
  
#reproject input landscape to albers (even if already in albers: just as fast)
landscape<-sf::st_transform(landscape, crs_alb) #reproject to the Albers Equal Area Conic projection used by NLCD
  
# for the FedData package need to convert to [sp]
landscape_alb<-sf::as_Spatial(sf::st_geometry(sf::st_zm(landscape)))

# Get the NLCD 
nlcd <- tryCatch(FedData::get_nlcd(template = landscape_alb,
                          year = year,
                          dataset = dataset,  
                          label = label, 
                          raw.dir = raw.dir,
                          extraction.dir = extraction.dir,
                          raster.options = raster.options,
                          force.redo = force.redo), 
                            error = function(e) stop("No data returned by NLCD; Check that input landscape is correctly projected and intersects the coterminous United States; try again using a new, unique label."))

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

#prepare output list
out<-list()

#if spatial = TRUE add spatial data to list "out"
if(spatial) {
  out$landscape <- landscape
  out$landscape_alb <- sf::st_as_sf(landscape_alb)
  out$nlcd <- nlcd
} 

# prepare output for for dataset == "canopy"
if(dataset == "canopy") { 
  tbl_nlcd <- data.frame(value = c(0:100), hex = nlcd@legend@colortable[1:101])
  tbl_nlcd$description <- c("no canopy / missing value", rep("%impervious", 100))
    
  #add the legend info to raster
  nlcd@legend@values <- tbl_nlcd$value 
  nlcd@legend@names <- tbl_nlcd$description
  nlcd@legend@color <- tbl_nlcd$hex
    
  # merge nlcd_freq & tbl_nlcd; rename nlcd_freq$value
  nlcd_freq <- merge(nlcd_freq, tbl_nlcd, by = 'value', all.x = TRUE)
  names(nlcd_freq)[1] <- 'percent_canopy'
  
  # calc total percents and areas for canopy and percent overlap
  out$total_percent_canopy <- round(sum(nlcd_freq$percent_canopy * nlcd_freq$count) / sum(nlcd_freq$count), 1)
  out$total_areaM2_canopy <- round(sum(0.01 * nlcd_freq$percent_canopy * nlcd_freq$areaM2), 1)
  out$percent_overlap <- NA
  out$warning <- "For the NLCD Canopy dataset missing values are scored as zeros so we cannot estimate percent_overlap; if you suspect that the input polygon might not be compeletly contained within the NLCD footprint rerun with dataset = 'impervious' or dataset = 'landcover'."
}

# prepare output for for dataset == "impervious"
if(dataset == "impervious") { 
  tbl_nlcd <- data.frame(value = c(0:100, 127), hex = nlcd@legend@colortable[c(1:101, 127)])
  tbl_nlcd$description <- c(rep("%impervious", 101), "missing_value")
  
  #add the legend info to raster
  nlcd@legend@values <- tbl_nlcd$value 
  nlcd@legend@names <- tbl_nlcd$description
  nlcd@legend@color <- tbl_nlcd$hex
  
  # merge nlcd_freq & tbl_nlcd; rename nlcd_freq$value
  nlcd_freq <- merge(nlcd_freq, tbl_nlcd, by = 'value', all.x = TRUE)
  names(nlcd_freq)[1] <- 'percent_impervious'
  nlcd_freq$percent_impervious[nlcd_freq$percent_impervious ==  127] <- NA
  
  # calc total percents and areas for canopy and percent overlap
  out$total_percent_impervious <- round(sum(nlcd_freq$percent_impervious * nlcd_freq$count) / sum(nlcd_freq$count), 1)
  out$total_areaM2_impervious <- round(sum(0.01 * nlcd_freq$percent_impervious * nlcd_freq$areaM2), 1)
  out$percent_overlap <- ifelse(any(is.na(nlcd_freq$percent_impervious)), 
                                round(100 * sum(nlcd_freq$count[is.na(nlcd_freq$percent_impervious)]) / sum(nlcd_freq$count), 1),
                                100)
  if(out$percent_overlap !=100) out$warning <- "If percent_overlap not equal 100%; input landscape may be partially outside USA NLCD boundaries"
}

# prepare output for for dataset == "landcover"
if(dataset == "landcover") { 
  load(here::here('data/nlcd_landuse_info.rda')) 
  
  #add the legend info to raster
  nlcd@legend@values <- nlcd_landuse_info$value 
  nlcd@legend@names <- nlcd_landuse_info$description
  nlcd@legend@color <- nlcd_landuse_info$hex
  
  # merge nlcd_freq & nlcd_landuse_info; rename nlcd_freq$value
  nlcd_freq <- merge(nlcd_freq, nlcd_landuse_info, by = 'value', all.x = TRUE)
  names(nlcd_freq)[1] <- 'landcover_class'
  
  # calc percent overlap
  out$percent_overlap <- ifelse(min(nlcd_freq$landcover_class) == 0, 100 - round(100 * nlcd_freq$proportion[1], 1), 100)
  if(out$percent_overlap !=100) out$warning <- "If percent_overlap not equal 100%; input landscape may be partially outside USA NLCD boundaries"
}

# add the nlcd_freq data to out
out$nlcd_freq <- nlcd_freq

return(out)
} 














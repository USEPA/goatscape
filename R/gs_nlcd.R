#' gs_nlcd
#'
#' This function takes an input landscape, a user-defined land area as an
#' [sf] polygon and returns the NLCD landscape, canopy, or impervious cover for the polygon.
#' The input polygon must be projected and must overlap a USA NLCD area
#' Data are returned for years 2001, 2006, or 2011; The default is 2011
#'
#' @param landscape A spatial polygon of class "sf" (see package [sf]). Note: the object must be projected (i.e. has a coordinate reference system and must overlap the NLCD area)
#' @param label A character string naming the study area.
#' @param dataset	A character string representing type of the NLCD product. Acceptable values are landcover' (default), 'impervious', and 'canopy'.
#' @param year An integer representing the year of desired NLCD product. Acceptable values are 2011 (default), 2006, and 2001.
#' @param dataset for 2000 level == "block_group" for 2010 choose level = "block" or level = "block_group"
#' @param spatial keep the spatial data? Default = TRUE
#' @param raw.dir	A character string indicating where raw downloaded files should be put. The directory will be created if missing. Defaults to './RAW/NED/'.
#' @param extraction.dir	A character string indicating where the extracted and cropped DEM should be put. The directory will be created if missing. Defaults to './EXTRACTIONS/NED/'.
#' @paramraster.options A vector of options for raster::writeRaster.
#' @param force.redo If an extraction for this template and label already exists, should a new one be created?
#'
#' @export
#' @examples
#' gs_nlcd()
gs_nlcd<-function(landscape, label, year = 2011, dataset = "landcover", spatial = TRUE, 
                  raw.dir = "./RAW/NLCD", extraction.dir = paste0("./EXTRACTIONS/", label,
                  "/NLCD"), raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9",
                  "INTERLEAVE=BAND"), force.redo = F) {

# reproject landscape to the Albers projection used by NLCD
landscape<-sf::st_transform(landscape, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 
                                    +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")  

# for the FedData package need to convert to [sp]
landscape <- sf::as_Spatial(landscape$geom) # This works

# Get the NLCD 
nlcd <- FedData::get_nlcd(template = landscape,
                          year = year,
                          dataset = dataset,  # could also be "canopy" or "impervious"
                          label = label)

# to limit the NLCD data to the input landscape [mask] is used to assign the values outside the landscape to NA
nlcd<-mask(nlcd, landscape)  

#update non-NULL colors and legend info for nlcd raster
# slotNames(nlcd)
# slotNames(nlcd@legend)
load(here::here('data/nlcd_colors.rda'))
load(here::here('data/nlcd_info.rda'))
nlcd@legend@values <- nlcd_info$code  
nlcd@legend@colortable <- nlcd_colors
nlcd@legend@names <- nlcd_info$description

# calculate areas and proportions of each LULC 
lulc<-freq(nlcd)

# the NA values
lulc<-as.data.frame(lulc[!is.na(lulc[,1]),])

#calculate the proportions in each class
lulc$proportion<-round(lulc$count/sum(lulc$count),3)

# calculate the total area of each class based on 30m x 30m grid cell size
lulc$areaM2<-lulc$count*30*30

#add the label
lulc$label<-label

# finally, add the labels
lulc<-merge(nlcd_info,lulc,by.x='code',by.y='value',all.y=TRUE)

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
# mixed forest and evergreen forest have same hex code.  Some hex codes difficult to distinguish

##########Test
a<-gs_nlcd(landscape, label, year, dataset, spatial=T)  #see below

plot(a$nlcd)
  legend('topright',nlcd_info$description,fill=nlcd_info$hex)
a$lulc


#get nla lakes
lake<-sf::st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')

# drop z-dimension
lake<-sf::st_zm(lake)

# get the albers projection used by NLCD
load(here::here('data/crs_alb.rda'))

# reproject
lake<-sf::st_transform(lake, crs_alb)

#select random lake
i<-sample(1:1159,1) #i = 610 is strange
lake1<-lake[i,]



# add buffer
landscape<-sf::st_difference(sf::st_buffer(lake1, 5000), lake1)

#other input
label<-as.character(landscape$SITEID)
year<-2011
dataset<-"landcover"

# get Jeff's NLCD lookup table      %%%%%%%%%%%%%%%%%%%%%%%%% NOTE: this needs to be added to the package data
ct<-read.csv(url('https://raw.githubusercontent.com/jhollist/miscPackage/master/inst/extdata/nlcd_lookup.csv'), stringsAsFactors = FALSE)

#add colors 1:256
ctbl <- rep("#000000", 256)

#update non-NULL colors
# slotNames(nlcd)
# slotNames(nlcd@legend)
ctbl[ct$code + 1] <- ct$hex 
nlcd@legend@values <- ct$code  
nlcd@legend@colortable <- ctbl
nlcd@legend@names <- ct$label

# calculate areas and proportions of each LULC 
lulc<-raster::freq(nlcd)

# the NA values
lulc<-as.data.frame(lulc[!is.na(lulc[,1]),])

#calculate the proportions in each class
lulc$proportion<-round(lulc$count/sum(lulc$count),3)

# calculate the total area of each class based on 30m x 30m grid cell size
lulc$areaM2<-lulc$count*30*30

# finally, add the labels
lulc<-merge(ct,lulc,by.x='code',by.y='value',all.y=TRUE)

# what a surprise, it is mostly developed land around the hotel
lulc









library(sf)
library(raster)
library(FedData)
library("SDMTools")

# get the albers projection used by NLCD
load(here::here('data/crs_alb.rda'))

#get nla lakes
lake<-st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')

# drop z-dimension
lake<-st_zm(lake)

# reproject
lake<-st_transform(lake, crs_alb)

#select random lake
i<-sample(1:1159,1)
lake1<-lake[i,]

# add buffer
buf<-st_difference(st_buffer(lake1, 5000), lake1)

# visualize whirled peas
plot(buf[1], col='red')  
plot(lake1[1], col='purple', add=T)  

# for the FedData package need to convert to [sp]
buf_sp <- sf::as_Spatial(buf$geom) # This works

# Get the NLCD 
nlcd <- FedData::get_nlcd(template = buf_sp,
                          year = 2011,
                          dataset = "landcover",  # could also be "canopy" or "impervious"
                          label = buf$SITEID)
# Plot with raster::plot
plot(nlcd)
plot(buf_sp, add = TRUE, lwd=2)  # sp 

plot(buf, add = TRUE)  # sf


# http://jwhollister.com/r_landscape_tutorial/tutorial.html
# http://jwhollister.com/iale_open_science/2015/07/05/03-Spatial-Data-In-R/

# get Jeff's NLCD lookup table 
ct<-read.csv(url('https://raw.githubusercontent.com/jhollist/miscPackage/master/inst/extdata/nlcd_lookup.csv'), stringsAsFactors = FALSE)

#add colors 1:256
ctbl <- rep("#000000", 256)

#update non-NULL colors
# slotNames(nlcd)
# slotNames(nlcd@legend)
ctbl[ct$code + 1] <- ct$hex 
nlcd@legend@values <- ct$code  
nlcd@legend@colortable <- ctbl
nlcd@legend@names <- ct$label


# to limit the NLCD data to the lake buffer we change use a mask to assign the values outside the buffer to NA (missing)
buf_nlcd<-mask(nlcd, buf_sp)

plot(buf_nlcd)


# caculate the area of each cover type in m2 (cell size = 30 m) 
table(values(buf_nlcd))*30*30  #simple

#more complete
nlcd_class_metrics <- ClassStat(mat = buf_nlcd, cellsize = 30)
dplyr::tbl_df(nlcd_class_metrics)


#Now calcualte total proportion of each LULC and save to a data.frame
lulc<-freq(bufNLCD)

#clean it up by removing the NA values
lulc<-as.data.frame(lulc[!is.na(lulc[,1]),])

#calculate the proportions in each class
lulc$proportion<-round(lulc$count/sum(lulc$count),3)

# calculate the total area of each class based on 30m x 30m grid cell size
lulc$areaM2<-lulc$count*30*30

# finally, add the labels
lulc<-merge(ct,lulc,by.x='code',by.y='value',all.y=TRUE)

# what a surprise, it is mostly developed land around the hotel
lulc





























####################


# add buffer
buf<-st_difference(st_buffer(lake1, 5000), lake1)

# visualize whirled peas
plot(buf[1], col='red')  
plot(lake1[1], col='purple', add=T)  

# for the FedData package need to convert to [sp]
buf_sp <- sf:::as_Spatial(buf$geom) # This works






# FedData Tester
library(FedData)
library(magrittr)

# Extract data for the Village Ecodynamics Project "VEPIIN" study area:
# http://veparchaeology.org
vepPolygon <- polygon_from_extent(raster::extent(672800, 740000, 4102000, 4170000),
                                  proj4string = "+proj=utm +datum=NAD83 +zone=12")

NLCD <- get_nlcd(template = vepPolygon,
                 year = 2011,
                 dataset = "landcover",
                 label = "VEPIIN")
# Plot with raster::plot
raster::plot(NLCD)


a<-st_transform(buf, "+proj=utm +datum=NAD83 +zone=12")
b<- sf:::as_Spatial(a$geom)

d <- FedData::get_nlcd(template = b,
                           year = 2011,
                           dataset = "landcover",  # could also be "canopy" or "impervious"
                           label = "test")

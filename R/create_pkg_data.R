# #create_pkg_data


# define the albers projection to use for the package
crs_alb<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
devtools::use_data(crs_alb, compress = "bzip2", overwrite = TRUE)

## some test polygons

# lake20
lakes <- sf::st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp') #get nla lakes
lakes<-sf::st_transform(lakes, crs_alb) #convert to albers
lake20 <- lakes[which(lakes$SITEID == 'NLA06608-0020'),] 
devtools::use_data(lake20, compress = "bzip2", overwrite = TRUE)

# buf20
buf20 <- sf::st_difference(sf::st_buffer(lake20, 5000), sf::st_geometry(lake20))
devtools::use_data(buf20, compress = "bzip2", overwrite = TRUE)

# buf20merc  add a mercator projected poly
merc<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
buf20merc <- sf::st_transform(buf20, merc)
devtools::use_data(buf20merc, compress = "bzip2", overwrite = TRUE)

# select a border lake from Jeff's lake morpho and add a buffer-this lake & buffer is part in the USA and part in Canada
border<-sf::st_read(dsn='Y:/data/lakeMorphometry/LakeMorphGdb.gdb', layer = "NorthEast01")
border<-sf::st_as_sf(dplyr::filter(border, COMID==166196269))
border<-sf::st_buffer(border, 5000)
border<-sf::st_transform(border, crs_alb) #convert to albers
devtools::use_data(border, compress = "bzip2", overwrite = TRUE)

#  polygon out of range (in canada)
oh_canada<-sf::st_read(here::here('temp/oh_canada.kml'))
devtools::use_data(oh_canada, compress = "bzip2", overwrite = TRUE)

# nlcd_landuse_info: lookup table for  nlcd codes, labels, and hex
  # modified from 'https://raw.githubusercontent.com/jhollist/miscPackage/master/inst/extdata/nlcd_lookup.csv'
nlcd_landuse_info<-read.csv(here::here('temp/nlcd_landuse_info.csv'))
devtools::use_data(nlcd_landuse_info, compress = "bzip2", overwrite = TRUE)


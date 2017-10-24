#create_pkg_data


#create_pkg_data

#counties download for year=1990
  #Note: this is for the low resolution data.  Use:  counties(resolution = "20m", year = 2000) for high res output.
county1990low<-tigris::counties(cb=TRUE, year = 1990) #get the data
county1990low<-sf::st_as_sf(county1990low) #convert to sf object
county1990low<-sf::st_transform(county1990low, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic

#counties download for year=2000
county2000low<-tigris::counties(cb=TRUE, year = 2000) #get the data
county2000low<-sf::st_as_sf(county2000low) #convert to sf object
county2000low<-sf::st_transform(county2000low, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic

#counties download for year=2010
county2010low<-tigris::counties(cb=TRUE, year = 2010) #get the data
county2010low<-sf::st_as_sf(county2010low) #convert to sf object
county2010low<-sf::st_transform(county2010low, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic


#compress and save data to /data
save(county1990low, file=here::here('data/county1990low.rda'))
save(county2000low, file=here::here('data/county2000low.rda'))
save(county2010low, file=here::here('data/county2010low.rda'))



#select a lake and create a buffer around it
#NLA06608-3890 #beach pond
#NLA06608-2162 #yawgoo pond
lake<-sf::st_read('C:/bryan/rscripts/lakescape/inst/extdata/nla_lakemorphometry.shp')
lake<-sf::st_as_sf(dplyr::filter(lake, nlaSITE_ID=='NLA06608-3890'))
lake<-sf::st_buffer(lake, 3000)

# select a border lake from Jeff's lake morpho and add a buffer
lake1<-sf::st_read(dsn='Y:/data/lakeMorphometry/LakeMorphGdb.gdb', layer = "NorthEast01") 
lake1<-sf::st_as_sf(dplyr::filter(lake1, COMID==166196269))
lake1<-sf::st_buffer(lake1, 3000)

#select a lake from the 2007 NLA shapefile-Texas is nice
lake2<-sf::st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')
lake2<-sf::st_as_sf(dplyr::filter(lake2, SITEID=='NLA06608-TX:14'))

#compress and save data to /data
devtools::use_data(lake, compress = "bzip2")
devtools::use_data(lake1, compress = "bzip2")
devtools::use_data(lake2, compress = "bzip2")


#  polygon out of range (in canada)
pol<-sf::st_read('C:/bryan/bryan_temp/tempPoly.kml')



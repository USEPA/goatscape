#create_pkg_data



#counties download for year=2000
tiger2000_county20m<-tigris::counties(resolution = "20m", year = 2000) #get the data
tiger2000_county20m<-sf::st_as_sf(tiger2000_county20m) #convert to sf object
tiger2000_county20m<-sf::st_transform(tiger2000_county20m, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)

#counties download for year=2010
tiger2010_county20m<-tigris::counties(resolution = "20m", year = 2010) #get the data
tiger2010_county20m<-sf::st_as_sf(tiger2010_county20m) #convert to sf object
tiger2010_county20m<-sf::st_transform(tiger2010_county20m, "+init=ESRI:102008") #reproject to North America Albers Equal Area Conic (ESRI:102008)

#compress and save data to /data
save(tiger2000_county20m, file=here::here('data/tiger2000_county20m.rda'))
save(tiger2010_county20m, file=here::here('data/tiger2010_county20m.rda'))
#devtools::use_data(tiger2000_county20m, compress = "bzip2")  #slightly larger than a straight "save"
#devtools::use_data(tiger2010_county20m, compress = "bzip2")



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



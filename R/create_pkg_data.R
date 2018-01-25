# #create_pkg_data
# #select a lake and create a buffer around it
# #NLA06608-3890 #beach pond
# #NLA06608-2162 #yawgoo pond
# lake<-sf::st_read('C:/bryan/rscripts/lakescape/inst/extdata/nla_lakemorphometry.shp')
# lake<-sf::st_as_sf(dplyr::filter(lake, nlaSITE_ID=='NLA06608-3890'))
# lake<-sf::st_buffer(lake, 3000)
# 
# # select a border lake from Jeff's lake morpho and add a buffer
# lake1<-sf::st_read(dsn='Y:/data/lakeMorphometry/LakeMorphGdb.gdb', layer = "NorthEast01") 
# lake1<-sf::st_as_sf(dplyr::filter(lake1, COMID==166196269))
# lake1<-sf::st_buffer(lake1, 3000)
# 
# #select a lake from the 2007 NLA shapefile-Texas is nice
# lake2<-sf::st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')
# lake2<-sf::st_as_sf(dplyr::filter(lake2, SITEID=='NLA06608-TX:14'))
# 
# #compress and save data to /data
# devtools::use_data(lake, compress = "bzip2", overwrite = TRUE)
# devtools::use_data(lake1, compress = "bzip2", overwrite = TRUE)
# devtools::use_data(lake2, compress = "bzip2", overwrite = TRUE)
# 
# #  polygon out of range (in canada)
# canada<-sf::st_read('C:/bryan/bryan_temp/tempPoly.kml')


##############NLCD
# get Jeff's NLCD lookup table 
nlcd_landuse_info<-read.csv(url('https://raw.githubusercontent.com/jhollist/miscPackage/master/inst/extdata/nlcd_lookup.csv'),
                    stringsAsFactors = FALSE)
nlcd_landuse_info<-dplyr::rename(nlcd_landuse_info, description = label, value = code) 
nlcd_landuse_info$hex <- nlcd@legend@colortable[nlcd_landuse_info$value+1] #use [gs_nlcd] to download raster "nlcd"
devtools::use_data(nlcd_landuse_info, compress = "bzip2", overwrite = TRUE)

# #add colors 1:256 for nlcd_landuse raster:  Note: not necessary-colortable already loaded in NLCD slot
# nlcd_landuse_colors <- rep("#000000", 256) #template
# nlcd_landuse_colors[nlcd_landuse_info$code + 1] <- nlcd_landuse_info$hex        #add colors for NCLD codes
# devtools::use_data(nlcd_landuse_colors, compress = "bzip2", overwrite = TRUE)

#####################

# save the NLCD crs
crs_alb<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
devtools::use_data(crs_alb, compress = "bzip2", overwrite = TRUE)



# #############################
# #read NLCD tile map and convert to sf
# 
# #Download data
# temp <- tempfile() #create a temp file
# 
# #download the data (this is a 1 TB file)
# download.file("https://s3-us-west-2.amazonaws.com/prd-tnm/StagedProducts/NLCD/data/2011/landcover/3x3/NLCD2011_LC_N24W078.zip",temp)
# 
# #unzip the file
# unzip(temp, exdir=tempdir())
# 
# #convert sf to shapefile
# nlcd_tiles<-sf::st_read(paste(tempdir(),'/Index_NLCD2011_LC.shp', sep=''))
# 
# #reproject to Albers Equal Area Conic
# nlcd_tiles<-st_transform(nlcd_tiles, "+init=ESRI:102008")
# 
# #save
# save(nlcd_tiles, file=here::here('data/nlcd_tiles.rda'))
# 
# ############################
# 
# 
# #ESRI USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
# AlbersContigUSGS<-CRS('+proj=
#                       
# aea +x_0=0 +y_0=0 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +lat_0=23 +units=m +datum=NAD83')  
# 
# 
# aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80
#sub_census.R
#create pop estimates for buffers of different sizes

library(sf)
library(goatscape)

#get the 5k buffer census data
load('C:/bryan/rscripts/nla2007_landscape/data/too_big/blk2010.rda')

#get nla lakes
lake<-st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')
lake<-st_transform(lake, "+init=ESRI:102008")

#select random lake & pop data
i<-sample(1:1159,1)
lake1<-st_geometry(lake[i,])
pop5k<-blk2010[[i]]$census_data

# add buffer
buf1<-st_difference(st_buffer(lake1, 1000), lake1)

#plot buffer on 5k pop
plot(pop5k[17])
plot(buf1[1], add = TRUE, col='green')

#create intersection of pop5k and buf1 
int<-sf::st_intersection(pop5k, buf1)

plot(pop5k[17])
plot(int[16], add = TRUE, col=NA, border="green", lwd = 3)

#calc area of intersection polygons
int$int_area<-as.numeric(sf::st_area(int))

#calc proportion of original block included in int polygons
int$pro_blk<-round(int$int_area / int$blk_area, 4)

#calc proportional population by block/block_group
int$pro_pop<-round(int$tot_pop * int$pro_blk, 2)

est_population<-round(sum(int$pro_pop), 2)

#calc input area in m2
input_area_km2<-round(as.numeric(sf::st_area(landscape) / 1000000), 2)

#calc pop density
est_pop_per_km2<-round(out$est_population / out$input_area_km2, 1)



names(int)

int1[1:5,c("tot_pop", "blk_area", "int_area", "pro_blk", "pro_pop")]
int[1:5,c("tot_pop", "blk_area", "int_area", "pro_blk", "pro_pop")]




plot(buf5[1], col='red')  
plot(buf3[1], col='purple', add=T)  
plot(buf1[1], col='pink', add=T)  


















####old

#get nla lakes
lake<-st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')
lake<-st_transform(lake, "+init=ESRI:102008")

#select lake NLA06608-0650 (two states and 3 counties)
#lake1<-st_as_sf(dplyr::filter(lake, SITEID=='NLA06608-0650'))

#select lake 'NLA06608-TX:14'
#lake1<-st_as_sf(dplyr::filter(lake, SITEID=='NLA06608-TX:14'))

#select random lake
i<-sample(1:1159,1)
lake1<-lake[i,]

# add buffers
buf1<-st_difference(st_buffer(lake1, 1000), lake1)
buf3<-st_difference(st_buffer(lake1, 3000), lake1)
buf5<-st_difference(st_buffer(lake1, 5000), lake1)

plot(buf5[1], col='red')  
plot(buf3[1], col='purple', add=T)  
plot(buf1[1], col='pink', add=T)  

plot(blk5$census_data[1], col='red')  
plot(blk3$census_data[1], col='purple', add=T)  
plot(blk1$census_data[1], col='pink', add=T)  

blk1$est_population #est_population
blk3$est_population
blk5$est_population

blk1$est_pop_per_km2  #est_pop_per_km2
blk3$est_pop_per_km2
blk5$est_pop_per_km2

#recreate the smaller buffers from buf5?


#create intersection of blk5 and buf1 
  int<-sf::st_intersection(buf1, blk5$census_data)
  
  plot(blk5$census_data[1])
  plot(buf1[1], add=TRUE)
  
  #calc area of intersection polygons
  int$int_area<-as.numeric(sf::st_area(int))
  
  #calc proportion of original block included in int polygons
  int$pro_blk<-round(int$int_area / int$blk_area, 4)
  
  #calc proportional population by block/block_group
  int$pro_pop<-round(int$tot_pop * int$pro_blk, 2)
  
  est_population<-round(sum(int$pro_pop), 2)
  
  #calc input area in m2
  input_area_km2<-round(as.numeric(sf::st_area(landscape) / 1000000), 2)
  
  #calc pop density
  est_pop_per_km2<-round(out$est_population / out$input_area_km2, 1)
  

  
  #create intersection of blk5 and buf3 
  int<-sf::st_intersection(buf3, blk5$census_data)
  
  plot(blk5$census_data[1])
  plot(buf3[1], add=TRUE)
  
  #calc area of intersection polygons
  int$int_area<-as.numeric(sf::st_area(int))
  
  #calc proportion of original block included in int polygons
  int$pro_blk<-round(int$int_area / int$blk_area, 4)
  
  #calc proportional population by block/block_group
  int$pro_pop<-round(int$tot_pop * int$pro_blk, 2)
  
  est_population<-round(sum(int$pro_pop), 2)
  
  #calc input area in m2
  input_area_km2<-round(as.numeric(sf::st_area(landscape) / 1000000), 2)
  
  #calc pop density
  est_pop_per_km2<-round(out$est_population / out$input_area_km2, 1)





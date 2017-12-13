
library(sf)

#year<-2001
year<-2006
#year<-2011

#load nlcd_tiles sf
load(here::here('data/nlcd_tiles.rda'))

#get nla lakes
lakes<-st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')

#convert to albers (the same version as NLCD)
lakes<-st_transform(lakes, st_crs(nlcd_tiles)$proj4string)

#select a lake
#select lake and buffer
i<-sample(1:1159, 1) #placeholder for loop  ***********************************
buf<-NA  #remove old output
buf<-tryCatch(st_difference(st_buffer(lakes[i,], 5000), st_geometry(lakes[i,])), error = function(e) NA)

#find intersection of buf and tiles  NOTE: for now we assume length==1; later need to add loop
a<-as.data.frame(st_intersection(buf, nlcd_tiles))
tiles<-unique(as.character(a$NLCD))
length(tiles)

#build link to download NLCD imperv data
i<-1 #placeholder for loop  ***********************************
link<-paste('https://s3-us-west-2.amazonaws.com/prd-tnm/StagedProducts/NLCD/data/', year, 
      '/impervious/3x3/NLCD', year, '_IMP_', tiles[i], '.zip', sep='')

#Download data
temp <- tempfile() #create a temp file

#download the data 
download.file(link, temp)

#unzip the file
unzip(temp, exdir=tempdir())

#Rasterize the data (package “raster”)
imperv <- raster::raster(paste(tempdir(), '/NLCD', year, '_IMP_', tiles[i], '.tif', sep=''))
#mapview::mapview(imperv)
raster::crs(imperv)

Imp<-imperv
Buf<-as_Spatial(st_geometry(st_zm(buf)))


nc = st_read(system.file("shape/nc.shp", package="sf"))
b<-as_Spatial(st_geometry(nc[1,]))

##############################
####function to calculate the impervious cover area and percent in the buffer.
CalcImperv<- function(Buf,Imp){ 
  gc() #release unused memory 
  Mask<-mask(Imp, Buf)  #extract impervious cover pixels in the buffer (all others changed to NA)
  a<-table(getValues(Mask),useNA='ifany')  #the NA are pixels outside of bbox of buffer.  Values of "127" are the real NA
  a<-na.exclude(data.frame(Percent=as.numeric(names(a)),a)[,-2]) #replace percents stored as factors with values
  a$ImpPix<-a$Percent*a$Freq/100 #processing step-convert frequency distribution to number of impervious pixels
  MissingPix<-ifelse(max(a$Percent)==127,a[a$Percent==127,'Freq'],0)  #number of missing pixels
  TotalPix<-sum(a$Freq)  #total number of pixels in buffer
  PercentNA<-round(MissingPix/TotalPix,3)*100  #percent NA cells in buffer
  BufAreaKm2<-round(sum(a$Freq)*30*30/1000000,3)  #total area of buffers (based on pixel number)-includes NA (value=127)
  a<-subset(a,a$Percent<=100)  #remove pixels with missing values (=127)
  BufAreaKm2Adj<-round(sum(a$Freq)*30*30/1000000,3)  #this doesn't include Pixels with missing data
  ImpervAreaKm2<-round(sum(a$ImpPix)*30*30/1000000,3) #calculate the area of impervious cover
  PercentImperv<-round(sum(ImpervAreaKm2)/sum(BufAreaKm2Adj),4)*100 #calculate the percent of impervious cover 
  data.frame(PercentImperv,ImpervAreaKm2,BufAreaKm2,BufAreaKm2Adj,PercentNA)
}
####function to calculate the number of people and density in the buffer.
####################works
Impervious<- function(WB_ID,PlotYN){       #WB_ID ID of lake, PlotYN enter 'Y' to generate figure or 'N' for text only
  Start<-Sys.time()  #Record Start Time
  gc() #release unused memory
  Lake<-MRB1Lakes[match(WB_ID,MRB1Lakes$WB_ID),]  #select lake
  radius<-round(sqrt(Lake$AlbersArea/pi)) #Calculate the approximate lake radius as sqrt(Area/pi)
  #Get the buffers
  Buf300<-gDifference(gBuffer(Lake,width=300),Lake)
  Buf1000<-gDifference(gBuffer(Lake,width=1000),Lake)
  Buf2500<-gDifference(gBuffer(Lake,width=2500),Lake)
  BufRadius<-gDifference(gBuffer(Lake,width=radius),Lake)
  #Select offset and extent for maximum grid    if(radius<=2500)B<-bbox(Buf2500) else B<-bbox(BufRadius)
  if(radius<=2500)B<-bbox(Buf2500) else B<-bbox(BufRadius)
  Extent<-c(B[1,1]-90,B[1,2]+90,B[2,1]-90,B[2,2]+90)
  #Get Impervious data for the Largest Lake Buffer + 3 30x30 pixels 
  Imp<-crop(Imperv,Extent)
  #plot grid and buffers    ###optional PlotYN='Y' to plot; PlotYN='N' to return text only.
  if(PlotYN=='Y'){
    image(Imp,main=paste('WB_ID = ',WB_ID))
    plot(Buf300,add=T)                  
    plot(Buf1000,add=T)
    plot(Buf2500,add=T)
    plot(BufRadius,add=T)
    title(sub=v)
  }
  #calculate imperv area and percent for each buffer
  a<-data.frame(matrix(nrow=4,ncol=7)) 
  a[1,]<-c(WB_ID,300,CalcImperv(Buf300,Imp)) 
  a[2,]<-c(WB_ID,1000,CalcImperv(Buf1000,Imp)) 
  a[3,]<-c(WB_ID,2500,CalcImperv(Buf2500,Imp)) 
  a[4,]<-c(WB_ID,radius,CalcImperv(BufRadius,Imp))  
  names(a)<-c('WB_ID','BufWidthM','PercentImperv','ImpervAreaKm2','BufferAreaKm2','BufferAreaKm2Adj','PercentNA')
  print(paste(WB_ID,' Time Elapsed = ',round(Sys.time()-Start),' seconds')) #how long did the process take?
  print('') #extra line
  
  return(a)
}    
############################
library(dplyr)
library(rgdal)
library(sf)
library(ncdf4)
library(raster)
library(spdplyr)
library(doParallel)

#define write dir
write.dir = "~/mt-climate-data/data/growing_degree_days/base_50_degrees_F/"

#import states shp file for clipping
mt = read_sf("~/mt-climate-data/shp/states/states.shp")%>%
  filter(STATE_ABBR == "MT")

#import minimum daily temp from gridMET and clip to MT (Abatzoglou, 2013)
min_temp = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmn_1979_CurrentYear_CONUS.nc", 
                 var= "daily_minimum_temperature") %>%
  
  crop(., extent(mt)) %>%
  mask(., mt)

#define proj4string
crs(min_temp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#import maximum daily temp from gridMET and clip to MT (Abatzoglou, 2013)
max_temp = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc", 
                 var= "daily_maximum_temperature") %>%
  
  crop(., extent(mt)) %>%
  mask(., mt)

#define proj4string
crs(max_temp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#calculate vector of time
time_min_temp = data.frame(datetime = as.Date(as.numeric(substring(names(min_temp),2)), origin="1900-01-01"))%>%
  mutate(day = strftime(datetime,"%m-%d"))%>%
  mutate(year = lubridate::year(datetime))

time_max_temp = data.frame(datetime = as.Date(as.numeric(substring(names(max_temp),2)), origin="1900-01-01"))%>%
  mutate(day = strftime(datetime,"%m-%d"))%>%
  mutate(year = lubridate::year(datetime))

#make sure time series are the same
all.equal(time_min_temp, time_max_temp)

#convert min temp form kelvin to F
max_temp_F = ((max_temp - 273.15) * (9/5) + 32)
#set max value for growing degree day (GDD) calculation 
max_temp_F[max_temp_F > 86] = 86
max_temp_F[max_temp_F < 50] = 50

#convert max temp form kelvin to F
min_temp_F = ((min_temp - 273.15) * (9/5) + 32)
#set min value for growing degree day (GDD) calculation 
min_temp_F[min_temp_F < 50] = 50
 
#set up paralell back end
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#calcualte growing degree day
GDD = list()
GDD = foreach(i=1:nlayers(max_temp)) %dopar% {
  sum(mean(max_temp_F[[i]], min_temp_F[[i]]) - 50)
}

GDD = brick(GDD)

annual_GDD = foreach(i=1:length(years)) %dopar% {
  years = c(1979:2018)
  sum(GDD[[which(time_max_temp$year == years[i])]], na.rm = T)
}

#write rasters
for(i in 1:length(years)){
  writeRaster(annual_GDD[[i]], paste0(write.dir,"annual_growing_degree_days_",years[i],".tif"))
}

#reorginize into a brick
annual_GDD = brick(annual_GDD)

#compute climatology
climatology = mean(annual_GDD[[which(years == 1981):which(years == 2010)]])

#write climatology
writeRaster(climatology, paste0(write.dir,"mean_annual_growing_degree_days_1981-2010.tif"))  

stopCluster(cl)

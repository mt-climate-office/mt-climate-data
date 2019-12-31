library(dplyr)
library(rgdal)
library(sf)
library(ncdf4)
library(raster)
library(spdplyr)
library(doParallel)

#define write dir
write.dir = "~/mt-climate-data/data/temperature/mean_annual_min/"

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

#calculate vector of time
time = data.frame(datetime = as.Date(as.numeric(substring(names(min_temp),2)), origin="1900-01-01"))%>%
  mutate(day = strftime(datetime,"%m-%d"))%>%
  mutate(year = lubridate::year(datetime))

#start cluster for parellel computing
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#years of interest
years = c(1979:2018)

#test
annual = mean(min_temp[[which(time$year == years[1])]])

#calculate the mean annual min temp in parellel (by years)
annual_min_temp = foreach(i=1:length(years)) %dopar% {
  years = c(1979:2018)
  annual = mean(min_temp[[which(time$year == years[i])]])
}

#write out data
for(i in 1:length(years)){
  writeRaster(annual_min_temp[[i]], paste0(write.dir,"mean_annual_min_temp_degrees_K_",years[i],".tif"), overwrite=TRUE)
}

#reorganize into a brick mainly for ease of indexing
annual_min_temp = brick(annual_min_temp)

#calculate the climatology
climatology = mean(annual_min_temp[[which(years == 1981):which(years == 2010)]])

#write out the climatology
writeRaster(climatology, paste0(write.dir,"mean_annual_min_temp_degrees_K_1981-2010.tif"))              

#lazy replication for max temp
max_temp = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc", 
                 var= "daily_maximum_temperature") %>%
  
  crop(., extent(mt)) %>%
  mask(., mt)

#define proj4string
crs(max_temp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#compute time (incase there are any differences)
time = data.frame(datetime = as.Date(as.numeric(substring(names(max_temp),2)), origin="1900-01-01"))%>%
  mutate(day = strftime(datetime,"%m-%d"))%>%
  mutate(year = lubridate::year(datetime))

#calculate the mean annual max temp in parellel (by years)
annual_max_temp = foreach(i=1:length(years)) %dopar% {
  years = c(1979:2018)
  annual = mean(max_temp[[which(time$year == years[i])]])
}

#new write dir
write.dir = "~/mt-climate-data/data/temperature/mean_annual_max/"

#write rasters
for(i in 1:length(years)){
  writeRaster(annual_max_temp[[i]], paste0(write.dir,"mean_annual_max_temp_degrees_K_",years[i],".tif"))
}

#reorginize into a brick
annual_max_temp = brick(annual_max_temp)

#compute climatology
climatology = mean(annual_max_temp[[which(years == 1981):which(years == 2010)]])

#write climatology
writeRaster(climatology, paste0(write.dir,"mean_annual_max_temp_degrees_K_1981-2010.tif"))  

#stop paralell cluster back end
stopCluster(cl)

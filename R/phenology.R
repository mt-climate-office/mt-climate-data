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

#calculate mean for daily average temp (NDAWN approach; https://ndawn.ndsu.nodak.edu/help-corn-growing-degree-days.html)
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

mean_temp = foreach(i=1:nlayers(max_temp)) %dopar% {
  mean(max_temp[[i]], min_temp[[i]])
}

mean_temp = brick(mean_temp)

#temp degree F
mean_temp_F = (mean_temp - 273.15) * (9/5) + 32

#Corn Growing Degree Days (times greater then 10 C, 50 C)



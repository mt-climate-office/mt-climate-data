library(dplyr)
library(rgdal)
library(sf)
library(ncdf4)
library(raster)
library(spdplyr)
library(doParallel)

#define write dir
write.dir = "~/mt-climate-data/data/temperature/annual_daily_min_mean/"

#import states shp file for clipping
mt = read_sf("~/mt-climate-data/shp/states/states.shp")%>%
  filter(STATE_ABBR == "MT")

#
min_temp = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmn_1979_CurrentYear_CONUS.nc", 
               var= "daily_minimum_temperature") %>%
  
  crop(., extent(mt)) %>%
  mask(., mt)

time = data.frame(datetime = as.Date(as.numeric(substring(names(min_temp),2)), origin="1900-01-01"))%>%
  mutate(day = strftime(datetime,"%m-%d"))%>%
  mutate(year = lubridate::year(datetime))

years = c(1979:2018)

#start cluster for parellel computing
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#sum and mask min_temp in parellel
annual_min_temp = foreach(i=1:length(years)) %dopar% {
  years = c(1979:2018)
  annual = mean(min_temp[[which(time$year == years[i])]])
}

for(i in 1:length(years)){
  writeRaster(annual_min_temp[[i]], paste0(write.dir,"annual_mean_min_temp_degrees_K_",years[i],".tif"))
}

annual_min_temp = brick(annual_min_temp)

climatology = mean(annual_min_temp[[which(years == 1981):which(years == 2010)]])

writeRaster(climatology, paste0(write.dir,"mean_min_temp_1981-2010.tif"))              

stopCluster(cl)
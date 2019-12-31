library(dplyr)
library(rgdal)
library(sf)
library(ncdf4)
library(raster)
library(spdplyr)
library(doParallel)

write.dir = "/home/zhoylman/mt-climate-data/data/precipitation/annual_sum/"

mt = read_sf("/home/zhoylman/mt-climate-data/shp/states/states.shp")%>%
  filter(STATE_ABBR == "MT")

precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", 
               var= "precipitation_amount") %>%
  crop(., extent(mt)) %>%
  mask(., mt)

precip = mask(precip,mt)

time = data.frame(datetime = as.Date(as.numeric(substring(names(precip),2)), origin="1900-01-01"))%>%
  mutate(day = strftime(datetime,"%m-%d"))%>%
  mutate(year = lubridate::year(datetime))

years = c(1979:2018)

#start cluster for parellel computing
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#sum and mask precip in parellel
annual_precip = foreach(i=1:length(years)) %dopar% {
  years = c(1979:2018)
  annual = sum(precip[[which(time$year == years[i])]])
}

for(i in 1:length(years)){
  writeRaster(annual_precip[[i]], paste0(write.dir,"annual_precipitation_mm_",years[i],".tif"))
}

annual_precip = brick(annual_precip)

climatology = mean(annual_precip[[which(years == 1981):which(years == 2010)]])

writeRaster(climatology, paste0(write.dir,"mean_precipitation_mm_1981-2010.tif"))              

stopCluster(cl)

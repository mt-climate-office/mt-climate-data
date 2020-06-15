library(dplyr)
library(rgdal)
library(sf)
library(ncdf4)
library(raster)
library(spdplyr)
library(doParallel)

#define write directory
write.dir = "~/mt-climate-data/data/atlas_datasets/precipitation/annual_precipitation/"

#import state shp file for clipping
mt = read_sf("~/mt-climate-data/data/shp/states/states.shp")%>%
  filter(STATE_ABBR == "MT")

#import gridMET precip and mask to MT (Abatzoglou, 2013)
precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", 
               var= "precipitation_amount") %>%
  crop(., extent(mt)) %>%
  mask(., mt)

#define proj4string
crs(precip) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#compute time
time = data.frame(datetime = as.Date(as.numeric(substring(names(precip),2)), origin="1900-01-01"))%>%
  mutate(day = strftime(datetime,"%m-%d"))%>%
  mutate(year = lubridate::year(datetime))

#define vector of years for clipping
years = c(1979:2018)

#start cluster for parellel computing
cl = makeCluster(20)
registerDoParallel(cl)

#sum and mask precip in parellel
annual_precip = foreach(i=1:length(years)) %dopar% {
  years = c(1979:2018)
  annual = sum(precip[[which(time$year == years[i])]])
}

#export indivitual years
for(i in 1:length(years)){
  writeRaster(annual_precip[[i]], paste0(write.dir,"annual_sum_precipitation_mm_",years[i],".tif"))
}

#redefine as a brick (easier to index for climatology)
annual_precip = brick(annual_precip)

#compute 1981-2010 climatology
climatology = mean(annual_precip[[which(years == 1981):which(years == 2010)]])

#write climatology raster
writeRaster(climatology, paste0(write.dir,"mean_annual_sum_precipitation_mm_1981-2010.tif"))              

#calculate binary precip grids
precip[precip>0] = 1

#sum and mask precip in parellel
precip_freq = foreach(i=1:length(years)) %dopar% {
  years = c(1979:2018)
  annual = sum(precip[[which(time$year == years[i])]])
}

write.dir = "~/mt-climate-data/data/precipitation/annual_precipitation_frequency/"

#export indivitual years
for(i in 1:length(years)){
  writeRaster(precip_freq[[i]], paste0(write.dir,"annual_precipitation_frequency_days_",years[i],".tif"))
}

#redefine as a brick (easier to index for climatology)
precip_freq = brick(precip_freq)

#compute 1981-2010 climatology
climatology = mean(precip_freq[[which(years == 1981):which(years == 2010)]])

#write climatology raster
writeRaster(climatology, paste0(write.dir,"annual_precipitation_frequency_days_1981-2010.tif"))              

#stop parallel cluster backend
stopCluster(cl)

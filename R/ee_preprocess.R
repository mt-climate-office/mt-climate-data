# gross script just meant to quickly get the rater stacks ready for 
# ee app

library(raster)
library(dplyr)
library(spdplyr)

precip = list.files("/home/zhoylman/mt-climate-data/data/precipitation/annual_precipitation", full.names = T)%>%
  lapply(., raster) 
precip[[41]] = NULL
precip = brick(precip)
precip = precip/25.4
writeRaster(precip,"/home/zhoylman/mt-climate-data/ee_data/precip_in.tif")

precip_freq = list.files("/home/zhoylman/mt-climate-data/data/precipitation/annual_precipitation_frequency/", full.names = T)%>%
  lapply(., raster)

precip_freq[[3]] = NULL
precip_freq = brick(precip_freq)
writeRaster(precip_freq,"/home/zhoylman/mt-climate-data/ee_data/precip_freq.tif")

temp = list.files("/home/zhoylman/mt-climate-data/data/temperature/mean_annual_temp/", full.names = T)%>%
  lapply(., raster) 
temp[[3]] = NULL
temp = brick(temp)
temp = ((temp - 273.15)*(9/5))+32
writeRaster(temp,"/home/zhoylman/mt-climate-data/ee_data/mean_temp_F.tif")

min_temp = list.files("/home/zhoylman/mt-climate-data/data/temperature/mean_annual_min/", full.names = T)%>%
  lapply(., raster) 
min_temp[[3]] = NULL
min_temp = brick(min_temp)
min_temp = ((min_temp - 273.15)*(9/5))+32
writeRaster(min_temp,"/home/zhoylman/mt-climate-data/ee_data/min_temp_F.tif")

max_temp = list.files("/home/zhoylman/mt-climate-data/data/temperature/mean_annual_max/", full.names = T)%>%
  lapply(., raster) 
max_temp[[3]] = NULL
max_temp = brick(max_temp)
max_temp = ((max_temp - 273.15)*(9/5))+32
writeRaster(max_temp,"/home/zhoylman/mt-climate-data/ee_data/max_temp_F.tif")

days_above_90 = list.files("/home/zhoylman/mt-climate-data/data/temperature/days_above_90F/", full.names = T)%>%
  lapply(.,raster)
days_above_90[[41]] = NULL
days_above_90 = brick(days_above_90)
days_above_90 = mask(days_above_90, sf::read_sf("/home/zhoylman/mt-climate-data/shp/states/states.shp")%>%dplyr::filter(STATE_ABBR == "MT"))
writeRaster(days_above_90,"/home/zhoylman/mt-climate-data/ee_data/days_above_90F.tif")

GGD = list.files("/home/zhoylman/mt-climate-data/data/growing_degree_days/base_50_degrees_F/", full.names = T)%>%
  lapply(.,raster)
GGD[[41]] = NULL
GGD = brick(GGD)
GGD = mask(GGD, sf::read_sf("/home/zhoylman/mt-climate-data/shp/states/states.shp")%>%dplyr::filter(STATE_ABBR == "MT"))
writeRaster(GGD,"/home/zhoylman/mt-climate-data/ee_data/Growing_Degree_Days.tif")

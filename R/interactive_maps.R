library(leaflet)
library(leaflet.extras)
library(raster)
library(sf)

#import state shp file for clipping
mt = read_sf("~/mt-climate-data/shp/states/states.shp")%>%
  dplyr::filter(STATE_ABBR == "MT")

# precip map
mean_precip = raster("~/mt-climate-data/data/precipitation/annual_precipitation/mean_annual_sum_precipitation_mm_1981-2010.tif")
mean_precip = mean_precip/25.4

precip_freq = raster("~/mt-climate-data/data/precipitation/annual_precipitation_frequency/annual_precipitation_frequency_days_1981-2010.tif")

lseq = function(from, to, length.out){
  x = round(exp(seq(log(from), log(to), length.out = length.out)),1)
  return(x)
}

base_map = leaflet::leaflet(options = leaflet::tileOptions(minZoom = 4, maxZoom = 10)) %>%
  leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
  leaflet::addProviderTiles("Stamen.TonerLines") %>%
  leaflet::addProviderTiles("Stamen.TonerLabels") %>%
  leaflet::setView(lng = -110, lat = 46.5, zoom = 6) %>%
  leaflet::addPolygons(data = mt, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  leaflet.extras::addDrawToolbar(markerOptions = FALSE,
                                 polylineOptions = FALSE,
                                 polygonOptions = FALSE,
                                 circleOptions = FALSE,
                                 rectangleOptions = FALSE,
                                 circleMarkerOptions = FALSE,
                                 editOptions = FALSE,
                                 singleFeature = FALSE,
                                 targetGroup='draw')

pal <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffff00", "#00ff00", "#00ffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                domain = 5:110, bins = lseq(5,110,10), na.color = "transparent")

pal_freq <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffff00", "#00ff00", "#00ffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                domain = 85:265, bins = round(seq(85,265,length.out = 10),0), na.color = "transparent")

precip_map = base_map %>%
  leaflet::addRasterImage(mean_precip, colors = pal, opacity = 0.7,group = "Average", project = T) %>%
  leaflet::addRasterImage(precip_freq, colors = pal_freq, opacity = 0.7,group = "Frequency", project = T) %>%
  addLegend(pal = pal, values = 5:110,
            title = paste0("<center>Average<br>Precipitation<br>(in)<br><center/>"),
            position = "bottomleft", group = "Average")%>%
  addLegend(pal = pal_freq, values = 85:265,
            title = paste0("<center>Average<br>Precipitation<br>Frequency (days)<br><center/>"),
            position = "bottomleft", group = "Frequency")%>%
  addLayersControl(position = "topright",
                   overlayGroups = c("Average", "Frequency"),
                   options = layersControlOptions(collapsed = FALSE))%>%
  leaflet::hideGroup("Frequency")

precip_map

htmlwidgets::saveWidget(precip_map, "~/mt-climate-data/maps/precip_map.html", selfcontained = T)


#temp map
mean = ((raster("~/mt-climate-data/data/temperature/mean_annual_temp/mean_annual_temp_degrees_K_1981-2010.tif")-273.15)*(9/5) + 32)
max = ((raster("~/mt-climate-data/data/temperature/mean_annual_max/mean_annual_max_temp_degrees_K_1981-2010.tif")-273.15)*(9/5) + 32)
min = ((raster("~/mt-climate-data/data/temperature/mean_annual_min/mean_annual_min_temp_degrees_K_1981-2010.tif")-273.15)*(9/5) + 32)
days_above_90 = raster("~/mt-climate-data/data/temperature/days_above_90F/mean_days_above_90_degrees_F_1981-2010.tif")%>%
  mask(., mt)

pal_mean <- colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#00ff00", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                     domain = 24:51, bins = round(seq(24,51,length.out = 10),0), na.color = "transparent")

pal_max <- colorBin(colorRamp(rev(c("#8b0000",'#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850',"#013220")), interpolate = "spline"), 
                     domain = 35:64, bins = round(seq(35,64,length.out = 10),0), na.color = "transparent")

pal_min <- colorBin(colorRamp(rev(c("#00ff00", "#00ffff", "#0000ff", "#000d66", "#301934")), interpolate = "spline"), 
                     domain = 13:39, bins = round(seq(13,39,length.out = 10),0), na.color = "transparent")

pal_days <- colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#00ff00", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                     domain = 0:51, bins = round(seq(0,51,length.out = 10),0), na.color = "transparent")

temp_map = base_map %>%   
  leaflet::addRasterImage(mean, colors = pal_mean, opacity = 0.7,group = "Mean Annual Temp", project = T) %>%
  leaflet::addRasterImage(max, colors = pal_max, opacity = 0.7,group = "Mean Annual Max Temp", project = T) %>%
  leaflet::addRasterImage(min, colors = pal_min, opacity = 0.7,group = "Mean Annual Min Temp", project = T) %>%
  leaflet::addRasterImage(days_above_90, colors = pal_days, opacity = 0.7,group = "Mean Annual Days above 90°F", project = T) %>%
  addLegend(pal = pal_mean, values = 24:51,
            title = paste0("<center>Mean Annual<br>Temperature (°F)<br><center/>"),
            position = "bottomleft", group = "Mean Annual Temp")%>%
  
  addLegend(pal = pal_min, values = 13:39,
            title = paste0("<center>Mean Annual<br>Minimum<br>Temperature (°F)<br><center/>"),
            position = "bottomleft", group = "Mean Annual Min Temp")%>%
  
  addLegend(pal = pal_max, values = 35:64,
            title = paste0("<center>Mean Annual<br>Maximum<br>Temperature (°F)<br><center/>"),
            position = "bottomleft", group = "Mean Annual Max Temp")%>%
  
  addLegend(pal = pal_days, values = 0:51,
            title = paste0("<center>Mean Annual<br>Days Above 90°F<br><center/>"),
            position = "bottomleft", group = "Mean Annual Days above 90°F")%>%
  
  addLayersControl(position = "topright",
                   overlayGroups = c("Mean Annual Temp", "Mean Annual Min Temp",
                                     "Mean Annual Max Temp", "Mean Annual Days above 90°F"),
                   options = layersControlOptions(collapsed = FALSE))%>%
  leaflet::hideGroup(c("Mean Annual Min Temp","Mean Annual Max Temp", "Mean Annual Days above 90°F"))
  

temp_map

htmlwidgets::saveWidget(temp_map, "~/mt-climate-data/maps/temp_map.html", selfcontained = T)


# growing degree days
GDD = raster("~/mt-climate-data/data/growing_degree_days/base_50_degrees_F/mean_annual_growing_degree_days_1981-2010.tif") %>%
  mask(., mt)

pal_GDD = colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#00ff00", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                   domain = 343:3220, bins = round(seq(343,3220,length.out = 10),0), na.color = "transparent")

GGD_map = base_map %>%
  leaflet::addRasterImage(GDD, colors = pal_GDD, opacity = 0.7,group = "Mean Annual Growing Degree Days", project = T) %>%
  addLegend(pal = pal_GDD, values = 343:3220,
            title = paste0("<center>Mean Annual<br>Growing Degree Days<br><center/>"),
            position = "bottomleft", group = "Mean Annual Growing Degree Days")%>%
  addLayersControl(position = "topright",
                   overlayGroups = c("Mean Annual Growing Degree Days"),
                   options = layersControlOptions(collapsed = FALSE))
GGD_map

htmlwidgets::saveWidget(GGD_map, "~/mt-climate-data/maps/GGD_map.html", selfcontained = T)

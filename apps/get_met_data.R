# shiny app to download any gridmet dataset for CONUS. 
# Point downloads only, daily or monthly. 

library(ncdf4)
library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tableHTML)

input_options = data.frame(urls = c("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc",
                                    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc",
                                    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmn_1979_CurrentYear_CONUS.nc",
                                    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_vpd_1979_CurrentYear_CONUS.nc",
                                    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_srad_1979_CurrentYear_CONUS.nc",
                                    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_vs_1979_CurrentYear_CONUS.nc",
                                    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_th_1979_CurrentYear_CONUS.nc",
                                    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc"),
                           summary = c("sum", "mean", "mean", "mean", "mean", "mean", "mean", "sum"),
                           names = c("Precipitation", "Maximum Temperature", "Minimum Temperature",
                                     "Vapor Pressure Deficit", "Surface Radiation", "Wind Velocity",
                                     "Wind Direction", "Potential ET"),
                           units = c("mm", "°C", "°C", "kPa", "W/m2", "m/s","degrees_clockwise_from_N", "mm"),
                           short_name = c("precip", "max_temp", "min_temp", "vpd", "srad", "wind_velocity", "wind_direction","PET"))

#to dos!
#start time, end time

get_met_data = function(lat_in, lon_in, i){
  #Define URL to net cdf 
  urltotal = input_options$urls[i] %>%
    as.character()
  # OPEN THE FILE
  nc = nc_open(urltotal)
  # find length of time variable for extraction
  endcount = nc$var[[1]]$varsize[3] 
  # Querry the lat lon matrix
  lon_matrix = nc$var[[1]]$dim[[1]]$vals
  lat_matrix = nc$var[[1]]$dim[[2]]$vals
  # find lat long that corispond
  lon=which(abs(lon_matrix-lon_in)==min(abs(lon_matrix-lon_in)))  
  lat=which(abs(lat_matrix-lat_in)==min(abs(lat_matrix-lat_in))) 
  # define variable name
  var= nc$var[[1]]$name
  # read data and time and extract useful time information
  data = data.frame(data = ncvar_get(nc, var, start=c(lon,lat,1),count=c(1,1,endcount))) %>%
    mutate(time = as.Date(ncvar_get(nc, "day", start=c(1),count=c(endcount)), origin="1900-01-01")) %>%
    mutate(day = yday(time)) %>%
    mutate(year = year(time)) %>%
    mutate(month = month(time, label = T, abbr = F))
  # close file
  nc_close(nc)
  
  #if data is in degress K, convert to C
  if(input_options$units[i] %>% as.character() == "°C"){
    data$data = data$data - 273.15
  }
  
  #define summary_fun as character
  summary_fun = input_options$summary[i] %>%
    as.character()
  
  #monthly data
  monthly_data = data %>%
    group_by(month, year) %>%
    dplyr::summarise(summary = get(summary_fun)(data)) %>%
    mutate(time = as.POSIXct(as.Date(as.yearmon(paste(year, month, sep = "-"),
                                                '%Y-%b')))) %>%
    arrange(time)
  
  #define some plotting data
  axis_name = input_options$names[i] %>%
    as.character()%>%
    paste0("Monthly " , ., " (", summary_fun,"; ", input_options$units[i] %>% as.character(),")")
  
  # define ggplot function to display 3 years of data
  plot_function = function(data, axis_name){
    var_plot = ggplot(data = data, aes(x = time, y = summary))+
      geom_point(stat = 'identity', fill = "black")+
      geom_line()+
      xlab("")+
      ylab(axis_name)+
      theme_bw(base_size = 16)+
      ggtitle("")+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 60, vjust = 0.5))+
      scale_x_datetime(breaks = date_breaks("3 month"), labels=date_format("%b / %Y"),
                       limits= as.POSIXct(c(data$time[length(data$time)-36], 
                                            data$time[length(data$time)]))) 
    return(var_plot)
  }
  # return a list of 3 things, the plot (using the function above), daily daya and monthly data
  export = list(final_plot = plot_function(monthly_data, axis_name), 
              daily_data = data.frame(time = data$time, daily_data = data$data),
              monthly_data = data.frame(month = monthly_data$month, 
                                        year = monthly_data$year,
                                        monthly_summary = monthly_data$summary))
  
  colnames(export$daily_data) = c("time", paste0("daily_data_", input_options$short_name[i] %>% as.character(),
                                  "_", input_options$units[i] %>% as.character()))
  
  colnames(export$monthly_data) = c("month", "year", paste0("monthly_summary_", input_options$short_name[i] %>% as.character(),"_",
                                                            input_options$units[i] %>% as.character(), "_", 
                                                            input_options$summary[i] %>% as.character()))
  
  return(export)
}


shinyApp(ui <- fluidPage(
  # build our UI defining that we want a vertical layout
  verticalLayout(),
  # first we want to display the map
  leafletOutput("mymap", height = 600),
  selectInput("variable", "Choose a Variable:",
              #cant figure out how to soft code this.....
              c("Precipitation" = 1,
                "Maximum Temperature" = 2,
                "Minimum Temperature" = 3,
                "Vapor Pressure Deficit" = 4,
                "Surface Radiation" = 5,
                "Wind Velocity" = 6,
                "Wind Direction" = 7,
                "Potential ET" = 8),
              selected = "Precipitation"),
  # add in a conditional message for when calculations are running. 
  tags$head(tags$style(type="text/css", "
                                  #loadmessage {
                                  position: fixed;
                                  top: 0px;
                                  left: 0px;
                                  width: 100%;
                                  padding: 5px 0px 5px 0px;
                                  text-align: center;
                                  font-weight: bold;
                                  font-size: 100%;
                                  color: #ffffff;
                                  background-color: #003366;
                                  z-index: 105;
                                  }
                                  ")),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Calculating Climatology...",id="loadmessage")),
  # display our precip plot
  plotOutput("plot", width = "100%", height = "400px"),
  # set up download buttons for the user to download data
  downloadButton("downloadDaily", "Download Daily Data (1979 - Present)"),
  downloadButton("downloadMonthly", "Download Monthly Data (1979 - Present)")
),
# now on to the server
server <- function(input, output, session) {
  # this is our map that we will display
  output$mymap <- renderLeaflet({
    leaflet() %>%
      # this is the base map  
      leaflet::addProviderTiles("Stamen.Toner") %>%
      # terrain tiles
      leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
      # lines and labels
      leaflet::addProviderTiles("Stamen.TonerLines") %>%
      leaflet::addProviderTiles("Stamen.TonerLabels") %>%
      # set default viewing location and zoom
      leaflet::setView(lng = -97.307564, lat = 40.368971, zoom = 4) %>%
      # modify some parameters (what tools are displayed with the map)
      leaflet.extras::addDrawToolbar(markerOptions = drawMarkerOptions(),
                                     polylineOptions = FALSE, polygonOptions = FALSE,
                                     circleOptions = FALSE, rectangleOptions = FALSE,
                                     circleMarkerOptions = FALSE, editOptions = FALSE,
                                     singleFeature = TRUE, targetGroup='draw')
  })
  
  # Now for our reactive portion which is when the user drops a pin on the map
  observeEvent(c(input$mymap_draw_new_feature),{
    var = input$variable %>% as.numeric()
    # create a variable "feature" that will be overwritten when pin drops
    feature = input$mymap_draw_new_feature
    # call our precip function and store the outputs as a variable
    function_out = get_met_data(feature$geometry$coordinates[[2]],
                              feature$geometry$coordinates[[1]], 
                              var)
    # render the plot from our function output
    output$plot <- renderPlot({
      function_out[[1]]
    })
    # render the daily data output from our function to a csv for download 
    # with a reactive name (lat long)
    output$downloadDaily <- downloadHandler(
      filename = function() {
        paste("daily_data_",round(feature$geometry$coordinates[[2]],4),"_",
              round(feature$geometry$coordinates[[1]],4),".csv", sep = "")
      },
      content = function(file) {
        write.csv(function_out$daily_data, file, row.names = FALSE)
      }
    )
    # render the monthly data output again with a reactive name
    output$downloadMonthly <- downloadHandler(
      filename = function() {
        paste("monthly_data_",round(feature$geometry$coordinates[[2]],4),"_",
              round(feature$geometry$coordinates[[1]],4),".csv", sep = "")
      },
      content = function(file) {
        write.csv(function_out$monthly_data, file, row.names = FALSE)
      }
    )
  })
})

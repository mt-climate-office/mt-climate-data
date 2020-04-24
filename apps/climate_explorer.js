////////////////////////////////////////////////////////////////////
//                    Import Multiband Data                       //
////////////////////////////////////////////////////////////////////

var mt = ee.FeatureCollection('users/zhoylman/Montana_Outline')
var temp = ee.Image('users/zhoylman/app_data/mean_temp_F')
var precip = ee.Image('users/zhoylman/app_data/precip_in')
var precip_freq = ee.Image('users/zhoylman/app_data/precip_freq')
var max_temp = ee.Image('users/zhoylman/app_data/max_temp_F')
var min_temp = ee.Image('users/zhoylman/app_data/min_temp_F')
var days_above_90 = ee.Image('users/zhoylman/app_data/days_above_90F')
var gdd = ee.Image('users/zhoylman/app_data/Growing_Degree_Days')

////////////////////////////////////////////////////////////////////
//                    Initial map definitions                     //
////////////////////////////////////////////////////////////////////



// define base map parameters
var darkBase = [{"stylers": [
            {"hue": "#ff1a00"},
            {"invert_lightness": true},
            {"saturation": -100},
            {"lightness": 33},
            {"gamma": 0.5}]},
    {
        "featureType": "water",
        "elementType": "geometry",
        "stylers": [{"color": "#2D333C"}]}
]

//add to initial map
Map.setOptions(
    'darkBase', {darkBase: darkBase});

// Configure the map.
Map.setCenter(-109.606, 46.932, 6);
Map.style().set('cursor', 'crosshair');

// initial map message
var title = ui.Label("Choose a variable from the list to initiate map.");
title.style().set('position', 'top-center');
title.style().set({
  fontWeight: 'bold',
  fontSize: '20px',
  padding: '10px'
})
Map.add(title);

////////////////////////////////////////////////////////////////////
//                    Preprocess data for app                     //
////////////////////////////////////////////////////////////////////

//define years of interest for gridmet processing
var years = [];
for (var i = 0; i <= 39; i++) {
    years.push(i);
}

var process_gridmet = function(variable_data, years, start_year){
  var annual_data = ee.ImageCollection(years.map(function(y) {
  var temp_y = ee.Image(variable_data.select([y]))
  var real_time = ee.Number(y).add(start_year)
  var time = ee.Date.fromYMD(real_time,12,31)
  return temp_y.set('index', real_time).set('system:time_start', time).rename('data')
  }))
  
  return annual_data
}

//gridMET preprocessing
var annual_temp = process_gridmet(temp, years, 1979)
var annual_temp_min = process_gridmet(min_temp, years, 1979)
var annual_temp_max = process_gridmet(max_temp, years, 1979)
var annual_above_90 = process_gridmet(days_above_90, years, 1979)
var annual_precip = process_gridmet(precip, years, 1979)
var annual_precip_freq = process_gridmet(precip_freq, years, 1979)
var annual_gdd = process_gridmet(gdd, years, 1979)

//Vegitation Indicies Preprocessing
// Load and display NDVI data.
var preprocess_npp = function(img) {
  return img.select("annualNPP").clip(mt).divide(10).rename('NPP')
  .copyProperties(img, ['system:time_start']).set('index', ee.Number.parse(img.get('system:index')));
};
var NPP = ee.ImageCollection("UMT/NTSG/v2/MODIS/NPP").map(preprocess_npp)

//MODIS ET
//define years of interest for modis processing
var years = [];
for (var i = 0; i <= 17; i++) {
    years.push(i);
}
var ET = ee.ImageCollection(years.map(function(y) {
  var start = ee.Date.fromYMD(ee.Number(y).add(2001),01,01)
  var end = ee.Date.fromYMD(ee.Number(y).add(2001),12,31)
  var temp_y = ee.ImageCollection("MODIS/006/MOD16A2").select('ET').filterDate(start, end).sum().clip(mt).divide(10)
  var index_year = ee.Number(y).add(2001)
  return temp_y.set('index', index_year).set('system:time_start', end)
}))

//Landsat top-of-atmosphere spectral returns
//define years of interest for landsat processing
var years = [];
for (var i = 0; i <= 35; i++) {
    years.push(i);
}

var NDVI = ee.ImageCollection(years.map(function(y) {
  var start = ee.Date.fromYMD(ee.Number(y).add(1984),05,01)
  var end = ee.Date.fromYMD(ee.Number(y).add(1984),10,31)
  //prefer LANDSAT 5 when earlier than 2012
  if(y < 28){
    var temp_y = ee.ImageCollection("LANDSAT/LT05/C01/T1_8DAY_NDVI").select('NDVI').filterDate(start, end).mean().clip(mt)
  }
  //prefer LANDSAT 7 only during 2012 (Banding is bad in Landsat 7)
  if(y == 28){
    var temp_y = ee.ImageCollection("LANDSAT/LE07/C01/T1_8DAY_NDVI").select('NDVI').filterDate(start, end).mean().clip(mt)
  }
  //prefer LANDSAT 8 when later than 2012
  if(y > 28){
    var temp_y = ee.ImageCollection("LANDSAT/LC08/C01/T1_8DAY_NDVI").select('NDVI').filterDate(start, end).mean().clip(mt)
  }
  var index_year = ee.Number(y).add(1984)
  return temp_y.set('index', index_year).set('system:time_start', end)
}))


////////////////////////////////////////////////////////////////////
//                    Build App UI and Analyis                    //
////////////////////////////////////////////////////////////////////

//function to add map and conduct analysis
var add_var = function(data_, name_, name_short, ylab, title_, min_val,
                       max_val, scale_, start_date, end_date, col_palette){
    Map.clear()
    
    Map.setOptions(
    'darkBase', {darkBase: darkBase});
    
    var title = ui.Label(title_);
    title.style().set('position', 'bottom-center');
    Map.add(title);
    
    // Set a callback function for when the user clicks the map.
    Map.onClick(function(coords) {
    
    // Create or update the location label (the second widget in the panel)
    var location = 'Longitude: ' + coords.lon.toFixed(2) + ', ' +
                   'Latitude: ' + coords.lat.toFixed(2);
    panel.widgets().set(2, ui.Label(location));

    // Add a red dot to the map where the user clicked.
    var point = ee.Geometry.Point(coords.lon, coords.lat);
    Map.layers().set(1, ui.Map.Layer(point, {color: 'FF0000'}));
  
    // Create a chart of NPP over time.
    var chart = ui.Chart.image.series(data_, point, ee.Reducer.mean(), scale_)
        .setOptions({
          title: name_short + " Over Time",
          vAxis: {title: ylab},
          lineWidth: 1,
          pointSize: 3,
          trendlines: {
          0: {
            type: 'linear',
            color: 'black',
            lineWidth: 3,
            opacity: 0.3,
            showR2: true,
            visibleInLegend: true
          }},
          legend: {position: 'none'},
        });
    
    panel.widgets().set(3, chart);
  });
  
  // Use the start of the collection and now to bound the slider.
  var start = start_date
  var now = Date.now();
  var end = end_date
  
  // Run this function on a change of the dateSlider.
  var showMosaic = function(range) {
    var year_selected = range.start().get('year').format()
    var mosaic = ee.Image(data_.filter(ee.Filter.eq("index",ee.Number.parse(year_selected))).mean());
    //print(mosaic)
    // Asynchronously compute the name of the composite.  Display it.
    range.start().get('year').evaluate(function(name) {
      var visParams = {min: min_val, max: max_val, palette: col_palette};
      var layer = ui.Map.Layer(mosaic, visParams, name + ' Data');
      Map.layers().set(0, layer);
      //print(layer)
    });
  };
  
  // Asynchronously compute the date range and show the slider.
  var dateRange = ee.DateRange(start, end).evaluate(function(range) {
    var dateSlider = ui.DateSlider({
      start: range['dates'][0],
      end: range['dates'][1],
      value: null,
      period: 365,
      onChange: showMosaic,
      style: {width: '200px', position: "bottom-right"}
    });
    Map.add(dateSlider.setValue(now));
  });
  
  var viz = {min: min_val, max: max_val, palette: col_palette};
  
  
  // set position of panel
  var legend = ui.Panel({
  style: {
  position: 'bottom-left',
  padding: '8px 15px'
  }
  });
   
  // Create legend title
  var legendTitle = ui.Label({
  value: ylab,
  style: {
  fontSize: '12px',
  margin: '0 0 4px 0',
  padding: '0'
  }
  });
   
  // Add the title to the panel
  legend.add(legendTitle);
   
  // create the legend image
  var lon = ee.Image.pixelLonLat().select('latitude');
  var gradient = lon.multiply((viz.max-viz.min)/100.0).add(viz.min);
  var legendImage = gradient.visualize(viz);
   
  // create text on top of legend
  var panel2 = ui.Panel({
  widgets: [
  ui.Label(viz['max'])
  ],
  });
   
  legend.add(panel2);
   
  // create thumbnail from the image
  var thumbnail = ui.Thumbnail({
  image: legendImage,
  params: {bbox:'0,0,10,100', dimensions:'20x100'},
  style: {padding: '1px', position: 'bottom-center'}
  });
   
  // add the thumbnail to the legend
  legend.add(thumbnail);
   
  // create text on top of legend
  var panel2 = ui.Panel({
  widgets: [
  ui.Label(viz['min'])
  ],
  });
   
  legend.add(panel2);
   
  Map.add(legend);
  
}

// Make a drop down menu of images.
var climateSelect = ui.Select({
  items: [
    {label: 'Temperature (Average)', value: 'annual_temp'},
    {label: 'Temperature (Maximum)', value: 'annual_temp_max'},
    {label: 'Temperature (Minimum)', value: 'annual_temp_min'},
    {label: 'Days Above 90°F', value: 'days_above_90'},
    {label: 'Precipitation (Average)', value: 'annual_precip'},
    {label: 'Precipitation Frequency (Days)', value: 'annual_precip_freq'},
    {label: 'Growing Degree Days', value: 'annual_gdd'}
    
  ],
  style:{padding: '0px 0px 0px 10px', stretch: 'horizontal'},
  onChange: function(value){
    if(value == 'annual_temp'){
      add_var(annual_temp, "Temperature (Average)", "Temperature", "Temperature (°F)", 
                          "Mean Annual Temperature (gridMET)", 30, 50, 4000, '1979', '2019',
                          ["000d66", "0000ff", "00ffff", "00ff00", "ffff00", "ff0000", "8b0000"])
    }
    
    if(value == 'annual_temp_max'){
      add_var(annual_temp_max, "Temperature (Maximum)", "Temperature", "Temperature (°F)", 
                          "Mean Annual Maximum Temperature (gridMET)", 35, 63, 4000, '1979', '2019',
                          ["000d66", "0000ff", "00ffff", "00ff00", "ffff00", "ff0000", "8b0000"])
    }
    
    if(value == 'annual_temp_min'){
      add_var(annual_temp_min, "Temperature (Minimum)", "Temperature", "Temperature (°F)", 
                          "Mean Annual Minimum Temperature (gridMET)", 14, 38, 4000, '1979', '2019',
                          ["000d66", "0000ff", "00ffff", "00ff00", "ffff00", "ff0000", "8b0000"])
    }
    
    if(value == 'days_above_90'){
      add_var(annual_above_90, "Days above 90°F", "Days above 90°F", "Days above 90°F", 
                          "Annual Days above 90°F (gridMET)", 0, 50, 4000, '1979', '2019',
                          ["000d66", "0000ff", "00ffff", "00ff00", "ffff00", "ff0000", "8b0000"])
    }
    
    if(value == 'annual_precip'){
      add_var(annual_precip, "Precipitaion (Average)", "Precipitaion", "Precipitaion (in)", 
                          "Annual Precipitation Sum (gridMET)", 0, 80, 4000, '1979', '2019',
                          ['8b0000' ,'ff0000', 'ffff00', '00ff00', "00ffff", "0000ff", "000d66"])
    }
    
    if(value == 'annual_precip_freq'){
      add_var(annual_precip_freq, "Precipitaion Frequency", "Precipitaion Frequency", "Precipitaion Frequency (days)", 
                          "Annual Precipitation Frequency (gridMET)", 85, 252, 4000, '1979', '2019',
                          ['8b0000' ,'ff0000', 'ffff00', '00ff00', "00ffff", "0000ff", "000d66"])
  }
  
    if(value == 'annual_gdd'){
      add_var(annual_gdd, "Growing Degree Days", "Growing Degree Days", "Growing Degree Days", 
                          "Annual Growing Degree Days (Base Temperature 50°F)", 350, 3000, 4000, '1979', '2019',
                          ["000d66", "0000ff", "00ffff", "00ff00", "ffff00", "ff0000", "8b0000"])
  }
  
}
  
});

// Make a drop down menu of images.
var vegSelect = ui.Select({
  items: [
    {label: 'Net Primary Productivity', value: 'NPP'},
    {label: 'Evapotranspiration', value: 'ET'},
    {label: 'Normalized Difference Vegetation Index', value: 'NDVI'}

    ],
  style:{padding: '0px 0px 0px 10px', stretch: 'horizontal'},
  onChange: function(value){
    if(value == 'NPP'){
      add_var(NPP, "Net Primary Productivity", "NPP", "Net Primary Productivity (gC m-2 y-1)", 
                          "Net Primary Productivity (MODIS)", 0, 1000, 250, '2001', '2019',
                          ['654321' ,'99c199', '006400', '006400'])
    }
    
    if(value == 'ET'){
      add_var(ET, "Evapotranspiration", "ET", "Evapotranspiration (kg m-2 y-1)", 
                          "Evapotranspiration (MODIS: TERRA)", 50, 500, 500, '2001', '2019',
                          ['ffffff', 'fcd163', '99b718', '66a000', '3e8601','004c00', '011301'])
    }
    
    if(value == 'NDVI'){
      add_var(NDVI, "Normalized Difference Vegetation Index", "NDVI", "NDVI", 
                          "Normalized Difference Vegetation Index (Landsat 5, 7 & 8; May - October)", 0, 0.5, 30, '1984', '2020',
                          ['ffffff', 'fcd163', '99b718', '66a000', '3e8601','004c00', '011301'])
    }
    
  }
});

// create pannel for plotting timeseries
var panel = ui.Panel({
  layout: ui.Panel.Layout.flow('vertical'),
  style: {width: '400px', fontSize: '22px'}})

// Add the panels to the ui.root.
ui.root.add(panel);

var select_labels = ui.Panel([ui.Label('Climate Variables'), ui.Label('Vegetation Variables')],
ui.Panel.Layout.flow('horizontal'))

panel.widgets().set(0,select_labels);

panel.widgets().set(1,ui.Panel([climateSelect, vegSelect],
ui.Panel.Layout.flow('horizontal')));

panel.widgets().add(ui.Label('Click on the map to see timeseries.'))


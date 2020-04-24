//Define the years of interest
var years = [];
for (var i = 1979; i <= 2019; i++) {
    years.push(i);
}

//import gridmet
var gridmet = ee.ImageCollection("IDAHO_EPSCOR/GRIDMET");

//define climatology function based on summary statistic (e.g. mean for temp, sum for precip)_
var annual_mean_climatology = function(years, data){
  //define cliamtology function (mean annual)
  //technically not needed to be defined within parent fucntion,
  //but helps with organization i supose...
  //nested functions 
  var climatology = function(data) {
    var annual = function(y){
      var time_clip = data.filter(ee.Filter.calendarRange(y,y,'year')).mean()
      return time_clip.rename(JSON.stringify(y)).set('index', "year_" + JSON.stringify(y))
    }
    return annual
  }
  //calcualte image collection 
  var img_col = ee.ImageCollection(years.map(climatology(data)))
  //store names from index
  var names = ee.List(img_col.aggregate_array('index'));
  //cnvert image collection to multiband image and rename
  var multiband = img_col.toBands().rename(names)
  //ready for export to asset
  return multiband
}

var annual_sum_climatology = function(years, data){
  //define cliamtology function (mean annual)
  //technically not needed to be defined within parent fucntion,
  //but helps with organization i supose...
  //nested functions 
  var climatology = function(data) {
    var annual = function(y){
      var time_clip = data.filter(ee.Filter.calendarRange(y,y,'year')).sum()
      return time_clip.rename(JSON.stringify(y)).set('index', "year_" + JSON.stringify(y))
    }
    return annual
  }
  //calcualte image collection 
  var img_col = ee.ImageCollection(years.map(climatology(data)))
  //store names from index
  var names = ee.List(img_col.aggregate_array('index'));
  //cnvert image collection to multiband image and rename
  var multiband = img_col.toBands().rename(names)
  //ready for export to asset
  return multiband
}


// Example usage of fucntion for multiband images 
// Can be exported to asset for use in app or exported as image for repository
// Right now it computes for all gridmet cells (e.g. conus).
// Also note unit conversions are handeled post aggregation, could be incorperated into functions


/*
var annual_precip = annual_sum_climatology(years, gridmet.select('pr')).divide(25.4)

//var mean_temp_min = ((annual_mean_climatology(years, gridmet.select('tmmn')).subtract(273.15)).multiply(ee.Number(9).divide(5))).add(32)

//var mean_temp_max = ((annual_mean_climatology(years, gridmet.select('tmmx')).subtract(273.15)).multiply(ee.Number(9).divide(5))).add(32)

var mean_rh_min = annual_mean_climatology(years, gridmet.select('rmin'))

var region = ee.FeatureCollection('ft:1tdSwUL7MVpOauSgRzqVTOwdfy17KDbw-1d9omPw')
.filter(ee.Filter.eq('Country', 'United States'));

Export.image.toAsset({
  image: mean_rh_min,
  description: 'annual_min_rh',
  assetId: 'annual_min_rh',
  scale: 4000,
  maxPixels: 1e12,  
  region: region.geometry().bounds(),
});

*/

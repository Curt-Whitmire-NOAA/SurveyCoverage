library(rstudioapi)
library(dplyr)
library(chron)
library(sf)
# library(tmap)

# import 'nwfscSurvey' package
library(nwfscSurvey)
# vignette("nwfscSurvey", package = "nwfscSurvey")

# Set working directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# import shapefiles
stns <- sf::st_read("WCGBTS_Grid_v2008_GCSWGS84.shp")
# sf::st_crs(poly)

# import haul data
haul_dat = PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange=c(2003,2019))

# convert haul data to point spatial object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
pts <- st_as_sf(x = haul_dat,                         
               coords = c("longitude_dd", "latitude_dd"),
               crs = st_crs(poly))

# convert polygon shapefile to polygon spatial object
stns <- st_as_sf(stns)

# Get start and end years from haul data
yr_srt = substr(as.character(min(pts$date_yyyymmdd)),1,4)
yr_end = substr(as.character(max(pts$date_yyyymmdd)),1,4)
# pts$date_yyyymmdd <- dates(pts$date_yyyymmdd, format = "ymd") # Proposed by John Wallace
yr_str = paste0(yr_srt, "_", yr_end)

# spatial join points with polygons
res1 <- st_join(pts, stns) %>% 
  filter(!is.na(CentroidID)) %>% 
  group_by(CentroidID) %>% 
  tally(name = paste0("cntHauls_", yr_str))

st_geometry(res1) <- NULL

# join results to polygon shape
res2 = left_join(stns, res1, by = c("CentroidID"))

st_geometry(res2) <- NULL

# Export to CSV or ArcGIS geodatabase table
library(readr)
outCSV = paste0("cntHauls_", yr_str, ".csv")
write_csv(res1, outCSV, na = "", append = FALSE)

# following code taken from https://hansenjohnson.org/post/leaflet-map-with-inset-in-r/
# make a few points
# pts = data.frame(lon = c(-65.3, -65.7, -64.1),
                 # lat = c(43.4, 43, 42.9))

# build a polygon (in this case the 'Roseway Basin Area To Be Avoided')
# ply = data.frame(lon = c(-64.916667, -64.983333, -65.516667, -66.083333),
                 # lat = c(43.266667, 42.783333, 42.65, 42.866667))

# required libraries
library(leaflet, quietly = T, warn.conflicts = F)
library(mapview, quietly = T, warn.conflicts = F)

# start basemap (note the argument to hide the zoom buttons)
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -121, lat = 41, zoom = 6) %>%
  # setMaxBounds(lng1 = -126, lat1 = 32, lng2 = -116, lat2 = 50) %>% 
  
  # add inset map
  addMiniMap(
    tiles = providers$Esri.OceanBasemap,
    position = 'topright', 
    width = 200, height = 200,
    toggleDisplay = FALSE) %>%
  
  # add graticules with nice labels (recommended for static plot)
  addSimpleGraticule(interval = 2) %>%
  
  # add graticules from a NOAA webserver (recommended for interactive plot)
  # addWMSTiles(
  #   "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
  #   layers = c("1-degree grid", "5-degree grid"),
  #   options = WMSTileOptions(format = "image/png8", transparent = TRUE),
  #   attribution = NULL,group = 'Graticules') %>%
  
  # add points (as circle markers)
  # addCircleMarkers(data = pts, lng = ~geometry(x), lat = ~geometry(y),
  #                  weight = 0.5,
  #                  col = 'black', 
  #                  fillColor = 'darkslategrey',
  #                  radius = 4, 
  #                  fillOpacity = 0.9, 
  #                  stroke = T, 
  #                  label = ~paste0('Point at: ', 
  #                                  as.character(round(lat,3)), ', ', 
  #                                  as.character(round(lon,3))), 
  #                  group = 'Points') %>%
  # 
  # add lines
  # addPolylines(data = lin, ~lon, ~lat,
  #              weight = 3,
  #              color = 'red',
  #              popup = 'This is a line!', 
  #              smoothFactor = 3,
  #              group = 'Lines') %>%
  
  # add polygons
  addPolygons(data=res2,
              weight = 1, 
              color = 'grey', 
              # fillColor = 'grey',
              # fill = T, 
              # fillOpacity = 0.25, 
              stroke = T, 
              dashArray = c(5,5), 
              smoothFactor = 3,
              options = pathOptions(clickable = F),
              group = 'Polygons')

# show map
map

# save map as static image
mapshot(map, file = 'leaflet_map.png')

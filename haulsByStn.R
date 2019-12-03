library(rstudioapi)
library(dplyr)
library(rgeos) # "Geomegry Engine- Open Source (GEOS)"
library(rgdal) # "Geospatial Data Analysis Library (GDAL)"
# library(sp) # superceded by 'sf' package
library(sf)
library(tmap)

# import 'nwfscSurvey' package
library(nwfscSurvey)
vignette("nwfscSurvey")

# Set working directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# import shapefiles
stns <- readOGR("WCGBTS_Grid_v2008_GCSWGS84.shp")

# import haul data
haul_dat = PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange=c(2003,2019))

# convert haul data to point spatial object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
pts <- st_as_sf(x = haul_dat,                         
               coords = c("longitude_dd", "latitude_dd"),
               crs = projcrs)

# convert polygon shapefile to polygon spatial object
stns <- st_as_sf(stns)

# Get start and end years from haul data
yr_srt = substr(as.character(min(pts$trawl_id)),1,4)
yr_end = substr(as.character(max(pts$trawl_id)),1,4)
yr_str = paste0(yr_srt, "_", yr_end)

# spatial join points with polygons
res1 <- st_join(pts, stns) %>% 
  filter(!is.na(CentroidID)) %>% 
  group_by(CentroidID) %>% 
  tally(name = paste0("cntHauls_", yr_str))


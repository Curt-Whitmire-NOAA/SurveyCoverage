library(rstudioapi)
library(tidyr)
library(dplyr)
library(sf)
library(nwfscSurvey)
library(chron)

# Set working directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# import shapefiles
poly <- sf::st_read("BOEM_CA_OR_WEAs_WGS84.shp")
# sf::st_crs(poly)

# import haul data
haul_dat = PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange=c(2003,2019))

# convert haul data to point spatial object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
pts <- st_as_sf(x = haul_dat,                         
               coords = c("longitude_dd", "latitude_dd"),
               crs = st_crs(poly))

# convert polygon shapefile to polygon spatial object
poly <- st_as_sf(poly)

# Get start and end years from haul data
yr_srt = substr(as.character(min(pts$date_yyyymmdd)),1,4)
yr_end = substr(as.character(max(pts$date_yyyymmdd)),1,4)
# pts$date_yyyymmdd <- dates(pts$date_yyyymmdd, format = "ymd") # Proposed by John Wallace; reformat datestr field
yr_str = paste0(yr_srt, "_", yr_end)

# Summarize pts by Area
res1a <- st_join(pts, poly)

st_geometry(res1a) <- NULL

res1a <- res1a %>% 
  mutate(Area_Name = replace_na(Area_Name, "Not BOEM")) %>% 
  mutate(TowsByPer=n()) %>% 
  group_by(Area_Name,TowsByPer) %>% 
  tally(name = "TowsByArea") %>% 
  mutate(pctTows = TowsByArea/TowsByPer*100)

# Summarize pts by Area, Year
res1b <- st_join(pts, poly)

st_geometry(res1b) <- NULL

res1b <- res1b %>% 
  mutate(Area_Name = replace_na(Area_Name, "Not BOEM")) %>% 
  mutate(BOEM = ifelse(Area_Name == "Not BOEM", 0,1)) %>% 
  # group_by(year=substr(date_yyyymmdd,1,4)) %>% 
  group_by(year=years(date_yyyymmdd)) %>% # Proposed by John Wallace
  mutate(TowsByYr=n()) %>% 
  group_by(Area_Name,year,TowsByYr) %>% 
  tally(name = "TowsByAreaYrTemp") %>% 
  mutate(TowsByAreaYr = ifelse(Area_Name == "Not BOEM", TowsByYr-TowsByAreaYrTemp, TowsByAreaYrTemp)) %>% 
  mutate(pctTows = TowsByAreaYr/TowsByYr*100) %>% 
  mutate(Area_Name = replace(Area_Name, Area_Name == "Not BOEM", "All BOEM Areas")) %>% 
  select(!TowsByAreaYrTemp)


# Export to CSV table
library(readr)
outCSV1a = paste0("cntHauls_", yr_str, "byArea_BOEM.csv")
write_csv(res1a, outCSV1a, na = "", append = FALSE)

outCSV1b = paste0("cntHauls_", yr_str, "byAreaYear_BOEM.csv")
write_csv(res1b, outCSV1b, na = "", append = FALSE)


# Create plot
library(ggplot2)

p1dat <- res1b

p1 <- ggplot(p1dat, aes(x = year, y = pctTows, group = Area_Name)) +
  geom_line() +
  facet_wrap(~ Area_Name) +
  scale_x_discrete(breaks = c(2005,2010,2015)) +
  labs(x = "Year", y = "% Coastwide WCGBTS Tows") +
  theme_bw()
p1

# The code below didn't work for me
library(lattice)

dev.new(width = 800, height = 500)
lattice::xyplot(pctTows ~ year | Area_Name, data = p1dat, type = 'o', ylab = 'Percent of Coastwide Hauls', as.table = TRUE)

png('Percent of Coastwide Hauls.png', width = 1200, height = 750)
lattice::xyplot(pctTows ~ year | Area_Name, data = p1dat, type = 'o', ylab = 'Percent of Coastwide Hauls', as.table = TRUE)
dev.off()

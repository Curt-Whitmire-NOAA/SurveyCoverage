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
# poly <- sf::st_read("BOEM_CA_OR_WEAs_WGS84.shp")
poly <- sf::st_read("WCGBTS_Grid_v2008_GCSWGS84.shp")
wea <- sf::st_read("BOEM_CA_OR_WEAs_WGS84.shp")
# sf::st_crs(poly)

# import haul data
haul_dat = PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange=c(2003,2019))

# import catch data
spec_set = c(
  'Anoplopoma fimbria' # sablefish
  ,'Atheresthes stomias' # arrowtooth flounder
  ,'Raja binoculata' # big skate; invalid synonym (Beringraja binoculata)
  ,'Raja rhina' # longnose skate; invalid synonym (Beringraja rhina)
  ,'Eopsetta jordani' # petrale sole
  ,'Glyptocephalus zachirus' # rex sole
  ,'Microstomus pacificus' # Dover sole
  ,'Ophiodon elongatus' # lingcod
  ,'Parophrys vetulus' # English sole
  ,'Sebastes aleutianus' # Rougheye/Blackspotted rockfish
  ,'Sebastes alutus' # Pacific ocean perch
  ,'Sebastes aurora' # aurora rockfish
  ,'Sebastes babcocki' # redbanded rockfish
  ,'Sebastes crameri' # darkblotched rockfish
  ,'Sebastes diploproa' # splitnose rockfish
  ,'Sebastes entomelas' # widow rockfish
  ,'Sebastes flavidus' # yellowtail rockfish
  ,'Sebastes goodei' # chilipepper rockfish
  ,'Sebastes paucispinis' # bocaccio
  ,'Sebastes pinniger' # canary rockfish
  ,'Sebastes ruberrimus' # yelloweye rockfish
  ,'Sebastolobus alascanus' # shortspine thornyhead
  ,'Sebastolobus altivelis' # longspine thornyhead
  ,'Squalus suckleyi' # Pacific spiny dogfish
)
catch_dat = PullCatch.fn(SciName = spec_set, SurveyName = "NWFSC.Combo")

# convert haul data to point spatial object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
haul_pts <-  st_as_sf(x = haul_dat,                         
                  coords = c("longitude_dd", "latitude_dd"),
                  crs = st_crs(poly))
catch_pts <- st_as_sf(x = catch_dat,                         
               coords = c("Longitude_dd", "Latitude_dd"),
               crs = st_crs(poly))

# convert polygon shapefile to polygon spatial object
poly <- st_as_sf(poly)

# Get start and end years from haul data
yr_srt = as.character(min(catch_dat$Year))
yr_end = as.character(max(catch_dat$Year))
# pts$date_yyyymmdd <- dates(pts$date_yyyymmdd, format = "ymd") # Proposed by John Wallace; reformat datestr field
yr_str = paste0(yr_srt, "_", yr_end)

# Summarize Tows by Station
res1a <- st_join(haul_pts, poly)

st_geometry(res1a) <- NULL

towSumm <- res1a %>% 
  mutate(TowsByPer=n()) %>% 
  group_by(CentroidID,TowsByPer) %>% 
  tally(name = "TowsByStn") %>% 
  mutate(pctTows = TowsByStn/TowsByPer*100)

# Summarize catch by Station, Taxa
res1b <- st_join(catch_pts, poly)

st_geometry(res1b) <- NULL

dataStr = "24_GF_spp" # change for chosen spp subset (e.g., All GF)

catch_summ <- res1b %>% 
  mutate(data=dataStr) %>% 
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% 
  filter(!is.na(CentroidID)) %>% 
  # group_by(CentroidID,Scientific_name) %>%
  group_by(data,CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name)
            ,meanCatchWgt = mean(total_catch_wt_kg, na.rm = TRUE)
            ,sumCatchWgt = sum(total_catch_wt_kg, na.rm = TRUE)
            )

# Export to CSV table
library(readr)
outCSV <-  paste0("WCGBTS_", yr_str, "_summ_", dataStr, ".csv")
write_csv(catch_summ, outCSV, na = "", append = FALSE)

# Create plot
library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# create points for state labels
library(maps)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
state_list <- c('California', 'Oregon', 'Washington')
states <- states %>% filter(ID == 'Oregon')
states <- cbind(states, st_coordinates(st_centroid(states, of_largest_polygon = TRUE)))
wea <- wea %>% filter(Area_Name == 'Coos Bay')

library(tools)
states$ID <- toTitleCase(states$ID)
head(states)

# join results to polygon shape
p1shp = left_join(poly, catch_summ, by = c("CentroidID"))
stns <- cbind(p1shp, st_coordinates(st_centroid(p1shp, of_largest_polygon = TRUE)))

# ggplot template
# ggplot(a, aes(x = X, y = Y))+geom_text(aes(label = VAL)) # for this purpose, label = numTows OR label = sppRich

sf_use_s2(FALSE)
# my_breaks <- c(0, 0.1, 1, 10, 100, 1000)
my_breaks <- c(0.1, 1, 10, 100, 1000)
# my_breaks <- c(0, 29, 80, 252, 930, 1000)
p1 <- ggplot(date = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_label(data = stns, aes(x=X, y=Y, label = numTows)) +
  geom_sf(data = p1shp, aes(fill = meanCatchWgt)) +
  # scale_fill_continuous(values=rev(brewer.pal(7, "YlGnBu")), na.value="grey90")+
  # scale_fill_gradient(low = "#132B43",
  #                     high = "#56B1F7",
  #                     space = "Lab",
  #                     na.value = "grey90",
  #                     guide = "colourbar",
  #                     aesthetics = "fill") +
  # scale_fill_viridis_c(trans = "log", alpha = .4, na.value="grey90") +
  scale_fill_viridis_c(breaks = my_breaks, labels = my_breaks
                       ,trans = scales::pseudo_log_trans(sigma = 0.1)
                       , direction = -1, alpha = .8, na.value="grey90", name = "Weight (kg)") +
  geom_sf(data = wea, fill = NA, color = "darkred") +
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-125.4, -124), ylim = c(43.0, 44.2), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("WCGBTS (2003-19)", subtitle = "(Mean catch weight per Survey Cell for 24 select groundfish taxa)") +
  theme(legend.justification=c(1,0), legend.position=c(1,0))
p1

# export plot

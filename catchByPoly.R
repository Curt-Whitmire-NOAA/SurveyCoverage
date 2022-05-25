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
wea <- sf::st_read("BOEM_CA_OR_WEAs_WGS84.shp", query = "SELECT * FROM \"BOEM_CA_OR_WEAs_WGS84\" WHERE Area_Name IN ('Coos Bay','Brookings')")
# sf::st_crs(poly)

# # import haul data
# haul_dat = PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange=c(2003,2019))
# 
# # Filter haul data geographically
# haul_dat <- haul_dat %>% 
#   filter(latitude_dd >= 42.0 & latitude_dd <= 46.25) # filter geographically

# import catch data
load("SPECIES.FMP.Updated with Sci Name and SPID, 24 May 2022.RData")
spec_set_first50 <- head(SPECIES.FMP.Updated$Scientific_Name,50) # for ALL groundfish FMP species
spec_set_last42 <- tail(SPECIES.FMP.Updated$Scientific_Name,42) # for ALL groundfish FMP species
# spec_set_sub <- SPECIES.FMP.Updated$Scientific_Name[35:65] # subset of groundfish FMP species
spec_set_22 = c(
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
  # ,'Sebastes goodei' # chilipepper rockfish
  # ,'Sebastes paucispinis' # bocaccio
  ,'Sebastes pinniger' # canary rockfish
  ,'Sebastes ruberrimus' # yelloweye rockfish
  ,'Sebastolobus alascanus' # shortspine thornyhead
  ,'Sebastolobus altivelis' # longspine thornyhead
  ,'Squalus suckleyi' # Pacific spiny dogfish
)

# dataStr = "22_GF_spp" # change for chosen spp subset (e.g., All GF)
dataStr_22 = paste0(as.character(length(spec_set_22)), "_GF_spp")
dataStr_ALL = paste0(as.character(length(spec_set_first50)+length(spec_set_last42)), "_GF_spp")

catch_dat_22 = PullCatch.fn(SciName = spec_set_22, SurveyName = "NWFSC.Combo")
catch_dat_first50 = PullCatch.fn(SciName = spec_set_first50, SurveyName = "NWFSC.Combo")
catch_dat_last42 = PullCatch.fn(SciName = spec_set_last42, SurveyName = "NWFSC.Combo")
catch_dat_ALL <-  rbind(catch_dat_first50, catch_dat_last42)

# Explore species distributions by depth and latitude, for subset of species
spp_dist_22 <- catch_dat_22 %>% 
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>%
  group_by(Scientific_name) %>% 
  summarize(dep_min = min(Depth_m)
         ,dep_max = max(Depth_m)
         ,lat_min = min(Latitude_dd)
         ,lat_max = max(Latitude_dd)
         ) %>% 
  mutate(dataStr=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp"))

# Explore species distributions by depth and latitude, for All groundfish species
spp_dist_ALL <- catch_dat_ALL %>% 
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>%
  group_by(Scientific_name) %>% 
  summarize(dep_min = min(Depth_m)
            ,dep_max = max(Depth_m)
            ,lat_min = min(Latitude_dd)
            ,lat_max = max(Latitude_dd)
  ) %>% 
  mutate(dataStr=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp"))

# Filter catch data geographically
catch_dat_22 <- catch_dat_22 %>% 
  filter(Latitude_dd >= 42.0 & Latitude_dd <= 46.25)
catch_dat_ALL <- catch_dat_ALL %>% 
  filter(Latitude_dd >= 42.0 & Latitude_dd <= 46.25)

# convert haul data to point spatial object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# haul_pts <-  st_as_sf(x = haul_dat,                         
#                   coords = c("longitude_dd", "latitude_dd"),
#                   crs = st_crs(poly))
catch_pts_22 <- st_as_sf(x = catch_dat_22,                         
               coords = c("Longitude_dd", "Latitude_dd"),
               crs = st_crs(poly))
catch_pts_ALL <- st_as_sf(x = catch_dat_ALL,                         
                         coords = c("Longitude_dd", "Latitude_dd"),
                         crs = st_crs(poly))

# convert polygon shapefiles to polygon spatial objects
poly <- st_as_sf(poly)
wea <- st_as_sf(wea)

# Get start and end years from haul data
yr_srt = as.character(min(catch_dat_22$Year))
yr_end = as.character(max(catch_dat_22$Year))
yr_str = paste0(yr_srt, "_", yr_end)

# Summarize Tows by Station
res1a <- st_join(haul_pts, poly)

st_geometry(res1a) <- NULL

towSumm <- res1a %>% 
  mutate(TowsByPer=n()) %>% 
  group_by(CentroidID,TowsByPer) %>% 
  tally(name = "TowsByStn") %>% 
  mutate(pctTows = TowsByStn/TowsByPer*100)

# Spatially join Catch data by station set
res1b <- st_join(catch_pts_22, poly)
res1c <- st_join(catch_pts_ALL, poly)

# Dissolve WEAs into a single extent
wea <- st_union(wea)

# Clip results to WEAs extent
res1b <- st_intersection(res1b, wea)
numSpp_1b <- n_distinct(res1b$Scientific_name)
res1c <- st_intersection(res1c, wea)
numSpp_1c <- n_distinct(res1c$Scientific_name)

# remove the geometry field
st_geometry(res1b) <- NULL
st_geometry(res1c) <- NULL

library(IRanges)
ir1 = with(spp_dist, IRanges(dep_min, dep_max))
ir2 = with(res1b, IRanges(Min_Dep_m, Max_Dep_m))
# df1$overlap = countOverlaps(ir1, ir2) != 0

# Develop logic for calculating total number of possible species based on recorded depth - GETTING CLOSER
test <- res1b %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>% 
  filter(!is.na(CentroidID)) %>%
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID) %>%
  summarize(overlap = countOverlaps( ir1,IRanges(Depth_m,Depth_m)) != 0) # use sapply to compare line by line?

# Calculate average and normalized CPUE by survey station
catch_summ_wgt_22 <- res1b %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>% 
  filter(!is.na(CentroidID)) %>%
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE)
            ,avgCatchWgt = mean(cpue_kg_km2, na.rm = TRUE)
            ) %>% 
  mutate(avgCatchWgt_norm = ( avgCatchWgt - min(avgCatchWgt) ) / ( max(avgCatchWgt) - min(avgCatchWgt) )
         )

# Calculate basic and proportional species richness by survey station
catch_summ_div_22 <- res1b %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>% 
  filter(!is.na(CentroidID)) %>%
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name) # calculate species richness for each cell
            ,sppRich_prop = sppRich / numSpp_1b ) # calculate proportion of taxa caught in each cell

# Calculate average and normalized CPUE by survey station
catch_summ_wgt_ALL <- res1c %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>% 
  filter(!is.na(CentroidID)) %>%
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE)
            ,avgCatchWgt = mean(cpue_kg_km2, na.rm = TRUE)
  ) %>% 
  mutate(avgCatchWgt_norm = ( avgCatchWgt - min(avgCatchWgt) ) / ( max(avgCatchWgt) - min(avgCatchWgt) )
  )

# Calculate basic and proportional species richness by survey station
catch_summ_div_ALL <- res1c %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>% 
  filter(!is.na(CentroidID)) %>%
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name) # calculate species richness for each cell
            ,sppRich_prop = sppRich / numSpp_1c ) # calculate proportion of taxa caught in each cell

# Join datasets
catch_summ_22 <- inner_join(catch_summ_wgt_22, catch_summ_div_22, by = c("data","CentroidID")) %>% 
  mutate(numTows_22 = numTows.x
         ,avgCatchWgt_norm_22 = avgCatchWgt_norm
         ,sppRich_prop_22 = sppRich_prop) %>% 
  select(data, CentroidID, numTows_22, sppRich_prop_22, avgCatchWgt_norm_22)

catch_summ_ALL <- inner_join(catch_summ_wgt_ALL, catch_summ_div_ALL, by = c("data","CentroidID")) %>% 
  mutate(numTows_ALL = numTows.x
         ,avgCatchWgt_norm_ALL = avgCatchWgt_norm
         ,sppRich_prop_ALL = sppRich_prop) %>% 
  select(data, CentroidID, numTows_ALL, sppRich_prop_ALL, avgCatchWgt_norm_ALL)

# Final join of data
catch_summ <- inner_join(catch_summ_22, catch_summ_ALL, by = "CentroidID") %>% 
  mutate(val_WCGBTS = sppRich_prop_22 + avgCatchWgt_norm_22 + avgCatchWgt_norm_ALL) %>% 
  select(CentroidID, numTows_ALL, sppRich_prop_22, avgCatchWgt_norm_22, sppRich_prop_ALL, avgCatchWgt_norm_ALL, val_WCGBTS)

# Export to CSV table
library(readr)
outCSV <-  paste0("WCGBTS_", yr_str, "_summ_Value", ".csv")
write_csv(catch_summ, outCSV, na = "", append = FALSE)

# Plotting
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
states <- states %>% filter(ID == 'oregon')
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
# my_breaks <- c(0.1, 1, 10, 100, 1000)
# my_breaks <- c(0, 29, 80, 252, 930, 1000)
# my_breaks <- c(0.001, 0.01, 1)
p1 <- ggplot(date = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_label(data = stns, aes(x=X, y=Y, label = numTows)) +
  geom_sf(data = p1shp, aes(fill = avgCatchWgt_norm)) +
  # scale_fill_continuous(values=rev(brewer.pal(7, "YlGnBu")), na.value="grey90")+
  # scale_fill_gradient(low = "#132B43",
  #                     high = "#56B1F7",
  #                     space = "Lab",
  #                     na.value = "grey90",
  #                     guide = "colourbar",
  #                     aesthetics = "fill") +
  # scale_fill_viridis_c(trans = "log", alpha = .4, na.value="grey90") +
  # scale_fill_viridis_c(breaks = my_breaks, labels = my_breaks
  #                      ,trans = scales::pseudo_log_trans(sigma = 0.1)
  #                      , direction = -1, alpha = .8, na.value="grey90", name = "Weight (kg)") +
  scale_fill_viridis_c(direction = -1, alpha = .8, na.value="grey90", name = "Weight (kg)") +
  geom_sf(data = wea, fill = NA, color = "darkred") +
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-125.4, -124), ylim = c(43.0, 44.2), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("WCGBTS (2003-19)", subtitle = paste0("(Normalized average CPUE per Survey Cell for ", length(spec_set), " select groundfish taxa)")) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))
p1

# export plot

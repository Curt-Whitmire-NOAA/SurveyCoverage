library(tidyr)
library(dplyr)
# library(chron) # recommended by John Wallace; didn't use here

# Set working directory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# import shapefiles
library(sf)
poly <- sf::st_read("WCGBTS_Grid_v2008_GCSWGS84.shp")
wea <- sf::st_read("BOEM_CA_OR_WEAs_WGS84.shp", query = "SELECT * FROM \"BOEM_CA_OR_WEAs_WGS84\" WHERE Area_Name IN ('Coos Bay','Brookings')")
# sf::st_crs(poly)

# convert polygon shapefiles to polygon spatial objects
poly <- st_as_sf(poly)
wea <- st_as_sf(wea)

# Dissolve WEAs into a single extent
wea <- st_union(wea)

# Designate species subsets
load("SPECIES.FMP.Updated with Sci Name and SPID, 24 May 2022.RData")
spec_set_first50 <- head(SPECIES.FMP.Updated$Scientific_Name,50) # for ALL groundfish FMP species
spec_set_last42 <- tail(SPECIES.FMP.Updated$Scientific_Name,42) # for ALL groundfish FMP species
spec_set_08 <- c(
  'Anoplopoma fimbria' # sablefish
  ,'Microstomus pacificus' # Dover sole
  ,'Raja rhina' # longnose skate; invalid synonym (Beringraja rhina)
  ,'Ophiodon elongatus' # lingcod
  ,'Sebastolobus alascanus' # shortspine thornyhead
  ,'Sebastolobus altivelis' # longspine thornyhead
  ,'Sebastes aleutianus' # Rougheye/Blackspotted rockfish
  ,'Sebastes crameri' # darkblotched rockfish
)
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

# Pull catch data from FRAM Data Warehouse using {nwfscSurvey}
library(nwfscSurvey)
catch_dat_08 <- PullCatch.fn(SciName = spec_set_08, SurveyName = "NWFSC.Combo") # for 7 select species
catch_dat_22 <- PullCatch.fn(SciName = spec_set_22, SurveyName = "NWFSC.Combo") # for 22 select species
catch_dat_first50 <- PullCatch.fn(SciName = spec_set_first50, SurveyName = "NWFSC.Combo") # for All groundfish species, set 1
catch_dat_last42 <- PullCatch.fn(SciName = spec_set_last42, SurveyName = "NWFSC.Combo") # for All groundfish species, set 2
catch_dat_80 <- rbind(catch_dat_first50, catch_dat_last42) # for All groundfish species, combiined

# Function to count # of potential species within each survey cell
# function needs to iterate through list of species and min/max depths, then
# count if the cell depth range overlaps the species depth range, then
# move on to the next species and so forth
# The total count returned should = the # of species with overlapping depth distributions
# Use {IRanges}
# See https://stackoverflow.com/questions/26905601/comparing-and-finding-overlap-range-in-r

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

# Create depth range classes for species and cells
library(IRanges)
ir1 = with(poly, IRanges(Min_Dep_m, Max_Dep_m)) # declare range for cell depths
ir2 = with(spp_dist_22, IRanges(dep_min, dep_max)) # declare range for species depth distributions
poly$numSpp22 = countOverlaps(ir1, ir2) # calculates how many species in the set overalp the cell depth range

# Explore species distributions by depth and latitude, for All groundfish species
spp_dist_80 <- catch_dat_80 %>% 
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>%
  group_by(Scientific_name) %>% 
  summarize(dep_min = min(Depth_m)
            ,dep_max = max(Depth_m)
            ,lat_min = min(Latitude_dd)
            ,lat_max = max(Latitude_dd)
  ) %>% 
  mutate(dataStr=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp"))

# Create depth range classes for species and cells
library(IRanges)
ir1 = with(poly, IRanges(Min_Dep_m, Max_Dep_m)) # declare range for cell depths
ir2 = with(spp_dist_80, IRanges(dep_min, dep_max)) # declare range for species depth distributions
poly$numSpp80 = countOverlaps(ir1, ir2) # calculates how many species in the set overalp the cell depth range

# Filter catch data geographically; this is now accomplished via a spatial intersect
# catch_dat_08 <- catch_dat_08 %>% 
#   filter(Latitude_dd >= 42.0 & Latitude_dd <= 46.25)
# catch_dat_22 <- catch_dat_22 %>% 
#   filter(Latitude_dd >= 42.0 & Latitude_dd <= 46.25)
# catch_dat_80 <- catch_dat_80 %>% 
#   filter(Latitude_dd >= 42.0 & Latitude_dd <= 46.25)

# convert haul data to point spatial object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

catch_pts_08 <- st_as_sf(x = catch_dat_08,                         
                        coords = c("Longitude_dd", "Latitude_dd"),
                        crs = st_crs(poly))
catch_pts_22 <- st_as_sf(x = catch_dat_22,                         
               coords = c("Longitude_dd", "Latitude_dd"),
               crs = st_crs(poly))
catch_pts_80 <- st_as_sf(x = catch_dat_80,                         
                         coords = c("Longitude_dd", "Latitude_dd"),
                         crs = st_crs(poly))

# Get start and end years from catch data set
yr_srt = as.character(min(catch_dat_80$Year))
yr_end = as.character(max(catch_dat_80$Year))
yr_str = paste0(yr_srt, "_", yr_end)

# Spatially join each Catch data set by station set
sj08spp <- st_join(catch_pts_08, poly)
sj22spp <- st_join(catch_pts_22, poly)
sj80Spp <- st_join(catch_pts_80, poly)

# Clip results to WEAs extent
sj08spp <- st_intersection(sj08spp, wea)
# numSpp_1b <- n_distinct(res1b$Scientific_name) # for testing purposes; # of unique species within data ran
sj22spp <- st_intersection(sj22spp, wea)
# numSpp_1b <- n_distinct(res1b$Scientific_name) # for testing purposes; # of unique species within data range
sj80Spp <- st_intersection(sj80Spp, wea)
# numSpp_1c <- n_distinct(res1c$Scientific_name) # for testing purposes; # of unique species within data range

# remove the geometry field
st_geometry(sj08spp) <- NULL
st_geometry(sj22spp) <- NULL
st_geometry(sj80Spp) <- NULL

# Calculate average and normalized CPUE by survey station
catch_summ_08_wgt <- sj08spp %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>%  # add data set unique identifier string
  filter(!is.na(CentroidID)) %>%
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE) # calculate total weight for each cell
  ) %>% 
  mutate(avgCatchWgt = sumCatchWgt / numTows ) %>% # calculate average weight for each cell
  mutate(avgCatchWgt_norm = ( avgCatchWgt - min(avgCatchWgt) ) / ( max(avgCatchWgt) - min(avgCatchWgt) )) # normalized

# Calculate average and normalized CPUE by survey station
catch_summ_22_wgt <- sj22spp %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>%  # add data set unique identifier string
  filter(!is.na(CentroidID)) %>%
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE) # calculate total weight for each cell
            ) %>% 
  mutate(avgCatchWgt = sumCatchWgt / numTows ) %>% # calculate average weight for each cell
  mutate(avgCatchWgt_norm = ( avgCatchWgt - min(avgCatchWgt) ) / ( max(avgCatchWgt) - min(avgCatchWgt) )) # normalized

# Calculate basic, proportional, and normalized species richness by survey station
catch_summ_22_div <- sj22spp %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>%  # add data set unique identifier string
  filter(!is.na(CentroidID)) %>%
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID, numSpp22) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name) # calculate species richness for each cell
            ) %>% 
  mutate(sppRich_prop = sppRich / numSpp22) %>% # calculate proportion of taxa caught in each cell
  ungroup() %>% # ungroup to calcualte min and max values on entire data range
  mutate(sppRich_norm = ( sppRich_prop - min(sppRich_prop) ) / ( max(sppRich_prop) - min(sppRich_prop) )) # normalized

# Calculate average and normalized CPUE by survey station
catch_summ_80_wgt <- sj80Spp %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>%  # add ddata set unique identifier string
  filter(!is.na(CentroidID)) %>%
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE) # calculate total weight for each cell
            ) %>% 
  mutate(avgCatchWgt = sumCatchWgt / numTows ) %>% # calculate average weight for each cell
  mutate(avgCatchWgt_norm = ( avgCatchWgt - min(avgCatchWgt) ) / ( max(avgCatchWgt) - min(avgCatchWgt) )) # normalized

# Calculate basic, proportional, and normalized species richness by survey station
catch_summ_80_div <- sj80Spp %>% 
  mutate(data=paste0(as.character(n_distinct(Scientific_name)), "_GF_spp")) %>% # add data set unique identifier string
  filter(!is.na(CentroidID)) %>%
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID, numSpp80) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name) # calculate species richness for each cell
            ) %>% 
  mutate(sppRich_prop = sppRich / numSpp80) %>%  # calculate proportion of taxa caught in each cell
  ungroup() %>% # ungroup to calcualte min and max values on entire data range
  mutate(sppRich_norm = ( sppRich_prop - min(sppRich_prop) ) / ( max(sppRich_prop) - min(sppRich_prop) )) # normalized

# Join datasets
catch_summ_22 <- inner_join(catch_summ_22_wgt, catch_summ_22_div, by = c("data","CentroidID")) %>% 
  mutate(numTows_22 = numTows.x
         ,avgCatchWgt_22_norm = avgCatchWgt_norm
         ,sppRich_22_norm = sppRich_norm) %>% 
  select(data, CentroidID, numTows_22, sppRich_22_norm, avgCatchWgt_22_norm)

catch_summ_80 <- inner_join(catch_summ_80_wgt, catch_summ_80_div, by = c("data","CentroidID")) %>% 
  mutate(numTows_80 = numTows.x
         ,avgCatchWgt_80_norm = avgCatchWgt_norm
         ,sppRich_80_norm = sppRich_norm) %>% 
  select(data, CentroidID, numTows_80, sppRich_80_norm, avgCatchWgt_80_norm)

# Final joins of data
catch_summ_22_80 <- inner_join(catch_summ_22, catch_summ_80, by = "CentroidID") %>% 
  mutate(val_WCGBTS = sppRich_80_norm + avgCatchWgt_22_norm + avgCatchWgt_80_norm) %>% 
  select(CentroidID, numTows_80, sppRich_80_norm, avgCatchWgt_22_norm, avgCatchWgt_80_norm, val_WCGBTS)

catch_summ <- inner_join(catch_summ_08_wgt, catch_summ_22_80, by = "CentroidID") %>% 
  mutate(avgCatchWgt_08_norm = avgCatchWgt_norm) %>% 
  mutate(val_WCGBTS = sppRich_80_norm + avgCatchWgt_08_norm + avgCatchWgt_22_norm + avgCatchWgt_80_norm) %>% 
  select(CentroidID, numTows_80, sppRich_80_norm, avgCatchWgt_08_norm, avgCatchWgt_22_norm, avgCatchWgt_80_norm, val_WCGBTS)

# QC check for normalized data
r1 <- range(min(catch_summ$sppRich_80_norm),max(catch_summ$sppRich_80_norm))
r2 <- range(min(catch_summ$avgCatchWgt_08_norm),max(catch_summ$avgCatchWgt_08_norm))
r3 <- range(min(catch_summ$avgCatchWgt_22_norm),max(catch_summ$avgCatchWgt_22_norm))
r4 <- range(min(catch_summ$avgCatchWgt_80_norm),max(catch_summ$avgCatchWgt_80_norm))
# r5 <- range(min(catch_summ$trawl_norm),max(catch_summ$trawl_norm))
r6 <- range(min(catch_summ$val_WCGBTS),max(catch_summ$val_WCGBTS))

# Export catch summary to CSV table
library(readr)
outCSV <-  paste0("WCGBTS_", yr_str, "_summ_Value", ".csv")
write_csv(catch_summ, outCSV, na = "", append = FALSE)

# Prepare layers for plotting
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Create points for state labels
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

library(ggplot2)
library(RColorBrewer)
library(ggspatial)
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

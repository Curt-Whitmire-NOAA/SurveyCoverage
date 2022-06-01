library(tidyr)
library(dplyr)
library(tidyverse)

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

# Dissolve WEAs into a single extent
wea <- st_union(wea)

# Select survey cells that intersect WEAs
poly <- poly %>% 
  mutate(wea_isct = lengths(st_intersects(poly, wea))) %>% 
  filter(wea_isct == 1) # filters out non-intersecting cells

# Designate species subsets
load("SPECIES.FMP.Updated with Sci Name and SPID, 24 May 2022.RData")
spec_set_first50 <- head(SPECIES.FMP.Updated$Scientific_Name,50) # for ALL groundfish FMP species
spec_set_last42 <- tail(SPECIES.FMP.Updated$Scientific_Name,42) # for ALL groundfish FMP species
spec_set_last42 <- spec_set_last42[spec_set_last42 != "Merluccius productus"] # exclude hake
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
spp_dist_08 <- catch_dat_08 %>% 
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
ir2 = with(spp_dist_08, IRanges(dep_min, dep_max)) # declare range for species depth distributions
poly$numSpp08 = countOverlaps(ir1, ir2) # calculates how many species in the set overalp the cell depth range

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
sj08spp <- st_join(poly, catch_pts_08, left = TRUE)
sj22spp <- st_join(poly, catch_pts_22, left = TRUE)
sj80Spp <- st_join(poly, catch_pts_80, left = TRUE)

# For testing purposes
numSpp_08 <- n_distinct(sj08spp$Scientific_name, na.rm = TRUE) # for testing purposes; # of unique species within data range
numSpp_22 <- n_distinct(sj22spp$Scientific_name, na.rm = TRUE) # for testing purposes; # of unique species within data range
numSpp_80 <- n_distinct(sj80Spp$Scientific_name, na.rm = TRUE) # for testing purposes; # of unique species within data range

# remove the geometry field
st_geometry(sj08spp) <- NULL
st_geometry(sj22spp) <- NULL
st_geometry(sj80Spp) <- NULL

# Calculate average and normalized CPUE by survey station
catch_summ_08_wgt <- sj08spp %>% 
  filter(!is.na(Trawl_id)) %>%
  mutate(data=paste0(as.character(n_distinct(Scientific_name, na.rm = TRUE)), "_GF_spp")) %>%  # add data set unique identifier string
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE) # calculate total weight for each cell
  ) %>% 
  ungroup() %>%
  mutate(avgCatchWgt = sumCatchWgt / numTows ) %>% # calculate average weight for each cell
  mutate(lnCatchWgt = log(avgCatchWgt)) %>% # ln transform the average catch data
  mutate(avgCatchWgt_norm = ( lnCatchWgt - min(lnCatchWgt) ) / ( max(lnCatchWgt) - min(lnCatchWgt) )) # normalized

# Calculate basic, proportional, and normalized species richness by survey station
catch_summ_08_div <- sj08spp %>% 
  filter(!is.na(Trawl_id)) %>%
  mutate(data=paste0(as.character(n_distinct(Scientific_name, na.rm = TRUE)), "_GF_spp")) %>%  # add data set unique identifier string
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID, DepStrata2, numSpp08) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name, na.rm = TRUE) # calculate species richness for each cell
  ) %>% 
  mutate(sppRich_prop = sppRich / numSpp08) %>% # calculate proportion of taxa caught in each cell
  ungroup() %>% # ungroup to calcualte min and max values on entire data range
  group_by(DepStrata2) %>% # group to normalize richness data by depth strata
  mutate(sppRich_norm = ( sppRich_prop - min(sppRich_prop) ) / ( max(sppRich_prop) - min(sppRich_prop) )) # normalized

# Calculate average and normalized CPUE by survey station
catch_summ_22_wgt <- sj22spp %>% 
  filter(!is.na(Trawl_id)) %>%
  mutate(data=paste0(as.character(n_distinct(Scientific_name, na.rm = TRUE)), "_GF_spp")) %>%  # add data set unique identifier string
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE) # calculate total weight for each cell
            ) %>% 
  ungroup() %>%
  mutate(avgCatchWgt = sumCatchWgt / numTows ) %>% # calculate average weight for each cell
  mutate(lnCatchWgt = log(avgCatchWgt)) %>% # ln transform the average catch data
  mutate(avgCatchWgt_norm = ( lnCatchWgt - min(lnCatchWgt) ) / ( max(lnCatchWgt) - min(lnCatchWgt) )) # normalized

# Calculate basic, proportional, and normalized species richness by survey station
catch_summ_22_div <- sj22spp %>% 
  filter(!is.na(Trawl_id)) %>%
  mutate(data=paste0(as.character(n_distinct(Scientific_name, na.rm = TRUE)), "_GF_spp")) %>%  # add data set unique identifier string
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID, DepStrata2, numSpp22) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name, na.rm = TRUE) # calculate species richness for each cell
            ) %>% 
  mutate(sppRich_prop = sppRich / numSpp22) %>% # calculate proportion of taxa caught in each cell
  ungroup() %>% # ungroup to calcualte min and max values on entire data range
  group_by(DepStrata2) %>% # group to normalize richness data by depth strata
  mutate(sppRich_norm = ( sppRich_prop - min(sppRich_prop) ) / ( max(sppRich_prop) - min(sppRich_prop) )) # normalized

# Calculate average and normalized CPUE by survey station
catch_summ_80_wgt <- sj80Spp %>% 
  filter(!is.na(Trawl_id)) %>%
  mutate(data=paste0(as.character(n_distinct(Scientific_name, na.rm = TRUE)), "_GF_spp")) %>%  # add ddata set unique identifier string
  group_by(data, CentroidID) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sumCatchWgt = sum(cpue_kg_km2, na.rm = TRUE) # calculate total weight for each cell
            ) %>% 
  ungroup() %>%
  mutate(avgCatchWgt = sumCatchWgt / numTows ) %>% # calculate average weight for each cell
  mutate(lnCatchWgt = log(avgCatchWgt)) %>% # ln transform the average catch data
  mutate(avgCatchWgt_norm = ( lnCatchWgt - min(lnCatchWgt) ) / ( max(lnCatchWgt) - min(lnCatchWgt) )) # normalized

# Calculate basic, proportional, and normalized species richness by survey station
catch_summ_80_div <- sj80Spp %>% 
  filter(!is.na(Trawl_id)) %>%
  mutate(data=paste0(as.character(n_distinct(Scientific_name, na.rm = TRUE)), "_GF_spp")) %>% # add data set unique identifier string
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID, DepStrata2, numSpp80) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name, na.rm = TRUE) # calculate species richness for each cell
            ) %>% 
  mutate(sppRich_prop = sppRich / numSpp80) %>%  # calculate proportion of taxa caught in each cell
  ungroup() %>% # ungroup to calcualte min and max values on entire data range
  group_by(DepStrata2) %>%  # group to normalize richness data by depth strata
  mutate(sppRich_norm = ( sppRich_prop - min(sppRich_prop) ) / ( max(sppRich_prop) - min(sppRich_prop) )) # normalized

# Join data summaries
catch_summ_08 <- full_join(catch_summ_08_wgt, catch_summ_08_div, by = "CentroidID") %>% 
  mutate(numTows_08 = numTows.x
         ,sppRich_08 = sppRich
         ,sppRich_08_norm = sppRich_norm
         ,avgCatchWgt_08 = avgCatchWgt
         ,avgCatchWgt_08_norm = avgCatchWgt_norm) %>% 
  select(CentroidID, DepStrata2, numTows_08
         ,sppRich_08, numSpp08, sppRich_08_norm, avgCatchWgt_08, avgCatchWgt_08_norm)

catch_summ_22 <- full_join(catch_summ_22_wgt, catch_summ_22_div, by = "CentroidID") %>% 
  mutate(numTows_22 = numTows.x
         ,sppRich_22 = sppRich
         ,sppRich_22_norm = sppRich_norm
         ,avgCatchWgt_22 = avgCatchWgt
         ,avgCatchWgt_22_norm = avgCatchWgt_norm) %>% 
  select(CentroidID, DepStrata2, numTows_22
         ,sppRich_22, numSpp22, sppRich_22_norm, avgCatchWgt_22, avgCatchWgt_22_norm)

catch_summ_80 <- full_join(catch_summ_80_wgt, catch_summ_80_div, by = "CentroidID") %>% 
  mutate(numTows_80 = numTows.x
         ,sppRich_80 = sppRich
         ,sppRich_80_norm = sppRich_norm
         ,avgCatchWgt_80 = avgCatchWgt
         ,avgCatchWgt_80_norm = avgCatchWgt_norm) %>% 
  select(CentroidID, DepStrata2, numTows_80
         ,sppRich_80, numSpp80, sppRich_80_norm, avgCatchWgt_80, avgCatchWgt_80_norm)

catch_summ_22_80 <- full_join(catch_summ_22, catch_summ_80, by = "CentroidID") %>% 
  mutate(DepStrat = DepStrata2.x) %>% 
  select(CentroidID, DepStrat, numTows_80
         ,sppRich_22, numSpp22, sppRich_22_norm, avgCatchWgt_22, avgCatchWgt_22_norm
         ,sppRich_80, numSpp80, sppRich_80_norm, avgCatchWgt_80, avgCatchWgt_80_norm)

# Final join of data summaries
catch_summ <- full_join(catch_summ_08, catch_summ_22_80, by = "CentroidID") %>% 
  mutate(numTows = numTows_80) %>% 
  left_join(juvenile.groundfish.to.WCGFBT.grid.data, by = "CentroidID") %>% 
  mutate(juv_all_spp_nona = ifelse(is.na(Norm_ALL_SPECIES), 0, Norm_ALL_SPECIES) ) %>% # convert unmatched cells to 0 value
  mutate(juv_norm = ( juv_all_spp_nona - min(juv_all_spp_nona) ) / ( max(juv_all_spp_nona) - min(juv_all_spp_nona) )) %>% # normalize
  mutate(trwluntrwl = 1) %>%   # categorical value for survey trawlability
  mutate(val_raw = sppRich_80_norm + avgCatchWgt_08_norm + avgCatchWgt_22_norm + avgCatchWgt_80_norm + juv_norm) %>% # raw additive value
  mutate(val_norm = ( val_raw - min(val_raw) ) / ( max(val_raw) - min(val_raw)) ) %>% # normalize
  select(CentroidID, DepStrat, numTows
         ,sppRich_08, numSpp08, sppRich_08_norm, avgCatchWgt_08, avgCatchWgt_08_norm
         ,sppRich_22, numSpp22, sppRich_22_norm, avgCatchWgt_22, avgCatchWgt_22_norm
         ,sppRich_80, numSpp80, sppRich_80_norm, avgCatchWgt_80, avgCatchWgt_80_norm
         ,juv_norm, val_raw, val_norm, trwluntrwl)

# QC check for normalized data
r1 <- range(min(catch_summ$sppRich_80_norm),max(catch_summ$sppRich_08_norm))
r2 <- range(min(catch_summ$sppRich_80_norm),max(catch_summ$sppRich_22_norm))
r3 <- range(min(catch_summ$sppRich_80_norm),max(catch_summ$sppRich_80_norm))
r4 <- range(min(catch_summ$avgCatchWgt_08_norm),max(catch_summ$avgCatchWgt_08_norm))
r5 <- range(min(catch_summ$avgCatchWgt_22_norm),max(catch_summ$avgCatchWgt_22_norm))
r6 <- range(min(catch_summ$avgCatchWgt_80_norm),max(catch_summ$avgCatchWgt_80_norm))
r7 <- range(min(catch_summ$juv_norm),max(catch_summ$juv_norm))
r8 <- range(min(catch_summ$val_raw),max(catch_summ$val_raw))
r9 <- range(min(catch_summ$val_norm),max(catch_summ$val_norm))

# Export catch summary output including shapefile and CSV table
outFile <-  paste0("WCGBTS_", yr_str, "_summ_Value")
# Join catch summary data frame to station polygon shapfile
shp <- right_join(poly, catch_summ, by = "CentroidID") %>% 
  mutate(numSpp08 = numSpp08.x
         ,numSpp22 = numSpp22.x
         ,numSpp80 = numSpp80.x) %>% 
  select(CentroidID, DepStrat, numTows
         ,sppRich_08, numSpp08, sppRich_08_norm, avgCatchWgt_08, avgCatchWgt_08_norm
         ,sppRich_22, numSpp22, sppRich_22_norm, avgCatchWgt_22, avgCatchWgt_22_norm
         ,sppRich_80, numSpp80, sppRich_80_norm, avgCatchWgt_80, avgCatchWgt_80_norm
         ,juv_norm, val_raw, val_norm, trwluntrwl) # include only specified variables
# Export catch summary to shapefile
outSHP <- st_write(shp, paste0("shapefiles/", outFile, ".shp"), append = FALSE) # write sf object to shapefile format
# Export catch summary to CSV
library(readr)
write_csv(catch_summ, paste0(outFile, ".csv"), na = "", append = FALSE)

# Prepare layers for plotting
# Workflow based on 25 Oct 2018 blog article from Mel Moreno and Mathieu Basille
# Source: https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
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

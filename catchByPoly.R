library(tidyr)
library(dplyr)
library(tidyverse)

# Set working directory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
dir <- setwd(dirname(current_path))
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
spec_set_last42 <- spec_set_last42[spec_set_last42 != "Merluccius productus"] # exclude Pacific hake
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
catch_dat_80 <- rbind(catch_dat_first50, catch_dat_last42) # for All groundfish species, combined

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
poly$numSpp08 = countOverlaps(ir1, ir2) # calculates how many species in the set overlap the cell depth range

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
poly$numSpp22 = countOverlaps(ir1, ir2) # calculates how many species in the set overlap the cell depth range

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
poly$numSpp80 = countOverlaps(ir1, ir2) # calculates how many species in the set overlap the cell depth range

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

# Return # of unique taxa per species set
numSpp_08 <- n_distinct(sj08spp$Scientific_name, na.rm = TRUE)
numSpp_22 <- n_distinct(sj22spp$Scientific_name, na.rm = TRUE)
numSpp_80 <- n_distinct(sj80Spp$Scientific_name, na.rm = TRUE)

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
catch_summ_80_div <- sj80Spp %>% 
  filter(!is.na(Trawl_id)) %>%
  mutate(data=paste0(as.character(n_distinct(Scientific_name, na.rm = TRUE)), "_GF_spp")) %>% # add data set unique identifier string
  filter(total_catch_numbers>0 | total_catch_wt_kg>0) %>% # filter out tows with no catch of a given taxon
  group_by(data, CentroidID, DepStrata2, numSpp80) %>%
  summarize(numTows=n_distinct(Trawl_id)
            ,sppRich = n_distinct(Scientific_name, na.rm = TRUE) # calculate species richness for each cell
            ) %>% 
  mutate(sppRich_prop = sppRich / numSpp80) %>%  # calculate proportion of taxa caught in each cell
  ungroup() %>% # ungroup to calculate min and max values on entire data range
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
# Load ancillary data files
juvenile.groundfish.to.WCGFBT.grid.data <- readRDS("~/Documents/GitHub/SurveyCoverage/juvenile.groundfish.to.WCGFBT.grid.data.Rds") # juvenile habitat
data.non.confidential <- readRDS("~/Documents/GitHub/SurveyCoverage/data.non.confidential.Rds") # commercial GF CPUE

catch_summ <- full_join(catch_summ_08, catch_summ_22_80, by = "CentroidID") %>% 
  mutate(numTows = numTows_80) %>% 
  left_join(juvenile.groundfish.to.WCGFBT.grid.data, by = "CentroidID") %>% 
  mutate(juv_all_spp_nona = ifelse(is.na(Norm_ALL_SPECIES), 0, Norm_ALL_SPECIES) ) %>% # convert unmatched cells to 0 value
  mutate(juv_norm = ( juv_all_spp_nona - min(juv_all_spp_nona) ) / ( max(juv_all_spp_nona) - min(juv_all_spp_nona) )) %>% # normalize
  mutate(trwluntrwl = 1) %>%   # categorical value for survey trawlability
  # left_join(data.non.confidential, by = "CentroidID") %>% 
  # mutate(commCPUE_norm = norm.mean.cpue.2011.2019) %>% 
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

# Plotting Section (from Kelly Andrews' "Mapping code.R" script)
library(ggOceanMapsData) # package error
library(ggOceanMaps)
library(ggnewscale)
library(marmap) #for getNOAA.bathy function

# Bring in US map borders
wc <- st_read("~/Documents/GIS/WC/WCstates100K_ply_utm.shp") %>%
  # filter(NAME %in% c("Washington","Oregon","California")) %>%
  st_transform(crs="+proj=tmerc +lat_0=31.96 +lon_0=-121.6 +k=1 +x_0=390000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

# Oregon Call Areas
# O <- st_read("/Users/kelly.andrews/Documents/GitLab/habitat-modification/GIS layers/Oregon Call Areas/Proposed_Call_Areas_2022_02_25.shp") %>%
#   filter(Name != "Bandon") %>%
O <- wea %>% 
  st_transform(crs="+proj=tmerc +lat_0=31.96 +lon_0=-121.6 +k=1 +x_0=390000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ") %>%
  st_union()

# Bring in bathymetry for smaller scale maps (i.e. Sanctuaries)
# bgdb <- "/Users/kelly.andrews/Documents/GitLab/habitat-modification/GIS layers/bathymetryContours/BathymetryContours.gdb"  #Bathymetry from https://marinecadastre.gov/nationalviewer/
bgdb <- "~/Documents/GIS/Mapping/NGDC/NGDCbathy.gdb"  
bathy <- st_read(dsn=bgdb, layer="bat_cont5diss_utm")
bathy <- st_transform(bathy, crs="+proj=tmerc +lat_0=31.96 +lon_0=-121.6 +x_0=390000 +y_0=0 +k=1.0 +datum=WGS84 +units=m +no_defs")
bathy <- st_crop(bathy, c(xmin=64196.97, xmax=500000, ymin=234822.1, ymax=1842822))
bathy.sub <- bathy[which(bathy$CONTOUR=="-55"|bathy$CONTOUR=="-185"|bathy$CONTOUR=="-550"|bathy$CONTOUR=="-1280"),]

# Designate map boundaries
xlim <- c(-125.5, -124)
ylim <- c(41.8, 44)

# Get bathymetry data for a better basemap
pt.baty = getNOAA.bathy(
  lon1 = -126, 
  lon2 = -123, 
  lat1 = 41, 
  lat2 = 45, resolution = 1)

# Mapping
# Component plots
# Catch Rate 08 spp
p1 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = outSHP, aes(fill = avgCatchWgt_08_norm), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  labs(title = "WCGBTS Mean CPUE, 8 spp.") + # title for individual plot
  # labs(title = "Mean CPUE, 8 spp.") + # title for arranged multiple plots
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)

# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_fig1_NormalizedMeanCPUE_08sp_2003_2019.png"), plot = p1, width = 4, height = 6, units = "in", bg = "white", dpi = 300)

# Catch Rate 22 spp
p2 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = outSHP, aes(fill = avgCatchWgt_22_norm), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  labs(title = "WCGBTS Mean CPUE, 22 spp.") + # title for individual plot
  # labs(title = "Mean CPUE, 22 spp.") + # title for arranged multiple plots
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)

# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_fig2_NormalizedMeanCPUE_22sp_2003_2019.png"), plot = p2, width = 4, height = 6, units = "in", bg = "white", dpi = 300)

# Catch Rate 80 spp
p3 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = outSHP, aes(fill = avgCatchWgt_80_norm), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  labs(title = "WCGBTS Mean CPUE, 79 spp.") + # title for individual plot
  # labs(title = "Mean CPUE, 79 spp.") + # title for arranged multiple plots
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)

# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_fig3_NormalizedMeanCPUE_79sp_2003_2019.png"), plot = p3, width = 4, height = 6, units = "in", bg = "white", dpi = 300)

# Species Richness
p4 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = outSHP, aes(fill = sppRich_80_norm), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  labs(title = "WCGBTS Spp. Richness") + # title for individual plot
  # labs(title = "Spp. Richness") + # title for arranged multiple plots
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)

# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_fig4_NormalizedSppRichness_2003_2019.png"), plot = p4, width = 4, height = 6, units = "in", bg = "white", dpi = 300)

# Juvenile Habitat, 13 spp
p5 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = outSHP, aes(fill = juv_norm), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  labs(title = "WCGBTS Juvenile Habitat, 13 spp.") + # title for individual plot
  # labs(title = "Juvenile Habitat, 13 spp.") + # title for arranged multiple plots
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)

# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_fig5_NormalizedJuvHabitat_2003_2019.png"), plot = p5, width = 4, height = 6, units = "in", bg = "white", dpi = 300)

# Value plot
p6 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = outSHP, aes(fill = val_norm), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  # labs(title = "WCGBTS \"Value\", 2003-2019") + # title for individual plot
  # labs(title = "WCGBTS \"Value\"") + # title for arranged multiple plots
  labs(title = "WCGBTS \"Value\", 2003-2019") + # title for arranged double plot
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  geom_sf_text(data = bathy.sub, aes(label = CONTOUR) # attempt to add contour labels; still needs work
               , stat = "sf_coordinates"
               , nudge_x = c(0.1, 0.1, -0.06, -0.05), nudge_y = c(0, 0, -0.01, 0)
               , color = "darkgray", size = 2, fontface = "bold") +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)
p6
# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_fig6_NormalizedValue_2003_2019_lab.png"), plot = p6, width = 4, height = 6, units = "in", bg = "white", dpi = 300)

# Number of Tows plot
p7 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = outSHP, aes(fill = numTows), lwd = 0.1) +
  scale_fill_gradientn(" \n ", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  labs(title = "WCGBTS Tows per Cell, 2003-2019") + # title for individual plot
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)
p7
# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_fig7_NumTows_2003_2019.png"), plot = p7, width = 4, height = 6, units = "in", bg = "white", dpi = 300)

# Create arranged multi-panel plot
library("ggpubr")
pAll <- ggarrange(p1+rremove("x.text"),NULL,p2+rremove("x.text")+rremove("y.text"),NULL,p3+rremove("x.text")+rremove("y.text")
                  ,p4+rremove("x.text"),NULL,p5+rremove("x.text")+rremove("y.text"),NULL,p6+rremove("x.text")+rremove("y.text")
                  # ,labels = c("Mean CPUE, 8 spp.", "Mean CPUE, 22 spp.", "Mean CPUE, 79 spp.", "Species Richness", "Juvenile Habitat, 13 spp.", ""Survey \"Value\"")
                  ,ncol = 5, nrow = 2
                  ,widths = c(1.1, -0.6, 1, -0.6, 1)
                  ,common.legend = TRUE, legend = "bottom"
                  )
pAll

# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS_6panel_2003_2019.png"), plot = pAll, width = 10, height = 8.5, units = "in", bg = "white", dpi = 300)

# Kelly's plot
p8 <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = data.non.confidential, aes(fill = norm.mean.cpue.2011.2019), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8)) +
  labs(title = "Mean Fishery CPUE, 2011-2019") + # title for arranged multiple plots
  geom_sf(data = wc, color = "black", size = 0.125, fill = "lightgray") +
  geom_sf(data = O, color = "black", size = 0.5, fill = "transparent") +
  geom_sf(data = bathy.sub, color = "darkgray", size = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE)

# Two-panel side-by-side plot
library("ggpubr")
# Legend Only plot
# See https://www.datanovia.com/en/blog/how-to-remove-legend-from-a-ggplot/
pVoid <- ggplot(pt.baty) +
  geom_raster(data = pt.baty, aes(x=x, y=y, fill=z)) +
  scale_fill_etopo(guide = "none") +
  new_scale_fill() +
  geom_sf(data = data.non.confidential, aes(fill = norm.mean.cpue.2011.2019), lwd = 0.1) +
  scale_fill_gradientn("Scaled\nvalue", 
                       colours = c("blue","cyan","khaki","orange","red"),
                       na.value = NA) +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5)
        # ,legend.key.size = unit(1, "cm")
        # ,legend.text = element_text(size =  12)
        # ,legend.title = element_text(size = 15, face = "bold")
        ) +
  guides(colour = guide_legend(override.aes = list(size=8)))
pVoid
# Construct two-panel plot with legend in between two plots
# See https://stackoverflow.com/questions/61534072/how-to-add-a-legend-after-arrange-several-plots-using-ggarrange-from-the-ggpub
pDbl <- ggarrange(p6+rremove("legend"), pVoid, p8+rremove("legend")+rremove("y.text")
                  ,ncol = 3, nrow = 1
                  ,widths = c(1, 0.05, 1)
                  )
pDbl
# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS-Fishery_2panel_legend_in_middle.png"), plot = pDbl, width = 10, height = 8.5, units = "in", bg = "white", dpi = 300)

# Construct more classic side-by-side plot
pDbl <- ggarrange(p6, NULL, p8+rremove("y.text")
                  ,ncol = 3, nrow = 1
                  ,widths = c(1, -0.35, 1)
                  ,common.legend = TRUE
                  ,legend = "bottom"
                  )
pDbl
# Save output plot
ggsave(paste0(dir, "/figures/WCGBTS-Fishery_2panel_legend_on_bottom.png"), plot = pDbl, width = 10, height = 8.5, units = "in", bg = "white", dpi = 300)


# # OLD plotting code
# Prepare layers for plotting
# Workflow based on 25 Oct 2018 blog article from Mel Moreno and Mathieu Basille
# Source: https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(sf)
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
# 
# # Create points for state labels
# library(maps)
# states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# state_list <- c('California', 'Oregon', 'Washington')
# states <- states %>% filter(ID == 'oregon')
# states <- cbind(states, st_coordinates(st_centroid(states, of_largest_polygon = TRUE)))
# wea <- wea %>% filter(Area_Name == 'Coos Bay')
# 
# library(tools)
# states$ID <- toTitleCase(states$ID)
# head(states)
# 
# # join results to polygon shape
# p1shp = left_join(poly, catch_summ, by = c("CentroidID"))
# stns <- cbind(p1shp, st_coordinates(st_centroid(p1shp, of_largest_polygon = TRUE)))

# library(ggplot2)
# library(RColorBrewer)
# library(ggspatial)
# # ggplot template
# # ggplot(a, aes(x = X, y = Y))+geom_text(aes(label = VAL)) # for this purpose, label = numTows OR label = sppRich
# 
# sf_use_s2(FALSE)
# # my_breaks <- c(0, 0.1, 1, 10, 100, 1000)
# # my_breaks <- c(0.1, 1, 10, 100, 1000)
# # my_breaks <- c(0, 29, 80, 252, 930, 1000)
# # my_breaks <- c(0.001, 0.01, 1)
# p1 <- ggplot(date = world) +
#   geom_sf() +
#   geom_sf(data = states, fill = NA) + 
#   geom_label(data = stns, aes(x=X, y=Y, label = numTows)) +
#   geom_sf(data = p1shp, aes(fill = avgCatchWgt_norm)) +
#   # scale_fill_continuous(values=rev(brewer.pal(7, "YlGnBu")), na.value="grey90")+
#   # scale_fill_gradient(low = "#132B43",
#   #                     high = "#56B1F7",
#   #                     space = "Lab",
#   #                     na.value = "grey90",
#   #                     guide = "colourbar",
#   #                     aesthetics = "fill") +
#   # scale_fill_viridis_c(trans = "log", alpha = .4, na.value="grey90") +
#   # scale_fill_viridis_c(breaks = my_breaks, labels = my_breaks
#   #                      ,trans = scales::pseudo_log_trans(sigma = 0.1)
#   #                      , direction = -1, alpha = .8, na.value="grey90", name = "Weight (kg)") +
#   scale_fill_viridis_c(direction = -1, alpha = .8, na.value="grey90", name = "Weight (kg)") +
#   geom_sf(data = wea, fill = NA, color = "darkred") +
#   geom_text(data = states, aes(X, Y, label = ID), size = 5) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   coord_sf(xlim = c(-125.4, -124), ylim = c(43.0, 44.2), expand = FALSE) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("WCGBTS (2003-19)", subtitle = paste0("(Normalized average CPUE per Survey Cell for ", length(spec_set), " select groundfish taxa)")) +
#   theme(legend.justification=c(1,0), legend.position=c(1,0))
# p1

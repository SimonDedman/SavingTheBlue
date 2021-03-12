# Map analysis & Shiny presentation,  shark catch cpue
# Simon Dedman simondedman@gmail.com
# 2021-03-04

# Drumline mapping priority for website.
# Acoustic & sat tag data looks less good.
# Arrows = time spent moving in 1 direction.

# Shiny for website.
library(ggmap)
# Please cite ggmap if you use it! See citation("ggmap") for details.
library(dplyr)
library(lubridate)
library(magrittr) # %>%
library(sf)
library(ggspatial)
library(tidylog) # verbose version of tidyverse
library(concaveman) # points to poly bounding box

source('R/01_data-import.R') # run data import if you changed the database.

shark <- readRDS(file = paste0("../../Data/", "2021-03-11", "_shark_capture_data.rds"))
drumline <- readRDS(file = paste0("../../Data/", "2021-03-11", "_drumline_data.rds"))

# bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
myLocation <- c(min(shark$Longitude), min(shark$Latitude), max(shark$Longitude), max(shark$Latitude))

myMap <- get_map(location = myLocation,
                 source = "osm",
                 maptype = "osm",
                 crop = FALSE)
# zoom = 10) # default?




# Ideas / tasks
# Heatmap for areas of high shark catch density / contours
# Colour points by species / facet by species

# Don't map silkies & duskies with the above plotting style.

# Use site rather than lat/long data (see my previous email about sites Si)
# we’ll want to choose a lat/long for each site, and from a mapping perspective probably want to
# set the margins pretty wide so that trends are observable but specific locations are not indicated to avoid potential issues.
#
# could plot all the locations of individual drums on a map
# pick a central location as the site location?
# [make centroid from range of latlons per site]
# [or polygon around all points]
# focus on sampling locations with decent soak times:
# Somerset, High Cay, Green Cay, Bristols Galley, Autec, North Bight, Shark Hole, Isla's, and Bigwood.
site_extents <- shark %>%
  group_by(Site2) %>%
  summarise(npoints = n(),
            minlat = min(Latitude, na.rm = TRUE),
            maxlat = max(Latitude, na.rm = TRUE),
            minlon = min(Longitude, na.rm = TRUE),
            maxlon = max(Longitude, na.rm = TRUE),
            meanlat = mean(Latitude, na.rm = TRUE),
            meanlon = mean(Longitude, na.rm = TRUE))

sf_shark <- sf::st_as_sf(shark, coords = c("Longitude","Latitude")) %>%
  sf::st_set_crs(4326)

polys <- st_sf(
  aggregate( # sf_shark$geometry but filtered for sites with >3 points using site_extents lookup
    sf_shark %>% filter(Site2 %in% (site_extents %>%
                                      filter(npoints > 3) %>%
                                      pull(Site2))) %>%
      pull(geometry),
    # same, sf_shark$Site but filtered for sites with >3 points using site_extents lookup
    list(sf_shark %>% filter(Site2 %in% (site_extents %>%
                                           filter(npoints > 3) %>%
                                           pull(Site2))) %>%
           pull(Site2)),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    } # from https://gis.stackexchange.com/questions/332427/converting-points-to-polygons-by-group
  )) %>%
  rename(Site2 = Group.1)



# split sf_shark (>3) into dfs by group, into a list object
# because site2 is a factor it breaks things
sf_shark$Site2 <- as.character(sf_shark$Site2)
sites_list <- split(x = sf_shark %>% filter(Site2 %in% (site_extents %>%
                                                          filter(npoints > 3) %>%
                                                          pull(Site2))),
                    f = sf_shark %>% filter(Site2 %in% (site_extents %>%
                                                          filter(npoints > 3) %>%
                                                          pull(Site2))) %>%
                      pull(Site2))

# lapply concaveman
sites_polys_list <- lapply(sites_list, concaveman)
sites_polys <- data.frame(Reduce(rbind, sites_polys_list)) # loses names
sites_polys_names <- do.call(rbind.data.frame, sites_polys_list)
sites_polys$Site2 <- rownames(sites_polys_names)
sf_sites_polys <- sf::st_as_sf(sites_polys, sf_column_name = "polygons") %>% # convert to sf
  sf::st_set_crs(4326)


# For species composition, there is a cool package in R that allows you to make "scatterpies"
# We could put a scatterpie at each site on a map and display species diversity.
# In the attached example, you can see the size of the circle is dictated by sample size.
# We could do the same for effort, but this would be related to overall captures of sharks per location and not CPUE of individual species.

# individual heat maps for each species


# Count & CPUE stats####
sites <- shark %>%
  group_by(Site2) %>% # group by site2
  summarise(Latitude = mean(Latitude, na.rm = TRUE), # summary stats
            Longitude = mean(Longitude, na.rm = TRUE),
            Depth_m = mean(Depth_m, na.rm = TRUE),
            Temperature_C = mean(Temperature_C, na.rm = TRUE),
            Salinity_ppt = mean(Salinity_ppt, na.rm = TRUE),
            DO_mg_L = mean(DO_mg_L, na.rm = TRUE))

# gear type? Filter by Gear2? group by Gear2?
# Common = mean(Common, na.rm = TRUE) # number of rows per each Common
SiteCounts <- shark %>%
  group_by(Site2, Common) %>% # group by site2
  summarise(Count = n()) %>%
  left_join(sites) # add summaries from above


# empty hooks constitute ½ of the soak time
# if bait_present = FALSE, soak_time = (soak_time * 0.5)
drumline$Soak_time2 <- drumline$Soak_time
drumline[which(drumline$Bait_present == FALSE), "Soak_time2"] <- drumline[which(drumline$Bait_present == FALSE), "Soak_time2"] * 0.5


# CPUE: per Site2, sharks per hook hour (Soak_time2)

# per Site2, (total soak time2)
soak_per_site <- drumline %>%
  group_by(Site2) %>%
  summarise(Total_Soak = sum(Soak_time2, na.rm = TRUE)) # within each site, need a column per shark species with their counts / total soaktime2# OR col1 commonname col2 count/soak. Split/wider/pivot_wider or whatever?
# do 2-stage, count each per site, and sum soaktime
# then divide each per soak
sharks_per_site <- drumline %>%
  group_by(Site2, Common) %>%
  summarise(Count = n()) %>%
  filter(Common %in% unique(shark$Common))

CPUE <- sharks_per_site %>%
  left_join(soak_per_site) %>% # Joining, by = "Site2"
  mutate(CPUE = Count / Total_Soak) %>% # generate CPUE
  left_join(sites) # Joining, by = "Site2"



# count of each shark species (discard nonsharks or filter later)
# divide count by total soaktime2

ggmap(myMap) +
  geom_point(aes(x = Longitude,
                 y = Latitude,
                 size = Count),
             data = SiteCounts,
             alpha = 0.5,
             color = "darkred") +
  scale_size_continuous() +
  facet_wrap(.~Common) + # facet by species
  labs(x = "Longitude",
       y = "Latitude") +
  ggtitle("Sharks caught off East Andros, all gear, counts") +
  theme(legend.position = c(0.88, 0.15), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggsave(paste0(today(), "_Counts_Sites_Facet_Species.png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 4, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
         height = 6.4, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", dpi = 600, limitsize = TRUE)

ggmap(myMap) +
  geom_point(aes(x = Longitude,
                 y = Latitude,
                 size = CPUE),
             data = CPUE,
             alpha = 0.5,
             color = "darkred") +
  scale_size_continuous() +
  facet_wrap(.~Common) + # facet by species
  labs(x = "Longitude",
       y = "Latitude",
       size = paste("CPUE (sharks/", "hook/soak-hr)", sep = "\n")) +
  ggtitle("Sharks caught off East Andros, drumlines, CPUE") +
  theme(legend.position = c(0.88, 0.92), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggsave(paste0(today(), "_CPUE_Sites_Facet_Species.png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 4, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
         height = 6.4, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", dpi = 600, limitsize = TRUE)
# fixed dodgy point, ~lon -77.77 ~lat 24.625
#
# annotation_spatial(sf_sites_polys, col = "black", lwd = 0.05, fill = NA) +
#
# geom_polygon(data = polys) +
# geom_density_2d(data = shark, # doesn't work well / look good / help
#                 aes(x = Longitude,
#                     y = Latitude),
#                 colour = "black",
#                 size = 0.25) +
#


Size histograms, all sites, per shark, per sex: will do, cheers.



# Website: preliminary insights page with maps. And also 2 female hammers giving a lot of data. (once we have drumline data?). Liaise with Annie & TG.
# And can also do histograms and bar plots and such. Markdown.


# Habitat: I finally got the new high res habitat data from The Nature Conservancy:
# nearly 3gb & a weird format so will take a bit of processing but a positive step forward nonetheless

# Map analysis & Shiny presentation
# Simon Dedman simondedman@gmail.com
# 2021-03-04

# Drumline mapping priority for website.
# Acoustic & sat tag data looks less good.
# Arrows = time spent moving in 1 direction.

# Getting scripts ready in R with errors around points,
# heat-map for spot tag data.
# GPS + estimated error for position.

# Hammerheads: 1 male 9 month track, 2 females sending data daily, not great positions,
# 1 better than other, could put those maps up to show people

# Shiny for website.

shark <- readRDS(file = paste0("../../Data/", today(), "_shark_capture_data.rds"))
drumline <- readRDS(file = paste0("../../Data/", today(), "_drumline_data.rds"))


library(ggmap)
# Please cite ggmap if you use it! See citation("ggmap") for details.
library(dplyr)
library(lubridate)
library(magrittr) # %>%
library(sf)
library(ggspatial)
library(tidylog) # verbose version of tidyverse
library(concaveman) # points to poly bounding box

# bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
myLocation <- c(min(shark$Longitude), min(shark$Latitude), max(shark$Longitude), max(shark$Latitude))

myMap <- get_map(location = myLocation,
                 source = "osm",
                 maptype = "osm",
                 crop = FALSE)
# zoom = 10) # default?

ggmap(myMap) +
  geom_point(aes(x = Longitude,
                 y = Latitude),
             data = shark,
             alpha = 0.5,
             color = "darkred",
             size = 3) +
  annotation_spatial(sf_sites_polys, col = "black", lwd = 0.05, fill = NA) +
  # geom_polygon(data = polys) +
  # geom_density_2d(data = shark, # doesn't work well / look good / help
  #                 aes(x = Longitude,
  #                     y = Latitude),
  #                 colour = "black",
  #                 size = 0.25) +
  facet_wrap(.~Species) + # facet by species
  labs(x = "Longitude",
       y = "Latitude") +
  ggtitle("Sharks caught off East Andros") +
  ggsave(paste0(today(), "_Catches_Points_Facet_Species.png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 4, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
         height = 6.3, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", dpi = 600, limitsize = TRUE)
# fixed dodgy point, ~lon -77.77 ~lat 24.625


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




# make the circles a bit bigger to reduce the resolution on the data
# we want people to know what we're doing and some of our results, but maybe we
# don;t want to give away too much.

# For species composition, there is a cool package in R that allows you to make "scatterpies"
# We could put a scatterpie at each site on a map and display species diversity.
# In the attached example, you can see the size of the circle is dictated by sample size.
# We could do the same for effort, but this would be related to overall captures of sharks per location and not CPUE of individual species.

# individual heat maps for each species

# CPUE using ggplot and geom_count(). This would show CPUE across sampling sites per species.
# (individual spreadsheets with) daily CPUEs for each site.
#  calculate the CPUE of each species per site. Then have an excel with species ID, site name, lat/lon, and CPUE
# CPUE (sharks per hook hour) for each day of fishing at each site for each species

# 0’s will need to be included for days that sharks of each species were not caught.
# how to handle empty hooks – I don’t think it’s appropriate to consider these to be
# fishing for the duration of the soak time since empty hooks don’t catch sharks.
# I’ve used the precedent set by Mike that empty hooks constitute ½ of the soak time




# Habitat: I finally got the new high res habitat data from The Nature Conservancy:
# nearly 3gb & a weird format so will take a bit of processing but a positive step forward nonetheless

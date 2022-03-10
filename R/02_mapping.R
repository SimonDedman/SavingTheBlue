# Map analysis & Shiny presentation,  shark catch cpue
# Simon Dedman simondedman@gmail.com
# 2021-03-04

# Drumline mapping priority for website.
# Acoustic & sat tag data looks less good.
# Arrows = time spent moving in 1 direction.

# Shiny for website.
library(ggmap) # get_map %>% ggmap
# Please cite ggmap if you use it! See citation("ggmap") for details.
library(dplyr) # %>% group_by summarise n filter pull rename left_join mutate ungroup
library(lubridate) # today
library(magrittr) # %>%
library(sf) # %>% st_as_sf st_set_crs st_sf st_cast st_combine
library(ggspatial) # aes ggplot
library(tidylog) # verbose version of tidyverse
library(concaveman) # points to poly bounding box

source('R/01_data-import.R') # run data import if you changed the database.

shark <- readRDS(file = paste0("../../Data/", "2021-03-20", "_shark_capture_data.rds"))
drumline <- readRDS(file = paste0("../../Data/", "2021-03-20", "_drumline_data.rds"))

# bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
myLocation <- c(min(shark$Longitude), min(shark$Latitude), max(shark$Longitude), max(shark$Latitude))

myMap <- get_map(location = myLocation,
                 # source = "osm",
                 # maptype = "osm",
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

sharks_per_site$Common %in% unique(shark$Common)

CPUE <- sharks_per_site %>%
  left_join(soak_per_site) %>% # Joining, by = "Site2"
  mutate(CPUE = Count / Total_Soak) %>% # generate CPUE
  left_join(sites) # Joining, by = "Site2"



shark$Site3 <- as.character(shark$Site2)
# pool nearby sites, such as blackbeards and shark hole. /  pool all north bight, shark hole, islas and blackbeards
shark[which(shark$Site3 == "Blackbeard's Channel"), "Site3"] <- "Wider North Bight"
shark[which(shark$Site3 == "Shark Hole"), "Site3"] <- "Wider North Bight"
shark[which(shark$Site3 == "North Bight"), "Site3"] <- "Wider North Bight"
shark[which(shark$Site3 == "Isla's Spot"), "Site3"] <- "Wider North Bight"
# If we only wanted to include core sites where we have a lot of effort, I think they would be Green cay, AUTEC channel, bigwood, TOTO, Islas - maybe north bight
# pick sites with > 10 captures

# unique(shark$Site3)
# Gibson Cay           Green Cay            TOTO Navy Buoy       Blackbeard's Channel Bigwood Channel      Fresh Creek          North Bight          AUTEC Channel        Isla's Spot
# Bristol Galley       Shark Hole           Somerset             High Cay

shark$Site3 <- factor(shark$Site3, levels = c("Fresh Creek", "Somerset", "High Cay", "Green Cay", "Bristol Galley", "AUTEC Channel", "Wider North Bight", "Bigwood Channel", "TOTO Navy Buoy", "Gibson Cay"))
# removing sites where effort is limited?
# Including these data might cause our public audience to draw conclusions about diversity/abundance at that location,
# but we might not have enough effort to make these conclusions realistic.
# An example would be blackbeards channel.
# I know we haven't fished there a ton (relatively speaking),
# but from the figures it seems as though only nurse sharks are present and I do not think that is the message we want to convey.





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
       y = "Latitude",
       caption = paste0("Saving The Blue, ", today())) +
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
       size = paste("CPUE (sharks/", "hook/soak-hr)", sep = "\n"),
       caption = paste0("Saving The Blue, ", today())) +
  ggtitle("Sharks caught off East Andros, drumlines, CPUE") +
  theme(legend.position = c(0.88, 0.92), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggsave(paste0(today(), "_CPUE_Sites_Facet_Species.png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 4,
         height = 6.4,
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
 # 285

# Size boxplots, x species, colour sex, all sites####
ggplot(shark %>%
         group_by(Common) %>%
         filter(length(PCL) > 1), # remove groups with 1 observation
       aes(x = Common, y = PCL)) +
  geom_boxplot(aes(fill = Sex), colour = "black", notch = F,
               position = position_dodge(preserve = "single")) + # preserves width when species have 0 data for 1 sex
  scale_y_continuous(limits = c(0, round(max(shark$PCL, na.rm = T), -2)),
                     breaks = seq(from = 0, to = round(max(shark$PCL, na.rm = T), -2), by = 50)) +
  labs(x = "Species", y = "Pre-caudal Length (cm)", caption = paste0("Saving The Blue, ", today())) +
  # stat_summary(fun.y = mean, geom = "point", shape = 23, size = 5, colour = "white") + # adds mean point
  # scale_x_discrete(labels = c("GSL-GOM", "GOM-GSL", "GSL-Med", "Med-GSL")) + # manually relabel to remove underscores
  # scale_fill_manual(values = c("red", "red", "blue", "blue")) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   axis.title.x = element_text(vjust = -2), # move x axis label down a bit
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(2)),
                                   legend.position = c(0.03, 0.92),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(today(), "_Size_Histo.png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 15, height = 6, units = "in", dpi = 600, limitsize = TRUE)




# Size boxplots, x species, colour sex, facet sites####
ggplot(shark %>%
         filter(!is.na(Sex)) %>%
         group_by(Site3) %>%
         filter(n() >= 10) %>%
         ungroup() %>%
         group_by(Site3, Common, Sex) %>%
         filter(length(PCL) > 1),
       aes(x = Common, y = PCL)) +
  geom_boxplot(aes(fill = Sex), colour = "black", notch = F, position = position_dodge(preserve = "single")) +
  facet_wrap(.~Site3, # facet by site
             scales = "free", # drops zero-shark bins
             drop = TRUE) + # otherwise plots empty sites even if they're not in the dbase so how does it know what they are?? From factor levels.
  scale_y_continuous(limits = c(0, round(max(shark$PCL, na.rm = T), -2)),
                     breaks = seq(from = 0, to = round(max(shark$PCL, na.rm = T), -2), by = 50)) +
  labs(x = "Species", y = "Pre-caudal Length (cm)", caption = paste0("Saving The Blue, ", today())) +
  # stat_summary(fun.y = mean, geom = "point", shape = 23, size = 5, colour = "white") + # adds mean point
  # scale_x_discrete(labels = c("GSL-GOM", "GOM-GSL", "GSL-Med", "Med-GSL")) + # manually relabel to remove underscores
  # scale_fill_manual(values = c("red", "red", "blue", "blue")) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1)),
                                   axis.title.x = element_text(vjust = -2), # move x axis label down a bit
                                   title = element_text(size = rel(1)),
                                   legend.position = c(0.03, 0.92),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(today(), "_Size_Histo_SiteFacet.png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 15, height = 6, units = "in", dpi = 600, limitsize = TRUE)


# ensure females are always on the left and males always on the right? You will see female = 0 and male >1, the male block ends up on the left (see high cay, blackbeards)
# taken care of by removing low-n groups


# Size histograms ####
# TG: size-frequency histograms instead? just include species with > 5 catches?
# do this overall for individual species catches (all sites) and only for species with > 5 captures. Could be split by colour for sex too?
# SD:L confused, asked for more info. He means column plots

ggplot(shark %>%
         filter(!is.na(Sex)) %>%
         group_by(Site3) %>%
         filter(n() >= 10) %>%
         ungroup() %>%
         group_by(Site3, Common, Sex) %>%
         filter(length(PCL) > 1),
       aes(x = PCL)) +
  geom_histogram(aes(fill = Sex), colour = "black", position = position_dodge(preserve = "single"), bins = 6) + #
  facet_wrap(facets = c("Common"), # facet by site
             scales = "free", # drops zero-shark bins
             drop = TRUE) + # otherwise plots empty sites even if they're not in the dbase so how does it know what they are?? From factor levels.
  # scale_y_continuous(limits = c(0, round(max(shark$PCL, na.rm = T), -2)),
  #                    breaks = seq(from = 0, to = round(max(shark$PCL, na.rm = T), -2), by = 50)) +
  labs(x = "Pre-caudal Length (cm)", y = "Count", caption = paste0("Saving The Blue, ", today())) +
  # stat_summary(fun.y = mean, geom = "point", shape = 23, size = 5, colour = "white") + # adds mean point
  # scale_x_discrete(labels = c("GSL-GOM", "GOM-GSL", "GSL-Med", "Med-GSL")) + # manually relabel to remove underscores
  # scale_fill_manual(values = c("red", "red", "blue", "blue")) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1)),
                                   axis.title.x = element_text(vjust = -2), # move x axis label down a bit
                                   title = element_text(size = rel(1)),
                                   legend.position = c(0.03, 0.92),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(today(), "_Size_Columns_CommonFacet.png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 15, height = 6, units = "in", dpi = 600, limitsize = TRUE)



# TODO####
# PM (both graphics): One concern is the readability for non-scientists – might be good for Annie and Gabby to have a look and see how they interpret them.
# BK Maybe we should only retain a species if n is a decent value (n >5??). We can be upfront and write size distributions for n > 5 (or whatever).
#  Perhaps we can also put n = X somewhere on each box? although this may be too much for the public.
#  When we submit these data for a scientific audience it makes sense to include n=1.




# Website: preliminary insights page with maps. And also 2 female hammers giving a lot of data. (once we have drumline data?). Liaise with Annie & TG.
# And can also do histograms and bar plots and such. Markdown.


# new high res habitat data from The Nature Conservancy: include?

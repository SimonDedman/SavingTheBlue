# Map analysis & Shiny presentation, hammerhead tracks
# Simon Dedman simondedman@gmail.com
# 2021-03-11

# Arrows = time spent moving in 1 direction.

# Getting scripts ready in R with errors around points,
# heat-map for spot tag data.
# GPS + estimated error for position.

# Hammerheads: 1 male 9 month track, 2 females sending data daily, not great positions,
# 1 better than other, could put those maps up to show people

# Shiny for website.
library(ggmap)
# Please cite ggmap if you use it! See citation("ggmap") for details.
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr) # %>%
library(sf)
library(ggspatial)
library(tidylog) # verbose version of tidyverse
# library(concaveman) # points to poly bounding box
hammerfolders <- list.dirs(path = "../../Data/Hammerhead SPOT tags/",
                           full.names = FALSE,
                           recursive = FALSE)

for (hammerdirs in hammerfolders) { # hammerdirs <- hammerfolders[1]

deploy <- read_csv(file = paste0("../../Data/Hammerhead SPOT tags/", hammerdirs, "/", hammerdirs, "-Deploy.csv")) %>%
  rename(ID = "Platform ID No.",
         Datetime = "Loc. date") %>%
  mutate(Datetime = as_datetime(Datetime, format = "%m/%d/%Y %H:%M:%S"))

hammertrack <- read_csv(file = paste0("../../Data/Hammerhead SPOT tags/", hammerdirs, "/", hammerdirs, "-All.csv")) %>%
  select("Platform ID No.", Latitude, Longitude, "Loc. quality", "Loc. date") %>%
  rename(ID = "Platform ID No.",
         Quality = "Loc. quality",
         Datetime = "Loc. date") %>%
  mutate(Datetime = as_datetime(Datetime, format = "%m/%d/%Y %H:%M:%S")) %>%
  drop_na() %>%
  filter(Datetime > deploy$Datetime) # Filter out all datetimes before the deploy datetime


# convert quality to numeric
# unique(hammertrack$Quality) # "3" "1" "2" "B" "0" "A" "Z"
# https://www.argos-system.org/wp-content/uploads/2016/08/r363_9_argos_users_manual-v1.6.6.pdf p18
# 3 = <250m, 4+ messages
# 2 = 250 : <500m, 4+ messages
# 1 = 500 : < 1500m, 4+ messages
# 0 = > 1500m, 4+ messages
# A: No/unbounded accuracy estimation, 3 messages
# B: No/unbounded accuracy estimation, 1:2 messages
# Z: Invalid location (available only for Service Plus/Auxiliary Location Processing)
hammertrack[which(hammertrack$Quality == "A"), "Quality"] <- "-1"
hammertrack[which(hammertrack$Quality == "B"), "Quality"] <- "-2"
hammertrack[which(hammertrack$Quality == "Z"), "Quality"] <- "-3"
hammertrack$Quality <- as.numeric(hammertrack$Quality)

# summarise data at same datetime, mean lat lon id, best quality
hammertrack %<>%
  group_by(Datetime) %>%
  summarise(across(.cols = c(ID, Latitude, Longitude),
                   .fns = mean),
            across(.cols = Quality,
                   .fns = max))

QualDist <- data.frame(Quality = 3:-3,
                       ErrorDistance = c(250, 500, 1500, 5000, NA, NA, NA)) # per above, >1500 bounded to 5000

hammertrack %<>%
  left_join(QualDist) %>% # Joining, by = "Quality"
  mutate(Month = month(Datetime),
         Day = yday(Datetime))

# bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
myLocation <- c(min(hammertrack$Longitude),
                min(hammertrack$Latitude),
                max(hammertrack$Longitude),
                max(hammertrack$Latitude))

# Automate width * height adjustments for different map extent / ratio
# myLocation[3] - myLocation[1] # longitude, x, width, bind as 6
# myLocation[4] - myLocation[2] # latitude, y, height
autoheight <- (6 / (myLocation[3] - myLocation[1])) * (myLocation[4] - myLocation[2])

myMap <- get_map(location = myLocation,
                 source = "osm",
                 maptype = "osm",
                 crop = FALSE)

sfhammertrack <- sf::st_as_sf(hammertrack, coords = c("Longitude","Latitude")) %>%
  sf::st_set_crs(4326) #points by day

# add error blobs, sized to QualDist$ErrorDistance, colour grey, do as geom_point underneath the path
sf_buffer <- st_transform(sfhammertrack, 6931) %>% # Transform from geographical to projected so we can buffer correctly. CRS for NAtl
  drop_na(ErrorDistance) %>% # NAtl https://epsg.io/6931 unit: metre
  sf::st_as_sf() # else loses sf for some reason!?

sf_buffer %<>% st_buffer(dist = sf_buffer$ErrorDistance) %>% # metres
  st_transform(4326) # convert back to normal coords

ggmap(myMap) +
  geom_sf(data = sf_buffer, # buffer circles around the points
          alpha = 0.1,
          inherit.aes = FALSE) +
  geom_path(aes(x = Longitude, y = Latitude, # track
                colour = Day),
            data = hammertrack,
            arrow = arrow(angle = 30,
                          length = unit(0.05, "inches"),
                          ends = "last",
                          type = "open")) +
  geom_point(aes(x = Longitude, y = Latitude), # add deploy triangle
             data = deploy,
             shape = 24,
             fill = "white",
             colour = "black",
             size = 4) +
  labs(x = "Longitude",
       y = "Latitude") +
  ggtitle(paste0("Hammerhead movement off East Andros, shark: ", hammerdirs)) +
  theme(legend.position = c(0.88, 0.15), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggsave(paste0(today(), "Hammertrack_", hammerdirs, ".png"),
         plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 6, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
         height = autoheight, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", dpi = 600, limitsize = TRUE)

} # end loop of hammerdirs
# ToDO####
# Path coloured by date month like blocklab (add dates?) or could do as coloured dots??
# Add deploy point to legend


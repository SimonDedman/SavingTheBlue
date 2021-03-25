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
# remove.packages("ggmap")
# can't change map soufce due to bug in CRAN version fixed in github see https://github.com/dkahle/ggmap/issues/287
# devtools::install_github("dkahle/ggmap")
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

  # Deployment point
  deploy <- read_csv(file = paste0("../../Data/Hammerhead SPOT tags/", hammerdirs, "/", hammerdirs, "-Deploy.csv")) %>%
    rename(ID = "Platform ID No.",
           Datetime = "Loc. date") %>%
    mutate(Datetime = as_datetime(Datetime, format = "%m/%d/%Y %H:%M:%S"),
           Date = as_date(Datetime),
           Name = paste0("Deploy: ", first(Date)))

  # Track
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
    mutate(Date = as_date(Datetime),
           Month = month(Datetime),
           Day = yday(Datetime))

  # Final point
  Last <- hammertrack %>%
    summarise_all(last) %>%
    mutate(Date = as_date(Datetime),
           Name = paste0("Last location: ", first(Date)))

  DeployLast <- deploy %>%
    select(Latitude, Longitude, Datetime, Date, Name) %>%
    bind_rows(Last %>%
                select(Latitude, Longitude, Datetime, Date, Name))

  # bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
  myLocation <- c(min(hammertrack$Longitude),
                  min(hammertrack$Latitude),
                  max(hammertrack$Longitude),
                  max(hammertrack$Latitude))

  googlemap = TRUE
  if (googlemap) { # grow bounds extents if requested
    expandfactor <- 1.6 # 1.3 - 1.5 same zoom as 1. 1.6 is a big leap up in zoom (out)
    xmid <- mean(myLocation[c(1,3)])
    ymid <- mean(myLocation[c(2,4)])
    xmax <- ((myLocation[3] - xmid) * expandfactor) + xmid #updated for sf/st
    xmin <- xmid - ((xmid - myLocation[1]) * expandfactor)
    ymax <- ((myLocation[4] - ymid) * expandfactor) + ymid
    ymin <- ymid - ((ymid - myLocation[2]) * expandfactor)
    myLocation <- c(xmin, ymin, xmax, ymax)
  }

  myMap <- get_map(location = myLocation,
                   source = "google",
                   maptype = "satellite",
                   crop = FALSE)

  # Automate width * height adjustments for different map extent / ratio
  # 6 (manually chosen width, below), divided by width range times by height range
  # Maintains ratio by scales height to width(6). Then *1.2 because it still wasn't perfect.
  # attr(myMap, "bb")[[4]] - attr(myMap, "bb")[[2]] # longitude, x, width, bind as 6
  # attr(myMap, "bb")[[3]] - attr(myMap, "bb")[[1]] # latitude, y, height
  autoheight <- (6 / (attr(myMap, "bb")[[4]] - attr(myMap, "bb")[[2]])) * (attr(myMap, "bb")[[3]] - attr(myMap, "bb")[[1]]) * 1.2

  # sfhammertrack <- sf::st_as_sf(hammertrack, coords = c("Longitude","Latitude")) %>%
  #   sf::st_set_crs(4326) #points by day
  #
  # # add error blobs, sized to QualDist$ErrorDistance, colour grey, do as geom_point underneath the path
  # sf_buffer <- st_transform(sfhammertrack, 6931) %>% # Transform from geographical to projected so we can buffer correctly. CRS for NAtl
  #   drop_na(ErrorDistance) %>% # NAtl https://epsg.io/6931 unit: metre
  #   sf::st_as_sf() # else loses sf for some reason!?
  #
  # sf_buffer %<>% st_buffer(dist = sf_buffer$ErrorDistance) %>% # metres
  #   st_transform(4326) # convert back to normal coords

  ggmap(myMap) +
    # geom_sf(data = sf_buffer, # buffer circles around the points for error from Argos ping quality
    #         alpha = 0.1, inherit.aes = FALSE) + # Not that good though, messy
    # geom_path(aes(x = Longitude, y = Latitude, colour = Day), # track line w/ arrows. A bit messy/ugly
    #           data = hammertrack,
    #           arrow = arrow(angle = 30, length = unit(0.05, "inches"), ends = "last", type = "open")) +
    geom_path(aes(x = Longitude, y = Latitude), # track line, simple thin black,
              data = hammertrack, colour = "black", size = 0.1) +
    geom_point(aes(x = Longitude, y = Latitude, fill = Month), # points per hit, coloured by month
               data = hammertrack, shape = 21, colour = "black", size = 2) +
    scale_fill_gradientn(colours = rev(rainbow(12)), limits = c(1, 12), # month colour controls
                         labels = month.abb, breaks = 1:12) +
    geom_point(aes(x = Longitude, y = Latitude, shape = Name), # add deploy triangle & last diamond
               data = DeployLast, fill = "white", colour = "black", size = 4) +
    scale_shape_manual(values = c(24, 23)) + # choose shapes manually, in order, deploy then last
    # geom_text(aes(x = Longitude - 0.3, y = Latitude, label = Date), # add deploy & last labels, shift location
    #           data = DeployLast, colour = "white") +
    labs(x = "Longitude", y = "Latitude", caption = paste0("Saving The Blue, ", today())) +
    ggtitle(paste0("Hammerhead movement off East Andros, shark: ", hammerdirs)) +
    theme(legend.position = c(0.1, 0.16), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
          legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
          legend.spacing.y = unit(0.1, 'cm'), #compress spacing between legend items, this is min
          legend.background = element_rect(fill = "white", colour = NA), # element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50"), # white background
          legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
    ggsave(paste0(today(), "_Hammertrack_", hammerdirs, ".png"),
           plot = last_plot(), device = "png", path = "../../Maps & Surveys/R_Plot_Outputs", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
           width = 6, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
           height = autoheight, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
           units = "in", dpi = 600, limitsize = TRUE)

} # end loop of hammerdirs

# ToDo####
# Better labelling?

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

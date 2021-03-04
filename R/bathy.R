# Andros Bathy Raster
# Simon Dedman simondedman@gmail.com
# 2020.07.08
# Isn't very good. GMRT is better. See /Maps & Surveys/Bathymetry/STB_Bathy.qgz

#getNOAA.bathy: low res, is etop01?####
# install.packages("marmap")
library(marmap)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
machine <- "/home/simon/Documents/Si Work/" #Aquarius
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bathymetry/getNOAA.bathy/"

natlanticextents <- data.frame(lon = c(-78.3, -77.3), lat = c(24, 25.2)) # WholeArea
extents <- sf::st_as_sf(natlanticextents, coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) #using jlondon example, may be incorrect? Is N.Pac
b = getNOAA.bathy(lon1 = natlanticextents[1,1], lon2 = natlanticextents[2,1], lat1 = natlanticextents[1,2], lat2 = natlanticextents[2,2], resolution = 1, keep = TRUE,
                  path = paste0(machine, "Blocklab/MapData/getNOAAbathy/")) # get bathymetry data. res 2 for wholearea, else 1
bf = fortify.bathy(b) # convert bathymetry to data frame


# source('~/Dropbox/Galway/Analysis/R/gbm.auto/R/gbm.basemap.R')
# gbm.basemap(bounds = c(-78.3, -77.3, 24, 25.2), # region to crop to: c(xmin,xmax,ymin,ymax)
#             getzip = "/home/simon/Documents/Si Work/Blocklab/iccat/SSM data/outputs/GSHHS_shp", # download & unpack GSHHS data to WD? "TRUE" else absolute/relative reference to GSHHS_shp folder, inc that folder
#             zipvers = "2.3.7", # GSHHS version, in case it updates. Please email developer if this is incorrect
#             savename = "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Coastline/Crop_Map.shp", #shapefile savename
#             res = "f", # resolution, 1:5 (low:high) OR c,l,i,h,f (coarse, low, intermediate, high, full) or "CALC" to calculate based on bounds
#             extrabounds = FALSE)
coast <- read_sf("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Coastline/Crop_Map.shp")

autoplot.bathy(x = b, geom = "tile", coast = FALSE) + # coast line too thick, do manually as a 0 contour below
  scale_fill_gradient2(low = alpha("navyblue", 1),
                       mid = alpha("white", 1),
                       high = alpha("red", 1),
                       midpoint = 350,
                       label = scales::label_number_si()) + # bathy colours good & land fine darkgreen
  # scale_x_continuous(breaks = seq(-180, 180, by = 1)) + #white lines on grey background. Happens anyway, don't need to force 10degrees
  # geom_contour(data = bf, aes(x = x, y = y, z = z), breaks = c(0), size = c(0.15), colour = "black") + # add coastline contour
  geom_contour(data = bf, aes(x = x, y = y, z = z), breaks = c(-300), size = c(0.1), colour = "black") +# add 300m contour
  geom_contour(data = bf, aes(x = x, y = y, z = z), breaks = c(-400), size = c(0.1), colour = "black") +# add 400m contour
  annotation_spatial(coast, fill = "grey", lwd = 0) +
  labs(x = "longitude", y = "latitude") +
  theme(legend.position = c(0.89, 0.1),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank()) +
  ggtitle(paste0("Andros Bathymetry, 300m & 400m contours")) +
  ggsave(paste0(saveloc, "Andros_Bathymetry_3-400mContours.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75,
         width = 4,
         height = 5,
         units = "in", dpi = 600, limitsize = TRUE)

# scale_colour_gradient2(
#   low = muted("blue"),
#   mid = "white",
#   high = muted("red"),
#   midpoint = 10,
#   space = "Lab",
#   na.value = "grey50",
#   guide = "colourbar",
#   aesthetics = "colour"
# )


#GEBCO####
# https://download.gebco.net/
library(raster)
R <- ("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bathymetry/GEBCO/gebco_2020_n25.374298095703125_s23.589019775390625_w-78.6016845703125_e-77.4261474609375.tif")
# R <- ("/home/simon/Documents/Si Work/PostDoc Work/Kroetz & Dedman Sawfish BRT/GIS shapefiles, rasters, etc/Everglades mask for sawfish project/July2020_updated_rasters/clip5_n/clip5_n1.tif")
# print(R)
# class(R)
R <- raster(R)
print(R)
plot(R)

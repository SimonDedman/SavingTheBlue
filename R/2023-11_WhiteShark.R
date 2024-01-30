# Tuna in Bahamas

# Load & Prep ####
library(dplyr) # group_by, mutate
library(tidyverse) # broom      cli        crayon     dbplyr     dplyr      forcats    ggplot2    haven      hms        httr       jsonlite   lubridate
# magrittr   modelr     pillar     purrr      readr      readxl     reprex     rlang      rstudioapi  rvest      stringr    tibble     tidyr      xml2       tidyverse
library(lubridate)
library(magrittr) # tidyverse magrittr doesn't load %<>%
library(tidylog)
library(data.table) # sudo apt install libgit2-dev # libssl-dev # libssh2-1-dev # libxml2-dev # libudunits2-dev # rgdal : gdal-config
library(sf)
library(ggspatial)
library(scales)
library(beepr)
library(openxlsx)
options(error = function() beep(9))  # give warning noise if it fails
machine <- "/home" #Aquarius
machine <- "C:/Users" # windows laptop

# 4. FishDays in Bahamas EEZ####
# ▪ Simple %. Spatial overlap R code, easy.
# ▪ Do we have EEZ shapefile? TG www.marineregions.org
# see /home/simon/Documents/Si Work/PostDoc Work/movegroup help/Liberty Boyd/Points in UD contours/PointsInWhichUDcontour.R

maploc = paste0(machine, "/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bahamas EEZ Shapefile/")
EEZ <- sf::st_read(paste0(maploc,"eez.shp")) # polygon

loadloc = paste0(machine, "/simon/Documents/Si Work/Blocklab/abft_diving/All_Daily") #per saveloc in MolaFoldersExtractLoop.R
# AllDailies <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames.Rds"))
AllDailies <- readRDS(file.path(loadloc, "AllDailies_HIFSDA_Stocknames_StocksPaperVersion.Rds"))

AllDailies %<>%
  sf::st_as_sf(coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) %>% # Convert points to sf
  mutate(Index = row_number()) # for indexing later

# pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly
TunainEEZ <- AllDailies[EEZ,]
TunaEEZsummer <- TunainEEZ |> filter(Month %in% c(5:10))
TunaEEZwinter <- TunainEEZ |> filter(Month %in% c(11:12, 1:4))

TunainEEZ |>
  group_by(Month) |>
  summarise(n()) |>
  sf::st_drop_geometry() |>
  rename(FishDays = `n()`) |>
  bind_rows(data.frame(Month = 7:10,
                       FishDays = c(0,0,0,0)
  )
  ) |>
  arrange(Month) |>
  mutate(Month = factor(Month)) |>
  ggplot() +
  geom_col(mapping = aes(x = Month, y = FishDays)) +
  theme(legend.position = c(0.84, 0.1), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggtitle("ABFT presence in Bahamas, FishDays per Month") # change sflines

ggsave(paste0(saveloc, today(), "_MonthlyVelByStock.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 6, #NA default. Manually adjust plot box in RStudio after ggplot()
       height = 4.5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE)


# Depth profiles in Bahamas ####
TunainEEZ |>
  sf::st_drop_geometry() |>
  summarise(MeanDepthDay = mean(MeanDepthDay, na.rm = TRUE),
            MeanDepthNight = mean(MeanDepthNight, na.rm = TRUE))# MeanDepthDay Night
# MeanDepthDay MeanDepthNight
#         157.           82.8

TunainEEZ |>
  sf::st_drop_geometry() |>
  group_by(Month, toppid) |>
  summarise(MeanDepthDay = mean(MeanDepthDay, na.rm = TRUE),
            MeanDepthNight = mean(MeanDepthNight, na.rm = TRUE)) |> # MeanDepthDay Night
  rowwise() |>
  mutate(BothNan = ifelse(all(c(is.nan(MeanDepthDay), is.nan(MeanDepthDay))), TRUE, FALSE)) |>
  ungroup() |>
  filter(!BothNan) |>
  select(!BothNan) |>
  bind_rows(data.frame(Month = 7:10,
                       toppid = c(0,0,0,0),
                       MeanDepthDay = c(NA,NA,NA,NA),
                       MeanDepthNight = c(NA,NA,NA,NA)
  )) |>
  # need to pivot so Day & Night depth are the same column, and DayNight is a separate column
  pivot_longer(cols = c(MeanDepthDay, MeanDepthNight),
               names_to = "DayNight",
               names_prefix = "MeanDepth",
               values_to = "Depth") |>
  mutate(Month = factor(Month),
         toppid = factor(toppid)) |>
  arrange(Month, toppid) |>
  mutate(Depth = -Depth) |>
  ggplot() +
  geom_violin(mapping = aes(x = Month, y = Depth, fill = DayNight)) +
  theme(legend.position = c(0.84, 0.1), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        # legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggtitle("ABFT day/night depth violins while in Bahamas, per Month") # change sflines




# Tracks for fish which go to Bahamas ####
TunainEEZ |>
  sf::st_drop_geometry() |>
  pull(toppid) |>
  unique() |>
  write.csv(file.path("C:/Users/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2023-11_White_Sharks", "BahamasTuna.csv"), row.names = F)


TunainEEZ |>
  sf::st_drop_geometry() |>
  group_by(toppid) |>
  summarise(Stock = first(Stock)) |>
  write.csv(file.path("C:/Users/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2023-11_White_Sharks", "BahamasTuna.csv"), row.names = F)

# Tracks copied to
# C:\Users\simon\Documents\Si Work\PostDoc Work\Saving The Blue\Projects\2023-11_White_Sharks\BahamasTunaTracks\MedTracks
# and GOM tracks, by eye
# Need a better polygon than EEZ if the true aim is to capture when tuna/sharks are in the middle of the Bahamas,
# not just touching the EEZ on their way to/from the GOM.

# Once I have a better polygon,
# Use it to filter toppids with any days in the polygon
# list toppids
tunalist <- "FILLME"
# loop/apply full depth etc files in
# C:\Users\simon\Documents\Si Work\Blocklab\abft_diving\7_DO2_append\7_DO2_append.7z
for (i in tunalist) {
  # "5104527_A2198_NO04_L12D.Rds"
  df_i <- readRDS(file.path("C:/Users/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2023-11_White_Sharks/BahamasTunaTracks", i))
  # filter out rows not in polygon
  df_i %<>%
    sf::st_as_sf(coords = c("lon","lat")) %>%
    sf::st_set_crs(4326) # Convert points to sf
  # pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly
  df_i <- df_i[EEZ,]

  # Add toppid column
  df_t$toppid <- i # assuming loop

  # filter out extraneous columns. Keep Date, Depth.m. isday
  # or don't
  df_i %<>% select(toppid, Date, Depth.m., isday)

  # create complete df # will be massive
  if (!exists("df_all")) {
    df_all <- df_i
  } else {
    df_all |>
      bind_rows(df_i)
  }

  # remove df_i
  rm(df_i)

  # end loop
}
# make day/night depth histograms, Sammy's bins
# pick a specific known fish and try this: 5102146, GOM




# 2024-01-04 acoustic map ####
loadloc <- paste0(machine, "/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2023-11_White_Sharks") #per saveloc in MolaFoldersExtractLoop.R
shark <- openxlsx::read.xlsx(xlsxFile = file.path(loadloc, "Skomal_White shark_Detections_ALL_Andros.xlsx")) |>
  # datelastmodified dd/mm/yyyy
  # datecollected dd/mm/yyyy hh:mm
  mutate(
    datelastmodified = as.POSIXct(datelastmodified * 3600 * 24,
                                  origin = "1899-12-30",
                                  tz = "UTC"),
    datecollected = as.POSIXct(datecollected * 3600 * 24,
                               origin = "1899-12-30",
                               tz = "UTC"),
    station = factor(station, levels = c("DROP5", "DROP4", "DROP3", "DROP2", "DROP1", "CCDROP", "BWCDROP")),
    fieldnumber = case_match(fieldnumber,
                             "A69-9002-4976" ~ "A69-9002-4975",
                             .default = fieldnumber)
  )
metadata <- openxlsx::read.xlsx(xlsxFile = file.path(loadloc, "Skomal_White shark_Detections_ALL_Andros.xlsx"),
                                sheet = 2)

receivers <- shark |>
  group_by(station) |>
  summarise(latitude = mean(latitude),
            longitude = mean(longitude),
            rcvrcatnumber = first(rcvrcatnumber),
            sensorname = first(sensorname),
            the_geom = first(the_geom))

allreceivers <- openxlsx::read.xlsx(xlsxFile = file.path("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Data/Acoustic/", "otn-instrument-deployment-short-form_GUTTRIDGE_2023_August.xlsx"),
                                    sheet = 2) |>
  group_by(STATION_NO) |>
  summarise(deploydate = min(`DEPLOY_DATE_TIME.(yyyy-mm-ddThh:mm:ss)`),
            bottomdepth = mean(BOTTOM_DEPTH, na.rm = TRUE)) |>
  rename(station = STATION_NO)

receivers <- receivers |>
  left_join(allreceivers)

# Map
# Detection/movement locations
## colour track by shark: fieldnumber
# sf_shark <- sf::st_as_sf(shark, coords = c("longitude","latitude")) %>%
#   sf::st_set_crs(4326)

# bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
myLocation <- c(min(shark$longitude),
                min(shark$latitude),
                max(shark$longitude),
                max(shark$latitude))

# plot(shark$longitude, shark$latitude)

googlemap = TRUE
if (googlemap) { # grow bounds extents if requested
  expandfactor <- 11 #1.6,3,6, 1.3 - 1.5 same zoom as 1. 1.6 is a big leap up in zoom (out). 1.9 (& maybe 1.7 or 1.8 is another step out, done manually for #2, 200368, because some points were out of bounds.)
  # Still need to improve this part
  xmid <- mean(myLocation[c(1,3)])
  ymid <- mean(myLocation[c(2,4)])
  xmax <- ((myLocation[3] - xmid) * expandfactor) + xmid #updated for sf/st
  xmin <- xmid - ((xmid - myLocation[1]) * expandfactor)
  ymax <- ((myLocation[4] - ymid) * expandfactor) + ymid
  ymin <- ymid - ((ymid - myLocation[2]) * expandfactor)
  myLocation <- c(xmin, ymin, xmax, ymax)
}

# ggmap::register_google("see /Code")

myMap <- ggmap::get_map(location = myLocation,
                        source = "google",
                        maptype = "satellite",
                        crop = FALSE)

# Automate width * height adjustments for different map extent / ratio
# 6 (manually chosen width, below), divided by width range times by height range
# Maintains ratio by scales height to width(6). Then *1.2 because it still wasn't perfect.
# attr(myMap, "bb")[[4]] - attr(myMap, "bb")[[2]] # longitude, x, width, bind as 6
# attr(myMap, "bb")[[3]] - attr(myMap, "bb")[[1]] # latitude, y, height
autoheight <- (6 / (attr(myMap, "bb")[[4]] - attr(myMap, "bb")[[2]])) * (attr(myMap, "bb")[[3]] - attr(myMap, "bb")[[1]]) * 1.2

allpings <- shark |>
  select(fieldnumber,
         datecollected,
         station,
         latitude,
         longitude) |>
  group_by(station) |>
  summarise(longitude = mean(longitude),
            latitude = mean(latitude),
            nHits = n()) |>
  select(longitude, latitude, everything())

sharkpings <- shark |>
  select(fieldnumber,
         datecollected,
         station,
         latitude,
         longitude) |>
  group_by(station, fieldnumber) |>
  summarise(longitude = mean(longitude),
            latitude = mean(latitude),
            nSharks = n()) |>
  select(longitude, latitude, everything()) |>
  group_by(station) |>
  summarise(longitude = mean(longitude),
            latitude = mean(latitude),
            nSharks = n()) |>
  select(longitude, latitude, everything())


ggmap::ggmap(myMap) +
  geom_point(data = allpings,
             aes(x = longitude,
                 y = latitude,
                 size = nHits), # points per hit, coloured by month   fill = monthcollected
             shape = 19,
             # size = 2,
             colour = "black") +
  geom_point(data = sharkpings,
             aes(x = longitude,
                 y = latitude,
                 size = nSharks), # points per hit, coloured by month   fill = monthcollected
             shape = 19,
             # size = 2,
             colour = "red") +
  # nobody but us care a out the receiver station ID names
  # though we need them to link to other non-map plots
  geom_label(data = receivers,
             aes(x = longitude,
                 y = latitude,
                 label = station), # points per hit, coloured by month   fill = monthcollected
             nudge_x = -0.01,
             size = 2.2) +

  # scale_fill_gradientn(colours = rev(rainbow(12)), limits = c(1, 12), # month colour controls
  #                      labels = month.abb, breaks = 1:12) +
  # geom_point(aes(x = longitude,
  #                y = latitude,
  #                shape = Name), # add deploy triangle & last diamond
  #            data = receivers,
  #            fill = "white",
  #            colour = "black",
  #            size = 4) +
  scale_shape_manual(values = c(24, 23)) + # choose shapes manually, in order, deploy then last
  # geom_text(aes(x = Longitude - 0.3, y = Latitude, label = Date), # add deploy & last labels, shift location
  #           data = DeployLast, colour = "white") +
  labs(x = "Longitude", y = "Latitude", caption = paste0("Saving The Blue, ", lubridate::today())) +
  ggtitle(paste0("White shark acoustic detections off East Andros, Bahamas")) +
  theme(legend.position = c(0.1, 0.16), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.spacing.y = unit(0.1, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_rect(fill = "white", colour = NA), # element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(today(), "_GWS-Map.png"),
       plot = last_plot(), device = "png", path = loadloc, scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 5, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
       height = 5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", dpi = 600, limitsize = TRUE)

# Receiver locations with deployment dates [need from T]
# Shark Detection locations
# Shark movement locations within array

# Detections are on receivers i.e. detections = receiver locations = all spatial data will be in 7 points.
# what can we do with this?


# Basic stats ####
# number of hits per shark per receiver
shark |>
  group_by(station, fieldnumber) |>
  summarise(n = n()) |>
  arrange(station)
# station fieldnumber        n
# DROP5   A69-9001-17848     2
# DROP5   A69-9001-17866     4
# DROP4   A69-9001-3077      2
# DROP4   A69-9001-62514     1
# DROP4   A69-9002-4975      1
# DROP3   A69-9001-6893      6
# DROP2   A69-9001-17866     2
# DROP2   A69-9002-4975      3
# DROP1   A69-9001-17866     2
# CCDROP  A69-9001-5287      4
# CCDROP  A69-9001-60747     2
# BWCDROP A69-9001-5287      3
# BWCDROP A69-9001-60747     4
# BWCDROP A69-9001-62514     6

# number sharks per receiver
shark |>
  group_by(station) |>
  summarise(n = length(unique(fieldnumber))) |>
  arrange(station)
# station     n
# DROP5       2
# DROP4       3
# DROP3       1
# DROP2       2
# DROP1       1
# CCDROP      2
# BWCDROP     3

# number of hits per month
shark |>
  group_by(monthcollected) |>
  summarise(n = n())
# monthcollected     n
# 2                  7
# 3                  4
# 4                 21
# 5                  4
# 11                 6

# number of hits per shark
shark |>
  group_by(fieldnumber) |>
  summarise(n = n()) |>
  arrange(desc(n))
# fieldnumber        n
# A69-9001-17866     8
# A69-9001-5287      7
# A69-9001-62514     7
# A69-9001-60747     6
# A69-9001-6893      6
# A69-9002-4975      4
# A69-9001-17848     2
# A69-9001-3077      2


# Abacus plot ####
ggplot(data = shark |>
         mutate(Date = as.Date(datecollected),
                Shark = factor(fieldnumber, levels = metadata |> # colour-ordered by shark size (default is shark ID ("fieldnumber")): Shark factor order by size
                                 arrange(SizeM) |>
                                 pull(fieldnumber)),
                Station = factor(station, levels = c("BWCDROP", "CCDROP", "DROP1", "DROP2", "DROP3", "DROP4", "DROP5"))) |> # ,Shark = factor(fieldnumber)
         group_by(Shark) |>
         filter(n() > 1) |> # remove 4975 only 1 hit
         ungroup() |>
         droplevels() |> # drop unused factor levels but the NA is because 4976 is missing from metadata
         left_join(metadata |> select(fieldnumber, SEX, locationTagged)) # icon shape could relate to sex
) +
  # border & shape
  geom_point(mapping = aes(x = Date,
                           y = Station,
                           fill = Shark, # definitely edits fill but all legend items are black (pch 21:24)
                           colour = factor(locationTagged, levels = c("New Brunswick, Canada", # definitely edits border colour but legend icons are filled  (pch 21:24)
                                                                      "Cape Cod, USA",
                                                                      "South Carolina, USA")),
                           shape = SEX),
             size = 6,
             stroke = 0.7, # point border width
  ) +
  scale_colour_manual(values = c("black", "blue", "red")) +
  geom_point(data = receivers,
             mapping = aes(x = as.Date(deploydate),
                           y = station),
             shape = 4) +
  scale_shape_manual(values = c(21:24)) + # have to be the shapes which have fill and colour
  guides(fill = guide_legend(override.aes = list(shape = 21))) + # https://stackoverflow.com/questions/77883100/ggplot-buggy-fill-and-colour-legends-for-shapes-pch-2125
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               date_minor_breaks = "1 month") +
  theme_classic() %+replace% theme(
    axis.text = element_text(size = rel(1.3)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1.5)),
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank(),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(today(), "_GWS-Abacus.png"),
       plot = last_plot(), device = "png", path = loadloc, scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 8.5, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
       height = 2.8, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", dpi = 600, limitsize = TRUE)



# raise issue as bug for ggplot ####
# https://stackoverflow.com/questions/77883100/ggplot-buggy-fill-and-colour-legends-for-shapes-pch-2125
data(mtcars)
mtcars$makeModel <- rownames(mtcars)
# shape - plot & legend as anticipated
ggplot(data = mtcars) +
  geom_point(mapping = aes(x = mpg,
                           y = makeModel,
                           shape = factor(gear))
  ) +
  scale_shape_manual(values = c(21:23))

# colour - plot & legend as anticipated
ggplot(data = mtcars) +
  geom_point(mapping = aes(x = mpg,
                           y = makeModel,
                           colour = factor(carb))
  )

# fill - does nothing on standard pch - plot & legend both black
ggplot(data = mtcars) +
  geom_point(mapping = aes(x = mpg,
                           y = makeModel,
                           fill = factor(carb))
  )

# shape & fill - fills correctly on plot but legend colours all black. Bug candidate.
ggplot(data = mtcars) +
  geom_point(mapping = aes(x = mpg,
                           y = makeModel,
                           shape = factor(gear),
                           fill = factor(carb))
  ) +
  scale_shape_manual(values = c(21:23))

# shape & colour - colours correctly on plot, legend colours are correct but fills not borders. Bug candidate 2.
ggplot(data = mtcars) +
  geom_point(mapping = aes(x = mpg,
                           y = makeModel,
                           shape = factor(gear),
                           colour = factor(carb))
  ) +
  scale_shape_manual(values = c(21:23))

# shape & colour & fill - colours correctly on plot, legend colours are correct but fills not borders. Fills are correct on plot but legend colours all black. Both bugs together.
ggplot(data = mtcars) +
  geom_point(mapping = aes(x = mpg,
                           y = makeModel,
                           shape = factor(gear),
                           fill = factor(cyl),
                           colour = factor(carb))
  ) +
  scale_shape_manual(values = c(21:23))

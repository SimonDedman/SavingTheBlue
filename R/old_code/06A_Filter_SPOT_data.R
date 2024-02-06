### ====================================================================================================
### Project:    Satellite Telemetry
### Analysis:   Initial filtering of KF satellite telemetry data of fin-mounted SPOT tags for further steps
### Author:     Vital Heim
### Version:    1.0
### ====================================================================================================

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Content: this R script contains the code to filter, clean and process raw data collected by fin-
###          mounted Smart Position and Temperature (SPOT) transmitters. The here used data was processed
###          at the satellite level using a Kalman Filter (KF).
###          To filter the raw data we used the speed-distance-angle filter from the argosfilter
###          package by Freitas et al. 2008.
###          Here processed data can then be used in further scripts and steps to model and analyze
###          SPOT tag data.
###          The filtering steps is parallelized allowing to filter multiple IDs at once.
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ....................................................................................................
### [A] Setwd, paths and parameters ----
### ....................................................................................................

# A1: clear memory ----

rm(list = ls())

# A2: load necessary packages ----

## install
# install.packages("devtools")
require("devtools")

devtools::install_version("argosfilter", version = "0.70")
install.packages("trip")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("patchwork")
install.packages("sf")
install.packages("sp")
install.packages("purrr")
install.packages("furrr")
install.packages("pander")
install.packages("ggplot2")
install.packages("ggspatial")
install.packages("RColorBrewer")
install.packages("xts")

## load
library(tidyverse)
library(magrittr)
library(patchwork)
library(sf)
library(sp)
library(purrr)
library(furrr)
library(pander)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(xts)
library(argosfilter) # to filter raw Argos data, i.e. SPOT tag data
library(trip)

# A3: Specify needed functions

## Function to deal with near duplicate  Argos observations
make_unique <- function(x) {
  xts::make.time.unique(x$date, eps = 10) # eps = number of seconds to make unique
}

# A4: Specify data and saveloc ----
saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Current/For_movegroup_tutorials/Output_data/SDA_filter/" # Adjust this

### ...............................................................................................
### [B] Data import and initial check and filtering of missing information ----
### ................................................................................................

# B1: Import data ----

## Movement data
mov_all <- "C:/Users/Vital Heim/switchdrive/Science/Current/For_movegroup_tutorials/Input_data/"
all_csv <- dir(mov_all, recursive = T, full.names = T, pattern = "\\-Locations.csv$") # import files in folders in path directory all at once
mydata <- lapply(all_csv, read.csv, sep = ",", dec = ".", stringsAsFactor = F, header = T) # import all .csv files containing TAT-Hiso data, but skip header lines
mydets <- do.call("rbind", mydata)

## Shark metadata
tags <- read.table("C:/Users/Vital Heim/switchdrive/Science/Current/For_movegroup_tutorials/Input_data/Metadata_SPOT_tags_movegroup_tutorial.csv", sep = ",", dec = ".", header = T, na.strings = c("", " ", NA))
tags$datetime_deployment <- as.POSIXct(tags$datetime_deployment, format = "%Y-%m-%d %H:%M", tz = "US/Eastern")
attr(tags$datetime_deployment, "tzone") <- "UTC" # change to UTC so deployment times match Argos raw data
tags <- dplyr::select(tags, ptt_id, species, sex, datetime_deployment, deployment_lat, deployment_lon, pcl, fl, stl)
colnames(tags) <- c("id", "species", "sex", "date", "lat", "lon", "pcl", "fl", "stl")
tags$lc <- "G" # we add a location class criteria for later joining with the movement data. we define the LC as "G" for gps, so that we do not need to worry about smaj,smin,eor
tags$id <- as.character(tags$id)

### create df with just tagging date
tagging_date <- tags[, c(1, 4)] # df for filtering of observations pre-deployment

### create df with just tagging location information
tagging_location <- tags[, c(1, 4, 10, 6, 5)] # df to add tagging location as observation for ctcrw fitting
tagging_location$smaj <- 50
tagging_location$smin <- 50
tagging_location$eor <- 0
colnames(tagging_location) <- c("id", "date", "lc", "lon", "lat", "smaj", "smin", "eor")

# B2: Basic housekeeping ----

ddet <- mydets %>%
  dplyr::select( # select relevant columns, here: id, date, location class (lc), lon, lat,
    Ptt,
    Date,
    Type,
    Quality,
    Longitude,
    Latitude,
    Error.Semi.major.axis,
    Error.Semi.minor.axis,
    Error.Ellipse.orientation
  ) %>%
  dplyr::filter(
    !Type %in% c("User"), # locations that were user specified within the Wildlife Data Portal
    !Quality %in% c("Z") # Z locations are invalid locations - remove them
  ) %>%
  dplyr::mutate( # define Date format
    Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y", tz = "UTC", usetz = T),
    Ptt = as.character(Ptt) # define tag id as character class
  ) %>%
  dplyr::rename( # rename the columns so they fit the requirements for the fit functions
    id = Ptt,
    date = Date,
    lc = Quality,
    lon = Longitude,
    lat = Latitude,
    smaj = Error.Semi.major.axis,
    smin = Error.Semi.minor.axis,
    eor = Error.Ellipse.orientation
  ) %>%
  dplyr::select( # remove Type column as it is not needed later
    -Type
  )

## add tagging date for subsequent filtering
ddet$tagging.date <- NA

for (i in 1:nrow(ddet)) {
  ddet[i, 9] <- as.character(tagging_date[which(tagging_date$id == ddet[i, 1]), 2])
}

## remove detections pre-tag deployment and add tagging location as data point
ddet %<>%
  dplyr::filter( # remove occurences that happened before the release time
    date >= tagging.date
  ) %>%
  dplyr::select( # get rid of unneeded columns
    -tagging.date
  ) %>%
  dplyr::bind_rows( # add tagging location as observation of class "gps"
    dplyr::semi_join(tagging_location, ddet, by = "id")
  ) %>%
  dplyr::arrange( # arrange by timestamp by individual so df can be used for fit_() functions
    id,
    date
  )

# B3: Deal with near duplicate observations ----

# Argos data often contain near duplicate records. These are identified by location estimates
# with the same date-time but differing coordinate or error values. In theory, crawl::crwMLE()
# can handle these situations, but we have found it is more reliable to fix these records.
# The first option for fixing the records would be to eliminate one of the duplicate records.
# However, it is often not possible to reliably identify which record is more appropriate to
# discard. For this reason, we advocate adjusting the date-time value for one of the records
# and increasing the value by 10 second. To facilitate this, we will rely on the
# xts::make.time.unique() function.

ddet_tc <- ddet %>% # create time corrected df
  dplyr::arrange(id, date) %>%
  dplyr::group_by(id) %>%
  tidyr::nest() %>%
  dplyr::mutate(unique_time = purrr::map(data, make_unique)) %>%
  tidyr::unnest(cols = c(data, unique_time)) %>%
  dplyr::select(-date) %>%
  dplyr::rename(date = unique_time)

dup_times <- ddet_tc %>%
  dplyr::group_by(id) %>%
  dplyr::filter(duplicated(date)) # if 0 you are good to go as you do not have near duplicate location estimates

## data summary
ddet %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    num_locs = n(),
    start_date = min(date),
    end_date = max(date)
  ) ## We see that there are also datapoints in there from the tag initiation, we will deal with this later

# B4: visualise raw data ----

sf_ddet <- sf::st_as_sf(ddet, coords = c("lon", "lat")) %>%
  sf::st_set_crs(4326)

sf_lines <- sf_ddet %>%
  dplyr::arrange(id, date) %>%
  sf::st_geometry() %>%
  sf::st_cast("MULTIPOINT", ids = as.integer(as.factor(sf_ddet$id))) %>%
  sf::st_cast("MULTILINESTRING") %>%
  sf::st_sf(deployid = as.factor(unique(sf_ddet$id)))

esri_ocean <- paste0(
  "https://services.arcgisonline.com/arcgis/rest/services/",
  "Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg"
)

## Define the number of colors you want
nb.cols <- length(unique(ddet$id))
mycolors <- colorRampPalette(brewer.pal(nb.cols, "YlOrRd"))(nb.cols)

ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  layer_spatial(sf_ddet, size = 0.5) +
  layer_spatial(sf_lines, size = 0.75, aes(color = deployid)) +
  scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
  scale_fill_manual(values = mycolors) +
  theme() +
  ggtitle("Observed Argos Location Paths - Raw data",
          subtitle = paste0("Male S. mokarran from the Florida Keys (222137) and Jupiter, FL (210603)")
  )

## save if needed
ggsave(paste0(saveloc, "Raw_argos_detections_pre_sda_filter.tiff"),
       width = 21, height = 15, units = "cm", device = "tiff", dpi = 150
)

### ...............................................................................................
### [C] Filter data based on speed, distance and turning angles using argosfilter::sdafilter() ----
### ................................................................................................

## To filter the track based on speed and turning angles, we use the package
## "argosfilter" from Freitas et al. 2008

## To filter data based on turning angles, speed, distance etc. one can use the sdafilter()
## function. The locations are filtered using the Freitas et al. 2008 algorithm.

## Locations are filtered using the algorithm described in Freitas et al. (2008). The algorithm first
## removes all locations with location class Z (-9), which are the points for which the location process
## failed. Then all locations requiring unrealistic swimming speeds are removed, using the MacConnell et al. (1992) algorithm, unless the point is located at less than 5 km from the previous
## location. This procedure enables retaining good quality locations for which high swimming speeds
## result from location being taken very close to each other in time. The default maximum speed
## threshold is 2 m/s. The last step is optional, and enables to remove unlikely spikes from the animal’s path. The angles of the spikes should be specified in ang, and their respective length in
## distlim. The default is c(15,25) for ang and c(2500,5000) for distlim, meaning that all spikes
## with angles smaller than 15 and 25 degrees will be removed if their extension is higher than 2500
## m and 5000 m respectively. No spikes are removed if ang=-1. ang and distlim vectors must have
## the same length

## The output will be a vector with the following elements:
## "removed" = location removed by filter
## "not" = location not removed
## "end_location" = location a the start/end of the track where the algorithm could not be applied

## The function needs the following arguments
## lat = numeric vector of latitudes in decimal degrees
## lon = numeric vector of longitudes in decimal degrees
## dtime = a vector of class POSIXct with date and time for each location
## lc = a numeric or character vector of Argos location classes. Z classes can be entered as "Z", "z", or -9
## vmax = speed threshold in ms-1. Default is 2ms-1
## ang = anles of the spikes to be removed. Default is c(15,25), No spikes are removed if ang = -1.
## distlim = lengths of the above spikes, in meters. Default is c(2500,5000).

# This analysis can be run in series, however, the process allows parallel processing.
# This can be done by using a purrr  and nested column tibble data structure.

# C1: apply filter ----

future::plan(multisession)

prefilter_obs <- ddet_tc

ddet_af <- ddet_tc %>%
  dplyr::arrange(id, date) %>%
  dplyr::group_by(id) %>%
  tidyr::nest()

# tbl_locs %>% dplyr::summarise(n = n())

ddet_af <- ddet_af %>%
  dplyr::mutate(
    filtered = furrr::future_map(
      data,
      ~ argosfilter::sdafilter(
        lat = .x$lat,
        lon = .x$lon,
        dtime = .x$date,
        lc = .x$lc,
        vmax = 2.1, # choose a biologically reasonable travelling speed for your study species
        ang = c(15, 25), # choose biologically reasonable turning angles
        distlim = c(5000, 8000) # choose the distance (in meters) between two consecutive locations for which the filter shold be applied
      )
    )
  ) %>%
  tidyr::unnest(cols = c(data, filtered)) %>%
  dplyr::filter(filtered %in% c("not", "end_location")) %>%
  dplyr::select(-filtered) %>%
  dplyr::arrange(id, date)

cat("You removed ", nrow(prefilter_obs) - nrow(ddet_af), " locations.")
prefilter_obs %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(n = n())
ddet_af %>% dplyr::summarise(n = n())

## Visualise the filtered tracks

esri_ocean <- paste0(
  "https://services.arcgisonline.com/arcgis/rest/services/",
  "Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg"
)

af_sf <- sf::st_as_sf(ddet_af, coords = c("lon", "lat")) %>%
  sf::st_set_crs(4326)

af_lines <- af_sf %>%
  dplyr::arrange(id, date) %>%
  sf::st_geometry() %>%
  sf::st_cast("MULTIPOINT", ids = as.integer(as.factor(af_sf$id))) %>%
  sf::st_cast("MULTILINESTRING") %>%
  sf::st_sf(id = as.factor(unique(af_sf$id)))

ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  layer_spatial(sf_ddet, size = 0.5) +
  layer_spatial(af_lines, size = 0.75, aes(color = id)) +
  scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
  # scale_fill_manual() +
  theme() +
  ggtitle("Argos detections with argosfilter::sdafilter()",
          subtitle = paste0("Male S. mokarran from the Florida Keys (222137) and Jupiter, FL (210603);
argsofilter::sdafilter() removes ", nrow(prefilter_obs) - nrow(ddet_af), " locations.")
  )

## save if needed
ggsave(paste0(saveloc, "Argos_detections_with_argosfilter_sdafilter.tiff"),
       width = 21, height = 15, units = "cm", device = "tiff", dpi = 150
)

# C2: Save the data ----

start <- as.Date(min(ddet_af$date), format = "%Y-%m")
end <- as.Date(max(ddet_af$date), format = "%Y-%m")

## txt
# write.table(ddet_af, paste0(saveloc, "Argosfilter_filtered_Sphyrna_SPOT_tracks_multiID_", start, "_", end,".txt"), row.names=F,sep=",",dec=".")

## csv
write.table(ddet_af, paste0(saveloc, "Argosfilter_filtered_Sphyrna_SPOT_tracks_multiID_", start, "_", end, ".csv"), row.names = F, sep = ",", dec = ".")

## RDS
saveRDS(ddet_af, paste0(saveloc, "Argosfilter_filtered_Sphyrna_SPOT_tracks_multiID_", start, "_", end, ".R"))

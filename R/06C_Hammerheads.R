#TOP####
# 2023-05-30 Simon Dedman simondedman@gmail.com
# kmeans & movegroup aka dBBMM

rm(list = ls())

library(tidyverse)
library(dplyr)
library(magrittr)
library(amt) #https://arxiv.org/pdf/1805.03227.pdf #sudo apt install libgsl-dev
library(RcppRoll)
library(lwgeom)
library(TropFishR) #VBGF
library(data.table)
library(lubridate)
library(beepr)
## if run by SD
source('~/Dropbox/Blocklab Monterey/Blocklab/liRolling.R') #my own function for rolling Linearity Index values
## if run by VH
# source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/vanMoorter-et-al_2010/liRolling.R") # Simon's function for rolling Linearity Index values
source("//Sharktank/Science/Rscripts/vanMoorter-et-al_2010/liRolling.R") # Simon's function for rolling Linearity Index values
options(error = function() beep(9))  # give warning noise if it fails
options(timeout = 3000) # manually increase time out threshold (needed when downloading basemap)
# for data cleaning
library(moments)
# for vanmoorter
library(beepr)
library(rgl) # # sudo apt install libglu1-mesa-dev
library(clusterSim)
## if run by SD
source('/home/simon/Dropbox/Blocklab Monterey/Blocklab/vanMoorter.etal.2010/p7_gap.statistic.r')
## if run by VH
# source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/vanMoorter-et-al_2010/p7_gap.statistic.r")
source("//Sharktank/Science/Rscripts/vanMoorter-et-al_2010/p7_gap.statistic.r")
# for movegroup
library(sf)
# remotes::install_github("SimonDedman/movegroup")
library(movegroup)
## to deal with shapefiles for EEZ
# install.packages("nngeo")
library(nngeo)
## if run by SD
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement"
## if run by VH
# saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/"
saveloc <- "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_output/"


options(warn=1) #set this to two if you have warnings that need to be addressed as errors via traceback()

## Function for range standardization
STrange <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

## Function to reverse-transform range standardization
rt_STrange <- function(x,y) {
  y*(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)) + min(x, na.rm = TRUE)
}

## if you want to use stamen/stadia and/or google maps for plotting your UD rasters, you need to register your corresponding API key first
## for google maps this can be done in the movegroup::plotraster() function with the gmapsAPI = "YOUR_KEY_HERE" argument.
## For stamen/stadia maps you can register your key here (this will be updated in the movegroup package in the near future)

ggmap::register_stadiamaps("YOUR_API_KEY_HERE", write = F)

### ....................................................................................................
### [A] Calculate time within Bahamian EEZ ----
### ....................................................................................................

## To calculate the nr. days each shark spent within the Bahamian EEZ, i.e. Shark Sanctuary,
## we use the CTCRW/SSM output fitted at original observation times to avoid loosing detections separated
## by long time gaps as this is not of concern for this calculation.

# A1. Data import -----

# ▪ Simple %. Spatial overlap R code, easy.
# ▪ Do we have EEZ shapefile? TG www.marineregions.org
# see /home/simon/Documents/Si Work/PostDoc Work/movegroup help/Liberty Boyd/Points in UD contours/PointsInWhichUDcontour.R
# hammerssf <- readRDS(file = paste0(saveloc, "/EEZoverlap/Hammers.Rds")) |> #SD
hammersSanctuary <- readRDS(file = "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_output/CTCRW/rerouted/Data_aniMotum_CRW_output_entire_track_rerouted_proj_WGS84_converted_with_coord_CIs_with_Argosfilter_data.rds") |> #VH
  dplyr::select(
    c(id, date, lon, lat)
  ) |>
  sf::st_as_sf(coords = c("lon","lat")) |> sf::st_set_crs(4326) |> # Convert points to sf
  mutate(Index = row_number()) # for indexing later
# maploc = "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bahamas EEZ Shapefile/" #SD
# EEZ <- sf::st_read(paste0(maploc,"eez.shp")) # SD, polygon
maploc = "//Sharktank/Science/Data_raw/Shapefiles/Bahamas/" #VH
eez_raw <- sf::st_read(paste0(maploc,"Bahamas_EEZ.shp")) |>
  sf::st_as_sf(coords = c("lon","lat")) |>sf::st_set_crs(4326) # VH, polygon

## make sure that your EEZ shapefile does not cut around landmass
EEZ<- nngeo::st_remove_holes(eez_raw)

# A2. Calculate nr. detections within EEZ----

# pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly
hammerssfinEEZ <- hammersSanctuary[EEZ,]
# hammerssfinAndros <- hammerssf |> filter(group == "Andros")
# hammerssfinBimini <- hammerssf |> filter(group == "Bimini")
hammerssfinSummer <- hammersSanctuary |> filter(month(date) %in% c(5:10))
hammerssfinWinter <- hammersSanctuary |> filter(month(date) %in% c(11:12, 1:4))
# hammerssfinAndrosSummer <- hammerssf |> filter(group == "Andros", month(date) %in% c(5:10))
# hammerssfinAndrosWinter <- hammerssf |> filter(group == "Andros", month(date) %in% c(11:12, 1:4))
# hammerssfinBiminiSummer <- hammerssf |> filter(group == "Bimini", month(date) %in% c(5:10))
# hammerssfinBiminiWinter <- hammerssf |> filter(group == "Bimini", month(date) %in% c(11:12, 1:4))

# hammerssfinEEZAndros <- hammerssfinAndros[EEZ,]
# hammerssfinEEZBimini <- hammerssfinBimini[EEZ,]
hammerssfinEEZSummer <- hammerssfinSummer[EEZ,]
hammerssfinEEZWinter <- hammerssfinWinter[EEZ,]
# hammerssfinEEZAndrosSummer <- hammerssfinAndrosSummer[EEZ,]
# hammerssfinEEZAndrosWinter <- hammerssfinAndrosWinter[EEZ,]
# hammerssfinEEZBiminiSummer <- hammerssfinBiminiSummer[EEZ,]
# hammerssfinEEZBiminiWinter <- hammerssfinBiminiWinter[EEZ,]

hammersSanctuary$EEZ <- as.logical(FALSE)
hammersSanctuary[hammersSanctuary$Index %in% hammerssfinEEZ$Index, "EEZ"] <- as.logical(TRUE)

# hammerssfinAndros$EEZAndros <- as.logical(FALSE)
# hammerssfinAndros[hammerssfinAndros$Index %in% hammerssfinEEZAndros$Index, "EEZAndros"] <- as.logical(TRUE)
# hammerssf[hammerssf$Index %in% hammerssfinAndros$Index, "EEZAndros"] <- hammerssfinAndros$EEZAndros

# hammerssfinBimini$EEZBimini <- as.logical(FALSE)
# hammerssfinBimini[hammerssfinBimini$Index %in% hammerssfinEEZBimini$Index, "EEZBimini"] <- as.logical(TRUE)
# hammerssf[hammerssf$Index %in% hammerssfinBimini$Index, "EEZBimini"] <- hammerssfinBimini$EEZBimini

hammerssfinSummer$EEZSummer <- as.logical(FALSE)
hammerssfinSummer[hammerssfinSummer$Index %in% hammerssfinEEZSummer$Index, "EEZSummer"] <- as.logical(TRUE)
hammersSanctuary[hammersSanctuary$Index %in% hammerssfinSummer$Index, "EEZSummer"] <- hammerssfinSummer$EEZSummer

hammerssfinWinter$EEZWinter <- as.logical(FALSE)
hammerssfinWinter[hammerssfinWinter$Index %in% hammerssfinEEZWinter$Index, "EEZWinter"] <- as.logical(TRUE)
hammersSanctuary[hammersSanctuary$Index %in% hammerssfinWinter$Index, "EEZWinter"] <- hammerssfinWinter$EEZWinter

# hammerssfinAndrosSummer$EEZAndrosSummer <- as.logical(FALSE)
# hammerssfinAndrosSummer[hammerssfinAndrosSummer$Index %in% hammerssfinEEZAndrosSummer$Index, "EEZAndrosSummer"] <- as.logical(TRUE)
# hammerssf[hammerssf$Index %in% hammerssfinAndrosSummer$Index, "EEZAndrosSummer"] <- hammerssfinAndrosSummer$EEZAndrosSummer

# hammerssfinAndrosWinter$EEZAndrosWinter <- as.logical(FALSE)
# hammerssfinAndrosWinter[hammerssfinAndrosWinter$Index %in% hammerssfinEEZAndrosWinter$Index, "EEZAndrosWinter"] <- as.logical(TRUE)
# hammerssf[hammerssf$Index %in% hammerssfinAndrosWinter$Index, "EEZAndrosWinter"] <- hammerssfinAndrosWinter$EEZAndrosWinter

# hammerssfinBiminiSummer$EEZBiminiSummer <- as.logical(FALSE)
# hammerssfinBiminiSummer[hammerssfinBiminiSummer$Index %in% hammerssfinEEZBiminiSummer$Index, "EEZBiminiSummer"] <- as.logical(TRUE)
# hammerssf[hammerssf$Index %in% hammerssfinBiminiSummer$Index, "EEZBiminiSummer"] <- hammerssfinBiminiSummer$EEZBiminiSummer
#
# hammerssfinBiminiWinter$EEZBiminiWinter <- as.logical(FALSE)
# hammerssfinBiminiWinter[hammerssfinBiminiWinter$Index %in% hammerssfinEEZBiminiWinter$Index, "EEZBiminiWinter"] <- as.logical(TRUE)
# hammerssf[hammerssf$Index %in% hammerssfinBiminiWinter$Index, "EEZBiminiWinter"] <- hammerssfinBiminiWinter$EEZBiminiWinter

print(paste0("Percent of detections in Bahamas EEZ, all data: ", round(length(which(hammersSanctuary$EEZ)) / length(hammersSanctuary$EEZ) * 100, 1), "%; ", length(hammersSanctuary$EEZ), " detections"))
print(paste0("Percent of detections in Bahamas EEZ, Summer: ", round(length(which(hammerssfinSummer$EEZSummer)) / length(hammerssfinSummer$EEZSummer) * 100, 1), "%; ", length(hammerssfinSummer$EEZSummer), " detections"))
print(paste0("Percent of detections in Bahamas EEZ, Winter: ", round(length(which(hammerssfinWinter$EEZWinter)) / length(hammerssfinWinter$EEZWinter) * 100, 1), "%; ", length(hammerssfinWinter$EEZWinter), " detections"))

## calculate detections within EEZ by individual

for (thisshark in unique(hammersSanctuary$id)){ # ALLYEAR

  ## subset df
  sharksubsetssf <- hammersSanctuary |> filter(id == thisshark)

  # pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly

  ## filter within EEZ
  sharksubsetinEEZ <- sharksubsetssf[EEZ,]

  ## index
  sharksubsetssf$EEZ <- as.logical(FALSE)
  sharksubsetssf[sharksubsetssf$Index %in% sharksubsetinEEZ$Index, "EEZ"] <- as.logical(TRUE)

  ## print results
  print(paste0("This are the metrics for shark ", thisshark))
  print(paste0("Percent of detections in Bahamas EEZ, all data: ", round(length(which(sharksubsetssf$EEZ)) / length(sharksubsetssf$EEZ) * 100, 1), "%; ", length(sharksubsetssf$EEZ), " detections"))
}

for (thisshark in unique(hammerssfinSummer$id)){ ## SUMMER
  #thisshark = 222133
  ## subset df
  sharksubsetssf <- hammersSanctuary |> filter(id == thisshark)

  # pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly

  ## filter within EEZ
  sharksubsetinEEZ <- sharksubsetssf[EEZ,]

  ## index
  sharksubsetssf$EEZ <- as.logical(FALSE)
  sharksubsetssf[sharksubsetssf$Index %in% sharksubsetinEEZ$Index, "EEZ"] <- as.logical(TRUE)

  ## susbet df for summer
  sharksubsetinSummer <- hammerssfinSummer |> filter(id == thisshark)
  # pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly

  ## filter within EEZ
  sharksubsetinEEZSummer <- sharksubsetinSummer[EEZ,]


  ## index
  sharksubsetinSummer$EEZSummer <- as.logical(FALSE)
  sharksubsetinSummer[sharksubsetinSummer$Index %in% sharksubsetinEEZSummer$Index, "EEZSummer"] <- as.logical(TRUE)
  #sharksubsetssf[sharksubsetssf$Index %in% sharksubsetinSummer$Index, "EEZSummer"] <- sharksubsetinSummer$EEZSummer

  ## print results
  print(paste0("This are the metrics for shark ", thisshark))
  print(paste0("Percent of detections in Bahamas EEZ, Summer: ", round(length(which(sharksubsetinSummer$EEZSummer)) / length(sharksubsetinSummer$EEZSummer) * 100, 1), "%; ", length(sharksubsetinSummer$EEZSummer), " detections"))
}

for (thisshark in unique(hammerssfinWinter$id)){ # Winter
  ## subset df
  sharksubsetssf <- hammersSanctuary |> filter(id == thisshark)

  # pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly

  ## filter within EEZ
  sharksubsetinEEZ <- sharksubsetssf[EEZ,]

  ## index
  sharksubsetssf$EEZ <- as.logical(FALSE)
  sharksubsetssf[sharksubsetssf$Index %in% sharksubsetinEEZ$Index, "EEZ"] <- as.logical(TRUE)

  ## susbet df for winter
  sharksubsetinWinter <- hammerssfinWinter |> filter(id == thisshark)
  # pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly

  ## filter within EEZ
  sharksubsetinEEZWinter <- sharksubsetinWinter[EEZ,]

  ## index
  sharksubsetinWinter$EEZWinter <- as.logical(FALSE)
  sharksubsetinWinter[sharksubsetinWinter$Index %in% sharksubsetinEEZWinter$Index, "EEZWinter"] <- as.logical(TRUE)
  sharksubsetssf[sharksubsetssf$Index %in% sharksubsetinWinter$Index, "EEZWinter"] <- sharksubsetinWinter$EEZWinter

  ## print results
  print(paste0("This are the metrics for shark ", thisshark))
  print(paste0("Percent of detections in Bahamas EEZ, Winter: ", round(length(which(sharksubsetinWinter$EEZWinter)) / length(sharksubsetinWinter$EEZWinter) * 100, 1), "%; ", length(sharksubsetinWinter$EEZWinter), " detections"))
}

# A3: Calculate nr. days within Bahamas EEZ by individual ----

# 3.1: check if points are within the EEZ
### if you have not run 5, run this again:
# hammerssf <- readRDS(file = "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/EEZoverlap/Hammers_KMeans.Rds") |> #VH
#   sf::st_as_sf(coords = c("lon","lat")) |> sf::st_set_crs(4326) |> # Convert points to sf
#   mutate(Index = row_number()) # for indexing later
# # maploc = "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bahamas EEZ Shapefile/" #SD
# # EEZ <- sf::st_read(paste0(maploc,"eez.shp")) # SD, polygon
# maploc = "C:/Users/Vital Heim/switchdrive/Science/Data/Shapefiles/Bahamas/" #VH
# EEZ <- sf::st_read(paste0(maploc,"Bahamas_EEZ.shp")) # VH, polygon
#
# hammerssf$EEZ <- st_within(hammerssf, EEZ, sparse = F)

## 3.2: create overall df with month definitions
### this will make summaries easier later
hammersSanctuary <- hammersSanctuary %>%
  dplyr::mutate( #create season grouping variable
    season = with(.,case_when(
      (month(date) %in% c(5:10)) ~ "summer",
      (month(date)  %in% c(11:12, 1:4)) ~ "winter",
    ))
  )

## 3rd step: summarise data and calculate days within EEZ

hammersNew <- as.data.frame(hammersSanctuary)

# class(hammersNew$date) # [1] "POSIXct" "POSIXt" , need to change to as.Date
hammersNew$date <- as.Date(hammersNew$date)

### count the number days each shark spent within and outside the EEZ
daysinEEZ <- hammersNew %>%
  # dplyr::filter( # filter for detections within EEZ
  #   EEZ == TRUE
  # ) %>%
  dplyr::group_by( # by individual
    id,
    EEZ
  ) %>%
  dplyr::summarise(
    days = n_distinct(date)
  ); write.csv(daysinEEZ, paste0(saveloc, "EEZoverlap/Days_in_and_out_BAH_EEZ_by_shark_all.csv"), row.names = F)

### count the number days each shark spent within and outside the EEZ by season
daysinEEZseas <- hammersNew %>%
  # dplyr::filter( # filter for detections within EEZ
  #   EEZ == TRUE
  # ) %>%
  dplyr::group_by( # by individual
    id,
    season,
    EEZ
  ) %>%
  dplyr::summarise(
    days = n_distinct(date)
  ); write.csv(daysinEEZseas, paste0(saveloc, "EEZoverlap/Days_in_and_out_BAH_EEZ_by_shark_all_by_season.csv"), row.names = F)

### calculate the percentage of days each shark spent within the EEZ
dal <- hammersNew %>% # we need the entire track duration for each shark first
  dplyr::group_by( # by individual
    id
  ) %>%
  dplyr::summarise( # days tracked
    tagging = first(date),
    end = last(date),
    liberty = n_distinct(date)
  )

withinEEZ <- hammersNew %>% # count days within the EEZ only
  dplyr::filter ( # filter detections within EEZ only
    EEZ == TRUE
  ) %>%
  dplyr::group_by( # by individual
    id
  ) %>%
  dplyr::summarise(
    daysIN = n_distinct(date)
  )

percentEEZ <- dal %>% # calculate the percentage of days spent within the EEZ for each shark
  dplyr::left_join(withinEEZ, by = "id") %>% # combine days at liberty df and df containing the nr. days within
  dplyr::mutate(
    nr_days_within = ifelse(is.na(daysIN), 0, daysIN), # deal with individuals
    percent_days_in = (nr_days_within/liberty)*100
  ); write.csv(percentEEZ, paste0(saveloc, "EEZoverlap/Percent_within_EEZ_by_shark_mixed.csv"), row.names = F)


## ISSUE: as we can see the in and out days sum is sometimes larger than the days at liberty
## this is a result from having two detections a days and it can be that one is within and one outside of the EEZ on the same day
## Let's fix this:
days_clean <- hammersNew %>%
  dplyr::group_by(
    id,
    date
  ) %>%
  dplyr::summarise(
    daysIN = any(EEZ), .groups = "drop"
  ) %>%
  # days_clean %<>%
  dplyr::group_by(
    id
  ) %>%
  dplyr::summarise(
    inside = sum(daysIN),
    outside = sum(!daysIN),
    nr_days = n()
  ) %>%
  dplyr::mutate(
    percent_days_in = (inside/nr_days)*100
  ); write.csv(days_clean, paste0(saveloc,"EEZoverlap/Percent_days_within_EEZ_by_shark_clean.csv"), row.names = F)

## And by seasons
days_clean_seas <- hammersNew %>%
  dplyr::group_by(
    id,
    date,
    season
  ) %>%
  dplyr::summarise(
    daysIN = any(EEZ), .groups = "drop"
  ) %>%
  # days_clean %<>%
  dplyr::group_by(
    id,
    season
  ) %>%
  dplyr::summarise(
    inside = sum(daysIN),
    outside = sum(!daysIN),
    nr_days = n()
  ) %>%
  dplyr::mutate(
    percent_days_in = (inside/nr_days)*100
  ); write.csv(days_clean_seas, paste0(saveloc,"EEZoverlap/Percent_days_within_EEZ_by_shark_clean_by_season.csv"), row.names = F)

# A4: Plot detections within and outside EEZ ----

## define plotting colors and symbols
inoutcols <- c("#336699","#A50021")
seasonsym <- c("summer" = 21, "winter" = 25)

## find min/max lat/lons for plot boundaries
### Extract coordinates
coords <- st_coordinates(hammersSanctuary)
### Find the minimum latitude
min_lat <- min(coords[, "Y"]) # Y represents latitude in sf coordinates
max_lat <- max(coords[, "Y"])
min_lon <- min(coords[, "X"]) # X represents longitude in sf coordinates
max_lon <- max(coords[, "X"])

## re-source basemaps and original df if needed
cropmap <- sf::st_read(paste0(saveloc,"/CroppedMap/Crop_Map.shp")) # VH, polygon
## or
library(rnaturalearth)
bg = ne_countries(scale = 10, continent = 'north america', returnclass = "sf") # needs to be adjusted depending where your study site is

bah_eez <- read_sf("//Sharktank/Science/Data_raw/Shapefiles/Bahamas/Bahamas_EEZ_boundary.shp")
st_crs(bah_eez)
bah_eez <- st_transform(bah_eez, st_crs = proj4string(bathyR))
## only if you want to use the eez shapefile
xlim_eez <- c(min_lon, -70.5105)
ylim_eez <- c(20.3735, max_lat)

# hammers <- readRDS(file = "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/EEZoverlap/Hammers_KMeans.Rds")

## Plot for each individual
dir.create(paste0(saveloc, "EEZoverlap/individualPlots"))
for (thisshark in unique(hammersSanctuary$id)){
  # thisshark <- "222133"
  # filter your shark
  eezplot_df <- hammersSanctuary %>% dplyr::filter(id == thisshark)
  eezplot_df <- eezplot_df %>%
    dplyr::mutate(lon = sf::st_coordinates(.)[,"X"],
                  lat = sf::st_coordinates(.)[,"Y"])
  # path_coords <- st_coordinates(eezplot_df)
  #define factors
  eezplot_df$EEZ <- factor(eezplot_df$EEZ, levels = c("TRUE", "FALSE"))
  eezplot_df$season <- factor(eezplot_df$season, levels = c("summer", "winter"))
  ## create plot with dark themed background
  #p = basemap(dt, bathymetry = T, expand.factor = 1.2) + # for bathymetry with ggOceanMaps package
  p <- ggplot() +

    # lines and points
    geom_path(data = eezplot_df,
               aes(x=lon,y=lat),
               alpha = 1, linewidth = 0.5)+
    geom_point(data = eezplot_df,
            aes(x = lon, y = lat, group = season, fill = EEZ, shape = season),
            alpha = 0.8, size = 3.25, color = "black", show.legend = T
            ) +


    # basemap
    geom_sf(data = bg, color = "black")+ # color is for border of geom object
    coord_sf(xlim = range(coords[, "X"], na.rm = TRUE),
             ylim = range(coords[, "Y"] , na.rm = TRUE),
             expand = T)+

    # bahamas eez shapefile
    geom_sf(data = bah_eez, colour = "black", fill = NA, linewidth = .25) +
    coord_sf(xlim = xlim_eez,
             ylim = ylim_eez+.25,
             expand = T)+

    # formatting
    # labs(x=NULL, y=NULL,
    #      fill = 'kmeans cluster',
    #      color = 'kmeans cluster')+
    scale_shape_manual(values = seasonsym,
                       name = "Season",
                       drop = F)+
    # scale_color_manual(values = inoutcols) +
    scale_fill_manual(name = "EEZ",
                      breaks = c("TRUE","FALSE"),
                      values = inoutcols,
                      labels = c("inside","outside"),
                      drop = FALSE,
                      guide = guide_legend(override.aes = list(fill = inoutcols, shape = 22, color = inoutcols))
                      ) +


    theme_light()+
    #theme(panel.background = element_rect(fill = "gray26", linewidth = 0.5, linetype = "solid", color = "black")) +
    labs(x = "Longitude", y = "Latitude") +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          axis.text = element_text(size = 8)) +  # adjust size of lat/lon axis labels+
    ggtitle(paste0("Detections within and outside of Bahamian EEZ for ", thisshark))
  p

  ggsave(paste0(saveloc, "EEZoverlap/individualPlots/EEZoverlap_",thisshark,"_", Sys.Date(), ".tiff"), width = 15, height = 10, units = "cm", dpi = 300)
}

### ....................................................................................................
### [B] Calculate behavioural clusters using kmeans ----
### ....................................................................................................

# BO: re-prepare your environment ----

## clear environment, global options, etc.
rm(list = ls())

options(warn=1) #set this to two if you have warnings that need to be addressed as errors via traceback()
options(error = function() beep(9))  # give warning noise if it fails
options(timeout = 3000) # manually increase time out threshold (needed when downloading basemap)

## load necessary packages and source needed algorithms

library(tidyverse)
library(dplyr)
library(magrittr)
library(amt) #https://arxiv.org/pdf/1805.03227.pdf #sudo apt install libgsl-dev
library(RcppRoll)
library(lwgeom)
library(TropFishR) #VBGF
library(data.table)
library(lubridate)
library(beepr)
## if run by SD
source('~/Dropbox/Blocklab Monterey/Blocklab/liRolling.R') #my own function for rolling Linearity Index values
## if run by VH
source("//Sharktank/Science/Rscripts/vanMoorter-et-al_2010/liRolling.R") # Simon's function for rolling Linearity Index values
# for data cleaning
library(moments)
# for vanmoorter
library(rgl) # # sudo apt install libglu1-mesa-dev
library(clusterSim)
## if run by SD
source('/home/simon/Dropbox/Blocklab Monterey/Blocklab/vanMoorter.etal.2010/p7_gap.statistic.r')
## if run by VH
source("//Sharktank/Science/Rscripts/vanMoorter-et-al_2010/p7_gap.statistic.r")
# for movegroup
library(sf)
# remotes::install_github("SimonDedman/movegroup")
# library(movegroup)

## define saveloc
## if run by SD
# saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement"
## if run by VH
saveloc <- "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_output/kmeans/predicted/"

## define needed functions, universal variables etc.

### Function for range standardization
STrange <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

### Function to reverse-transform range standardization
rt_STrange <- function(x,y) {
  y*(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)) + min(x, na.rm = TRUE)
}

# B1: Data import ----

# hammers <- readRDS("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Data/Hammerhead SPOT tags/Output_data_for_kMeans_and_dBBMM/Data_aniMotum_CRW_output_fitted_proj_WGS84_converted_with_coord_CIs_S.mokarran.rds") |> # pre 2023-07-07
# create col from id, removing . & all after. Use for left_join
# mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\.")[,1] - 1)))

### START: if run by SD ###
#  movement data
#  use the regularised movement data output from the 12h CTCRW
# hammers <- readRDS("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement/CTCRW/Data_aniMotum_CRW_output_segmented_rerouted_proj_WGS84_converted_with_coord_CIs_with_Argosfilter_data.rds") |> # 2023-07-07 & post
#   mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1)))
#
# # metadata for length measurements
# meta <- read_csv("../../Data/Hammerhead SPOT tags/Datasheet_Bahamas_Smok_Tagging_Metadata_NEW.csv") |>
#   rename(shark = ptt_id,
#          FishLengthCm = stl)
# hammers %<>% left_join(meta)  # , by = join_by(shark == id) # doesn't work naming columns, has gotten worse.
# hammers %<>% filter(group != "Bimini") # 2023-08-08 remove Bimini sharks
### END: if run by SD ###

### START: if run by VH ###
# movement data
# if you use the regularised movement data output from the 12h CTCRW
hammers <- readRDS(file = paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CTCRW/rerouted/Data_aniMotum_CRW_output_segmented_rerouted_proj_WGS84_converted_with_coord_CIs_with_Argosfilter_data.rds"))|>
  mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1)))

# metadata for length measurements
meta <- read_csv("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/Datasheet_Andros_Smok_Tagging_Metadata.csv")|>
  dplyr::select( # keep needed columns only
    ptt_id,
    datetime_deployment,
    stl,
    sex,
    species
  ) |>
  dplyr::rename(
    shark = ptt_id,
    FishLengthCm = stl)

##  combine movement data with metadata
hammers %<>% left_join(meta)  # , by = join_by(shark == id) # doesn't work naming columns, has gotten worse.
### END: if run by VH ###

## VERY IMPORTANT: The movement data needs to have time stamps in ascending order,
## double check that this is true by ordering the dataframe
hammers %<>%
  dplyr::arrange(
    shark,
    date
  ) %>%
  dplyr::select( # and sine we are at it, the old id column can be removed
    -id
  )

# B2: Step Length & Turn Angles ----

# Step lengths will be calculated in km and relative to body length of corresponding sharks

# blocklab, diveclassify1transit
# > li5day {5 day linearity index. 1: linear paths, 0: tortuous paths}
# > StepLengthKm {distance from previous day; Km}
# > StepLengthBL {distance from previous day; body lengths of that fish}
# > TurnAngleRelDeg {turning angle from previous day, relative to previous day at 0}
# > TurnAngleAzimDeg  {turning angle from previous day, azimuthal degrees}
# > FishLengthCm {fish length in cm}
hammers$li5day <- as.numeric(rep(NA, nrow(hammers)))
hammers$StepLengthKm <- as.numeric(rep(NA, nrow(hammers)))
hammers$StepLengthBL <- as.numeric(rep(NA, nrow(hammers)))
hammers$TurnAngleRelDeg <- as.numeric(rep(NA, nrow(hammers)))
hammers$TurnAngleAzimDeg <- as.numeric(rep(NA, nrow(hammers)))
hammers$Index <- 1:nrow(hammers)

if (!all(is.na(hammers$lat))) { # if not all lats are NA, i.e. there's something to be done
  # amt steps etc here
  #StepLength, TurnAngleRelDeg, TurnAngleAzimDeg
  #dfi remove NA dates and lats and lons rows
  # df_nona <- hammers[!is.na(hammers$Date),] # omit rows with NA values for date, downsample to days only
  df_nona <- hammers
  df_nona[which(df_nona$lat > 90), "lat"] <- NA # lat over 90 breaks, impossible value, will fix upstream
  df_nona[which(df_nona$lat < -90), "lat"] <- NA # ditto
  df_nona[which(df_nona$lon > 180), "lon"] <- NA # lon over 180 breaks, impossible value, will fix upstream
  df_nona[which(df_nona$lon < -180), "lon"] <- NA # ditto
  df_nona <- df_nona[!is.na(df_nona$lat),] # omit rows with NA values for lat, downsample to days only
  df_nona <- df_nona[!is.na(df_nona$lon),] # omit rows with NA values for lon, downsample to days only

  # make a list with all id's to loop through
  fishlist <- unique(df_nona$shark)

  ## *B2.1: loop over id, to calculate li5day, and make track ----
  for (i in fishlist) { # i <- fishlist[1]
    df_nonai <- df_nona[which(df_nona$shark == i),] #subset to each fish
    print(paste0(which(fishlist == i), " of ", length(fishlist), "; adding transit dive data to ", i))
    setDF(df_nonai) # else liRolling breaks
    if (nrow(df_nonai) > 5) df_nonai$li5day <- liRolling(x = df_nonai,
                                                         coords = c("lon", "lat"), # original, "lat", "lon"
                                                         roll = 5)
    # 1: linear paths, 0: tortuous paths
    if (!nrow(df_nonai) > 1) next # track etc breaks with < 2 points
    track <- amt::mk_track(df_nonai,
                           .x = lon,
                           .y = lat,
                           .t = date, # was DateTimeUTCmin5
                           crs = 4326) %>%  #crs = sp::CRS("+init=epsg:4326")) %>%
      transform_coords(6931) # EPSG:6931 is WGS 84 / NSIDC EASE-Grid 2.0 North
    stps <- steps(track)
    # step  length  (sl;  in  CRSunits), 6931=m
    df_nonai$StepLengthKm <- c(NA, (stps$sl_/1000)) #add NA first since results start from second row

    # **convert StepLengthKM to per bodylength (BL) ----
    # 1 km/day = 1000m/day = 100000cm/day
    df_nonai$StepLengthBL <- (df_nonai$StepLengthKm * 100000) / df_nonai$FishLengthCm

    # results is distance in body lengths per day.
    # Body lengths per second (mean value per day):
    # summary(df_nonai$StepLengthBL/24/60/60)

    # **turning angles & track2 ----
    # turning angles (ta; in degrees;  notice that it cannot be calculated for steps that are not
    # preceded by a valid step), angles are between -pi and pi, *57.29578 to convert to +-180
    df_nonai$TurnAngleRelDeg <- c(NA, (stps$ta_*57.29578))

    # amt::movement_metrics
    # all could be added to metadata, one value per track
    # see behavr https://rethomics.github.io/behavr.html
    # straightness(track)
    # cum_dist(track)
    # tot_dist(track)
    # msd(track)
    # intensity_use(track)
    # sinuosity(track)
    # tac(track)
    ## Do I actually want 1 value per track though?? Up to 6 years, becomes meaningless.

    track2 <- amt::mk_track(df_nonai,
                            .x = lon,
                            .y = lat,
                            .t = date, # was DateTimeUTCmin5
                            crs = 4326) # sp::CRS("+init=epsg:4326")
    #TurnAngleAzimDeg: compass bearing of new direction, can compute for directionality, migration
    TurnAngleAzimDeg <- direction_abs(track2, full_circle = FALSE, zero_dir = "N", lonlat = TRUE, clockwise = TRUE) %>%
      as_degree
    TurnAngleAzimDeg <- TurnAngleAzimDeg[1:length(TurnAngleAzimDeg) - 1] #remove NA from back
    df_nonai$TurnAngleAzimDeg <- c(NA, TurnAngleAzimDeg) # add NA to front
    df_nonai %<>% dplyr::select(c(date, li5day, StepLengthKm, StepLengthBL, TurnAngleRelDeg, TurnAngleAzimDeg, FishLengthCm, shark))

    # **save track data to df ----
    setDT(hammers) # convert to data.table without copy
    setDT(df_nonai) # convert to data.table without copy
    # join and update "hammers" by reference, i.e. without copy
    # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
    # If you want to return only df_nonai that have a matching hammers (10i.e. rows where the key is in both tables), set the nomatch argument of data.table to 0.
    # nomatch isn't relevant together with :=, ignoring nomatch
    hammers[df_nonai, on = c("date", "shark"), li5day := i.li5day]
    hammers[df_nonai, on = c("date", "shark"), StepLengthKm := i.StepLengthKm]
    hammers[df_nonai, on = c("date", "shark"), StepLengthBL := i.StepLengthBL]
    hammers[df_nonai, on = c("date", "shark"), TurnAngleRelDeg := i.TurnAngleRelDeg]
    hammers[df_nonai, on = c("date", "shark"), TurnAngleAzimDeg := i.TurnAngleAzimDeg]
    hammers[df_nonai, on = c("date", "shark"), FishLengthCm := i.FishLengthCm]
  } # close i
  hammers <- hammers[order(hammers[,"Index"]),] #reorder by index
  hammers %<>% dplyr::select(-Index) # remove unneeded Index col. Might need later?
  hammers <- as.data.frame(hammers)
} else {# close if (!all(is.na(hammers$lat)))
  print("all new days missing latitude data, can't get external data, nothing to do")
}

# B3: Kmeans VanMoorter/SD ----

# p7_analysis_suppl_BL.R
# > kmeans2cluster {resident/transiting	movement cluster based on body lengths}
# > kmeansBinary {0/1	movement cluster based on body lengths}
# >> clusterinfo.csv saved in saveloc

## prepare cluster columns
hammers$kmeans2cluster <- as.character(rep(NA, nrow(hammers)))
hammers$kmeansBinary <- as.integer(rep(NA, nrow(hammers)))

# *B3.1:Prepare steplength & TA data ----

# /500 issue; auto scale 500 scalar based on input data L201; use KM not BL?; SL values shouldn't influence cluster results??
# VanM paper: We log-transformed both activity measures and steplength to reduce positive skew. Furthermore, we standardized all variable values on their range (Steinley 2006a).
# Because we found no outliers (no data seemed outlying by visual inspection of the distribution, nor were any observations >3.3 SDs from the mean, which corresponds to a density of 0.001 in a normal distribution), we did not remove any data before data standardization.
# To demonstrate the importance of data preprocessing, we first performed a cluster analysis on the raw data, these data after log transformation and then after range standardization. In subsequent analyses, we used the transformed and standardized data, which is the recommended strategy.
# The raw data without any data preprocessing did not reveal any cluster structure; the gap statistic was maximum for one cluster (i.e., no cluster structure). After log transforming both activity measures and step lengths, we found a structure with 2 clusters.
# Steinley, D. 2006a. K-means clustering: a half-century synthesis. British Journal of Mathematical and Statistical Psychology 59:1–34.
# Standardisation on the range (not ((data - mean)/SD)) recommended but can't get that paper:
# Steinley, D. (2004a). Standardizing variables in K-means clustering. In D. Banks, L. House, F. R. McMorris, P. Arabie, & W. Gaul (Eds.), Classification, clustering, and data mining applications (pp. 53–60). New York: Springer.
# Suspect it's just data-mean

# **test for positive skew, log transform if needed ----

## Steplengths
hist(hammers$StepLengthBL)
moments::skewness(hammers$StepLengthBL, na.rm = TRUE) # positive/negative skew, 0 = symmetrical. # p:2.993276, f: 7.74
moments::kurtosis(hammers$StepLengthBL, na.rm = TRUE) # long tailed? Normal distribution = 3. # p: 15.17431, f: 94.69
hammers$StepLengthBLlog1p <- log1p(hammers$StepLengthBL) # log transform
hist(hammers$StepLengthBLlog1p)
moments::skewness(hammers$StepLengthBLlog1p, na.rm = TRUE) # p:-0.13, f: -0.602 = fine
moments::kurtosis(hammers$StepLengthBLlog1p, na.rm = TRUE) # p:2.87, f: 3.11 = fine

## TurningAngles
hist(hammers$TurnAngleRelDeg)
moments::skewness(hammers$TurnAngleRelDeg, na.rm = TRUE) # positive/negative skew, 0 = symmetrical. # p:0.025, f: -0.008
moments::kurtosis(hammers$TurnAngleRelDeg, na.rm = TRUE) # long tailed? Normal distribution = 3. # p: 4.82, f: 5.033
## TA data does not need transformation
hammers$TurnAngleRelDeglog1p <- log1p(abs(hammers$TurnAngleRelDeg))

# **range standardisation of variables ----

# As per van Morter et al. 2010: "Due to the role of the minimum and maximum value of a variable in range standardization,
# it is crucial to inspect the data for outliers (i.e., atypical data that are distant from the rest of the data) before
# performing this standardization."
# And: "outliers (no data seemed outlying by visual inspection of the distribution, nor were any observations >3.3 SDs
# from the mean, which corresponds to a density of 0.001 in a normal distribution).

## test outliers, remove >3.3 SDs from the mean
## Given that the definition of outliers depends on the mean, and we will later calculate the kmeans by individual,
## we need to test for outliers at the individual level as well.

## safety copy in case I mess up
df_i <- hammers

## Plot values with outliers
png(filename = paste0(saveloc, "Kmeans-StepLength-TurnAngle-Scatter_", today(),"_all_data.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
plot(df_i$StepLengthBLlog1, df_i$TurnAngleRelDeglog1p, xlab = "log(Step Length [BLs]) w/ outliers", ylab = "log(abs(Turn Angle Relative Degress w/ outliers))")
dev.off()
## StepLength Outliers
if (any(df_i$StepLengthBLlog1p > mean(df_i$StepLengthBLlog1p, na.rm = TRUE) + sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$StepLengthBLlog1p[which(df_i$StepLengthBLlog1p > mean(df_i$StepLengthBLlog1p, na.rm = TRUE) + sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3)] <- NA
if (any(df_i$StepLengthBLlog1p < mean(df_i$StepLengthBLlog1p, na.rm = TRUE) - sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$StepLengthBLlog1p[which(df_i$StepLengthBLlog1p > mean(df_i$StepLengthBLlog1p, na.rm = TRUE) + sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3)] <- NA
## TurningAngles Outliers
if (any(df_i$TurnAngleRelDeglog1p > mean(df_i$TurnAngleRelDeglog1p, na.rm = TRUE) + sd(df_i$TurnAngleRelDeglog1p, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$TurnAngleRelDeg[which(df_i$TurnAngleRelDeg > mean(df_i$TurnAngleRelDeg, na.rm = TRUE) + sd(df_i$TurnAngleRelDeg, na.rm = TRUE) * 3.3)] <- NA
if (any(df_i$TurnAngleRelDeglog1p < mean(df_i$TurnAngleRelDeglog1p, na.rm = TRUE) - sd(df_i$TurnAngleRelDeglog1p, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$TurnAngleRelDeg[which(df_i$TurnAngleRelDeg > mean(df_i$TurnAngleRelDeg, na.rm = TRUE) + sd(df_i$TurnAngleRelDeg, na.rm = TRUE) * 3.3)] <- NA
## Plot values without outliers
png(filename = paste0(saveloc, "Kmeans-StepLength-TurnAngle-Scatter_", today(),"_no_outliers_all_data.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
plot(df_i$StepLengthBLlog1, df_i$TurnAngleRelDeglog1p, xlab = "log(Step Length [BLs]) w/o outliers", ylab = "log(abs(Turn Angle Relative Degrees w/o outliers))")
dev.off()

## Now we can range-standardise the values for step lenhts and turning angles
## We go with the range standardization, i.e. zi = (xi - min(x))/(max(x)-min(x)), suggested by Steinley (2004 and 2006)
df_i$StepLengthBLlog1pST <- STrange(df_i$StepLengthBLlog1p)
df_i$TurnAngleRelDeglog1pST <- STrange(df_i$TurnAngleRelDeglog1p) ## store cleaned and outlier free data for each individual in the list
## plot range standardised values
png(filename = paste0(saveloc, "Standardised_Kmeans-StepLength-TurnAngle-Scatter_", today(),"_all_sharks.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
plot(df_i$StepLengthBLlog1pST, df_i$TurnAngleRelDeglog1pST, xlab = "Standardised log(Step Length [BLs])", ylab = "Standardised log(abs(Turn Angle Relative Degrees))")
dev.off()

# *B3.2: calculate k-means ----

hammers <- df_i # convert back if above steps work correctly

setDT(hammers) # convert to data.table without copy

if (!all(is.na(hammers$lat))) { # if not all lats are NA, i.e. there's something to be done
  fishlist <- unique(hammers$shark) # already exists, is same
  for (i in fishlist) { # i <- fishlist[1]
    df_i <- hammers[which(hammers$shark == i),] #subset to each fish
    print(paste0(which(fishlist == i), " of ", length(fishlist), "; calculating KMeans clustering for ", i))
    if (nrow(df_i) < 5) next # skip else kmeans will break.
    setDF(df_i) # else things break

    # data <- read.csv("data.csv", header = T, sep = ",")   ##load data
    # data <- read.csv("R/vanMoorter.etal.2010/p7_data.csv", header = T, sep = ",")   ##load data

    # x <- data[,c("ACT1", "ACT2", "SL_METERS", "TURN_DEGRE")]
    # downsample df_i to days
    x <- df_i[!is.na(df_i$StepLengthBLlog1pST),] # omit NA rows
    x <- x[!is.na(x$TurnAngleRelDeglog1pST),] # ditto
    if (nrow(x) < 5) next # skip else kmeans will break.
    x <- x[,c("StepLengthBLlog1pST", "TurnAngleRelDeglog1pST")] # , "Date", "Index"

    ###take the absolute value from the turning angle, as the direction of turn is not of interest here
    # x$TA <- abs(x$TURN_DEGRE)
    # x$TURN_DEGRE <- NULL #what's the point of this? TURN_DEGRE not used again
    # x$TurnAngleRelDegST <- abs(x$TurnAngleRelDegST) # done previously, redundant here

    # ###visual inspection of the raw data
    # par(mfrow = c(2,2))
    # for (i in 1:ncol(x)) {
    #     hist(x[,i])
    # }
    #
    # ###log-transformation of all variables except TA
    # for (i in 1:3) {
    #     x[,i] <- log(x[,i] + 1)
    # }
    #
    # for (i in 1:2) {
    #     x[,i] <- log(x[,i] + 1)
    # }
    #
    # ###range standardization of all variables
    # resc <- function(x){
    #     (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    #     }
    # for (i in 1:ncol(x)) {
    #     x[,i] <- resc(x[,i])
    #     }
    #
    # ###visual inspection of the transformed data
    # par(mfrow = c(2,2))
    # for (i in 1:ncol(x)) {
    #     hist(x[,i])
    # }

    # x <- x[,c(1, 2, 3, 4)]  ##use all variables #don't need to run this line since x is already this structure?
    # x <- x[,c(3, 4)]        ##use only trajectory measures
    # #x <- x[,c(1, 2)]        ##use only activity measures

    setDF(x) # else things break
    res <- data.frame(GAP = NA, s = NA, Wo = NA, We = NA)
    for (j in 1:5) { ##determine the GAP-statistic for 1 to 4 clusters. Changed from 10 to reduce compute time and we don't anticipate >4 XY XYT movement clusters
      if (j == 1) {  ##clall is the vector (in matrix format) of integers indicating the group to which each datum is assigned
        ones <- rep(1, nrow(x))
        clall <- matrix(ones)}
      if (j > 1) {
        cl1 <- kmeans(x, j, iter.max = 100)
        clall <- matrix(cl1$cluster)}
      g <- index.Gap.modif(x, clall, reference.distribution = "pc", B = 50, method = "k-means")
      res[j,] <- c(g$gap, g$s, g$Wo, g$We)
    } # close j

    ##### plot GAP stat per clusters+SE####
    par(mfrow = c(1,1))
    k <- seq(1:length(res$GAP))
    # png(filename = paste0(saveloc, "/KmeansClusters_", i, ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    png(filename = paste0(saveloc, "KmeansClusters_", i,"_", today(), ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    plot(k, res$GAP,
         xlab = expression(bold("Nr. clusters k")),
         ylab = expression(bold("GAP")),
         main = paste0("GAP statistic for shark id ",i), font.main = 2,
         type = "b")
    segments(k, c(res$GAP - res$s), k, c(res$GAP + res$s))
    kstar <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - res$s)[-1])) # if none of the first set of values are individually smaller than their paired counterparts in the second set of values then which() produces all FALSEs and min() fails.
    kstar2 <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - (1.96 * res$s))[-1])) # same
    if (!is.infinite(kstar2)) points(kstar2, res$GAP[kstar2], pch = 22, bg = "gray", cex = 1.25) #grey square box for tolerance2 # meaningless if kstar2 fails (now not run if so)
    if (!is.infinite(kstar)) points(kstar, res$GAP[kstar], col = "black", pch = 19, cex = 0.6) # black dot for tolerance1  # meaningless if kstar fails #change this to a different symbol?
    dev.off()

    # var1 vs var2 clustering scatterplots
    # png(filename = paste0(saveloc, "/Kmeans-StepLength-TurnAngle-Scatter_", i, ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    png(filename = paste0(saveloc, "Kmeans-StepLength-TurnAngle-Scatter_", i, "_", today(),"_final.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    plot(x$StepLengthBLlog1pST, x$TurnAngleRelDeglog1pST, xlab = "Standardised log(Step Length [BLs])", ylab = "Standardised log(abs(Turn Angle Relative Degrees))")
    dev.off()

    # then redo kmeans with selected number of clusters (kmeans output cl1 gets overwritten per i)
    ##### TODO make centers dynamic ####
    # See "Show how many clusters were chosen most commonly"
    # kstar & kstar2, might be different.
    kmeans2 <- kmeans(x, centers = 2, iter.max = 100) #run kmeans
    df_i[as.integer(names(kmeans2$cluster)), "kmeans2cluster"] <- kmeans2$cluster

    ##### label transit/resident clusters algorithmically####
    # kmeans2$centers
    # # StepLengthBLlog1p TurnAngleRelDeg
    # # 1    0.5816330       0.3355378
    # # 2    0.4993464       0.7817846
    # # these are standardised lognormalised values, not actual steplengths.
    # # Can get actual mean values since cluster bins are now added to extracted.
    cl1sl <- mean(df_i[which(df_i$kmeans2cluster == 1), "StepLengthBLlog1pST"], na.rm = T) # mean(extractedK1$StepLengthBLlog1p) #65.93499
    cl1ta <- mean(abs(df_i[which(df_i$kmeans2cluster == 1), "TurnAngleRelDeglog1pST"]), na.rm = T) # mean(extractedK1$TurnAngleRelDeg) #6.476207
    cl2sl <- mean(df_i[which(df_i$kmeans2cluster == 2), "StepLengthBLlog1pST"], na.rm = T) # mean(extractedK2$StepLengthBLlog1p) #39.47376
    cl2ta <- mean(abs(df_i[which(df_i$kmeans2cluster == 2), "TurnAngleRelDeglog1pST"]), na.rm = T) # mean(extractedK2$TurnAngleRelDeg) #74.15654
    # tuna: Group 1: 66km step, 6deg angle, long and straight, transition/transit/migration
    # tuna: Group 2: 39km step, 74deg angle, short and turny, resident/forage/spawn

    df_i$kmeansBinary <- rep(NA, nrow(df_i))
    df_i$kmeansCharacter <- rep(NA, nrow(df_i))

    if (all(cl1sl > cl2sl, cl1ta < cl2ta, na.rm = TRUE)) { # cl1 longer & straighter
      df_i[which(df_i$kmeans2cluster == 1), "kmeansCharacter"] <- "transit" #replace 1&2 with named groups
      df_i[which(df_i$kmeans2cluster == 2), "kmeansCharacter"] <- "resident"
    } else if (all(cl1sl < cl2sl, cl1ta > cl2ta, na.rm = TRUE)) { #cl2 longer & straighter
      df_i[which(df_i$kmeans2cluster == 1), "kmeansCharacter"] <- "resident"
      df_i[which(df_i$kmeans2cluster == 2), "kmeansCharacter"] <- "transit"
    } else if (cl1sl > cl2sl) { # cl1 longer but also twistier
      if ((cl1sl / cl2sl) - (cl1ta / cl2ta) > 0) { #ratio of straightness > ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeansCharacter"] <- "transit"
        df_i[which(df_i$kmeans2cluster == 2), "kmeansCharacter"] <- "resident"
      } else { #ratio of straightness < ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeansCharacter"] <- "resident"
        df_i[which(df_i$kmeans2cluster == 2), "kmeansCharacter"] <- "transit"
      }
    } else if (cl1sl < cl2sl) { # cl1 shorter but also straighter
      if ((cl2sl / cl1sl) - (cl2ta / cl1ta) > 0) { #ratio of straightness > ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeansCharacter"] <- "resident"
        df_i[which(df_i$kmeans2cluster == 2), "kmeansCharacter"] <- "transit"
      } else { #ratio of straightness < ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeansCharacter"] <- "transit"
        df_i[which(df_i$kmeans2cluster == 2), "kmeansCharacter"] <- "resident"
      }
    } # close if elses block
    # see Blocklab/abft_diving/X_PlotsMisc/KMeans/clusterinfo.ods for worksheet looking at these.
    df_i[which(df_i$kmeansCharacter == "resident"), "kmeansBinary"] <- 0
    df_i[which(df_i$kmeansCharacter == "transit"), "kmeansBinary"] <- 1

    # kmeans2$withinss
    # [1] 4.705283 9.493627
    # kmeans2$size
    # [1] 137 291

    ##### plot the scatter with color and centroids ####
    centroids2 <- kmeans2$centers # extract centroid locations for plotting
    png(filename = paste0(saveloc, "KmeansClusters_", i,"_", today(), "_cluster_identities.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    plot(df_i$StepLengthBLlog1pST, df_i$TurnAngleRelDeglog1pST,
         col = ifelse(df_i$kmeansCharacter == "resident", "skyblue4", "darkgoldenrod2"),
         pch = 16,
         xlab = expression(bold("Standardised log(step length in body lengths)")),
         ylab = expression(bold("Standardised log(abs(turning angle relative °)")))
    points(centroids2, col = "black", pch = 10, cex = 2, lwd = 4)
    dev.off()

    ##### reverse transform ####
    #df_i$StepLengthBLlog1p <-  expm1(df_i$StepLengthBLlog1p + logmean) # old, original script
    df_i$StepLengthBL_rt <-  expm1(rt_STrange(hammers$StepLengthBLlog1p, df_i$StepLengthBLlog1pST))
    df_i$TurnAngleRelDeg_rt <-  expm1(rt_STrange(hammers$TurnAngleRelDeglog1p, df_i$TurnAngleRelDeglog1pST))

    #####save metadata clusterinfo.csv####
    clusterinfoadd <- data.frame(nClustersTolerance1 = kstar,
                                 nClustersTolerance2 = kstar2,
                                 # actual data
                                 TransitClusterStepLengthBLlog1pSTMean = mean(df_i[which(df_i$kmeansCharacter == "transit"), "StepLengthBLlog1pST"], na.rm = T),
                                 TransitClusterTurnAngleAbsRelDeglog1pSTMean = mean(abs(df_i[which(df_i$kmeansCharacter == "transit"), "TurnAngleRelDeglog1pST"]), na.rm = T),
                                 ResidentClusterStepLengthBLlog1pSTMean = mean(df_i[which(df_i$kmeansCharacter == "resident"), "StepLengthBLlog1pST"], na.rm = T),
                                 ResidentClusterTurnAngleAbsRelDeglog1pSTMean = mean(abs(df_i[which(df_i$kmeansCharacter == "resident"), "TurnAngleRelDeglog1pST"]), na.rm = T),
                                 # reverse transformed data
                                 TransitClusterStepLengthBL_rt_Mean = mean(df_i[which(df_i$kmeansCharacter == "transit"), "StepLengthBL_rt"], na.rm = T),
                                 TransitClusterTurnAngleAbsRelDeg_rt_Mean = mean(abs(df_i[which(df_i$kmeansCharacter == "transit"), "TurnAngleRelDeg_rt"]), na.rm = T),
                                 ResidentClusterStepLengthBL_rt_Mean = mean(df_i[which(df_i$kmeansCharacter == "resident"), "StepLengthBL_rt"], na.rm = T),
                                 ResidentClusterTurnAngleAbsRelDeg_rt_Mean = mean(abs(df_i[which(df_i$kmeansCharacter == "resident"), "TurnAngleRelDeg_rt"]), na.rm = T),
                                 stringsAsFactors = FALSE)
    if (!exists("clusterinfo")) { #if this is the first i, create sensordates object
      clusterinfo <- clusterinfoadd
    } else { #else add to existing object
      clusterinfo <- rbind(clusterinfo,
                           clusterinfoadd,
                           stringsAsFactors = FALSE) # add to existing file
    } #close if else

    setDT(df_i) # convert to data.table without copy
    # join and update "df_i" by reference, i.e. without copy
    # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
    hammers[df_i, on = c("date", "shark"), kmeans2cluster := i.kmeans2cluster]
    hammers[df_i, on = c("date", "shark"), kmeansCharacter := i.kmeansCharacter]
    hammers[df_i, on = c("date", "shark"), kmeansBinary := i.kmeansBinary]
  } #close i

  clusterinfo <- round(clusterinfo, 3)
  clusterinfo <- bind_cols(id = fishlist, clusterinfo)

  # write.csv(x = clusterinfo, file = paste0(saveloc, "/", today(), "_KmeansClusterinfo.csv"), row.names = FALSE) #SD
  write.csv(x = clusterinfo, file = paste0(saveloc, today(), "_individual_KmeansClusterinfo.csv"), row.names = FALSE) #VH
} else {# close if (!all(is.na(alldaily$lat)))
  print("all new days missing latitude data, can't get external data, nothing to do")
}

# Show how many clusters were chosen most commonly
clustersvec <- c(clusterinfo$nClustersTolerance1, clusterinfo$nClustersTolerance2)
clustersvec <- clustersvec[!is.infinite(clustersvec)]
clustersvec %>%
  as_tibble() %>%
  group_by(value) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# for all log transformed and all range standardies predicted movement data at 12h time interval
# A tibble: 3 × 2
# value     n
# <dbl> <int>
# 1     2    15
# 2     3     2
# 3     1     1

# could do this more systematically. Could also weight the Tolerance1/2 differently? Leave it for now, perfect enemy of good.
# See L325 TOT make centres dynamic

# *B3.3: Conclude k-means calculations ----

setDF(hammers)
# hammers$StepLengthBLlog1p <-  expm1(hammers$StepLengthBLlog1p + logmean) # has been taken care of upstream
# saveRDS(object = hammers, file = paste0(saveloc, "/Hammers_KMeans.Rds")) #SD
## if with fitted data at original observation times
# dir.create(paste0(saveloc,"fitted"))
saveRDS(object = hammers, file = paste0(saveloc, "Hammers_KMeans.Rds")) #VH

## summarise steplength and TAs by cluster
mean_SlTa_trares <- hammers %>%
  dplyr::group_by(
    kmeansCharacter
  ) %>%
  dplyr::summarise(
    # SL log and ST
    meanSL_log1p_ST = mean(StepLengthBLlog1pST),
    sdSL_log1p_ST = sd(StepLengthBLlog1pST),
    # SL reverse T
    # meanSL_rt = mean(StepLengthBL_rt),
    # sdSL_rt = sd(StepLengthBL_rt),
    # SL BL raw
    meanSL_BL = mean(StepLengthBL),
    sdSL_BL = sd(StepLengthBL),
    # SL KM raw
    meanSL_KM = mean(StepLengthKm),
    sdSL_KM = sd(StepLengthKm),
    # TA log and ST
    meanTA_log_RST = mean(TurnAngleRelDeglog1pST),
    sdTA_log_RST = sd(TurnAngleRelDeglog1pST),
    # TA reverse T
    # meanTA_rt = mean(TurnAngleRelDeg_rt),
    # sdTA_rt = sd(TurnAngleRelDeg_rt),
    # TA rel degree raw
    meanTA = mean(abs(TurnAngleRelDeg)),
    sdTA = sd(abs(TurnAngleRelDeg))
  );mean_SlTa_trares

# write.csv(mean_SlTa_trares, paste0(saveloc, "Data_kmeans_cluster_summary.csv"), row.names = F) #SD
## fitted
write.csv(mean_SlTa_trares, paste0(saveloc, "Data_kmeans_cluster_summary_global_all_data.csv"), row.names = F) #VH

# clusters_info <- read.csv(paste0(saveloc, today(), "_KmeansClusterinfo.csv")) #SD
## predicted
clusters_info <- read.csv(paste0(saveloc, today(), "_individual_KmeansClusterinfo.csv")) #VH

## summarise
summary_all <- clusters_info %>% dplyr::summarise(
  ## log & range standardised
  meanSL_resident_log_RST = mean(ResidentClusterStepLengthBLlog1pSTMean),
  sdSL_resident_log_RST = sd(ResidentClusterStepLengthBLlog1pSTMean),
  meanTA_resident_log_RST = mean(ResidentClusterTurnAngleAbsRelDeglog1pSTMean),
  sdTA_resident_log_RST = sd(ResidentClusterTurnAngleAbsRelDeglog1pSTMean),
  meanSL_transit_log_RST = mean(TransitClusterStepLengthBLlog1pSTMean),
  sdSL_transit_log_RST = sd(TransitClusterStepLengthBLlog1pSTMean),
  meanTA_transit_log_RST = mean(TransitClusterTurnAngleAbsRelDeglog1pSTMean),
  sdTA_transit_log_RST = sd(TransitClusterTurnAngleAbsRelDeglog1pSTMean),
  ## backtransformed values
  meanSL_resident_rt = mean(ResidentClusterStepLengthBL_rt_Mean),
  sdSL_resident_rt = sd(ResidentClusterStepLengthBL_rt_Mean),
  meanTA_resident_rt = mean(ResidentClusterTurnAngleAbsRelDeg_rt_Mean),
  sdTA_resident_rt = sd(ResidentClusterTurnAngleAbsRelDeg_rt_Mean),
  meanSL_transit_rt = mean(TransitClusterStepLengthBL_rt_Mean),
  sdSL_transit_rt = sd(TransitClusterStepLengthBL_rt_Mean),
  meanTA_transit_rt = mean(TransitClusterTurnAngleAbsRelDeg_rt_Mean),
  sdTA_transit_rt = sd(TransitClusterTurnAngleAbsRelDeg_rt_Mean)
) %>%
  tidyr::pivot_longer(cols = everything(),  # Pivot all columns
                      names_to = "Variable",  # First column will store the column names
                      values_to = "Value" # Second column will store the corresponding values
  ); summary_all



write.csv(clusters_info, paste0(saveloc, today(), "_overall_Kmeans_cluster_SL_TA_summaries.csv"), row.names = F) #VH

## comment out line 560-563 if run by VH
# rm(list = ls()) #remove all objects
# beep(8) #notify completion
# lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))

# B4: 2D Barplot maps KMeans ----
library(mapplots)
# remotes::install_github("SimonDedman/gbm.auto")
library(gbm.auto)
# library(marmap)
## if run by SD
# saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement/kmeans"
## if run by VH
# saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/"
# hammers <- readRDS(file = paste0(saveloc, "/Hammers_KMeans.Rds")) #SD
hammers <- readRDS(file = paste0(saveloc, "Hammers_KMeans.Rds"))#VH

## only run if you need new basemap - takes a long time!
cropmap <- gbm.auto::gbm.basemap(grids = hammers,
                                 gridslat = 6,
                                 gridslon = 3,
                                 savedir = saveloc)
## if basemap already downloaded
cropmap <- sf::st_read("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CroppedMap/Crop_Map.shp") # VH, polygon

## bathymetry map
bathysavepath <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData//getNOAAbathy/"
# map2dbpSaveloc <- paste0(saveloc, "/2DbarplotMap/") #SD
map2dbpSaveloc <- paste0(saveloc)

## if run by SD
# source("~/Dropbox/Galway/Analysis/R/My Misc Scripts/barplot2dMap.R")
## if run by VH
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/Barplot2dMap/Barplot2dMap.R")
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/Mapplot/makeXYZ_HGerritsen.R")

for (i in c(0.25, 0.5, 1)) {
  barplot2dMap(x = hammers |> tidyr::drop_na(kmeansCharacter),
               groupcol = "kmeansCharacter", # col name in x
               baseplot = cropmap,
               bathysavepath = bathysavepath,
               cellsize = c(i, i),
               # mycolours = c("#015475", "#F6D408"),
               mycolours = c("#0072B2", "#D55E00"),
               legendloc = "topright",
               legendtitle = NULL,
               saveloc = map2dbpSaveloc,
               plotname = paste0(lubridate::today(), "_2DBarplot_Count_", i, "deg"))
}

# B5: plot kmeans clusters by individual by detection ----

dir.create(paste0(saveloc, "/individualPlots/")) #VH

### Prep background shapefile
library(rnaturalearth)
bg = ne_countries(scale = 10, continent = 'north america', returnclass = "sf") # needs to be adjusted depending where your study site is

### Prep Bahamian EEZ shapefile
bah_eez <- read_sf("//Sharktank/Science/Data_raw/Shapefiles/Bahamas/Bahamas_EEZ_boundary.shp")
st_crs(bah_eez)
bah_eez <- st_transform(bah_eez, st_crs = proj4string(bathyR))
# bah_eez_plot <- fortify(bah_eez)
## only if you want to use the eez shapefile and represent the entire area
xlim_eez <- c(min(hammers$lon), -70.5105)
ylim_eez <- c(20.3735, max(hammers$lat))

### define your plotting colors
behav_col <- c("#0274A2", "#EACA08") # needs to be adjusted if more than 2 clusters


### create individual plots for each ptt id and save them
for (thisshark in unique(hammers$shark)){
  # filter your shark
  kplot_df <- hammers %>% dplyr::filter(shark == thisshark)
  kplot_df <- kplot_df[!is.na(kplot_df$kmeansCharacter),] # 2 clusters
  # kplot_df <- kplot_df[!is.na(kplot_df$kmeans2cluster),] # 3+ clusters

  #define factors
  kplot_df$kmeansCharacter <- as.factor(kplot_df$kmeansCharacter) # 2 clusters
  # kplot_df$kmeansCharacter <- as.factor(kplot_df$kmeans2cluster) # 3+ clusters
  ## create plot with dark themed background
  #p = basemap(dt, bathymetry = T, expand.factor = 1.2) + # for bathymetry with ggOceanMaps package
  p <- ggplot() +

    # lines and points
    geom_path(data = kplot_df,
              aes(x=lon,y=lat),
              alpha = 1, linewidth = 0.5)+
    geom_point(data = kplot_df,
               aes(x=lon,y=lat, group = kmeansCharacter, fill = kmeansCharacter, shape = kmeansCharacter), # 2 clusters
               # aes(x=lon,y=lat, group = kmeans2cluster, fill = kmeans2cluster, shape = kmeans2cluster), # 3+ clusters

               alpha = 0.8, size = 3.25, color = "black")+
    # if you create individually tailored plots, you will undoubtedly run into overlapping axis labels
    # depending on the range of your lon/lat values for a specific animal
    # here we can adjust the nr. breaks for each axis
    # breaks_x <- seq(min(kplot_df$lon), max(kplot_df$lon), length.out = 3)
    scale_x_continuous(
      breaks = seq(min(kplot_df$lon), max(kplot_df$lon), length.out = 3),
      labels = function(x) paste0(number_format(accuracy = 0.1)(x), "°W")  # Append 'W' to each label as we lose the °W info when we scale the axis
    ) +

    # basemap
    geom_sf(data = bg, color = "black")+ # color is for border of geom object
    ## comment out the next three lines if you want to plot maps tailored to the movement extent of individual sharks
    ## and use L1187-1189 instead)
    # coord_sf(xlim = range(hammers$lon, na.rm = TRUE),
    #          ylim = range(hammers$lat , na.rm = TRUE),
    #          expand = T)+

    # bahamas eez shapefile
    geom_sf(data = bah_eez, colour = "black", fill = NA, linewidth = .25) +
    ## for individuall tailored plots comment out L1184-1186 and use L1187-1189 instead
    # coord_sf(xlim = xlim_eez,
    #          ylim = ylim_eez+.25,
    #          expand = T)+
    coord_sf(xlim = c(min(kplot_df$lon)-0.2, max(kplot_df$lon)+0.2),
             ylim = c(min(kplot_df$lat)-0.2, max(kplot_df$lat)+0.2),
             expand = T)+

    # formatting
    # labs(x=NULL, y=NULL,
    #      fill = 'kmeans cluster',
    #      color = 'kmeans cluster')+
    scale_shape_manual(values = c(22,24))+ # 2 clusters
    # scale_shape_manual(values = c(22,24, 25))+ # 3+ clusters
    scale_color_manual(values = behav_col) +
    scale_fill_manual(values = behav_col) +

    # theme
    theme_light()+
    #theme(panel.background = element_rect(fill = "gray26", linewidth = 0.5, linetype = "solid", color = "black")) +
    theme(panel.grid = element_blank(), # remove grid lines
          legend.title = element_blank(), # remove legend title
          plot.title = element_text(face = "bold", size = 11.5),
          axis.text = element_text(size = 8)) +  # adjust size of lat/lon axis labels
    labs(x = "Longitude", y = "Latitude") +
    ggtitle(paste0("Kmeans derived movement states for shark id ", thisshark))
  p

  ggsave(paste0(saveloc, "individualPlots/", today(), "_kmeans_",thisshark,".tiff"), width = 15, height = 10, units = "cm", dpi = 300)
}


### ....................................................................................................
### [C] Movegroup dBBMMs ----
### ....................................................................................................

# C0: re-set saveloc
saveloc <- "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_output/"

# C1: import data ----

# hammers <- readRDS(paste0(saveloc, "/Hammers_KMeans.Rds")) #SD
# hammers <- readRDS(paste0(saveloc, "kmeans/predicted/Hammers_KMeans.Rds")) #VH
hammers <- readRDS("//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_input/dBBMM/Data_aniMotum_CRW_output_segmented_rerouted_proj_WGS84_converted_with_coord_CIs_with_Argosfilter_data.rds") %>%
  mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1)))

# C2: calculate moveLocError ----

# calcs from /home/simon/Dropbox/Blocklab Monterey/Blocklab/ConfidenceIntervalPointsToPoly.R
reproject <- function(x, coorda, coordb, latloncrs, projectedcrs) {
  x <- sf::st_as_sf(x, coords = c(coorda, coordb)) |>
    sf::st_set_crs(latloncrs) |> # latlon degrees sf object
    st_transform(projectedcrs) |> # eastings northings units metres
    dplyr::select(-everything()) # remove all columns. Geometry is protected and retained
  return(x)
}

loncol = "lon"
latcol = "lat"
lon025 = "lon025"
lon975 = "lon975"
lat025 = "lat025"
lat975 = "lat975"
latloncrs = 4326
projectedcrs = 32617

tracksfmean <- reproject(x = hammers,
                         coorda = loncol,
                         coordb = latcol,
                         latloncrs = latloncrs,
                         projectedcrs = projectedcrs)

meanMoveLocDist <- list(
  c(loncol, lat975), # U
  c(lon975, latcol), # R
  c(loncol, lat025), # D
  c(lon025, latcol) # L
) |>
  lapply(function(x) reproject(x = hammers,
                               coorda = x[1],
                               coordb = x[2],
                               latloncrs = latloncrs,
                               projectedcrs = projectedcrs
  )) |>
  set_names(c("U", "R", "D", "L")) |> # set names of list elements
  lapply(
    function(vertextrack) { # distance from vertices to centre
      st_distance(
        x = tracksfmean,
        y = vertextrack,
        by_element = TRUE
      )
    }
  ) |>
  purrr::map_df(~.x) |> # collapse list to df of 4 columns
  rowMeans()# make row means
# (make overall mean of them?)
hammers$meanMoveLocDist <- meanMoveLocDist

# C3: calculate timeDiffLong ----

hammers$diffmins <- c(as.numeric(NA), as.numeric(hammers$date[2:length(hammers$date)] - hammers$date[1:length(hammers$date) - 1]))
# gives error but still works
# get index of first row per id
firstrows <- hammers |>
  mutate(Index = 1:nrow(hammers)) |>
  group_by(shark) |>
  summarise(firstrowid = first(Index))
# use this to blank out the diffmins since it's the time difference between 1 shark ending & another staring which is meaningless
hammers[firstrows$firstrowid, "diffmins"] <- NA

# hammers |>
#   group_by(shark) %T>%
#   # {hist(.$diffmins)} |>
#   ggplot(aes(x = diffmins)) +
#   geom_histogram() |>
#   dplyr::summarise(meandiffmins = mean(diffmins, na.rm = TRUE),
#                    sd3 = sd(diffmins, na.rm = TRUE) * 3) |>
#   ungroup()
## 20240827 - becomes redundant if you use normalised tracks

# can plot within pipe using %T>% but doesn't respect group_by
# see https://stackoverflow.com/questions/76443954/how-to-use-magrittr-tee-pipe-t-to-create-multiple-ggplots-for-grouped-data-in
# mtcars |>
#   group_by(cyl) %T>%
#   group_walk(~ print(
#     ggplot(.) + geom_histogram(aes(x = carb))
#   )) |>
#   summarise(
#     meancarb = mean(carb, na.rm = TRUE),
#     sd3 = sd(carb, na.rm = TRUE) * 3
#   )
# # Works. Tee pipe only working on 1 command (group_walk), which is only being given "1" command, but it's a formula for a print chain.
# mtcars |>
#   group_by(cyl) |>
#   group_walk(~ {
#     print(ggplot(.x) + geom_histogram(aes(x = carb)))
#     .x
#   }) |>
#   summarise(
#     meancarb = mean(carb, na.rm = TRUE),
#     sd3 = sd(carb, na.rm = TRUE) * 3
#   )
# # Works, doesn't need tee pipe. Uses braces to evaluate that bit immediately
hammers |>
  group_by(shark) |>
  group_walk(~ {
    p <- ggplot(.x) + geom_histogram(aes(x = diffmins))
    # ggsave(plot = p, filename = paste0(saveloc, "movegroup dBBMMs/timeDiffLong histograms/", lubridate::today(), "_diffMinsHistGG_", .y$id, ".png")) #SD
    ggsave(plot = p, filename = paste0(saveloc, "timeDiffLong histograms/", lubridate::today(), "_diffMinsHistGG_", .y$shark, ".png"))

    .x
  }) |>
  summarise(meandiffmins = mean(diffmins, na.rm = TRUE),
            sd3 = sd(diffmins, na.rm = TRUE) * 3)
# Following discussion, trying 24 hours, doing in mins since they're already in mins
# Update 20240903: VH -do 13 hrs so that 12 hour strucutre of predicted locations remains

##### rasterResolution ####
# With default = 6: Error: cannot allocate vector of size 2286.7 Gb
2 * mean(meanMoveLocDist) # 55601.91
hist(meanMoveLocDist) # very left skewed
summary(meanMoveLocDist) # median 12068.77
# choose 1000 - update 20240827: 10000

length(unique(hammers$shark))
# 9
# saveRDS(hammers,file = paste0(saveloc, "EEZoverlap/Hammers.Rds"))

# mysubsets <- c("All", "Andros", "Bimini", "Summer", "Winter")
mysubsets <- c("All", "Summer", "Winter") # 2023-08-08 remove Bimini sharks, and thus Andros also since ANdros = All


for (thissubset in mysubsets) { # all worked, had to make edits to hammersubset$meanMoveLocDist for the others. Then Andros
  # subset of interest. case_match returns a vector not a df/anything else
  if (thissubset == "All") hammersubset <- hammers
  if (thissubset == "Andros") hammersubset <- hammers |> filter(group == "Andros")
  # if (thissubset == "Bimini") hammersubset <- hammers |> filter(group == "Bimini")
  if (thissubset == "Summer") hammersubset <- hammers |> filter(month(date) %in% c(5:10))
  if (thissubset == "Winter") hammersubset <- hammers |> filter(month(date) %in% c(11:12, 1:4))

  # dir.create(paste0(saveloc, "movegroup dBBMMs/", thissubset, "/")) #SD
  dir.create(paste0(saveloc, "dBBMM/", thissubset, "/")) #VH

  # prevent code from crashing if a shark does not have detections during both seasons, i.e. check if the subset is empty
  if (nrow(hammersubset) == 0) {
    # Skip to the next iteration if the subset is empty
    next
  }

  for (TDL in 13) { # c(18, 24, 36, 1000) # update 20240903: change from 1200 to 13 and change units to hours
    # dir.create(paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/")) #SD
    dir.create(paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/")) #VH
    movegroup::movegroup(
      data = hammersubset,
      ID = "shark",
      Datetime = "date", # DateTime
      Lat = "lat",
      Lon = "lon",
      # Group = NULL,
      # dat.TZ = "US/Eastern",
      # proj = sp::CRS("+proj=longlat +datum=WGS84"),
      # projectedCRS = "+init=epsg:32617", # https://epsg.io/32617 Bimini, Florida, ERROR "unused argument" commented out 20240827
      # sensor = "VR2W",
      moveLocError = hammersubset$meanMoveLocDist,
      ##### timeDiffLong 18 24 36 trials####
      timeDiffLong = TDL, #original (TDL * 60), #adjust
      # Single numeric value. Threshold value in timeDiffUnits designating the length of long breaks in re-locations. Used for bursting a movement track into segments, thereby removing long breaks from the movement track. See ?move::bursted for details.
      timeDiffUnits = "hours",# original: "mins",
      # center = TRUE,
      buffpct = 10, #0.6, # Buffer extent for raster creation, proportion of 1.
      # rasterExtent = NULL,
      rasterCRS = sp::CRS("+proj=utm +zone=17 +datum=WGS84"),
      rasterResolution = 10000, # changed from 1000 to 10000 on 20240827
      # Single numeric value to set raster resolution - cell size in metres? 111000: 1 degree lat = 111km.
      # Tradeoff between small res = big file & processing time.
      # Should be a function of the spatial resolution of your receivers or positioning tags.
      # Higher resolution will lead to more precision in the volume areas calculations.
      # Try using 2*dbblocationerror.
      ##### Why did we choose 1000m?####
      dbbext = .3, # Ext param in the 'brownian.bridge.dyn' function in the 'move' package. Extends bounding box around track. Numeric single (all edges), double (x & y), or 4 (xmin xmax ymin ymax). Default 0.3. - changed to 3 20240827
      # dbbwindowsize = 23,
      # writeRasterFormat = "ascii",
      # writeRasterExtension = ".asc",
      # writeRasterDatatype = "FLT4S",
      # absVolumeAreaSaveName = "VolumeArea_AbsoluteScale.csv",
      # savedir = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"), #SD
      savedir = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/"), #VH
      alerts = TRUE
    )

    scaleraster(
      # path = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"), #SD
      path = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/"), #VH
      # pathsubsets = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"), # SD, will be able to leave NULL following change I just made
      pathsubsets = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/"), # VH, will be able to leave NULL following change I just made
      # pattern = ".asc",
      # weighting = 1,
      # format = "ascii",
      # datatype = "FLT4S",
      # bylayer = TRUE,
      # overwrite = TRUE,
      # scalefolder = "Scaled",
      # weightedsummedname = "All_Rasters_Weighted_Summed",
      # scaledweightedname = "All_Rasters_Scaled_Weighted",
      # crsloc = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/") # SD, will be able to leave NULL following change I just made
      crsloc = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/") # VH, will be able to leave NULL following change I just made

      # , returnObj = FALSE
    )

    # dir.create(paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/Plot")) #SD
    dir.create(paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/Scaled/Plot")) #VH

    # ggmap::register_google()
    plotraster(
      # x = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/All_Rasters_Scaled_Weighted_UDScaled.asc"), #SD
      # crsloc = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"), #SD
      x = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/Scaled/All_Rasters_Scaled_Weighted_UDScaled.asc"), #VH
      crsloc = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/"), #VH
      trim = TRUE,
      myLocation = c(-83.5, 23, -75.5, 29),
      googlemap = FALSE,
      # gmapsAPI = NULL,
      expandfactor = 1,
      mapzoom = 8,
      mapsource = "stadia",
      maptype = "stamen_toner_background",
      contour1colour = "orange",
      contour2colour = "red",
      # plottitle = "Aggregated 95% and 50% UD contours",
      plotsubtitle = "Scaled contours. n = 9",
      # legendtitle = "Percent UD Contours",
      # plotcaption = paste0("movegroup, ", lubridate::today()),
      # axisxlabel = "Longitude",
      # axisylabel = "Latitude",
      # legendposition = c(0.9, 0.89),
      legendposition = c(1.12, .75),
      fontsize = 12,
      # fontfamily = "Times New Roman",
      # filesavename = paste0(lubridate::today(), "_dBBMM-contours.png"),
      # savedir = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/Plot"), #SD
      savedir = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/Scaled/Plot"), #VH
      # receiverlats = NULL,
      # receiverlons = NULL,
      # receivernames = NULL,
      # receiverrange = NULL,
      # recpointscol = "black",
      # recpointsfill = "white",
      # recpointsalpha = 0.5,
      # recpointssize = 1,
      # recpointsshape = 21,
      # recbufcol = "grey75",
      # recbuffill = "grey",
      # recbufalpha = 0.5,
      # reclabcol = "black",
      # reclabfill = NA,
      # reclabnudgex = 0,
      # reclabnudgey = -200,
      # reclabpad = 0,
      # reclabrad = 0.15,
      # reclabbord = 0,
      surface = TRUE
    )

    # 01. worked, extents too large. Change buffpct to 0.3 from 1:
    # Error in .local(object, raster, location.error = location.error, ext = ext,: Lower x grid not large enough, consider extending the raster in that direction or enlarging the ext argument
    # Try 0.6
    # 02. exactly the same. Try crop in plot.R.
    # 03. expandfactor from 1.6 to 1. no change
    # 04. mapzoom from 5 to 10, 8, 7. Moved legend to right
    # 05. from hammers$id to hammers$shark, n=10 to 9


    # # per shark, scaled:
    # for (thisshark in make.names(unique(hammers$shark))) {
    #   plotraster(
    #     x = paste0(saveloc, "movegroup dBBMMs/Scaled/", thisshark, ".asc"),
    #     crsloc = paste0(saveloc, "movegroup dBBMMs/"),
    #     googlemap = TRUE,
    #     expandfactor = 1,
    #     mapzoom = 7,
    #     plotsubtitle = "Scaled contours. n = 10",
    #     legendposition = c(0.9, 0.89),
    #     filesavename = paste0(lubridate::today(), "_", thisshark, "_dBBMM-contours.png"),
    #     savedir = paste0(saveloc, "movegroup dBBMMs/Scaled/Plot"))}

    # No difference, same as unscaled

    # per shark, unscaled:
    for (thisshark in make.names(unique(hammersubset$shark))) {

      # check if the file that needs plotting exists
      if (!file.exists(paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/", thisshark, ".asc"))) {
        # If the file does not exist, skip to the next iteration
        message(paste("File not found, skipping:", thissubset, "-", thisshark))
        next
      }
      ##### ISSUE####
      # movegroup said: "processing 7 of 7" i.e. not 8 of 8
      plotraster(
        # x = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/", thisshark, ".asc"), #SD
        # crsloc = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"), #SD
        x = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/", thisshark, ".asc"), #VH
        crsloc = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/"), #VH
        trim = TRUE,
        myLocation = c(-83.5, 22.5, -74.2, 29),
        expandfactor = 1, # original 1
        mapzoom = 8,
        googlemap = F,
        mapsource = "stadia",
        maptype = "stamen_toner_background",
        contour1colour = "orange",
        contour2colour = "red",
        # plotsubtitle = "Scaled contours. n = 8",
        plotsubtitle = paste0("Scaled contours: ",thisshark,"-",thissubset),
        legendposition = c(1.15, 0.75),
        fontsize = 12,
        filesavename = paste0(lubridate::today(), "_", thisshark, "_dBBMM-contours.png"),
        # savedir = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/Plot"))} #SD
        savedir = paste0(saveloc, "dBBMM/", thissubset, "/", TDL, "h/Scaled/Plot"))} #VH


  } # close for loop 18 24 36 1000 1200h
} # close for (thissubset in mysubsets)

##### compare movegroup volumeareas####
# compare <- read_csv(paste0(saveloc, "movegroup dBBMMs/timeDiffLong_comparison.csv")) #SD
compare <- read_csv(paste0(saveloc, "dBBMM/timeDiffLong_comparison.csv"))

compare |>
  filter(str_sub(string = ID, start = 1, end = 1) == "X") |>  # filter out scaled summaries, remain only sharks
  # `absolute-scaled` == "scaled") |> # scaled only
  group_by(hours,
           `absolute-scaled`) |>
  summarise_all(mean) |>
  ggplot() +
  geom_point(mapping = aes(x = hours, y = core.use, colour = as.factor(hours), shape = `absolute-scaled`)) +
  theme_minimal() %+replace% theme(plot.background = element_rect(fill = "white"),
                                   panel.background = element_rect(fill = "white"))
# ggsave(filename = paste0(saveloc, "movegroup dBBMMs/", lubridate::today(), "_timeDiffLong_comparison.png")) #SD
ggsave(filename = paste0(saveloc, "dBBMM/", lubridate::today(), "_timeDiffLong_comparison.png"))











#(#### TODOLIST#### transit shorter steplengthBL than resident. But transit
#angles all (bar1) smaller.

# if kstar1 & 2 both = 2, great. But what if they both = 3? Or are different? Or are Inf?

# make whole thing info function (2 functions? to keep loops separate? They're both fishlist loops though? reduce fail points tho), collect user variables, age, steplength, id, lat, lon, date, database name hammers. autodelete cruft objects. L291 make centers dynamic

# Documentation
# kstar / tolerance, from VanM paper
# Tolerance T is analogous to setting the alpha level in the standard hypothesis testing framework, where increased tolerance is similar to selecting a smaller alpha rejection region. Tibshirani et al. (2001) used a tolerance of 1, but larger values of tolerance increase the strength of evidence
# required to include additional clusters (see Tibshirani et al. 2001 for full details and formulations). Tibshirani et al. (2001) demonstrated that the gap statistic performed well in detecting number of clusters when clusters are well separated; however, it was sensitive to the amount of overlap
# between clusters. Fortunately, the bias in sensitivity is such that it is likely to identify fewer clusters than there are in truth (Tibshirani, R., G. Walther, and T. Hastie. 2001. Estimating the number of clusters in a dataset via the gap statistic. Journal of the Royal Statistical Society B 63:411–423)


## ERRORS RUN 2024-08-27:

# Error with gbm.auto::basemap():
# trying URL 'https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-2.3.7.zip'
# Content type 'application/zip' length 149157845 bytes (142.2 MB)
# downloaded 6.8 MB
#
# Warning in download.file(url = paste0("https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-",  :
#                                         downloaded length 7094272 != reported length 149157845
#                                       Warning in download.file(url = paste0("https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-",  :
#                                                                               URL 'https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-2.3.7.zip': Timeout of 60 seconds was reached
#                                                                             Error in download.file(url = paste0("https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-",  :# download from 'https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-2.3.7.zip' failed
# SOLVED (stupid): simple timeout issue... add options(timeout = 3000) # increase timeout to 3000 seconds
# prior to gbm.auto::basemap() so manually increase time out limits -done.

# Error 1 with movegroup::movegroup()
# Error in movegroup::movegroup(data = hammersubset, ID = "shark", Datetime = "date",  :
#                                 unused argument (projectedCRS = "+init=epsg:32617")
#  SOLVED (20240827) by commenting out argument and executing "rastersCRS =" argument with UTM17 string



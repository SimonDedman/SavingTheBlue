### ====================================================================================================
### Project:    Misc.
### Analysis:   Quick summary statistics for movement data (e.g. days at liberty, etc.)
### Script:     ~/SavingTheBlue/R/09_Movement_data_summaries.R
### Author:     Vital Heim
### Version:    1.0
### ====================================================================================================

### ....................................................................................................
### Content: script is meant as a collection of code snippets to quickly summarise movement data
###          from different sources to explore simple metrics such as days at liberty, tracklengths, etc.
### ....................................................................................................

### ....................................................................................................
### [A] Ready environment, load packages ----
### ....................................................................................................

# A1: clear memory ----

rm(list = ls())

# A2: load necessary packages ----

## if for the first time
# install.packages("remotes")
# library(remotes)
# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("lubridate")
# install.packages("data.table")
# install.packages("sf")
# install.packages("geosphere")
# install.packages("amt")

## load
library(tidyverse)
library(magrittr)
library(lubridate)
library(data.table)
library(sf)
library(geosphere)
library(amt)

# A3: specify saveloc

saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/Summaries/"

### ...................................................................................................
### [B] Data preparation ----
### ...................................................................................................

# B1 step: import data ----

## Tagging metadata
TAG <- read.csv("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/Datasheet_Andros_Smok_Tagging_Metadata.csv", header = T, sep =",", na.strings = c("n.a.",""," ", "#WERT!", "#DIV/0!", "n.a", "na"))

## Spatail data (here Argos derived movement data)
DET <- readRDS(file = "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CTCRW/fitted/Data_aniMotum_CTCRW_output_fitted_non-projected_with_Argosfilter_data.rds")
DET$date <- as.POSIXct(DET$date,format="%Y-%m-%d %H:%M:%S",tz="UTC")
DET$species <- "S.mokarran"
head(DET)

# B2: basic housekeeping ----

## Tagging metadata
TAG_f <- TAG
TAG_f$deployment_lat <- round(as.numeric(TAG$deployment_lat),3)
TAG_f$deployment_lon <- round(as.numeric(TAG$deployment_lon),3)
TAG_f %<>%
  dplyr::mutate(
    latlong = paste0(deployment_lat, ", ", deployment_lon), #combine lat/lon
    ptt_id = as.character(ptt_id)
  ) %>%
  dplyr::select(
    ptt_id,
    species,
    sex,
    # pcl,
    fl,
    stl,
    group,
    datetime_deployment,
    deployment_lat,
    deployment_lon,
    latlong,
    last_loc,
    status
  )

## Detections
DET %<>%
  dplyr::arrange(
    id,
    date
  ) %>%
  dplyr::mutate(
    ptt_id = id
  #   ptt_id = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1)) # only needed if segmented tracks
  ) %>%
  dplyr::select(
    ptt_id,
    date,
    lon,
    lat,
    species
  )

### ...................................................................................................
### [C] Summary: track duration, i.e. days at liberty ----
### ...................................................................................................

# C1: trackduration tidyverse approach ----

## ATTENTION: tidyverse approach is based on movement data, i.e. potential post-
## release mortalities are not included in the output table! Use the metadata for those.
firstlast <- DET %>%
  dplyr::group_by(
    ptt_id
  ) %>%
  dplyr::summarise(
    num_locs = n(),
    first_date = min(as.Date(date)),
    last_date = max(as.Date(date))
  ) %>%
  dplyr::mutate(
    liberty_days = as.numeric(last_date - first_date) + 1) # +1 so that last day is also included in calculation

# C1: calculate number days between tagging and last loc using metadata ----

## ATTETNION: make sure that the newest metadata with newest last_loc info is used
## If necessary update using my.wildlifecomputers.org

## prepare columns
TAG_f$start <- as.Date(TAG_f$datetime_deployment, format = "%Y-%m-%d")
TAG_f$end <- as.Date(TAG_f$last_loc, format = "%Y-%m-%d")

## calculate differences
TAG_f$trackduration <- as.numeric(difftime(TAG_f$end,TAG_f$start, units = c("days"))) + 1 # add 1 to include last location day too

### ...................................................................................................
### [D] Summary: track lengths ----
### ...................................................................................................

# D1: using the geosphere package ----

DET <- DET %>%
  group_by(
    ptt_id
  ) %>%
  mutate(
    distance_m = distGeo(cbind(lon, lat),
              cbind(lag(lon), lag(lat)))
  ) %>%
  dplyr::group_by(
    ptt_id
  ) %>%
  summarise(
    Tracklength_km = sum(distance_m, na.rm = T)/1000
  )

### ...................................................................................................
### [E] Combine dataframes and summarise ----
### ...................................................................................................

# E1: make final df ----

final_all <- TAG_f %>%
  left_join(DET, by = "ptt_id")

write.csv(final_all, paste0(saveloc, "Data_summaries_duration_and_tracklengths_all.csv"), row.names = F)

final_active <- TAG_f %>%
  inner_join(DET, by = "ptt_id")

write.csv(final_active, paste0(saveloc, "Data_summaries_duration_and_tracklengths_active_only.csv"), row.names = F)

# E2: Summarise misc. metrics ----

## tracklengths
avg_TL <- final_active %>%
  group_by(
    species,
    sex
  ) %>%
  dplyr::summarise(
    n = n(),
    meanTL = mean(Tracklength_km),
    sdTL = sd(Tracklength_km),
    minTL = min(Tracklength_km),
    maxTL = max(Tracklength_km)
  );avg_TL

write.csv(avg_TL, paste0(saveloc, "Data_summaries_mean_tracklengths_by_species_sex.csv"), row.names = F)

## days_at_liberty
avg_TDur <- final_active %>%
  group_by(
    species
  ) %>%
  dplyr::summarise(
    n = n(),
    meanTDur = mean(trackduration),
    sdTDur = sd(trackduration),
    minTDur = min(trackduration),
    maxTDur = max(trackduration)
  ); avg_TDur

write.csv(avg_TDur, paste0(saveloc, "Data_summaries_mean_trackdruations_by_species.csv"), row.names = F)

## sizes - take 1
avg_size <- final_all %>%
  group_by(
    species,
    sex
  ) %>%
  dplyr::summarise(
    n = n(),
    mean_stl = mean(stl),
    sd_stl = sd(stl),
    min_stl = min(stl),
    max_stl = max(stl)
  ); avg_size

write.csv(avg_size, paste0(saveloc, "Data_summaries_mean_STLs_by_species_sex.csv"), row.names = F)

## sizes - take 2
avg_size_spp <- final_all %>%
  group_by(
    species
  ) %>%
  dplyr::summarise(
    n = n(),
    mean_stl = mean(stl),
    sd_stl = sd(stl),
    min_stl = min(stl),
    max_stl = max(stl)
  ); avg_size_spp

write.csv(avg_size_spp, paste0(saveloc, "Data_summaries_mean_STLs_by_species.csv"), row.names = F)

## numbers
tag_nrs <- final_all %>%
  group_by(
    species,
    sex
  ) %>%
  dplyr::summarise(
    n = n()
  ); tag_nrs

write.csv(tag_nrs, paste0(saveloc, "Data_summaries_tagged_sharks_numbers_species_sex.csv"), row.names = F)

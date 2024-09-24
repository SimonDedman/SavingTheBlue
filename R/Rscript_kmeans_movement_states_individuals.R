### ====================================================================================================
### Project:    TBC
### Analysis:   Defining movement states using kmeans clustering - individuals
### Script:     ~SavingTheBlue/R/Rscript_kmeans_movement_states_individuals.R
### Author:     SD/VH
### Version:    1.0
### ====================================================================================================

### ....................................................................................................
### Content: TBC
### ....................................................................................................

### ....................................................................................................
### [A] Setwd, paths and parameters ----
### ....................................................................................................

# A1: clear memory, define global options ----

rm(list = ls())

options(warn=1) #set this to two if you have warnings that need to be addressed as errors via traceback()
options(error = function() beep(9))  # give warning noise if it fails
options(timeout = 3000) # manually increase time out threshold (needed when downloading basemap)

# A2: load necessary packages and source needed algorithms ----

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
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/vanMoorter-et-al_2010/liRolling.R") # Simon's function for rolling Linearity Index values
# for data cleaning
library(moments)
# for vanmoorter
library(rgl) # # sudo apt install libglu1-mesa-dev
library(clusterSim)
## if run by SD
source('/home/simon/Dropbox/Blocklab Monterey/Blocklab/vanMoorter.etal.2010/p7_gap.statistic.r')
## if run by VH
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/vanMoorter-et-al_2010/p7_gap.statistic.r")
# for movegroup
library(sf)
# remotes::install_github("SimonDedman/movegroup")
# library(movegroup)

# A3: define saveloc ----

## if run by SD
# saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement"
## if run by VH
saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/kmeans/predicted/"

# A4: define needed functions, universal variables etc.----

## Function for range standardization
STrange <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

## Function to reverse-transform range standardization
rt_STrange <- function(x,y) {
  y*(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)) + min(x, na.rm = TRUE)
}

### ....................................................................................................
### [B] Calculate behavioural clusters using kmeans ----
### ....................................................................................................

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

# if you use the fitted movement data output from the original observation times CTCRW
# hammers <- readRDS(file = paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CTCRW/rerouted/Data_aniMotum_CRW_output_entire_track_rerouted_proj_WGS84_converted_with_coord_CIs_with_Argosfilter_data.rds"))|>
# mutate(shark = as.numeric(id))

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

# combine movement data with metadata
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

# fishlist <- unique(hammers$shark) # create a list with individuals to subset
# outlier_list <- list() # make a list to store cleaned df subsets in it
#
# for (i in fishlist) { # i <- fishlist[4]
#   df_i <- hammers[which(hammers$shark == i),] #subset to each fish
#   ## Plot values with outliers
#   png(filename = paste0(saveloc, "Kmeans-StepLength-TurnAngle-Scatter_", i, "_", today(),"_all_data.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
#   plot(df_i$StepLengthBLlog1, df_i$TurnAngleRelDeg, xlab = "log(Step Length [BLs]) w/ outliers", ylab = "Turn Angle Relative Degress w/ outliers")
#   dev.off()
#   # if (any(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3, na.rm = TRUE)) hammers$StepLengthBLlog1p[which(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3)] <- NA  # 17309.58
#   # if (any(hammers$StepLengthBLlog1p <= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) - sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3, na.rm = TRUE)) hammers$StepLengthBLlog1p[which(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3)] <- NA  # -12597.38
#   ## StepLength Outliers
#   if (any(df_i$StepLengthBLlog1p > mean(df_i$StepLengthBLlog1p, na.rm = TRUE) + sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$StepLengthBLlog1p[which(df_i$StepLengthBLlog1p > mean(df_i$StepLengthBLlog1p, na.rm = TRUE) + sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3)] <- NA
#   if (any(df_i$StepLengthBLlog1p < mean(df_i$StepLengthBLlog1p, na.rm = TRUE) - sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$StepLengthBLlog1p[which(df_i$StepLengthBLlog1p > mean(df_i$StepLengthBLlog1p, na.rm = TRUE) + sd(df_i$StepLengthBLlog1p, na.rm = TRUE) * 3.3)] <- NA
#   ## TurningAngles Outliers
#   if (any(df_i$TurnAngleRelDeg > mean(df_i$TurnAngleRelDeg, na.rm = TRUE) + sd(df_i$TurnAngleRelDeg, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$TurnAngleRelDeg[which(df_i$TurnAngleRelDeg > mean(df_i$TurnAngleRelDeg, na.rm = TRUE) + sd(df_i$TurnAngleRelDeg, na.rm = TRUE) * 3.3)] <- NA
#   if (any(df_i$TurnAngleRelDeg < mean(df_i$TurnAngleRelDeg, na.rm = TRUE) - sd(df_i$TurnAngleRelDeg, na.rm = TRUE) * 3.3, na.rm = TRUE)) df_i$TurnAngleRelDeg[which(df_i$TurnAngleRelDeg > mean(df_i$TurnAngleRelDeg, na.rm = TRUE) + sd(df_i$TurnAngleRelDeg, na.rm = TRUE) * 3.3)] <- NA
#   ## store cleaned and outlier free data for each individual in the list
#   outlier_list[[i]] <- df_i
#   ## Plot values without outliers
#   png(filename = paste0(saveloc, "Kmeans-StepLength-TurnAngle-Scatter_", i, "_", today(),"_no_outliers.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
#   plot(df_i$StepLengthBLlog1, df_i$TurnAngleRelDeg, xlab = "log(Step Length [BLs]) w/o outliers", ylab = "Turn Angle Relative Degrees w/o outliers")
#   dev.off()
# }

# hammers_clean <- do.call(rbind, outlier_list) # combine all outlier-free individuals df into one global df once more

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
# hammers$StepLengthBLlog1pST <- STrange(hammers$StepLengthBLlog1p)
# hammers$TurnAngleRelDegST <- STrange(hammers$TurnAngleRelDeg)
df_i$StepLengthBLlog1pST <- STrange(df_i$StepLengthBLlog1p)
df_i$TurnAngleRelDeglog1pST <- STrange(df_i$TurnAngleRelDeglog1p) ## store cleaned and outlier free data for each individual in the list
## plot range standardised values
png(filename = paste0(saveloc, "Standardised_Kmeans-StepLength-TurnAngle-Scatter_", today(),"_all_sharks.png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
plot(df_i$StepLengthBLlog1pST, df_i$TurnAngleRelDeglog1pST, xlab = "Standardised log(Step Length [BLs])", ylab = "Standardised log(abs(Turn Angle Relative Degrees))")
dev.off()

# *B3.1.: calculate k-means ----

# hammers <- hammers_clean
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

# *B3.2. Conclude k-means calculations ----

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

# B4. 2D Barplot maps KMeans ----
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

cropmap <- gbm.auto::gbm.basemap(grids = hammers,
                                 gridslat = 6,
                                 gridslon = 3,
                                 savedir = saveloc)
## if already downloaded
cropmap <- sf::st_read("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CroppedMap/Crop_Map.shp") # VH, polygon

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
bah_eez <- read_sf("C:/Users/Vital Heim/switchdrive/Science/Data/Shapefiles/Bahamas/Bahamas_EEZ_boundary.shp")
st_crs(bah_eez)
bah_eez <- st_transform(bah_eez, st_crs = proj4string(bathyR))
# bah_eez_plot <- fortify(bah_eez)
## only if you want to use the eez shapefile
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

               alpha = 0.75, size = 1.5, color = "black")+

    # basemap
    geom_sf(data = bg, color = "black")+ # color is for border of geom object
    coord_sf(xlim = range(hammers$lon, na.rm = TRUE),
             ylim = range(hammers$lat , na.rm = TRUE),
             expand = T)+

    # bahamas eez shapefile
    geom_sf(data = bah_eez, colour = "white", fill = NA, linewidth = .75) +
    coord_sf(xlim = xlim_eez,
             ylim = ylim_eez+.25,
             expand = T)+

    # formatting
    # labs(x=NULL, y=NULL,
    #      fill = 'kmeans cluster',
    #      color = 'kmeans cluster')+
    scale_shape_manual(values = c(22,24))+ # 2 clusters
    # scale_shape_manual(values = c(22,24, 25))+ # 3+ clusters
    scale_color_manual(values = behav_col) +
    scale_fill_manual(values = behav_col) +

    theme_light()+
    #theme(panel.background = element_rect(fill = "gray26", linewidth = 0.5, linetype = "solid", color = "black")) +
    theme(panel.grid = element_blank(), # remove grid lines
          legend.title = element_blank(),
          plot.title = element_text(face = "bold", size = 11.5)) + # remove legend title
    labs(x = "Longitude", y = "Latitude") +
    ggtitle(paste0("Kmeans derived movement states for shark id ", thisshark))
  p

  ggsave(paste0(saveloc, "individualPlots/", today(), "_kmeans_",thisshark,".tiff"), width = 15, height = 10, units = "cm", dpi = 300)
}

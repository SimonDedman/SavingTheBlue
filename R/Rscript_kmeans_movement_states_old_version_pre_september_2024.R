# B0: re-load necessary packages and functions in case you have not run  [A]

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
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/vanMoorter-et-al_2010/liRolling.R") # Simon's function for rolling Linearity Index values
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
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/vanMoorter-et-al_2010/p7_gap.statistic.r")
# for movegroup
library(sf)
# remotes::install_github("SimonDedman/movegroup")
library(movegroup)
## if run by SD
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement"
## if run by VH
saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/"

options(warn=1) #set this to two if you have warnings that need to be addressed as errors via traceback()

## Function for range standardization
STrange <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

## Function to reverse-transform range standardization
rt_STrange <- function(x,y) {
  y*(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)) + min(x, na.rm = TRUE)
}

# B1. Data import ----

# hammers <- readRDS("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Data/Hammerhead SPOT tags/Output_data_for_kMeans_and_dBBMM/Data_aniMotum_CRW_output_fitted_proj_WGS84_converted_with_coord_CIs_S.mokarran.rds") |> # pre 2023-07-07
# create col from id, removing . & all after. Use for left_join
# mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\.")[,1] - 1)))

### START: if run by SD ###
#  movement data
#  use the regularised movement data output from the 12h CTCRW
hammers <- readRDS("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement/CTCRW/Data_aniMotum_CRW_output_fitted_proj_WGS84_converted_with_coord_CIs_S.mokarran_with_Argosfilter_data.rds") |> # 2023-07-07 & post
  mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1)))

# metadata for length measurements
meta <- read_csv("../../Data/Hammerhead SPOT tags/Datasheet_Bahamas_Smok_Tagging_Metadata_NEW.csv") |>
  rename(shark = ptt_id,
         FishLengthCm = stl)
hammers %<>% left_join(meta)  # , by = join_by(shark == id) # doesn't work naming columns, has gotten worse.
hammers %<>% filter(group != "Bimini") # 2023-08-08 remove Bimini sharks
### END: if run by SD ###

### START: if run by VH ###
# movement data
# if you use the regularised movement data output from the 12h CTCRW
hammers <- readRDS(file = paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CTCRW/rerouted/Data_aniMotum_CRW_output_segmented_rerouted_proj_WGS84_converted_with_coord_CIs_with_Argosfilter_data.rds"))|>
  mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1)))

# if you use the fitted movement data output from the original observation times CTCRW
hammers <- readRDS(file = paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CTCRW/rerouted/Data_aniMotum_CRW_output_entire_track_rerouted_proj_WGS84_converted_with_coord_CIs_with_Argosfilter_data.rds"))|>
  mutate(shark = as.numeric(id))

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

# B2. Step Length & Turn Angles ----

# Step lengths will be calculated in km and relative to body length of correspondin sharks

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
  fishlist <- unique(df_nona$shark)

  ##### loop id, calc li5day, make track####
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
      transform_coords(6931)
    stps <- steps(track)
    # step  length  (sl;  in  CRSunits), 6931=m
    df_nonai$StepLengthKm <- c(NA, (stps$sl_/1000)) #add NA first since results start from second row

    # body lengths####
    # use vonB in reverse to convert age to length per day in dfnona
    # VonBertalanffy parameters, Restrepo et al 2010:
    # Linf (cm): 314.9
    # k: 0.089
    # t0 (year): -1.13
    # variance Linf: 19.43 (not reqd?)
    # have age from length, want length from age
    # if age is missing, populate with NA instead of crashing

    # if (is.na(df_nonai$age[1])) {
    #   df_nonai$FishLengthCm <- rep(NA, nrow(df_nonai))
    #   df_nonai$StepLengthBL <- rep(NA, nrow(df_nonai))
    # } else {
    #   df_nonai$FishLengthCm <- TropFishR::VBGF(param = list(Linf = 314.9,
    #                                                         K = 0.089,
    #                                                         t0 = -1.13),
    #                                            t = df_nonai$age)


    # then divide StepLengthKm to BL
    # 1 km/day = 1000m/day = 100000cm/day
    df_nonai$StepLengthBL <- (df_nonai$StepLengthKm * 100000) / df_nonai$FishLengthCm

    # results is distance in body lengths per day.
    # Body lengths per second (mean value per day):
    # summary(df_nonai$StepLengthBL/24/60/60)

    # turning angles & track2####
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
    df_nonai %<>% dplyr::select(c(date, li5day, StepLengthKm, StepLengthBL, TurnAngleRelDeg, TurnAngleAzimDeg, FishLengthCm, shark)) # 20240826 replace "id" with "shark"

    # save data to df####
    setDT(hammers) # convert to data.table without copy
    setDT(df_nonai) # convert to data.table without copy
    # join and update "hammers" by reference, i.e. without copy
    # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
    # If you want to return only df_nonai that have a matching hammers (i.e. rows where the key is in both tables), set the nomatch argument of data.table to 0.
    # nomatch isn't relevant together with :=, ignoring nomatch
    hammers[df_nonai, on = c("date", "shark"), li5day := i.li5day]
    hammers[df_nonai, on = c("date", "shark"), StepLengthKm := i.StepLengthKm]
    hammers[df_nonai, on = c("date", "shark"), StepLengthBL := i.StepLengthBL]
    hammers[df_nonai, on = c("date", "shark"), TurnAngleRelDeg := i.TurnAngleRelDeg]
    hammers[df_nonai, on = c("date", "shark"), TurnAngleAzimDeg := i.TurnAngleAzimDeg]
    hammers[df_nonai, on = c("date", "shark"), FishLengthCm := i.FishLengthCm]

    # kinda flat, low depth range. See weekly pdfs.
    # dives per Y hours < Z  (depends on definition of ‘dive’) = 0
    # distance covered per day > A? Do histogram of distances, then per
    # behaviour type to see if distance covered is sig higher when transitioning, e.g.
  } # close i
  hammers <- hammers[order(hammers[,"Index"]),] #reorder by index
  hammers %<>% dplyr::select(-Index) # remove unneeded Index col. Might need later?
  hammers <- as.data.frame(hammers)
} else {# close if (!all(is.na(hammers$lat)))
  print("all new days missing latitude data, can't get external data, nothing to do")
}

# B3. Kmeans VanMoorter/SD ----
# p7_analysis_suppl_BL.R
# > kmeans2cluster {resident/transient	movement cluster based on body lengths}
# > kmeansBinary {0/1	movement cluster based on body lengths}
# >> clusterinfo.csv saved in saveloc

hammers$kmeans2cluster <- as.character(rep(NA, nrow(hammers)))
hammers$kmeansBinary <- as.integer(rep(NA, nrow(hammers)))

##### Prepare steplength & TA data ####
# /500 issue; auto scale 500 scalar based on input data L201; use KM not BL?; SL values shouldn't influence cluster results??
# VanM paper: We log-transformed both activity measures and steplength to reduce positive skew. Furthermore, we standardized all variable values on their range (Steinley 2006a).
# Because we found no outliers (no data seemed outlying by visual inspection of the distribution, nor were any observations >3.3 SDs from the mean, which corresponds to a density of 0.001 in a normal distribution), we did not remove any data before data standardization.
# To demonstrate the importance of data preprocessing, we first performed a cluster analysis on the raw data, these data after log transformation and then after range standardization. In subsequent analyses, we used the transformed and standardized data, which is the recommended strategy.
# The raw data without any data preprocessing did not reveal any cluster structure; the gap statistic was maximum for one cluster (i.e., no cluster structure). After log transforming both activity measures and step lengths, we found a structure with 2 clusters.
# Steinley, D. 2006a. K-means clustering: a half-century synthesis. British Journal of Mathematical and Statistical Psychology 59:1–34.
# Standardisation on the range (not ((data - mean)/SD)) recommended but can't get that paper:
# Steinley, D. (2004a). Standardizing variables in K-means clustering. In D. Banks, L. House, F. R. McMorris, P. Arabie, & W. Gaul (Eds.), Classification, clustering, and data mining applications (pp. 53–60). New York: Springer.
# Suspect it's just data-mean

# test positive skew, log transform
hist(hammers$StepLengthBL)
moments::skewness(hammers$StepLengthBL, na.rm = TRUE) # positive/negative skew, 0 = symmetrical. # p:4.39, f: 7.74
moments::kurtosis(hammers$StepLengthBL, na.rm = TRUE) # long tailed? Normal distribution = 3. # p: 43.64, f: 94.69
hammers$StepLengthBLlog1p <- log1p(hammers$StepLengthBL) # log transform
hist(hammers$StepLengthBLlog1p)
moments::skewness(hammers$StepLengthBLlog1p, na.rm = TRUE) # p:-0.22, f: -0.602 = fine
moments::kurtosis(hammers$StepLengthBLlog1p, na.rm = TRUE) # p:2.81, f: 3.11 = fine

hist(hammers$TurnAngleRelDeg)
moments::skewness(hammers$TurnAngleRelDeg, na.rm = TRUE) # positive/negative skew, 0 = symmetrical. # p:NA, f: -0.008
moments::kurtosis(hammers$TurnAngleRelDeg, na.rm = TRUE) # long tailed? Normal distribution = 3. # p: NA, f: 5.033
## TA data does not need transformation


# test outliers, remove >3.3 SDs from the mean
# if (any(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3, na.rm = TRUE)) hammers$StepLengthBLlog1p[which(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3)] <- NA  # 17309.58
# if (any(hammers$StepLengthBLlog1p <= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) - sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3, na.rm = TRUE)) hammers$StepLengthBLlog1p[which(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3)] <- NA  # -12597.38
# mean(hammers$StepLengthBLlog1p, na.rm = TRUE) - sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3 # 1.401105
# hammers$StepLengthBLlog1p[762] # 1.256006
# Only 1 outlier, slightly outside the line, I left it in.

#OLD#
# standardise all variables on their range (AKA centre?)
# subtract the mean. Divide by SD after = z-score, not recommended by Steinley.
# logmean <- mean(hammers$StepLengthBLlog1p, na.rm = TRUE)
# hammers$StepLengthBLlog1p <- hammers$StepLengthBLlog1p - logmean
# hist(hammers$StepLengthBLlog1p)

## ADD 20240826 ##
# standardise all variables on their range
# subtract the mean. Divide by SD after = z-score, not recommended by Steinley.
# Steinley 2004 and 2006 recommends range standardization (or substracting the maximum).
# We go with the range standardization, i.e. zi = (xi - min(x))/(max(x)-min(x))

# hammers$StepLengthBLlog1p <- STrange(hammers$StepLengthBLlog1p) # original script, but we need original values later for reverse transform, so you line below that creates new column
hammers$StepLengthBLlog1pST <- STrange(hammers$StepLengthBLlog1p)
hammers$TurnAngleRelDegST <- STrange(hammers$TurnAngleRelDeg)

# hammers$StepLengthBLlog1pST <- hammers$StepLengthBL
# hammers$TurnAngleRelDegST <- hammers$TurnAngleRelDeg

# *B3.1.: calculate k-means ----

setDT(hammers) # convert to data.table without copy

if (!all(is.na(hammers$lat))) { # if not all lats are NA, i.e. there's something to be done
  fishlist <- unique(hammers$shark) # already exists, is same
  for (i in fishlist) { # i <- fishlist[1]
    df_i <- hammers[which(hammers$shark == i),] #subset to each fish
    print(paste0(which(fishlist == i), " of ", length(fishlist), "; calculating KMeans clustering for ", i))
    if (nrow(df_i) < 5) next # skip else kmeans will break. fish 51 (3) & 82 (7)
    setDF(df_i) # else things break

    # data <- read.csv("data.csv", header = T, sep = ",")   ##load data
    # data <- read.csv("R/vanMoorter.etal.2010/p7_data.csv", header = T, sep = ",")   ##load data

    # x <- data[,c("ACT1", "ACT2", "SL_METERS", "TURN_DEGRE")]
    # downsample df_i to days
    x <- df_i[!is.na(df_i$StepLengthBLlog1pST),] # omit NA rows
    x <- x[!is.na(x$TurnAngleRelDegST),] # ditto
    if (nrow(x) < 5) next # skip else kmeans will break. fish 28
    x <- x[,c("StepLengthBLlog1pST", "TurnAngleRelDegST")] # , "Date", "Index"

    ###take the absolute value from the turning angle, as the direction of turn is not of interest here
    # x$TA <- abs(x$TURN_DEGRE)
    # x$TURN_DEGRE <- NULL #what's the point of this? TURN_DEGRE not used again
    x$TurnAngleRelDegST <- abs(x$TurnAngleRelDegST)

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

    #ERROR from tuna data
    # [1] "30 of 551; calculating KMeans clustering for 5100253_99P0716_CO00_P23P"
    # Error: cannot allocate vector of size 513.3 Gb
    # > traceback()
    # 4: diag(var(Xuse)) at p7_gap.statistic.r#75
    # 3: GAP(X, clall[, 1], reference.distribution, B, method, d, centrotypes) at p7_gap.statistic.r#137
    # 2: index.Gap.modif(x, clall, reference.distribution = "pc", B = 50,
    #                    method = "k-means") at p7_analysis_suppl_BL.r#142
    # 1: KMeansAppend(loadlistcompare = F, machine = machine)


    ##### plot GAP stat per clusters+SE####
    par(mfrow = c(1,1))
    k <- seq(1:length(res$GAP))
    # png(filename = paste0(saveloc, "/KmeansClusters_", i, ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    png(filename = paste0(saveloc, "kmeans/raw/KmeansClusters_", i,"_", today(), ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    plot(k, res$GAP, xlab = "number of clusters k", ylab = "GAP", main = "GAP statistic", type = "b")
    segments(k, c(res$GAP - res$s), k, c(res$GAP + res$s))
    kstar <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - res$s)[-1])) # if none of the first set of values are individually smaller than their paired counterparts in the second set of values then which() produces all FALSEs and min() fails.
    kstar2 <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - (1.96 * res$s))[-1])) # same
    if (!is.infinite(kstar2)) points(kstar2, res$GAP[kstar2], pch = 22, bg = "gray", cex = 1.25) #grey square box for tolerance2 # meaningless if kstar2 fails (now not run if so)
    if (!is.infinite(kstar)) points(kstar, res$GAP[kstar], col = "black", pch = 19, cex = 0.6) # black dot for tolerance1  # meaningless if kstar fails #change this to a different symbol?
    dev.off()

    # var1 vs var2 clustering scatterplots
    # png(filename = paste0(saveloc, "/Kmeans-StepLength-TurnAngle-Scatter_", i, ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    png(filename = paste0(saveloc, "kmeans/raw/Kmeans-StepLength-TurnAngle-Scatter_", i, "_", today(),".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    plot(x$StepLengthBLlog1pST, x$TurnAngleRelDegST, xlab = "Step Length [BLs])", ylab = "Turn Angle Degrees")
    dev.off()

    # then redo kmeans with selected number of clusters (kmeans output cl1 gets overwritten per i)
    ##### TODO make centers dynamic ####
    # See L413 "Show how many clusters were chosen most commonly"
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
    cl1ta <- mean(abs(df_i[which(df_i$kmeans2cluster == 1), "TurnAngleRelDegST"]), na.rm = T) # mean(extractedK1$TurnAngleRelDeg) #6.476207
    cl2sl <- mean(df_i[which(df_i$kmeans2cluster == 2), "StepLengthBLlog1pST"], na.rm = T) # mean(extractedK2$StepLengthBLlog1p) #39.47376
    cl2ta <- mean(abs(df_i[which(df_i$kmeans2cluster == 2), "TurnAngleRelDegST"]), na.rm = T) # mean(extractedK2$TurnAngleRelDeg) #74.15654
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
    # # 874.1479 751.0032
    # kmeans2$size
    # # 20920 18726

    ##### reverse transform ####
    #df_i$StepLengthBLlog1p <-  expm1(df_i$StepLengthBLlog1p + logmean) # old, original script
    df_i$StepLengthBLlog1p <-  expm1(rt_STrange(hammers$StepLengthBLlog1p, df_i$StepLengthBLlog1pST))
    df_i$TurnAngleRelDeg <-  expm1(rt_STrange(hammers$TurnAngleRelDegST, df_i$TurnAngleRelDegST))

    #####save metadata clusterinfo.csv####
    clusterinfoadd <- data.frame(nClustersTolerance1 = kstar,
                                 nClustersTolerance2 = kstar2,
                                 TransitClusterStepLengthBLlog1pMean = mean(df_i[which(df_i$kmeansCharacter == "transit"), "StepLengthBLlog1pST"], na.rm = T),
                                 TransitClusterTurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeansCharacter == "transit"), "TurnAngleRelDegST"]), na.rm = T),
                                 ResidentClusterStepLengthBLlog1pMean = mean(df_i[which(df_i$kmeansCharacter == "resident"), "StepLengthBLlog1pST"], na.rm = T),
                                 ResidentClusterTurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeansCharacter == "resident"), "TurnAngleRelDegST"]), na.rm = T),
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

  clusterinfo <- round(clusterinfo, 1)
  clusterinfo <- bind_cols(id = fishlist, clusterinfo)

  # write.csv(x = clusterinfo, file = paste0(saveloc, "/", today(), "_KmeansClusterinfo.csv"), row.names = FALSE) #SD
  write.csv(x = clusterinfo, file = paste0(saveloc, "kmeans/raw/", today(), "_KmeansClusterinfo.csv"), row.names = FALSE) #VH
} else {# close if (!all(is.na(alldaily$lat)))
  print("all new days missing latitude data, can't get external data, nothing to do")
}

## WARNING: 20240828
# [1] "7 of 9; calculating KMeans clustering for 244607"
# Warning in min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - res$s)[-1])) :
#   no non-missing arguments to min; returning Inf

# Show how many clusters were chosen most commonly
clustersvec <- c(clusterinfo$nClustersTolerance1, clusterinfo$nClustersTolerance2)
clustersvec <- clustersvec[!is.infinite(clustersvec)]
clustersvec %>%
  as_tibble() %>%
  group_by(value) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# predicted
# A tibble: 4 × 2
# value     n
# <dbl> <int>
# 1     2     7
# 2     1     4
# 3     3     4
# 4     4     1
# fitted - only SL log and range ST
# A tibble: 3 × 2
# value     n
# <dbl> <int>
# 2     7
# 3     7
# 1     4
# fitted - all standardised
# A tibble: 1 × 2
# value     n
# <dbl> <int>
# 1    18
# fitted - raw
# A tibble: 2 × 2
# value     n
# <dbl> <int>
# 1    13
# 2     5


# could do this more systematically. Could also weight the Tolerance1/2 differently? Leave it for now, perfect enemy of good.
# See L325 TOT make centres dynamic

# *B3.2. Conclude k-means calculations ----

setDF(hammers)
# hammers$StepLengthBLlog1p <-  expm1(hammers$StepLengthBLlog1p + logmean) # has been taken care of upstream
# saveRDS(object = hammers, file = paste0(saveloc, "/Hammers_KMeans.Rds")) #SD
## with predicted data at 12hr intervals
dir.create(paste0(saveloc,"kmeans/predicted"))
saveRDS(object = hammers, file = paste0(saveloc, "kmeans/predicted/Hammers_KMeans.Rds")) #VH
saveRDS(object = hammers, file = paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/dBBMM/Hammers_KMeans.Rds")) #VH
## if with fitted data at original observation times
dir.create(paste0(saveloc,"kmeans/fitted"))
saveRDS(object = hammers, file = paste0(saveloc, "kmeans/raw/Hammers_KMeans.Rds")) #VH

## summarise steplength and TAs by cluster
mean_SlTa_trares <- hammers %>%
  dplyr::group_by(
    # kmeansCharacter
    kmeans2cluster
  ) %>%
  dplyr::summarise(
    meanSL_STrange = mean(StepLengthBLlog1pST),
    meanSL_BL = mean(StepLengthBL),
    meanSL_KM = mean(StepLengthKm),
    meanTA = mean(abs(TurnAngleRelDeg))
  );mean_SlTa_trares
# A tibble: 3 × 5
# kmeansCharacter meanSL_STrange meanSL_BL meanSL_KM meanTA
# <chr>                    <dbl>     <dbl>     <dbl>  <dbl>
# resident                 0.570     2857.      8.52  125.
# transit                  0.563     2877.      8.51   20.7

# write.csv(mean_SlTa_trares, paste0(saveloc, "Data_kmeans_cluster_summary.csv"), row.names = F) #SD
## predicted
write.csv(mean_SlTa_trares, paste0(saveloc, "kmeans/predicted/Data_kmeans_cluster_summary.csv"), row.names = F) #VH
## fitted
write.csv(mean_SlTa_trares, paste0(saveloc, "kmeans/raw/Data_kmeans_cluster_summary.csv"), row.names = F) #VH

# clusters_info <- read.csv(paste0(saveloc, today(), "_KmeansClusterinfo.csv")) #SD
## predicted
clusters_info <- read.csv(paste0(saveloc, "kmeans/predicted/", today(), "_KmeansClusterinfo.csv")) #VH
## fitted
clusters_info <- read.csv(paste0(saveloc, "kmeans/raw/", today(), "_KmeansClusterinfo.csv")) #VH

## summarise
clusters_info %>% dplyr::summarise(
  meanSL_resident = mean(ResidentClusterStepLengthBLlog1pMean),
  sdSL_resident = sd(ResidentClusterStepLengthBLlog1pMean),
  meanTA_resident = mean(ResidentClusterTurnAngleRelDegAbsMean),
  sdTA_resident = sd(ResidentClusterTurnAngleRelDegAbsMean),
  meanSL_transit = mean(TransitClusterStepLengthBLlog1pMean),
  sdSL_transit = sd(TransitClusterStepLengthBLlog1pMean),
  meanTA_transit = mean(TransitClusterTurnAngleRelDegAbsMean),
  sdTA_transit = sd(TransitClusterTurnAngleRelDegAbsMean)
)
# meanSL_resident sdSL_resident meanTA_resident sdTA_resident
#       2165.588      1221.804        127.6625      9.468736
# meanSL_transit sdSL_transit meanTA_transit sdTA_transit
#       2424.512     1489.819        18.4375     7.164583

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
saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/"
# hammers <- readRDS(file = paste0(saveloc, "/Hammers_KMeans.Rds")) #SD
hammers <- readRDS(file = paste0(saveloc, "kmeans/fitted/Hammers_KMeans.Rds"))#VH

cropmap <- gbm.auto::gbm.basemap(grids = hammers,
                                 gridslat = 6,
                                 gridslon = 3,
                                 savedir = saveloc)
## if already downloaded
cropmap <- sf::st_read(paste0(saveloc,"/CroppedMap/Crop_Map.shp")) # VH, polygon

bathysavepath <- paste0(saveloc, "/getNOAAbathy/")
# map2dbpSaveloc <- paste0(saveloc, "/2DbarplotMap/") #SD
map2dbpSaveloc <- paste0(saveloc, "kmeans/2DbarplotMap/")

## if run by SD
# source("~/Dropbox/Galway/Analysis/R/My Misc Scripts/barplot2dMap.R")
## if run by VH
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/Barplot2dMap/Barplot2dMap.R")
source("C:/Users/Vital Heim/switchdrive/Science/Rscripts/Mapplot/makeXYZ_HGerritsen.R")

for (i in c(0.25, 0.5, 1)) {
  barplot2dMap(x = hammers |> tidyr::drop_na(kmeans2cluster),
               baseplot = cropmap,
               bathysavepath = bathysavepath,
               cellsize = c(i, i),
               mycolours = c("blue","red","green"), # adjust based on nr. clusters
               legendloc = "topright",
               saveloc = map2dbpSaveloc,
               plotname = paste0(lubridate::today(), "_2DBarplot_Count_", i, "deg"))
}

# B5: plot kmeans clusters by individual by detection ----

dir.create(paste0(saveloc, "kmeans/individualPlots/")) #VH

### Prep background shapefile
library(rnaturalearth)
bg = ne_countries(scale = "large", continent = 'north america', returnclass = "sf") # needs to be adjusted depending where your study site is

### Prep Bahamian EEZ shapefile
bah_eez <- read_sf("C:/Users/Vital Heim/switchdrive/Science/Data/Shapefiles/Bahamas/Bahamas_EEZ_boundary.shp")
st_crs(bah_eez)
bah_eez <- st_transform(bah_eez, st_crs = proj4string(bathyR))
# bah_eez_plot <- fortify(bah_eez)
## only if you want to use the eez shapefile
xlim_eez <- c(min(hammers$lon), -70.5105)
ylim_eez <- c(20.3735, max(hammers$lat))

### define your plotting colors
behav_col <- c("#01AAC7", "#F9DF44", "black")
# transitcol <- "#F9DF44"
# residentcol <- "#01AAC7"

### create individual plots for each ptt id and save them
for (thisshark in unique(hammers$shark)){
  # filter your shark
  kplot_df <- hammers %>% dplyr::filter(shark == thisshark)
  # kplot_df <- kplot_df[!is.na(kplot_df$kmeansCharacter),] # 2 clusters
  kplot_df <- kplot_df[!is.na(kplot_df$kmeans2cluster),] # 3+ clusters

  #define factors
  # kplot_df$kmeansCharacter <- as.factor(kplot_df$kmeansCharacter) # 2 clusters
  kplot_df$kmeansCharacter <- as.factor(kplot_df$kmeans2cluster) # 3+ clusters
  ## create plot with dark themed background
  #p = basemap(dt, bathymetry = T, expand.factor = 1.2) + # for bathymetry with ggOceanMaps package
  p <- ggplot() +

    # lines and points
    geom_path(data = kplot_df,
              aes(x=lon,y=lat),
              alpha = 1, linewidth = 0.5)+
    geom_point(data = kplot_df,
               # aes(x=lon,y=lat, group = kmeansCharacter, fill = kmeansCharacter, shape = kmeansCharacter), # 2 clusters
               aes(x=lon,y=lat, group = kmeans2cluster, fill = kmeans2cluster, shape = kmeans2cluster), # 3+ clusters

               alpha = 0.9, size = 2, color = "black")+

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
    # scale_shape_manual(values = c(22,24))+ # 2 clusters
    scale_shape_manual(values = c(22,24, 25))+ # 3+ clusters
    scale_color_manual(values = behav_col) +
    scale_fill_manual(values = behav_col) +

    theme_dark()+
    #theme(panel.background = element_rect(fill = "gray26", linewidth = 0.5, linetype = "solid", color = "black")) +
    theme(panel.grid = element_blank(), legend.title = element_text(face = "bold"))+
    ggtitle(paste0("Kmeans-Clusters for PTT ", thisshark))
  p

  ggsave(paste0(saveloc, "kmeans/individualPlots/kmeans_",thisshark,".tiff"), width = 15, height = 10, units = "cm", dpi = 300)
}

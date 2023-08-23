#TOP####
# 2023-05-30 Simon Dedman simondedman@gmail.com
# kmeans & movegroup aka dBBMM

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
source('~/Dropbox/Blocklab Monterey/Blocklab/liRolling.R') #my own function for rolling Linearity Index values
options(error = function() beep(9))  # give warning noise if it fails
# for data cleaning
library(moments)
# for vanmoorter
library(beepr)
library(rgl) # # sudo apt install libglu1-mesa-dev
library(clusterSim)
source('/home/simon/Dropbox/Blocklab Monterey/Blocklab/vanMoorter.etal.2010/p7_gap.statistic.r')
# for movegroup
library(sf)
# remotes::install_github("SimonDedman/movegroup")
library(movegroup)
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement/"



# 1. Step Length & Turn Angles####

# hammers <- readRDS("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Data/Hammerhead SPOT tags/Output_data_for_kMeans_and_dBBMM/Data_aniMotum_CRW_output_fitted_proj_WGS84_converted_with_coord_CIs_S.mokarran.rds") |> # pre 2023-07-07
# create col from id, removing . & all after. Use for left_join
# mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\.")[,1] - 1)))
hammers <- readRDS("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement/CTCRW/Data_aniMotum_CRW_output_fitted_proj_WGS84_converted_with_coord_CIs_S.mokarran_with_Argosfilter_data.rds") |> # 2023-07-07 & post
  mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1)))


meta <- read_csv("../../Data/Hammerhead SPOT tags/Datasheet_Bahamas_Smok_Tagging_Metadata_NEW.csv") |>
  rename(shark = ptt_id,
         FishLengthCm = stl)
hammers %<>% left_join(meta)  # , by = join_by(shark == id) # doesn't work naming columns, has gotten worse.
hammers %<>% filter(group != "Bimini") # 2023-08-08 remove Bimini sharks

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
  df_nona <- df_nona[!is.na(df_nona$lat),] # omit rows with NA values for lat, downsample to days only
  df_nona <- df_nona[!is.na(df_nona$lon),] # omit rows with NA values for lon, downsample to days only
  fishlist <- unique(df_nona$id)

  # loop id, calc li5day, make track####
  for (i in fishlist) { # i <- fishlist[1]
    df_nonai <- df_nona[which(df_nona$id == i),] #subset to each fish
    print(paste0(which(fishlist == i), " of ", length(fishlist), "; adding transit dive data to ", i))
    setDF(df_nonai) # else liRolling breaks
    if (nrow(df_nonai) > 5) df_nonai$li5day <- liRolling(x = df_nonai,
                                                         coords = c("lat", "lon"),
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
    df_nonai %<>% dplyr::select(c(date, li5day, StepLengthKm, StepLengthBL, TurnAngleRelDeg, TurnAngleAzimDeg, FishLengthCm, id))

    # save data to df####
    setDT(hammers) # convert to data.table without copy
    setDT(df_nonai) # convert to data.table without copy
    # join and update "hammers" by reference, i.e. without copy
    # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
    # If you want to return only df_nonai that have a matching hammers (i.e. rows where the key is in both tables), set the nomatch argument of data.table to 0.
    # nomatch isn't relevant together with :=, ignoring nomatch
    hammers[df_nonai, on = c("date", "id"), li5day := i.li5day]
    hammers[df_nonai, on = c("date", "id"), StepLengthKm := i.StepLengthKm]
    hammers[df_nonai, on = c("date", "id"), StepLengthBL := i.StepLengthBL]
    hammers[df_nonai, on = c("date", "id"), TurnAngleRelDeg := i.TurnAngleRelDeg]
    hammers[df_nonai, on = c("date", "id"), TurnAngleAzimDeg := i.TurnAngleAzimDeg]
    hammers[df_nonai, on = c("date", "id"), FishLengthCm := i.FishLengthCm]

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











# 2. Kmeans VanMoorter/SD####
# p7_analysis_suppl_BL.R
# > kmeans2cluster {resident/transient	movement cluster based on body lengths}
# > kmeansBinary {0/1	movement cluster based on body lengths}
# >> clusterinfo.csv saved in saveloc

hammers$kmeans2cluster <- as.character(rep(NA, nrow(hammers)))
hammers$kmeansBinary <- as.integer(rep(NA, nrow(hammers)))

# Prepare steplength & TA data ####
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
moments::skewness(hammers$StepLengthBL, na.rm = TRUE) # positive/negative skew, 0 = symmetrical. # 5.8
moments::kurtosis(hammers$StepLengthBL, na.rm = TRUE) # long tailed? Normal distribution = 3. # 53.9
hammers$StepLengthBLlog1p <- log1p(hammers$StepLengthBL) # log transform
hist(hammers$StepLengthBLlog1p)
moments::skewness(hammers$StepLengthBLlog1p, na.rm = TRUE) # -0.22 = fine
moments::kurtosis(hammers$StepLengthBLlog1p, na.rm = TRUE) # 2.93 = fine


# test outliers, remove >3.3 SDs from the mean
# if (any(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3, na.rm = TRUE)) hammers$StepLengthBLlog1p[which(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3)] <- NA  # 17309.58
# if (any(hammers$StepLengthBLlog1p <= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) - sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3, na.rm = TRUE)) hammers$StepLengthBLlog1p[which(hammers$StepLengthBLlog1p >= mean(hammers$StepLengthBLlog1p, na.rm = TRUE) + sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3)] <- NA  # -12597.38
# mean(hammers$StepLengthBLlog1p, na.rm = TRUE) - sd(hammers$StepLengthBLlog1p, na.rm = TRUE) * 3 # 1.401105
# hammers$StepLengthBLlog1p[762] # 1.256006
# Only 1 outlier, slightly outside the line, I left it in.

# standardise all variables on their range (AKA centre?)
# subtract the mean. Divide by SD after = z-score, not recommended by Steinley.
logmean <- mean(hammers$StepLengthBLlog1p, na.rm = TRUE)
hammers$StepLengthBLlog1p <- hammers$StepLengthBLlog1p - logmean
# hist(hammers$StepLengthBLlog1p)

setDT(hammers) # convert to data.table without copy

if (!all(is.na(hammers$lat))) { # if not all lats are NA, i.e. there's something to be done
  fishlist <- unique(hammers$id) # already exists, is same
  for (i in fishlist) { # i <- fishlist[3]
    df_i <- hammers[which(hammers$id == i),] #subset to each fish
    print(paste0(which(fishlist == i), " of ", length(fishlist), "; calculating KMeans clustering for ", i))
    if (nrow(df_i) < 5) next # skip else kmeans will break. fish 51 (3) & 82 (7)
    setDF(df_i) # else things break

    # data <- read.csv("data.csv", header = T, sep = ",")   ##load data
    # data <- read.csv("R/vanMoorter.etal.2010/p7_data.csv", header = T, sep = ",")   ##load data

    # x <- data[,c("ACT1", "ACT2", "SL_METERS", "TURN_DEGRE")]
    # downsample df_i to days
    x <- df_i[!is.na(df_i$StepLengthBLlog1p),] # omit NA rows
    x <- x[!is.na(x$TurnAngleRelDeg),] # ditto
    if (nrow(x) < 5) next # skip else kmeans will break. fish 28
    x <- x[,c("StepLengthBLlog1p", "TurnAngleRelDeg")] # , "Date", "Index"

    ###take the absolute value from the turning angle, as the direction of turn is not of interest here
    # x$TA <- abs(x$TURN_DEGRE)
    # x$TURN_DEGRE <- NULL #what's the point of this? TURN_DEGRE not used again
    x$TurnAngleRelDeg <- abs(x$TurnAngleRelDeg)

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
    res <- data.frame(GAP = NA, s = NA, Wo = NA, We = NA) # what are s (SD?) Wo & We?
    for (j in 1:4 ) { ##determine the GAP-statistic for 1 to 4 clusters. Changed from 10 to reduce compute time and we don't anticipate >4 XY XYT movement clusters
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


    # plot GAP stat per clusters+SE####
    par(mfrow = c(1,1))
    k <- seq(1:length(res$GAP))
    png(filename = paste0(saveloc, "KmeansClusters_", i, ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    plot(k, res$GAP, xlab = "number of clusters k", ylab = "GAP", main = "GAP statistic", type = "b")
    segments(k, c(res$GAP - res$s), k, c(res$GAP + res$s))
    kstar <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - res$s)[-1])) # if none of the first set of values are individually smaller than their paired counterparts in the second set of values then which() produces all FALSEs and min() fails.
    kstar2 <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - (1.96 * res$s))[-1])) # same
    if (!is.infinite(kstar2)) points(kstar2, res$GAP[kstar2], pch = 22, bg = "gray", cex = 1.25) #grey square box for tolerance2 # meaningless if kstar2 fails (now not run if so)
    if (!is.infinite(kstar)) points(kstar, res$GAP[kstar], col = "black", pch = 19, cex = 0.6) # black dot for tolerance1  # meaningless if kstar fails #change this to a different symbol?
    dev.off()

    # var1 vs var2 clustering scatterplots
    png(filename = paste0(saveloc, "Kmeans-StepLength-TurnAngle-Scatter_", i, ".png"), width = 4*480, height = 4*480, units = "px", pointsize = 4*12, bg = "white", res = NA, family = "", type = "cairo-png")
    plot(x$StepLengthBLlog1p, x$TurnAngleRelDeg, xlab = "Step Length (Body Lengths)", ylab = "Turn Angle Degrees")
    dev.off()

    # then redo kmeans with selected number of clusters (kmeans output cl1 gets overwritten per i)
    # TODO make centers dynamic ####
    # See L413 "Show how many clusters were chosen most commonly"
    # kstar & kstar2, might be different.
    kmeans2 <- kmeans(x, centers = 2, iter.max = 100) #run kmeans
    df_i[as.integer(names(kmeans2$cluster)), "kmeans2cluster"] <- kmeans2$cluster

    # label transit/resident clusters algorithmically####
    # kmeans2$centers
    # # StepLengthBLlog1p TurnAngleRelDeg
    # # 1    0.5816330       0.3355378
    # # 2    0.4993464       0.7817846
    # # these are standardised lognormalised values, not actual steplengths.
    # # Can get actual mean values since cluster bins are now added to extracted.
    cl1sl <- mean(df_i[which(df_i$kmeans2cluster == 1), "StepLengthBLlog1p"], na.rm = T) # mean(extractedK1$StepLengthBLlog1p) #65.93499
    cl1ta <- mean(abs(df_i[which(df_i$kmeans2cluster == 1), "TurnAngleRelDeg"]), na.rm = T) # mean(extractedK1$TurnAngleRelDeg) #6.476207
    cl2sl <- mean(df_i[which(df_i$kmeans2cluster == 2), "StepLengthBLlog1p"], na.rm = T) # mean(extractedK2$StepLengthBLlog1p) #39.47376
    cl2ta <- mean(abs(df_i[which(df_i$kmeans2cluster == 2), "TurnAngleRelDeg"]), na.rm = T) # mean(extractedK2$TurnAngleRelDeg) #74.15654
    # tuna: Group 1: 66km step, 6deg angle, long and straight, transition/transit/migration
    # tuna: Group 2: 39km step, 74deg angle, short and turny, resident/forage/spawn

    df_i$kmeansBinary <- rep(NA, nrow(df_i))

    if (all(cl1sl > cl2sl, cl1ta < cl2ta, na.rm = TRUE)) { # cl1 longer & straighter
      df_i[which(df_i$kmeans2cluster == 1), "kmeans2cluster"] <- "transit" #replace 1&2 with named groups
      df_i[which(df_i$kmeans2cluster == 2), "kmeans2cluster"] <- "resident"
    } else if (all(cl1sl < cl2sl, cl1ta > cl2ta, na.rm = TRUE)) { #cl2 longer & straighter
      df_i[which(df_i$kmeans2cluster == 1), "kmeans2cluster"] <- "resident"
      df_i[which(df_i$kmeans2cluster == 2), "kmeans2cluster"] <- "transit"
    } else if (cl1sl > cl2sl) { # cl1 longer but also twistier
      if ((cl1sl / cl2sl) - (cl1ta / cl2ta) > 0) { #ratio of straightness > ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeans2cluster"] <- "transit"
        df_i[which(df_i$kmeans2cluster == 2), "kmeans2cluster"] <- "resident"
      } else { #ratio of straightness < ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeans2cluster"] <- "resident"
        df_i[which(df_i$kmeans2cluster == 2), "kmeans2cluster"] <- "transit"
      }
    } else if (cl1sl < cl2sl) { # cl1 shorter but also straighter
      if ((cl2sl / cl1sl) - (cl2ta / cl1ta) > 0) { #ratio of straightness > ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeans2cluster"] <- "resident"
        df_i[which(df_i$kmeans2cluster == 2), "kmeans2cluster"] <- "transit"
      } else { #ratio of straightness < ratio of twistyness
        df_i[which(df_i$kmeans2cluster == 1), "kmeans2cluster"] <- "transit"
        df_i[which(df_i$kmeans2cluster == 2), "kmeans2cluster"] <- "resident"
      }
    } # close if elses block
    # see Blocklab/abft_diving/X_PlotsMisc/KMeans/clusterinfo.ods for worksheet looking at these.
    df_i[which(df_i$kmeans2cluster == "resident"), "kmeansBinary"] <- 0
    df_i[which(df_i$kmeans2cluster == "transit"), "kmeansBinary"] <- 1

    # kmeans2$withinss
    # # 874.1479 751.0032
    # kmeans2$size
    # # 20920 18726

    # reverse transform ####
    df_i$StepLengthBLlog1p <-  expm1(df_i$StepLengthBLlog1p + logmean)

    #save metadata clusterinfo.csv####
    clusterinfoadd <- data.frame(nClustersTolerance1 = kstar,
                                 nClustersTolerance2 = kstar2,
                                 TransitClusterStepLengthBLlog1pMean = mean(df_i[which(df_i$kmeans2cluster == "transit"), "StepLengthBLlog1p"], na.rm = T),
                                 TransitClusterTurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeans2cluster == "transit"), "TurnAngleRelDeg"]), na.rm = T),
                                 ResidentClusterStepLengthBLlog1pMean = mean(df_i[which(df_i$kmeans2cluster == "resident"), "StepLengthBLlog1p"], na.rm = T),
                                 ResidentClusterTurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeans2cluster == "resident"), "TurnAngleRelDeg"]), na.rm = T),
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
    hammers[df_i, on = c("date", "id"), kmeans2cluster := i.kmeans2cluster]
    hammers[df_i, on = c("date", "id"), kmeansBinary := i.kmeansBinary]
  } #close i

  clusterinfo <- round(clusterinfo, 1)
  clusterinfo <- bind_cols(id = fishlist, clusterinfo)

  write.csv(x = clusterinfo, file = paste0(saveloc, today(), "_KmeansClusterinfo.csv"), row.names = FALSE)
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
# value     n
#     2     8
#     3     7
#     1     2
# could do this more systematically. Could also weight the Tolerance1/2 differently? Leave it for now, perfect enemy of good.
# See L325 TOT make centres dynamic

# 2023-08-08 remove Bimini sharks
# value     n
#     3     6   #more k=3 than k=2
#     2     4

# wrap up####
setDF(hammers)
hammers$StepLengthBLlog1p <-  expm1(hammers$StepLengthBLlog1p + logmean)
saveRDS(object = hammers, file = paste0(saveloc, "/kmeans/Hammers_KMeans.Rds"))
rm(list = ls()) #remove all objects
beep(8) #notify completion
lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))





# 3. movegroup dBBMMs ####
hammers <- readRDS(paste0(saveloc, "/kmeans/Hammers_KMeans.Rds"))

# moveLocError ####
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


# timeDiffLong ####
hammers$diffmins <- c(as.numeric(NA), as.numeric(hammers$date[2:length(hammers$date)] - hammers$date[1:length(hammers$date) - 1]))
# gives error but still works
# get index of first row per id
firstrows <- hammers |>
  mutate(Index = 1:nrow(hammers)) |>
  group_by(id) |>
  summarise(firstrowid = first(Index))
# use this to blank out the diffmins since it's the time difference between 1 shark ending & another staring which is meaningless
hammers[firstrows$firstrowid, "diffmins"] <- NA


# hammers |>
#   group_by(id) %T>%
#   # {hist(.$diffmins)} |>
#   ggplot(aes(x = diffmins)) +
#   geom_histogram() |>
#   dplyr::summarise(meandiffmins = mean(diffmins, na.rm = TRUE),
#                    sd3 = sd(diffmins, na.rm = TRUE) * 3) |>
#   ungroup()
# id       meandiffmins    sd3
# 177940.1         448    4212
# 177941.1         643    4509
# 177942.1         452    2934
# 183623.1         443    2590
# 200368.1         453    2994
# 200369.1        2910   16222
# 200369.2        1274    8642
# 209020.1        2725   13285
# 222133.1         662    3321
#  23596.1         581    2580

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
  group_by(id) |>
  group_walk(~ {
    p <- ggplot(.x) + geom_histogram(aes(x = diffmins))
    ggsave(plot = p, filename = paste0(saveloc, "movegroup dBBMMs/timeDiffLong histograms/", lubridate::today(), "_diffMinsHistGG_", .y$id, ".png"))
    .x
  }) |>
  summarise(meandiffmins = mean(diffmins, na.rm = TRUE),
            sd3 = sd(diffmins, na.rm = TRUE) * 3)
# Following discussion, trying 24 hours, doing in mins since they're already in mins

# rasterResolution ####
# With default = 6: Error: cannot allocate vector of size 2286.7 Gb
2 * mean(meanMoveLocDist) # 8265.945
hist(meanMoveLocDist) # very left skewed
summary(meanMoveLocDist) # median 1927.83
# choose 1000

length(unique(hammers$shark))
# 9
saveRDS(hammers,file = paste0(saveloc, "/EEZoverlap/Hammers.Rds"))

# mysubsets <- c("All", "Andros", "Bimini", "Summer", "Winter")
mysubsets <- c("All", "Summer", "Winter") # 2023-08-08 remove Bimini sharks, and thus Andros also since ANdros = All


for (thissubset in mysubsets) { # all worked, had to make edits to hammersubset$meanMoveLocDist for the others. Then Andros
  # subset of interest. case_match returns a vector not a df/anything else
  if (thissubset == "All") hammersubset <- hammers
  if (thissubset == "Andros") hammersubset <- hammers |> filter(group == "Andros")
  if (thissubset == "Bimini") hammersubset <- hammers |> filter(group == "Bimini")
  if (thissubset == "Summer") hammersubset <- hammers |> filter(month(date) %in% c(5:10))
  if (thissubset == "Winter") hammersubset <- hammers |> filter(month(date) %in% c(11:12, 1:4))

  dir.create(paste0(saveloc, "movegroup dBBMMs/", thissubset, "/"))

  for (TDL in 1200) { # c(18, 24, 36, 1000)
    dir.create(paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"))
    movegroup::movegroup(
      data = hammersubset,
      ID = "shark",
      Datetime = "date", # DateTime
      Lat = "lat",
      Lon = "lon",
      # Group = NULL,
      # dat.TZ = "US/Eastern",
      # proj = sp::CRS("+proj=longlat +datum=WGS84"),
      projectedCRS = "+init=epsg:32617", # https://epsg.io/32617 Bimini, Florida
      # sensor = "VR2W",
      moveLocError = hammersubset$meanMoveLocDist,
      # timeDiffLong 18 24 36 trials####
      timeDiffLong = (TDL * 60),
      # Single numeric value. Threshold value in timeDiffUnits designating the length of long breaks in re-locations. Used for bursting a movement track into segments, thereby removing long breaks from the movement track. See ?move::bursted for details.
      timeDiffUnits = "mins",
      # center = TRUE,
      buffpct = 0.6, # Buffer extent for raster creation, proportion of 1.
      # rasterExtent = NULL,
      # rasterCRS = sp::CRS("+proj=utm +zone=17 +datum=WGS84"),
      rasterResolution = 1000,
      # Single numeric value to set raster resolution - cell size in metres? 111000: 1 degree lat = 111km.
      # Tradeoff between small res = big file & processing time.
      # Should be a function of the spatial resolution of your receivers or positioning tags.
      # Higher resolution will lead to more precision in the volume areas calculations.
      # Try using 2*dbblocationerror.
      dbbext = 0.3, # Ext param in the 'brownian.bridge.dyn' function in the 'move' package. Extends bounding box around track. Numeric single (all edges), double (x & y), or 4 (xmin xmax ymin ymax). Default 0.3.
      # dbbwindowsize = 23,
      # writeRasterFormat = "ascii",
      # writeRasterExtension = ".asc",
      # writeRasterDatatype = "FLT4S",
      # absVolumeAreaSaveName = "VolumeArea_AbsoluteScale.csv",
      savedir = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"),
      alerts = TRUE
    )

    scaleraster(
      path = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"),
      pathsubsets = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"), # will be able to leave NULL following change I just made
      # pattern = ".asc",
      # weighting = 1,
      # format = "ascii",
      # datatype = "FLT4S",
      # bylayer = TRUE,
      # overwrite = TRUE,
      # scalefolder = "Scaled",
      # weightedsummedname = "All_Rasters_Weighted_Summed",
      # scaledweightedname = "All_Rasters_Scaled_Weighted",
      crsloc = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/") # will be able to leave NULL following change I just made
      # , returnObj = FALSE
    )

    dir.create(paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/Plot"))

    # ggmap::register_google()
    plotraster(
      x = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/All_Rasters_Scaled_Weighted_UDScaled.asc"),
      crsloc = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"),
      # trim = TRUE,
      # myLocation = NULL,
      googlemap = TRUE,
      # gmapsAPI = NULL,
      expandfactor = 1,
      mapzoom = 7,
      # mapsource = "google",
      # maptype = "satellite",
      # contour1colour = "red",
      # contour2colour = "orange",
      # plottitle = "Aggregated 95% and 50% UD contours",
      plotsubtitle = "Scaled contours. n = 10",
      # legendtitle = "Percent UD Contours",
      # plotcaption = paste0("movegroup, ", lubridate::today()),
      # axisxlabel = "Longitude",
      # axisylabel = "Latitude",
      legendposition = c(0.9, 0.89),
      # fontsize = 12,
      # fontfamily = "Times New Roman",
      # filesavename = paste0(lubridate::today(), "_dBBMM-contours.png"),
      savedir = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/Plot"),
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
      # ISSUE####
      # Summer subset, ID = "X177942", not created by movegroup, thus can't be found to be plotted
      # movegroup said: "processing 7 of 7" i.e. not 8 of 8
      plotraster(
        x = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/", thisshark, ".asc"),
        crsloc = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/"),
        googlemap = TRUE,
        expandfactor = 1,
        mapzoom = 7,
        plotsubtitle = "Scaled contours. n = 10",
        legendposition = c(0.9, 0.89),
        filesavename = paste0(lubridate::today(), "_", thisshark, "_dBBMM-contours.png"),
        savedir = paste0(saveloc, "movegroup dBBMMs/", thissubset, "/", TDL, "h/Scaled/Plot"))}


  } # close for loop 18 24 36 1000 1200h
} # close for (thissubset in mysubsets)

# compare movegroup volumeareas####
compare <- read_csv(paste0(saveloc, "movegroup dBBMMs/timeDiffLong_comparison.csv"))
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
ggsave(filename = paste0(saveloc, "movegroup dBBMMs/", lubridate::today(), "_timeDiffLong_comparison.png"))








# 4. %Days in Bahamas EEZ####
# ▪ Simple %. Spatial overlap R code, easy.
# ▪ Do we have EEZ shapefile? TG www.marineregions.org
# see /home/simon/Documents/Si Work/PostDoc Work/movegroup help/Liberty Boyd/Points in UD contours/PointsInWhichUDcontour.R
hammerssf <- readRDS(file = paste0(saveloc, "/EEZoverlap/Hammers.Rds")) |>
  sf::st_as_sf(coords = c("lon","lat")) |> sf::st_set_crs(4326) |> # Convert points to sf
  mutate(Index = row_number()) # for indexing later
maploc = "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bahamas EEZ Shapefile/"
EEZ <- sf::st_read(paste0(maploc,"eez.shp")) # polygon

# pointsinpolysubset <- points[polygon,] #sf objects subset points occurring in poly
hammerssfinEEZ <- hammerssf[EEZ,]
hammerssfinAndros <- hammerssf |> filter(group == "Andros")
hammerssfinBimini <- hammerssf |> filter(group == "Bimini")
hammerssfinSummer <- hammerssf |> filter(month(date) %in% c(5:10))
hammerssfinWinter <- hammerssf |> filter(month(date) %in% c(11:12, 1:4))
hammerssfinAndrosSummer <- hammerssf |> filter(group == "Andros", month(date) %in% c(5:10))
hammerssfinAndrosWinter <- hammerssf |> filter(group == "Andros", month(date) %in% c(11:12, 1:4))
hammerssfinBiminiSummer <- hammerssf |> filter(group == "Bimini", month(date) %in% c(5:10))
hammerssfinBiminiWinter <- hammerssf |> filter(group == "Bimini", month(date) %in% c(11:12, 1:4))

hammerssfinEEZAndros <- hammerssfinAndros[EEZ,]
hammerssfinEEZBimini <- hammerssfinBimini[EEZ,]
hammerssfinEEZSummer <- hammerssfinSummer[EEZ,]
hammerssfinEEZWinter <- hammerssfinWinter[EEZ,]
hammerssfinEEZAndrosSummer <- hammerssfinAndrosSummer[EEZ,]
hammerssfinEEZAndrosWinter <- hammerssfinAndrosWinter[EEZ,]
hammerssfinEEZBiminiSummer <- hammerssfinBiminiSummer[EEZ,]
hammerssfinEEZBiminiWinter <- hammerssfinBiminiWinter[EEZ,]

hammerssf$EEZ <- as.logical(FALSE)
hammerssf[hammerssf$Index %in% hammerssfinEEZ$Index, "EEZ"] <- as.logical(TRUE)

hammerssfinAndros$EEZAndros <- as.logical(FALSE)
hammerssfinAndros[hammerssfinAndros$Index %in% hammerssfinEEZAndros$Index, "EEZAndros"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinAndros$Index, "EEZAndros"] <- hammerssfinAndros$EEZAndros

hammerssfinBimini$EEZBimini <- as.logical(FALSE)
hammerssfinBimini[hammerssfinBimini$Index %in% hammerssfinEEZBimini$Index, "EEZBimini"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinBimini$Index, "EEZBimini"] <- hammerssfinBimini$EEZBimini

hammerssfinSummer$EEZSummer <- as.logical(FALSE)
hammerssfinSummer[hammerssfinSummer$Index %in% hammerssfinEEZSummer$Index, "EEZSummer"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinSummer$Index, "EEZSummer"] <- hammerssfinSummer$EEZSummer

hammerssfinWinter$EEZWinter <- as.logical(FALSE)
hammerssfinWinter[hammerssfinWinter$Index %in% hammerssfinEEZWinter$Index, "EEZWinter"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinWinter$Index, "EEZWinter"] <- hammerssfinWinter$EEZWinter

hammerssfinAndrosSummer$EEZAndrosSummer <- as.logical(FALSE)
hammerssfinAndrosSummer[hammerssfinAndrosSummer$Index %in% hammerssfinEEZAndrosSummer$Index, "EEZAndrosSummer"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinAndrosSummer$Index, "EEZAndrosSummer"] <- hammerssfinAndrosSummer$EEZAndrosSummer

hammerssfinAndrosWinter$EEZAndrosWinter <- as.logical(FALSE)
hammerssfinAndrosWinter[hammerssfinAndrosWinter$Index %in% hammerssfinEEZAndrosWinter$Index, "EEZAndrosWinter"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinAndrosWinter$Index, "EEZAndrosWinter"] <- hammerssfinAndrosWinter$EEZAndrosWinter

hammerssfinBiminiSummer$EEZBiminiSummer <- as.logical(FALSE)
hammerssfinBiminiSummer[hammerssfinBiminiSummer$Index %in% hammerssfinEEZBiminiSummer$Index, "EEZBiminiSummer"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinBiminiSummer$Index, "EEZBiminiSummer"] <- hammerssfinBiminiSummer$EEZBiminiSummer

hammerssfinBiminiWinter$EEZBiminiWinter <- as.logical(FALSE)
hammerssfinBiminiWinter[hammerssfinBiminiWinter$Index %in% hammerssfinEEZBiminiWinter$Index, "EEZBiminiWinter"] <- as.logical(TRUE)
hammerssf[hammerssf$Index %in% hammerssfinBiminiWinter$Index, "EEZBiminiWinter"] <- hammerssfinBiminiWinter$EEZBiminiWinter

print(paste0("Percent of days in Bahamas EEZ, all data: ", round(length(which(hammerssf$EEZ)) / length(hammerssf$EEZ) * 100, 1), "%; ", length(hammerssf$EEZ), " days"))
# Percent of days in Bahamas EEZ, all data: 65.6%; 3733 days
print(paste0("Percent of days in Bahamas EEZ, Andros-tagged: ", round(length(which(hammerssfinAndros$EEZAndros)) / length(hammerssfinAndros$EEZAndros) * 100, 1), "%; ", length(hammerssfinAndros$EEZAndros), " days"))
# Percent of days in Bahamas EEZ, Andros-tagged: 68%; 2211 days
print(paste0("Percent of days in Bahamas EEZ, Bimini-tagged: ", round(length(which(hammerssfinBimini$EEZBimini)) / length(hammerssfinBimini$EEZBimini) * 100, 1), "%; ", length(hammerssfinBimini$EEZBimini), " days"))
# Percent of days in Bahamas EEZ, Bimini-tagged: 62%; 1522 days
print(paste0("Percent of days in Bahamas EEZ, Summer: ", round(length(which(hammerssfinSummer$EEZSummer)) / length(hammerssfinSummer$EEZSummer) * 100, 1), "%; ", length(hammerssfinSummer$EEZSummer), " days"))
# Percent of days in Bahamas EEZ, Summer: 59.1%; 1659 days
print(paste0("Percent of days in Bahamas EEZ, Winter: ", round(length(which(hammerssfinWinter$EEZWinter)) / length(hammerssfinWinter$EEZWinter) * 100, 1), "%; ", length(hammerssfinWinter$EEZWinter), " days"))
# Percent of days in Bahamas EEZ, Winter: 70.7%; 2074 days
print(paste0("Percent of days in Bahamas EEZ, Andros-tagged, Summer: ", round(length(which(hammerssfinAndrosSummer$EEZAndrosSummer)) / length(hammerssfinAndrosSummer$EEZAndrosSummer) * 100, 1), "%; ", length(hammerssfinAndrosSummer$EEZAndrosSummer), " days"))
# Percent of days in Bahamas EEZ, Andros-tagged, Summer: 61.3%; 1013 days
print(paste0("Percent of days in Bahamas EEZ, Andros-tagged, Winter: ", round(length(which(hammerssfinAndrosWinter$EEZAndrosWinter)) / length(hammerssfinAndrosWinter$EEZAndrosWinter) * 100, 1), "%; ", length(hammerssfinAndrosWinter$EEZAndrosWinter), " days"))
# Percent of days in Bahamas EEZ, Andros-tagged, Winter: 73.6%; 1198 days
print(paste0("Percent of days in Bahamas EEZ, Bimini-tagged, Summer: ", round(length(which(hammerssfinBiminiSummer$EEZBiminiSummer)) / length(hammerssfinBiminiSummer$EEZBiminiSummer) * 100, 1), "%; ", length(hammerssfinBiminiSummer$EEZBiminiSummer), " days"))
# Percent of days in Bahamas EEZ, Bimini-tagged, Summer: 55.6%; 646 days
print(paste0("Percent of days in Bahamas EEZ, Bimini-tagged, Winter: ", round(length(which(hammerssfinBiminiWinter$EEZBiminiWinter)) / length(hammerssfinBiminiWinter$EEZBiminiWinter) * 100, 1), "%; ", length(hammerssfinBiminiWinter$EEZBiminiWinter), " days"))
# Percent of days in Bahamas EEZ, Bimini-tagged, Winter: 66.8%; 876 days




# TODOLIST####
# transit shorter steplengthBL than resident. But transit angles all (bar1) smaller.

# if kstar1 & 2 both = 2, great. But what if they both = 3? Or are different? Or are Inf?

# make whole thing info function (2 functions? to keep loops separate? They're both fishlist loops though? reduce fail points tho), collect user variables, age, steplength, id, lat, lon, date, database name hammers. autodelete cruft objects. L291 make centers dynamic

# Documentation
# kstar / tolerance, from VanM paper
# Tolerance T is analogous to setting the alpha level in the standard hypothesis testing framework, where increased tolerance is similar to selecting a smaller alpha rejection region. Tibshirani et al. (2001) used a tolerance of 1, but larger values of tolerance increase the strength of evidence
# required to include additional clusters (see Tibshirani et al. 2001 for full details and formulations). Tibshirani et al. (2001) demonstrated that the gap statistic performed well in detecting number of clusters when clusters are well separated; however, it was sensitive to the amount of overlap
# between clusters. Fortunately, the bias in sensitivity is such that it is likely to identify fewer clusters than there are in truth (Tibshirani, R., G. Walther, and T. Hastie. 2001. Estimating the number of clusters in a dataset via the gap statistic. Journal of the Royal Statistical Society B 63:411–423)

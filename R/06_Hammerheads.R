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
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2022-09 Great Hammerhead habitat movement/"

hammers <- readRDS("/home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Data/Hammerhead SPOT tags/Output_data_for_kMeans_and_dBBMM/Data_aniMotum_CRW_output_fitted_proj_WGS84_converted_with_coord_CIs_S.mokarran.rds") |>
  # create col from id, removing . & all after. Use for left_join
  mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\.")[,1] - 1)))

meta <- read_csv("../../Data/Hammerhead SPOT tags/Datasheet_Bahamas_Smok_Tagging_Metadata_NEW.csv") |>
  rename(shark = ptt_id,
         FishLengthCm = stl)
hammers %<>% left_join(meta) # , by = join_by(shark == id) # doesn't work naming columns, has gotten worse.



# Step Length & Turn Angles####
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
    track <- mk_track(df_nonai,
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











# Kmeans VanMoorter/SD####
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
    # replace StepLengthBL with StepLengthBLlog1p in selection here down, 12 occurrences####
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


    ###plots GAP stat per clusters +SE####
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

  # TODO test & fix /500 issue####
  hammers |>
    dplyr::filter(id == "177940.1") |>
    pull(StepLengthBLlog1p) |>
    mean(na.rm = TRUE)

  write.csv(x = clusterinfo, file = paste0(saveloc, "KmeansClusterinfo.csv"), row.names = FALSE)
} else {# close if (!all(is.na(alldaily$lat)))
  print("all new days missing latitude data, can't get external data, nothing to do")
}

# Show how amny clusters were chosen most commonly
clustersvec <- c(clusterinfo$nClustersTolerance1, clusterinfo$nClustersTolerance2)
clustersvec <- clustersvec[!is.infinite(clustersvec)]
clustersvec %>%
  as_tibble() %>%
  group_by(value) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# value     n
#     2    12
#     3     4
#     1     3
# could do this more systematically. Could also weight the Tolerance1/2 differently? Leave it for now, perfect enemy of good.

# wrap up####
setDF(hammers)
hammers$StepLengthBLlog1p <-  expm1(hammers$StepLengthBLlog1p + logmean)
saveRDS(object = hammers, file = paste0(saveloc, "Hammers_KMeans.Rds"))
rm(list = ls()) #remove all objects
beep(8) #notify completion
lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))

# TODOLIST####


# transit shorter steplengthBL than resident. But transit angles all (bar1) smaller.

# if kstar1 & 2 both = 2, great. But what if they both = 3? Or are different? Or are Inf?

# make whole thing info function (2 functions? to keep loops separate? They're both fishlist loops though? reduce fail points tho), collect user variables, age, steplength, id, lat, lon, date, database name hammers. autodelete cruft objects. L291 make centers dynamic

# Documentation
# kstar / tolerance, from VanM paper
# Tolerance T is analogous to setting the alpha level in the
# standard hypothesis testing framework, where increased
# tolerance is similar to selecting a smaller alpha rejection
# region. Tibshirani et al. (2001) used a tolerance of 1, but
# larger values of tolerance increase the strength of evidence
# required to include additional clusters (see Tibshirani et al. 2001 for full details and formulations). Tibshirani et al.
# (2001) demonstrated that the gap statistic performed well in
# detecting number of clusters when clusters are well
# separated; however, it was sensitive to the amount of
# overlap between clusters. Fortunately, the bias in sensitivity
# is such that it is likely to identify fewer clusters than there
# are in truth
# Tibshirani, R., G. Walther, and T. Hastie. 2001. Estimating the number of clusters in a dataset via the gap statistic. Journal of the Royal Statistical Society B 63:411–423.

# Bram van Moorter 2010 90% of code
# Simon Dedman 2019 some tweaks & plotting/extracting at the end

###Make sure the library(clusterSim) with all its dependencies is properly installed
###Make sure R is using as its working folder the folder with the files:
###                "gap.statistic.r", and "data.csv" or your own data.

# cmpfun(KMeansAppend)
# KMeansAppend() # KMeansAppend(machine = "/media/Seagate/Work/")
KMeansAppend <- function(machine = "/home/simon/Documents/Si Work/",
                         loadlistcompare = TRUE) { # run everything (as function

    library(compiler)
    # sudo apt-get install libglu1-mesa-dev
    library(rgl)
    library(clusterSim)
    library(data.table)
    library(beepr)
    options(error = function() beep(9))  # give warning noise if it fails

    # machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
    # machine <- "/media/Seagate/Work/" #Poseidon
    # loadloc = paste0(machine, "Blocklab/abft_diving/12_TransitDive_append/") #per saveloc in MolaFoldersExtractLoop.R
    loadloc = paste0(machine, "Blocklab/abft_diving/All_Daily/") #ensure trailing /slash
    # saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/") #ensure trailing /slash
    setwd(loadloc) #run all as chunk
    # source("gap.statistic.r")  ##this function loads the function "index.Gap.modif"
    source('/home/simon/Dropbox/Blocklab Monterey/Blocklab/vanMoorter.etal.2010/p7_gap.statistic.r')
    # df_i$TurnAngleRelDeg
    # df_i$StepLengthKm
    alldaily <- readRDS("AllDailies_Transit.Rds")
    alldaily$kmeans2cluster <- as.character(rep(NA, nrow(alldaily)))
    alldaily$kmeansBinary <- as.integer(rep(NA, nrow(alldaily)))

    setDT(alldaily) # convert to data.table without copy

    #loadlist comparison chunk####
    if (loadlistcompare) {
        savetarget <- readRDS("AllDailies_KMeans.Rds")
        savelist <- paste0(savetarget$fishID, "_", savetarget$Date) #list files based on specified pattern above
        loadlist <- paste0(alldaily$fishID, "_", alldaily$Date) #list files based on specified pattern above
        if (length(which(!loadlist %in% savelist)) != 0) { # if there are fishID days in (newer) Lunar & not in (older) DST
            print(paste0(length(which(!loadlist %in% savelist)), " fishID days present in new AllDailies & not present in old"))
            AllDailies <- alldaily # save full object for later
            alldaily <- alldaily[which(!loadlist %in% savelist),] # subset alldaily to only those fishIDdays
            rm("loadlist", "savelist")
        } # close loadlist compare length check
    } # close loadlistcompare

    fishlist <- unique(alldaily$fishID)
    for (i in fishlist) { # i <- fishlist[28]
        df_i <- alldaily[which(alldaily$fishID == i),] #subset to each fish
        print(paste0(which(fishlist == i), " of ", length(fishlist), "; calculating KMeans clustering for ", i))
        if (nrow(df_i) < 5) next # skip else kmeans will break. fish 51 (3) & 82 (7)
        setDF(df_i) # else things break

        # data <- read.csv("data.csv", header = T, sep = ",")   ##load data
        # data <- read.csv("R/vanMoorter.etal.2010/p7_data.csv", header = T, sep = ",")   ##load data

        # x <- data[,c("ACT1", "ACT2", "SL_METERS", "TURN_DEGRE")]
        # downsample df_i to days
        x <- df_i[!is.na(df_i$StepLengthKm),] # omit rows with NA values for date, downsample to days only
        x <- x[!is.na(x$TurnAngleRelDeg),] #
        if (nrow(x) < 5) next # skip else kmeans will break. fish 28
        x <- x[,c("StepLengthKm", "TurnAngleRelDeg")] # , "Date", "Index"

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

        ###To allow you to obtain exactly the same results as in the paper:
        ##it is recommended to vary this value, to check the effect of the initial random values
        # set.seed(1)
        setDF(x) # else things break
        res <- data.frame(GAP = NA, s = NA, Wo = NA, We = NA) # what are s (SD?) Wo & We?
        for (j in 1:4 ) { ##determine the GAP-statistic for 1 to 4 clusters. CHanged from 10 to reduce compute time and we don't anticipate >4 XY XYT movement clusters
            if (j == 1) {  ##clall is the vector (in matrix format) of integers indicating the group to which each datum is assigned
                ones <- rep(1, nrow(x))
                clall <- matrix(ones)}
            if (j > 1) {
                cl1 <- kmeans(x, j, iter.max = 100)
                clall <- matrix(cl1$cluster)}
            g <- index.Gap.modif(x, clall, reference.distribution = "pc", B = 50, method = "k-means")
            res[j,] <- c(g$gap, g$s, g$Wo, g$We)
        } # close j

        # ###plot the GAP statistic for each number of clusters together with its standard error
        # par(mfrow = c(1,1))
        # k <- seq(1:length(res$GAP))
        # plot(k, res$GAP, xlab = "number of clusters k", ylab = "GAP", main = "GAP statistic", type = "b")
        # segments(k, c(res$GAP - res$s), k, c(res$GAP + res$s))
        # kstar <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - res$s)[-1]))
        # kstar2 <- min(which(res$GAP[-length(res$GAP)] >= c(res$GAP - (1.96*res$s))[-1]))
        # points(kstar2, res$GAP[kstar2], pch = 22, bg = "gray", cex = 1.25) #grey square box for tolerance2
        # points(kstar, res$GAP[kstar], col = "black", pch = 19, cex = 0.6) # black dot for tolerance1
        #
        # # how to get the table which shows means etc for each cluster group?
        # # would be nice also to get Fig1 var1 vs var2 clustering scatterplots
        # plot(x$StepLengthKm, x$TurnAngleRelDeg)
        # df_i$TurnAngleRelDeg <- abs(df_i$TurnAngleRelDeg)
        # plot(df_i$StepLengthKm, df_i$TurnAngleRelDeg)
        # hist(df_i$TurnAngleRelDeg)
        # plot(df_i[df_i$StepLengthKm < 50, "StepLengthKm"], df_i[df_i$StepLengthKm < 50, "TurnAngleRelDeg"]) # SL<50 only
        # hist(df_i[df_i$StepLengthKm < 50, "TurnAngleRelDeg"])
        # plot(df_i[df_i$StepLengthKm < 100 & df_i$StepLengthKm > 50, "StepLengthKm"], df_i[df_i$StepLengthKm < 100 & df_i$StepLengthKm > 50, "TurnAngleRelDeg"]) # 50>SL>100 only
        # hist(df_i[df_i$StepLengthKm < 100 & df_i$StepLengthKm > 50, "TurnAngleRelDeg"])
        # plot(df_i[df_i$StepLengthKm > 100, "StepLengthKm"], df_i[df_i$StepLengthKm > 100, "TurnAngleRelDeg"]) # SL>100 only
        # hist(df_i[df_i$StepLengthKm > 100, "TurnAngleRelDeg"])

        # then redo kmeans with selected number of clusters (kmeans output cl1 gets overwritten per i)
        kmeans2 <- kmeans(x, 2, iter.max = 100) #run kmeans
        # extracted <- cbind(extracted, kmeans2$cluster) # add cluster value to each row
        df_i[as.integer(names(kmeans2$cluster)), "kmeans2cluster"] <- kmeans2$cluster

        # kmeans2$centers
        # # StepLengthKm TurnAngleRelDeg
        # # 1    0.5816330       0.3355378
        # # 2    0.4993464       0.7817846
        # # these are standardised lognormalised values, not actual steplengths.
        # # Can get actual mean values since cluster bins are now added to extracted.
        # extractedK1 <- extracted[extracted$kmeans2cluster == 1,]
        # extractedK2 <- extracted[extracted$kmeans2cluster == 2,]
        # mean(extractedK1$StepLengthKm) #65.93499
        # mean(df_i[which(df_i$kmeans2cluster == 1), "StepLengthKm"], na.rm = T)
        # mean(extractedK1$TurnAngleRelDeg) #6.476207
        # mean(abs(df_i[which(df_i$kmeans2cluster == 1), "TurnAngleRelDeg"]), na.rm = T)
        # mean(extractedK2$StepLengthKm) #39.47376
        # mean(df_i[which(df_i$kmeans2cluster == 2), "StepLengthKm"], na.rm = T)
        # mean(extractedK2$TurnAngleRelDeg) #74.15654
        # mean(abs(df_i[which(df_i$kmeans2cluster == 2), "TurnAngleRelDeg"]), na.rm = T)
        # # Group 1: 66km step, 6deg angle, long and straight, transition/transit/migration
        # # Group 2: 39km step, 74deg angle, short and turny, resident/forage/spawn
        #
        # kmeans2$withinss
        # # 874.1479 751.0032
        # kmeans2$size
        # # 20920 18726

        #save metadata for later####
        if (!exists("clusterinfo")) { #if this is the first i, create sensordates object
            clusterinfo <- data.frame(cluster1StepLengthKmMean = mean(df_i[which(df_i$kmeans2cluster == 1), "StepLengthKm"], na.rm = T),
                                      cluster1TurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeans2cluster == 1), "TurnAngleRelDeg"]), na.rm = T),
                                      cluster2StepLengthKmMean = mean(df_i[which(df_i$kmeans2cluster == 2), "StepLengthKm"], na.rm = T),
                                      cluster2TurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeans2cluster == 2), "TurnAngleRelDeg"]), na.rm = T),
                                      stringsAsFactors = FALSE)
        } else { #else add to existing object
            clusterinfoadd <- data.frame(cluster1StepLengthKmMean = mean(df_i[which(df_i$kmeans2cluster == 1), "StepLengthKm"], na.rm = T),
                                         cluster1TurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeans2cluster == 1), "TurnAngleRelDeg"]), na.rm = T),
                                         cluster2StepLengthKmMean = mean(df_i[which(df_i$kmeans2cluster == 2), "StepLengthKm"], na.rm = T),
                                         cluster2TurnAngleRelDegAbsMean = mean(abs(df_i[which(df_i$kmeans2cluster == 2), "TurnAngleRelDeg"]), na.rm = T),
                                         stringsAsFactors = FALSE)
            clusterinfo <- rbind(clusterinfo,
                                 clusterinfoadd,
                                 stringsAsFactors = FALSE) # add to existing file
        } #close if else

        df_i[which(df_i$kmeans2cluster == 1), "kmeans2cluster"] <- "transit" #replace 1&2 with named groups
        df_i[which(df_i$kmeans2cluster == 2), "kmeans2cluster"] <- "resident"
        df_i$kmeansBinary <- rep(NA, nrow(df_i))
        df_i[which(df_i$kmeans2cluster == "resident"), "kmeansBinary"] <- 0
        df_i[which(df_i$kmeans2cluster == "transit"), "kmeansBinary"] <- 1

        setDT(df_i) # convert to data.table without copy
        # join and update "df_i" by reference, i.e. without copy
        # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
        # If you want to return only df_nonai that have a matching df_i (i.e. rows where the key is in both tables), set the nomatch argument of data.table to 0.
        # nomatch isn't relevant together with :=, ignoring nomatch
        alldaily[df_i, on = c("Date", "fishID"), kmeans2cluster := i.kmeans2cluster]
        alldaily[df_i, on = c("Date", "fishID"), kmeansBinary := i.kmeansBinary]

        # write.csv(x = extracted,
        #           file = paste0(saveloc, "extracted_kmeans2.csv"),
        #           row.names = F)

    } #close i

    if (loadlistcompare) {
        library(data.table)
        setDT(AllDailies) # convert to data.table without copy
        setDT(savetarget) # convert to data.table without copy
        AllDailies[alldaily, on = c("Date", "fishID"), kmeans2cluster := i.kmeans2cluster]
        AllDailies[alldaily, on = c("Date", "fishID"), kmeansBinary := i.kmeansBinary]
        AllDailies[savetarget, on = c("Date", "fishID"), kmeans2cluster := i.kmeans2cluster]
        AllDailies[savetarget, on = c("Date", "fishID"), kmeansBinary := i.kmeansBinary]
        alldaily <- AllDailies
    } # close loadlistcompare

    setDF(alldaily)
    saveRDS(object = alldaily, file = paste0(loadloc, "AllDailies_KMeans.Rds"))
    saveRDS(object = clusterinfo, file = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/KMeans/clusterinfo.Rds")) #save
    write.csv(x = clusterinfo, file = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/KMeans/clusterinfo.csv"), row.names = FALSE)
    rm(list = ls()) #remove all objects
    beep(8) #notify completion
    lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
    invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))
    # gc() #garbage collection, free up memory
} # close function

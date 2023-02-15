#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simon Dedman simondedman@gmail.com started 2019.06.13
# 1. manually classify each of 8 dive types (hereafter BB8):
#     1. transition/transit/travel
#     2. glide
#     3. oscillation / bounce dive A: 100m depth, v. steep thermocline
#     4. oscillationB: 300m depth, depth thermocline to bottom of dive profile
#     5. oscillationC: To ocean bottom: feeding on cod etc.
#     6. Diel Vertical Migration DVM
#     7. Deep dive
#     8. Spike dive, sunrise/sunset
# Will likely require additional secondary data e.g. sunset/sunrise/day/night

# 2. Test classification skill against plots, and Wilson2009 where applicable
# 3. Tweak 1 as required until satisfied. Devise algorithmic rules for tweaking
# 4. Automatically classify N divetypes using TOOLS: collate notes, choose candidate tools
# 5. Run 4 & compare to BB8. Different?
#    YES: are differences justifiable?
#         YES: describe new types, formalise the process of testing?
#         NO: Tweak 4, rerun
#    NO: Continue
# 6. Run 4 on ABFT in loop, save
# 7. Test against PBFT, Marlin, etc: descending similarity
# 8. Release as package?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1 transition/transit/travel####
# adds: HourFloor, DepthRangeHr, Hhrs50mLesDepRange, StepLengthKm, TurnAngleRelDeg, TurnAngleAzimDeg

# Horton 2019:
# Daily locations of each track were assigned a short-term behaviour (either
# resident or migrating), classified using adehabitatLT in R (Clément Calenge
# 2006; Clement Calenge 2007). Inter-location distances were calculated for
# successive daily locations and periods
##define period
# with a mean of 50 km (mean value
# travelled per day) considered to indicate residency and searching behaviour
# 50 +-100km/day = <=150km/day
# and periods with a mean of 125 km considered to indicate fast-migration.
# 125+-100km/day = >150?
# A standard deviation of 100 km was used to account for the latitudinal and
# longitudinal error associated with light-based geolocation (estimated at 1.1 ±
# 0.3° latitude and 1.4 ± 0.4° longitude for the present study).
#
# The maximum distance travelled by an ABT (234cm CFL) in a single day was 275
# km (1.4 body lengths s-1) and 93% of daily movements for all ABT combined were
# less than 125 km (n=2,570 days) with 67% of combined daily movements less than
# 50 km (n=1,850 days). ABT exhibited a fast-transiting behaviour for between 2
# and 50 days tag-1
# daily dist km #done
# daily dist bl #done

# Edelhoff2016path review paper ref 21 33 45 esp 21 Van Moorter etal 2010

# cmpfun(diveClassify1transit)
# diveClassify1transit() # diveClassify1transit(machine = "/media/Seagate/Work/")
diveClassify1transit <- function(machine = "/home/simon/Documents/Si Work/",
                                 loadlistcompare = TRUE) { # run everything (as function

  # library(tidyverse)
  library(dplyr)
  library(magrittr)
  library(amt) #https://arxiv.org/pdf/1805.03227.pdf #sudo apt install libgsl-dev
  library(RcppRoll)
  library(lwgeom)
  library(TropFishR) #VGBF
  library(data.table)
  library(lubridate)
  library(beepr)
  source('~/Dropbox/Blocklab Monterey/Blocklab/liRolling.R') #my own function for rolling Linearity Index values
  options(error = function() beep(9))  # give warning noise if it fails
  # library(compiler)

  #start section####
  # machine <- "/home/simon/Documents/Si Work/" #Nautilus
  # machine <- "/media/Seagate/Work/" #Poseidon
  # loadloc = paste0(machine, "Blocklab/abft_diving/11_DVMdive_append/") #ensure trailing /slash
  loadloc = paste0(machine, "Blocklab/abft_diving/All_Daily/") #ensure trailing /slash
  # saveloc = paste0(machine, "Blocklab/abft_diving/12_TransitDive_append/") #ensure trailing /slash
  # whichfiles <- ".Rds" # "5112003_L231-0194_L_D" #

  setwd(loadloc) #run all as chunk

  # loadlist <- list.files(pattern = whichfiles) #list files based on specified pattern above
  # loadlist <- loadlist[96:167] #96 missed, 5107036 155mb, 125 5112003 381mb

  # for (i in loadlist) { #sequential # i <- loadlist[1]
  df_i <- readRDS("AllDailies_Eddy.Rds")
  df_i$li5day <- as.numeric(rep(NA, nrow(df_i)))
  df_i$StepLengthKm <- as.numeric(rep(NA, nrow(df_i)))
  df_i$StepLengthBL <- as.numeric(rep(NA, nrow(df_i)))
  df_i$TurnAngleRelDeg <- as.numeric(rep(NA, nrow(df_i)))
  df_i$TurnAngleAzimDeg <- as.numeric(rep(NA, nrow(df_i)))
  df_i$FishLengthCm <- as.numeric(rep(NA, nrow(df_i)))
  AllDailies <- df_i # for later if loadlistcompare=F else overwritten
  setDT(AllDailies) # convert to data.table without copy
  # df_i <- readRDS(i) #assign file to name, prepend with x to avoid numerical named object

  #bigfile
  # library(magrittr)
  # df_i %<>% dplyr::select(c(DateTimeUTCmin5, Depth.m., Date, lat, lon, age))
  # gc()

  #run section####
  # df_i$HourFloor <- floor_date(df_i$DateTimeUTCmin5, unit = "hour") #overwrites existing column only for next line
  # Hourstats <- df_i %>%  #mutate(HourFloor = floor_date(DateTimeUTCmin5, unit = "hour")) %>%
  #   group_by(HourFloor) %>%
  #   summarise(DepthRangeHr = max(Depth.m., na.rm = TRUE) - min(Depth.m., na.rm = TRUE)) %>%
  #   ungroup()
  # df_i[duplicated(df_i$HourFloor),"HourFloor"] <- NA  # remove duplicates, column back as it was
  #
  # df_i <- left_join(x = df_i, #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)
  #                   y = Hourstats)
  # rm(Hourstats)
  #
  # df_i$Date <- floor_date(df_i$DateTimeUTCmin5, unit = "day") #overwrites existing column only for next line
  # DayStats <- df_i %>%
  #   group_by(Date) %>%
  #   # Depth range < 50m per hour for > (e.g.) 20 hours/day = transit day
  #   summarise(Hrs50mLesDepRange = sum(DepthRangeHr <= 50, na.rm = T)) %>%
  #   ungroup()
  # df_i[duplicated(df_i$Date),"Date"] <- NA  # remove duplicates, column back as it was
  #
  # df_i <- left_join(x = df_i, #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)
  #                   y = DayStats)
  # rm(DayStats)


  #loadlist comparison chunk####
  if (loadlistcompare) {
    savetarget <- readRDS("AllDailies_Transit.Rds")
    savelist <- paste0(savetarget$fishID, "_", savetarget$Date) #list files based on specified pattern above
    loadlist <- paste0(df_i$fishID, "_", df_i$Date) #list files based on specified pattern above
    difflist <- which(!loadlist %in% savelist)

    if (length(difflist) == 0) { # if there are no differences, left join existing Transit, to updated (typically) Eddy save, clean, close
      library(dplyr)
      library(magrittr)
      savetarget %<>% dplyr::select(Date, fishID, li5day, StepLengthKm, StepLengthBL, TurnAngleRelDeg, TurnAngleAzimDeg, FishLengthCm) # remove extra cols
      df_i %<>%
        select(-li5day, -StepLengthKm, -StepLengthBL, -TurnAngleRelDeg, -TurnAngleAzimDeg, -FishLengthCm) %>% # remove NA column first
        left_join(savetarget) # Joining, by = c("Date", "fishID")
      saveRDS(object = df_i, file = paste0(loadloc, "AllDailies_Transit.Rds"))
      rm(list = ls()) #remove all objects
      beep(8) #notify completion
      stop("Success: Transit file already existed; data were joined successfully")
      lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
      invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))
      gc() #garbage collection, free up memory
    } # close if if (length(difflist) == 0)

    if (length(difflist) != 0) {
      print(paste0(length(savelist[which(savelist %in% loadlist)]), " fishID days present in old AllDailies & also present in new AllDailies; merging in"))
      library(data.table)
      AllDailies <- df_i
      setDT(AllDailies) # convert to data.table without copy
      setDT(savetarget) # convert to data.table without copy
      AllDailies[savetarget, on = c("Date", "fishID"), li5day := i.li5day]
      AllDailies[savetarget, on = c("Date", "fishID"), StepLengthKm := i.StepLengthKm]
      AllDailies[savetarget, on = c("Date", "fishID"), StepLengthBL := i.StepLengthBL]
      AllDailies[savetarget, on = c("Date", "fishID"), TurnAngleRelDeg := i.TurnAngleRelDeg]
      AllDailies[savetarget, on = c("Date", "fishID"), TurnAngleAzimDeg := i.TurnAngleAzimDeg]
      AllDailies[savetarget, on = c("Date", "fishID"), FishLengthCm := i.FishLengthCm]
      print(paste0(length(difflist), " fishID days present in new AllDailies & not present in old"))
      AllDailies <<- df_i # save updated full object for later
      df_i <- df_i[difflist,] # subset df_i to fishIDdays present in new AllDailies & not present in old
      rm("loadlist", "savelist", "difflist")
    } # close if (length(difflist) != 0)
  } # close loadlistcompare

  if (!all(is.na(df_i$lat))) { # if not all lats are NA, i.e. there's something to be done
    # amt steps etc here
    #StepLength, TurnAngleRelDeg, TurnAngleAzimDeg
    #dfi remove NA dates and lats and lons rows
    # df_nona <- df_i[!is.na(df_i$Date),] # omit rows with NA values for date, downsample to days only
    df_nona <- df_i
    df_nona[which(df_nona$lat > 90), "lat"] <- NA # lat over 90 breaks, impossible value, will fix upstream
    df_nona[which(df_nona$lat < -90), "lat"] <- NA # ditto
    df_nona <- df_nona[!is.na(df_nona$lat),] # omit rows with NA values for lat, downsample to days only
    df_nona <- df_nona[!is.na(df_nona$lon),] # omit rows with NA values for lon, downsample to days only
    fishlist <- unique(df_nona$fishID)

    for (i in fishlist) { # i <- fishlist[172]
      df_nonai <- df_nona[which(df_nona$fishID == i),] #subset to each fish
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
                        .t = Date, # was DateTimeUTCmin5
                        crs = sp::CRS("+init=epsg:4326")) %>%
        transform_coords(sp::CRS("+init=epsg:6931"))
      stps <- steps(track)
      # step  length  (sl;  in  CRSunits), 6931=m
      df_nonai$StepLengthKm <- c(NA, (stps$sl_/1000)) #add NA first since results start from second row
      # convert to body lengths
      # use vonB in reverse to convert age to length per day in dfnona
      # VonBertalanffy parameters, Restrepo et al 2010:
      # Linf (cm): 314.9
      # k: 0.089
      # t0 (year): -1.13
      # variance Linf: 19.43 (not reqd?)
      # have age from length, want length from age
      # if age is missing, populate with NA instead of crashing
      if (is.na(df_nonai$age[1])) {
        df_nonai$FishLengthCm <- rep(NA, nrow(df_nonai))
        df_nonai$StepLengthBL <- rep(NA, nrow(df_nonai))
      } else {
        df_nonai$FishLengthCm <- TropFishR::VBGF(param = list(Linf = 314.9,
                                                              K = 0.089,
                                                              t0 = -1.13),
                                                 t = df_nonai$age)
        # then divide StepLengthKm to BL
        # 1 km/day = 1000m/day = 100000cm/day
        df_nonai$StepLengthBL <- (df_nonai$StepLengthKm * 100000) / df_nonai$FishLengthCm
      } # close else
      # results is distance in body lengths per day.
      # Body lengths per second (mean value per day):
      # summary(df_nonai$StepLengthBL/24/60/60)

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
                              .t = Date, # was DateTimeUTCmin5
                              crs = sp::CRS("+init=epsg:4326"))
      #TurnAngleAzimDeg: compass bearing of new direction, can compute for directionality, migration
      TurnAngleAzimDeg <- direction_abs(track2, full_circle = FALSE, zero_dir = "N", lonlat = TRUE, clockwise = TRUE) %>%
        as_degree
      TurnAngleAzimDeg <- TurnAngleAzimDeg[1:length(TurnAngleAzimDeg) - 1] #remove NA from back
      df_nonai$TurnAngleAzimDeg <- c(NA, TurnAngleAzimDeg) # add NA to front
      df_nonai %<>% dplyr::select(c(Date, li5day, StepLengthKm, StepLengthBL, TurnAngleRelDeg, TurnAngleAzimDeg, FishLengthCm, fishID))
      # df_i %<>% left_join(y = df_nonai) #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)

      setDT(df_i) # convert to data.table without copy
      setDT(df_nonai) # convert to data.table without copy
      # join and update "df_i" by reference, i.e. without copy
      # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
      # If you want to return only df_nonai that have a matching df_i (i.e. rows where the key is in both tables), set the nomatch argument of data.table to 0.
      # nomatch isn't relevant together with :=, ignoring nomatch
      df_i[df_nonai, on = c("Date", "fishID"), li5day := i.li5day]
      df_i[df_nonai, on = c("Date", "fishID"), StepLengthKm := i.StepLengthKm]
      df_i[df_nonai, on = c("Date", "fishID"), StepLengthBL := i.StepLengthBL]
      df_i[df_nonai, on = c("Date", "fishID"), TurnAngleRelDeg := i.TurnAngleRelDeg]
      df_i[df_nonai, on = c("Date", "fishID"), TurnAngleAzimDeg := i.TurnAngleAzimDeg]
      df_i[df_nonai, on = c("Date", "fishID"), FishLengthCm := i.FishLengthCm]

      # kinda flat, low depth range. See weekly pdfs.
      # dives per Y hours < Z  (depends on definition of ‘dive’) = 0
      # distance covered per day > A? Do histogram of distances, then per
      # behaviour type to see if distance covered is sig higher when transitioning, e.g.


      # Big file
      # df_imain <- readRDS(i) #assign file to name, prepend with x to avoid numerical named object
      # df_i <- cbind(df_imain, df_i[,c("HourFloor",
      #                                 "DepthRangeHr",
      #                                 "Hrs50mLesDepRange",
      #                                 "StepLengthKm",
      #                                 "StepLengthBL",
      #                                 "TurnAngleRelDeg",
      #                                 "TurnAngleAzimDeg",
      #                                 "FishLengthCm")])
      # rm(df_imain)

      #end section####
      # saveRDS(object = df_i, file = paste0(saveloc,i))
      # rm(list = c("df_i", "df_nonai", "stps", "track", "track2", "TurnAngleAzimDeg")) # remove objects
      # gc()
    } #close i
    df_i <- df_i[order(df_i[,"Index"]),] #reorder by index
    df_i <- as.data.frame(df_i)
    setDT(df_i) # convert to data.table without copy
    AllDailies[df_i, on = c("Date", "fishID"), li5day := i.li5day]
    AllDailies[df_i, on = c("Date", "fishID"), StepLengthKm := i.StepLengthKm]
    AllDailies[df_i, on = c("Date", "fishID"), StepLengthBL := i.StepLengthBL]
    AllDailies[df_i, on = c("Date", "fishID"), TurnAngleRelDeg := i.TurnAngleRelDeg]
    AllDailies[df_i, on = c("Date", "fishID"), TurnAngleAzimDeg := i.TurnAngleAzimDeg]
    AllDailies[df_i, on = c("Date", "fishID"), FishLengthCm := i.FishLengthCm]
  } else {# close if (!all(is.na(df_i$lat)))
    print("all new days missing latitude data, can't get external data, nothing to do")
  }
  setDF(AllDailies)
  saveRDS(object = AllDailies, file = paste0(loadloc, "AllDailies_Transit.Rds"))
  rm(list = ls()) #remove all objects
  beep(8) #notify completion
  lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))
  # gc() #garbage collection, free up memory. Crashes R?
} # close function

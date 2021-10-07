# TS plot loop
# Simon Dedman simondedman@gmail.com started 2019.11.13 updated 2020-11-21 for STB

# cmpfun(TSplots)
# TSplots() # or TSplots(machine = "/media/Seagate/Work/")
TSplots <- function(machine = "/home/simon/Documents/Si Work/",
                    loadlistcompare = TRUE,
                    labelbehaviour = FALSE) {
  # library(data.table) # nafill locf. Masks between, first, last (dplyr) & hour, isoweek, mday, minute, month, quarter, second, wday, week, yday, year (lubridate) so load first
  library(lubridate)
  # library(tidyr) #pivot_wider
  library(gridExtra) # grid.arrange
  # library(dplyr)
  # library(tidyr) # replace_na
  library(beepr)
  library(magrittr) # %$%
  library(ggplot2)
  library(fs) # dir_create
  library(stringr) # str_remove
  library(scales) # date_format
  library(foreach)
  library(doMC)
  library(progress)
  library(sf)
  library(grid) # imagegrob
  library(ggspatial)
  library(compiler)
  library(tidyverse) # broom      cli        crayon     dbplyr     dplyr      forcats    ggplot2    haven      hms        httr       jsonlite   lubridate
  # magrittr   modelr     pillar     purrr      readr      readxl     reprex     rlang      rstudioapi  rvest      stringr    tibble     tidyr      xml2       tidyverse
  library(tidylog)
  options(error = function() beep(9))  # give warning noise if it fails

  # machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = "../../Data/2020-11-20_silk_tiger_1s_logger_analysis_plots/" #ensure trailing /slash
  # dir.create(paste0(loadloc, "TSplots/"))
  saveloc = "../../Projects/TSplots/" #ensure trailing /slash
  whichfiles <- "\\.csv" # "5112003_L231-0194_L_D" #
  setwd(loadloc) #run all as chunk
  dir.create("clean_csvs")
  loadlist <- list.files(pattern = whichfiles) #list files based on specified pattern above
  # blank df
  depthtempranges <- data.frame(maxdepth = as.numeric(rep(NA, length(loadlist))),
                                maxtemp = as.numeric(rep(NA, length(loadlist))),
                                mintemp = as.numeric(rep(NA, length(loadlist))))

  for (i in loadlist) { # i <- loadlist[1]
    # get start time in proper format
    start_i <- read_csv(file = i,
                        col_names = F, # else the only row read will become the header
                        skip = 1, # skip 1 row, start @ 2
                        n_max = 2) # only read 2nd row
    start_i <- start_i[1,3]

    # read in & clean data
    df_i <- read_csv(file = i,
                     col_names = F,
                     skip_empty_rows = TRUE) # doesn't work?
    # crop junk top & bottom & sides
    df_i <- df_i[(which(df_i$X1 == "Time Stamp") + 1):max(which(!is.na(df_i$X3))), (1:3)]
    # set colnames
    colnames(df_i) <- c("DateTimeUTCmin5", "Depth_m", "Temp_c")
    # fix datetimes
    if (nchar(df_i$DateTimeUTCmin5[1]) == 16) { # 10/11/2020 08:00
      df_i$DateTimeUTCmin5 <- dmy_hm(df_i$DateTimeUTCmin5, tz = "America/Nassau") + 0:59 # add seconds, ignore warning
    } else { # 10/11/2020 08:00:00 i.e. nchar==19
      df_i$DateTimeUTCmin5 <- dmy_hms(df_i$DateTimeUTCmin5, tz = "America/Nassau")
    }
    # crop to correct start time
    start_i <- ymd_hms(paste0(date(df_i$DateTimeUTCmin5)[1], " ", start_i$X3), tz = "America/Nassau") # add first date, correct format
    df_i <- df_i[(which(df_i$DateTimeUTCmin5 == start_i)):nrow(df_i), ]
    # change data column formats
    df_i$Depth_m <- as.double(df_i$Depth_m)
    df_i$Temp_c <- as.double(df_i$Temp_c)

    # 2021-10-06 StartTime EndTime Pressure Adjustments
    if (i == "MOBIUS_20201114_001_ReefShark1A17903_14-11-2020.csv") {
      # adjust start time, remove all datetimes before
      df_i <- df_i[which(df_i$DateTimeUTCmin5 >= as.POSIXct("2020-11-14 10:43:06 EST", tz = "America/Nassau")),]
      # adjust end time, remove all datetimes after
      df_i <- df_i[which(df_i$DateTimeUTCmin5 <= as.POSIXct("2020-11-14 15:32:46 EST", tz = "America/Nassau")),]
      # adjust pressure
      df_i$Depth_m <- df_i$Depth_m - 9.1
    }
    if (i == "ReefShark220201115A17898_15-11-2020.csv") {
      df_i <- df_i[which(df_i$DateTimeUTCmin5 >= as.POSIXct("2020-11-14 14:36:24 EST", tz = "America/Nassau")),]
      df_i <- df_i[which(df_i$DateTimeUTCmin5 <= as.POSIXct("2020-11-15 13:35:16 EST", tz = "America/Nassau")),]
      df_i$Depth_m <- df_i$Depth_m - 3.75
    }
    if (i == "Reef sharkA17903_16-11-2020.csv") {
      df_i <- df_i[which(df_i$DateTimeUTCmin5 >= as.POSIXct("2020-11-15 12:41:14 EST", tz = "America/Nassau")),]
      df_i <- df_i[which(df_i$DateTimeUTCmin5 <= as.POSIXct("2020-11-16 12:28:41 EST", tz = "America/Nassau")),]
      df_i$Depth_m <- df_i$Depth_m - 9.9
    }
    if (i == "Reef5.2A17898_17-11-2020.csv") {
      df_i <- df_i[which(df_i$DateTimeUTCmin5 >= as.POSIXct("2020-11-16 12:12:18 EST", tz = "America/Nassau")),]
      df_i <- df_i[which(df_i$DateTimeUTCmin5 <= as.POSIXct("2020-11-17 09:51:25 EST", tz = "America/Nassau")),]
      df_i$Depth_m <- df_i$Depth_m - 4.6
    }
    if (i == "Reef7A17900_17-11-2020.csv") {
      df_i <- df_i[which(df_i$DateTimeUTCmin5 >= as.POSIXct("2020-11-16 13:20:02 EST", tz = "America/Nassau")),]
      df_i <- df_i[which(df_i$DateTimeUTCmin5 <= as.POSIXct("2020-11-17 11:56:03 EST", tz = "America/Nassau")),]
      df_i$Depth_m <- df_i$Depth_m - 6.1
    }

    # save as cleaned
    write_csv(x = df_i,
              file = paste0("clean_csvs/", str_remove(i, ".csv"), "_clean.csv"))

    depthtempranges$maxdepth[which(loadlist %in% i)] <- max(df_i$Depth_m, na.rm = T)
    depthtempranges$maxtemp[which(loadlist %in% i)] <- max(df_i$Temp_c, na.rm = T)
    depthtempranges$mintemp[which(loadlist %in% i)] <- min(df_i$Temp_c, na.rm = T)
    print(paste0(which(loadlist == i), " of ", length(loadlist), "; cleaned file ", i))
  } # close loadlist i loop

  dir.create("depthtempranges")
  write.csv(x = depthtempranges,
            file = "./depthtempranges/depthtempranges.csv",
            row.names = FALSE)
  depthtempranges <- read.csv(file = "./depthtempranges/depthtempranges.csv")
  maxdepth <- max(pretty(c(0, max(depthtempranges$maxdepth)))) # to scale plot. Could get this by looping through clean files, getting max, rounding to nearest logical number
  maxtemp <- max(pretty(c(min(depthtempranges$mintemp), max(depthtempranges$maxtemp))))
  mintemp <- min(pretty(c(min(depthtempranges$mintemp), max(depthtempranges$maxtemp))))
  # could have an option to force insert manual values here ####
  # maxdepth <- 1000
  # maxtemp <- 0
  # mintemp <- 45

  for (i in loadlist) { # i <- loadlist[1]
    print(paste0(which(loadlist == i), " of ", length(loadlist), "; now generating timeseries plots for ", i))
    df_i <- read_csv(file =  paste0("clean_csvs/", str_remove(i, whichfiles), "_clean.csv"),
                     col_names = T)
    df_i$DateTimeUTCmin5 <- as.POSIXct(df_i$DateTimeUTCmin5, tz = "America/Nassau") # already "POSIXct" "POSIXt", this does nothing?
    df_i$fishID <- str_remove(i, whichfiles) #get fishID, append to df_i for dir_create later
    df_i$Date <- as.Date(df_i$DateTimeUTCmin5, tz = "America/Nassau") # using date() changes days at UTC midnight only
    # Check this in tuna pipeline ####

    df_i %$% dir_create(paste0(saveloc, unique(fishID)))

    #Transform data for plot####
    df_i$Depth_m <- maxdepth - df_i$Depth_m #recalibrate depth to the difference from maxdepth for 2nd Y axis plotting
    depthbreaks <- c(0, 25, 50, 75, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000) # nice breaks for depth
    depthbreaks <- depthbreaks[which(depthbreaks <= maxdepth)] # shrink to the maxdepth limits
    depthbreakslabels <- as.character(depthbreaks)
    depthbreaks <- maxdepth - depthbreaks # reverse
    temprange <- maxtemp - mintemp # zero scale so e.g. 10:46 becomes 0:36
    depthtempconversion <- maxdepth/temprange #conversion to have temp plotted on depth scale (which is the coordinate system)

    mycols <- c("Depth" = "blue",
                "External Temperature" = "green")

    #Depth & Temp P1####
    p1 <- df_i %>% group_by(Date) %>%
      do(Temp = ggplot(data = .) + #p1<-df_i %>%ggplot()+ #original
           # do is essentially an assign/lapply: for each df_i group_by Date, Temp[[1/2/3/...]] is the ggplot of the parent-call data (.)
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = Depth_m, colour = "Depth"), size = rel(0.5)) + # Depth
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = (Temp_c - mintemp) * depthtempconversion, colour = "External Temperature"), size = rel(0.5)) +

           scale_x_datetime(labels = date_format("%H", tz = "America/Nassau"), # GMT
                            # limits: ensure "EST", tz, labels tz, and as.Date tz, are all aligned
                            limits = c(as.POSIXct(paste0(as.character(first(.$Date)), " 00:00:00 EST"), tz = "America/Nassau"), # America/Nassau
                                       as.POSIXct(paste0(as.character(first(.$Date)), " 23:59:59 EST"), tz = "America/Nassau")),
                            date_breaks = "1 hour",
                            minor_breaks = NULL,
                            position = "top",
                            expand = expansion(mult = 0, add = 180)) + #limit to 0:23hrs, plus a bit to get the 00:00 line at the end

           scale_y_continuous(limits = c(0, maxdepth), #could set this higher?
                              expand = expansion(mult = 0, add = 0),
                              #keep tight to margins; add to keep title underscores off 0m depth lines. But sticks little lines out
                              # Fix this by removing the title and instead generating it within the multiplot?
                              breaks = depthbreaks,
                              minor_breaks = NULL,
                              labels = depthbreakslabels,
                              sec.axis = sec_axis(~ (. / depthtempconversion) + mintemp, # . / depthtempconversion
                                                  # scale temp to depth range then add back mintemp to correct for earlier zeroing
                                                  name = "Temperature (°C)",
                                                  breaks = seq(from = 0, to = maxtemp, by = 5))) +
           labs(y = "Depth (m)",
                x = NULL,
                title = paste0(first(df_i$fishID), "      ", .$Date)) +
           theme_minimal() %+replace% theme(axis.title.y.right = element_text(angle = 90),
                                            plot.title = element_text(hjust = 0.5,
                                                                      size = 13),
                                            axis.text.x = element_text(size = rel(1.2)),
                                            axis.text.y = element_text(size = rel(1.2)),
                                            axis.title.x = element_text(size = rel(1.2)),
                                            axis.title.y = element_text(size = rel(1.2),
                                                                        angle = 90),  #overwriting size screws elements
                                            legend.position = c(0.5, 0.01), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
                                            legend.direction = "horizontal",
                                            legend.spacing.x = unit(0.1, 'cm'), #compress spacing between legend items, 0 is min
                                            legend.text = element_text(size = rel(1.2)),
                                            panel.grid.major  = element_line(size = rel(0.5))) +
           scale_colour_manual(name = NULL,
                               values = mycols,
                               guide = guide_legend(override.aes = aes(fill = NA),
                                                    nrow = 1)) # can't change linetype, is fine
      ) # close do

    # plot save loop####
    print(paste0("saving daily png plots for ", str_remove(i, whichfiles)))
    if (nrow(p1) > 1) {pb <- txtProgressBar(min = 1, max = nrow(p1), style = 3)} # create progress bar unless it's only 1 day
    for (j in 1:nrow(p1)) { #length of p1 & p2 need to be the same each time. Should be, both grouped on Date
      if (nrow(p1) > 1) {setTxtProgressBar(pb, j)} # update progress bar
      ggsave(file = paste0(saveloc, str_remove(i, whichfiles), "/", str_remove(i, ".csv"), "_", p1$Date[j], ".png"), #plot_df$Date[j]
             plot = arrangeGrob(p1$Temp[[j]]),
             device = "png",
             path = "./", # added 2021-10-06 after it worked fine the first time
             scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
             width = 10, #NA default. Manually adjust plot box in RStudio after ggplot() 6.32
             height = 7.3, #NA default; Then ggsave with defaults, changes from 7x7" to e.g. 4
             units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
             dpi = 90, #originally 300, tried 72, 90 is a picturesize vs filesize tradeoff sweetspot
             limitsize = TRUE)
    }
    if (nrow(p1) > 1) {close(pb)} # close for j save loop & progress bar
    # us letter: 8.5 by 11 inches (215.9 by 279.4 mm), A4 8.3 x 11.7in, 210 x 297mm, 1" margin: 7.5 x 10 & 7.3 x 10.7. Min: 7.3 x 10

  } #close i / for foreach

  # rm(list = ls()) #remove all objects
  # beep(8) #notify completion
  # invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
  # gc() #garbage collection, free up memory
} # close function

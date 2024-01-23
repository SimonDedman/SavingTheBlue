# 2021-06-09 Reef shark drumline analysis
# Simon Dedman simondedman@gmail.com
# With Tristan Guttridge & Phil Matich

# Setup, load, prep data####
library(magrittr) # %<>%
library(dplyr) # %>% matches last filter mutate case_when relocate everything rename across last_col bind_cols group_by tally pull summarise n_distinct left_join arrange select bind_rows # %>% matches last filter mutate case_when relocate everything rename across last_col bind_cols group_by tally pull summarise n_distinct left_join arrange select bind_rows
library(lubridate) # minute yday month is.POSIXt today
library(tidyverse) # "No used functions found" # "No used functions found"
library(tidylog) # filter mutate relocate pivot_wider rename replace_na group_by tally summarise left_join drop_na select pivot_longer # filter mutate relocate pivot_wider rename replace_na group_by tally summarise left_join drop_na select pivot_longer
remotes::install_github("SimonDedman/gbm.auto")
library(gbm.auto)
source("~/Dropbox/Galway/Analysis/R/My Misc Scripts/lognegs.R")

# READ THIS!!
# Run processing runall to ~L338, then:
# include distance to dropoff as independent variable:
# -Presumably this will correlate with depth and potentially longitude (and maybe latitude)
# -include variable(s) that are most influential (hopefully distance to dropoff based on the work you've been doing to get this data)
# /home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bathymetry/2023-02-13 Bathy process.txt bottom section
# Then see bathy.R, run 2023-02-16 update section


{ # processing runall
  # read in data
  # source('R/01_data-import.R') # run data import if you changed the database.
  drumline <- list.files(path = "../../Data/") %>% # list all files in Data folder
    .[matches("_drumline_data.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date # _dropoff
    last() # last one is highest date i.e. latest
  drumline <- readRDS(file = paste0("../../Data/", drumline))

  # Response data####
  # shark CPUE on drumline deployments. Common name vs NA, per adjusted soak time

  # unique(drumline$Common)
  # "Caribbean Reef"   "Blacknose"        "Nurse"            "Barracuda"        "Blacktip"         "Tiger"            "Lemon"
  # "Sharpnose"        "Great Hammerhead" "Horse-eye jack"   "Bull"             "Cuberra Snapper"
  # of interest (not not used, could be for a loop or across)
  # sharklist <- c("Caribbean Reef", "Blacknose", "Nurse", "Blacktip", "Tiger", "Lemon", "Sharpnose", "Great Hammerhead", "Bull")

  # Soak time adjustment & CPUE ####
  # Bait present = TRUE: full soak, response = 0. Nothing to do.
  # Bait present = false + species = reef: full soak, response = 1. Nothing to do.
  drumline %<>%
    relocate(Soak_time, .after = everything()) %>% # put Soak_time at end near it's about-to-exist friends
    mutate(
      Soak_time_CaribbeanReef = as.numeric(case_when(
        # Bait present = false + species = not-reef: half soak, response = 0
        Bait_present == FALSE & (Common != "Caribbean Reef" | is.na(Common)) ~ Soak_time / 2,
        # Bait present = false + species = NA + biteoff = false: half soak, response = 0. Covered by above
        # Bait present = false + species = NA + biteoff = true: half soak, response = 0. Covered by above
        TRUE ~ Soak_time)), # else existing soak time, because either the bait was present or it caught the shark species of interest
      Soak_time_Blacknose = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Blacknose" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      Soak_time_Nurse = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Nurse" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      Soak_time_Blacktip = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Blacktip" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      Soak_time_Tiger = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Tiger" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      Soak_time_Lemon = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Lemon" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      Soak_time_Sharpnose = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Sharpnose" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      Soak_time_GreatHammerhead = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Great Hammerhead" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      Soak_time_Bull = as.numeric(case_when(
        Bait_present == FALSE & (Common != "Bull" | is.na(Common)) ~ Soak_time / 2,
        TRUE ~ Soak_time)),
      CommonValues = 1 # for Binomial SpeciesCPUE columns via pivot_wider:
    ) %>%
    pivot_wider(names_from = Common, # Make values 1 when specific shark caught, else 0
                values_from = CommonValues) %>%
    rename(CaribbeanReef = "Caribbean Reef",
           GreatHammerhead = "Great Hammerhead") %>%
    mutate(across("NA":last_col(), ~ replace_na(., 0)), # https://stackoverflow.com/a/63970397/3975144
           CaribbeanReef_CPUE = CaribbeanReef / Soak_time_CaribbeanReef)


  # Fishery / method:
  # Bait_type
  # Bottom_top

  # Subsetting variables: sex, size.

  # Add shark table traits by PIT####
  shark <- list.files(path = "../../Data/") %>% # list all files in Data folder
    .[matches("_shark_capture_data.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date # _dropoff
    last() # last one is highest date i.e. latest
  shark <- readRDS(file = paste0("../../Data/", shark))

  # Initial left_join created 242405 obs = dupes
  # shark 345 obs but 132 unique Caseys
  # length(unique(shark$Casey_Tag_no[!is.na(shark$Casey_Tag_no)])) # 138 non na entries, 131 unique = 7 dupes
  # 398587 398587 398574 398562 398974 398574 406255 (2 doubles, 5 unique tags)

  # # Need to join by PIT not Casey, almost twice as many PITs
  # length(unique(shark$Casey_Tag_no[!is.na(shark$Casey_Tag_no)])) # 131
  # length(unique(shark$PIT_Tag_Full_ID_no[!is.na(shark$PIT_Tag_Full_ID_no)])) # 256
  #
  # tmp <- drumline %>% left_join( # Joining, by = "PIT_Tag_Full_ID_no"
  #   shark %>%
  #     # filter(!Casey_Tag_no %in% badcasey) %>% # exclude bad tags
  #     # arrange(Time) %>% # arrange by datetime
  #     # group_by(Casey_Tag_no) %>%
  #     # rename(Casey = Casey_Tag_no) %>% # for left join
  #     select(PIT_Tag_Full_ID_no, Sex, Mature, PCL, FL, TL, STL, Girth)
  # ) # initially creates 68249 rows
  #
  # length(unique(shark$PIT_Tag_Full_ID_no[!is.na(shark$PIT_Tag_Full_ID_no)])) # 138 non na entries, 131 unique = 7 dupes

  dupePITs <- shark %>%     # create dupePIT list
    filter(!is.na(PIT_Tag_Full_ID_no)) %>% # remove nas
    group_by(PIT_Tag_Full_ID_no) %>%
    tally() %>% # number of rows per group
    filter(n > 1) %>% # remove 1s leaving dupes
    pull(PIT_Tag_Full_ID_no) # length 34, all unique

  # vector of same PITs, different Common
  badPITs <- shark %>%
    filter(PIT_Tag_Full_ID_no %in% dupePITs) %>%
    group_by(PIT_Tag_Full_ID_no) %>%
    summarise(howmany = n_distinct(Common)) %>%
    filter(howmany > 1) %>%
    pull(PIT_Tag_Full_ID_no) # length 6


  # noNA PIT shark table. DeDupes noNA PIT sharks. Ignore warnings
  noNApitShark <- shark %>% # was 360 now 315 after Gear filter, 100 after Common, 93 after removing polyball
    # exclude bad tags with same PIT for 2 different species.
    filter(!PIT_Tag_Full_ID_no %in% badPITs,
           # only include drumline sampling & Block-rig
           # see next block also
           Gear %in% c("Drumline-bottom",
                       "Drumline-top",
                       "Block-rig")
           # ,
           # Common == "Caribbean Reef"
    ) %>%
    # drop_na, else joins NA pits in shark w all NAs pits in drumline, populates same values throughout.
    # but means sacrificing sharks with no PIT tags which is a waste.
    # 2-stage: dedupe nona PITs here, then create Na PITs below, then rbind, then leftjoin
    drop_na(PIT_Tag_Full_ID_no) %>%
    arrange(Time) %>% # arrange by datetime
    select(Date, PIT_Tag_Full_ID_no, Sex, Mature, PCL, FL, TL, STL, Girth) %>% # remove unneeded cols
    mutate(across(where(is.factor), as.character)) %>% # else breaks the NA fixes below
    # group & summarise to collapse dupe PITs
    group_by(Date, PIT_Tag_Full_ID_no) %>%
    # # across numericals want max (na rm = T) else last (na rm = T)
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), # The `...` argument of `across()` is deprecated as of dplyr 1.1.0
              across(where(~ is.character(.) | is.POSIXt(.) | is.logical(.) | is.Date(.)), last)) %>%
    # This means a recaptured shark ON THE SAME DAY will have the mean length values
    # convert -Inf (from max/last of 2+ NAs) to NA
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
           across(where(~ is.character(.)), ~ ifelse(is.infinite(.), NA, .))) %>% # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
    mutate(Sex = factor(Sex, levels = c("F", "M")))

  # add noNApitShark (shark table) traits to drumline
  drumline %<>% left_join(noNApitShark %>%
                            select(Date, PIT_Tag_Full_ID_no, Sex, Mature, PCL, FL, TL, STL, Girth),
                          by = c("Date", "PIT_Tag_Full_ID_no"))
  rm(noNApitShark)
  # Leaves NA PIT individuals in drumline & sharks unmatched, so no traits from sharks added to drumline sharks


  # Add data by Casey####
  # Then: are there any instances where there are no pit tag match but there are casey matches?
  # Yes, e.g 398538
  # Both have casey & !(shark has PIT & drumline has PIT)
  bothdbs <- bind_rows(
    shark %>%
      filter(!is.na(Casey_Tag_no)) %>% # Both have casey
      rename(Casey = Casey_Tag_no) %>%
      select(PIT_Tag_Full_ID_no, Casey) %>%
      mutate(dbid = "shark"),
    drumline %>%
      filter(!is.na(Casey)) %>% # Both have casey
      select(PIT_Tag_Full_ID_no, Casey) %>%
      mutate(dbid = "drumline")
  )

  caseybothdbs <- bothdbs %>%
    group_by(Casey) %>%
    summarise(howmany = n_distinct(dbid)) %>%
    filter(howmany > 1) %>%
    pull(Casey)
  # & !(shark has PIT & drumline has PIT)

  caseybothdbsnopit <- bothdbs %>%
    filter(Casey %in% caseybothdbs) %>%
    arrange(Casey) %>%
    group_by(Casey) %>%
    summarise(nalength = length(which(is.na(PIT_Tag_Full_ID_no)))) %>% # remove when PIT not present in both
    filter(nalength > 1) %>%
    pull(Casey) # "406260" "406261" "406262" "406263" # 2023-02-15 "398952"

  # Can join on these Caseys. But will wipe out everything else. Unless dplyr has fixed that?

  # can use base R index replacement
  caseyjoinimport <- as.data.frame(shark %>%
                                     rename(Casey = Casey_Tag_no) %>%
                                     filter(Casey %in% caseybothdbsnopit) %>%
                                     select(Casey, Sex, Mature, PCL, FL, TL, STL, Girth) %>%
                                     mutate(across(where(is.factor), as.character)) # fixes Sex
                                   # %>% arrange(Casey) # means index join below silently puts data in the wrong place
  )

  drumline[which(drumline$Casey %in% caseybothdbsnopit), # rows indexed by Casey column logical check
           c("Casey", "Sex", "Mature", "PCL", "FL", "TL", "STL", "Girth")] <- caseyjoinimport # send data to these columns for those rows. Data sent: that table. Adds 2 STLs @ 2023-12-18

  # dupecaseys <- shark %>% # create dupecasey list
  #   filter(!is.na(Casey_Tag_no)) %>% # remove nas
  #   group_by(Casey_Tag_no) %>%
  #   tally() %>% # number of rows per group
  #   filter(n > 1) %>% # remove 1s leaving dupes
  #   pull(Casey_Tag_no)
  #
  # # If casey & pit are the same
  # badcasey <- shark %>%
  #   filter(Casey_Tag_no %in% dupecaseys) %>%
  #   group_by(Casey_Tag_no) %>%
  #   summarise(howmany = n_distinct(PIT_Tag_Full_ID_no)) %>% # >1 = different PITs same casey. Which we want to exclude the casey number of
  #   filter(howmany > 1) %>%
  #   pull(Casey_Tag_no)
  # # then arrange by datetime & summarise the last value of Sex, Mature, PCL, FL, TL, STL, Girth
  #
  # drumline %<>% left_join(
  #   shark %<>%
  #     filter(!Casey_Tag_no %in% badcasey) %>% # exclude bad tags
  #     arrange(Time) %>% # arrange by datetime
  #     group_by(Casey_Tag_no) %>%
  #     rename(Casey = Casey_Tag_no) %>% # for left join
  #     select(Casey, Sex, Mature, PCL, FL, TL, STL, Girth) %>% # remove unneeded cols
  #     mutate(Sex = as.character(Sex)) %>%
  #     # summarise_all(last) # across numericals want max (na rm = T) else last (na rm = T)
  #     summarise(across(where(is.numeric), \(x) max(x, na.rm = TRUE)),
  #               across(where(~ is.character(.) | is.POSIXt(.)), last)) %>%
  #     # convert -Inf (from max/last of 2+ NAs) to NA
  #     mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
  #            across(where(~ is.character(.)), ~ ifelse(is.infinite(.), NA, .))) # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
  # ) # Joining, by = "Casey"






  # Add shark table traits, no PIT####
  # NA PIT shark table
  NApitShark <- shark %>% # was 95 now 89 after gear filter, 14 after Common filter, 14 after removing polyball, 12 after removing NA traits
    filter(!PIT_Tag_Full_ID_no %in% badPITs,
           is.na(PIT_Tag_Full_ID_no),
           Gear %in% c("Drumline-bottom",
                       "Drumline-top",
                       "Block-rig"),
           Common == "Caribbean Reef") %>%
    # remove rows with NA for all traits: useless as won't add anything even if joined
    rowwise() %>%
    mutate(natraits = ifelse(all(is.na(c(Sex, Mature, PCL, FL, TL, STL, Girth))), NA, 1)) %>%
    drop_na(natraits) %>%
    select(-natraits)
  # select(Sex, Mature, PCL, FL, TL, STL, Girth)


  # Date: if there's only 1 in both (NApitShark and drumline) then join
  # get only n=1 date rows from NA PIT shark
  NApitSharkOneDate <- NApitShark %>%
    group_by(Date) %>%
    summarise(across(everything(), first),
              n = n()) %>%
    filter(n == 1) %>%
    select(-n)

  # subset remaining candidate matches based on species & date overlap, includes dupe date drumline hits
  drumlineSameDates <- drumline %>%
    filter(CaribbeanReef == 1,
           Date %in% NApitSharkOneDate$Date)

  # get vector of unique dates in drumline which overlap with unique dates in shark and both have Caribbean Reefs
  drumlineSameDatesUnique <- drumlineSameDates %>%
    group_by(Date) %>%
    summarise(Date = first(Date),
              n = n()) %>%
    filter(n == 1) %>%
    select(-n) %>%
    pull(Date)

  # use above vector as base R row index on drumline$Date to add shark traits data
  drumline[which(drumline$Date %in% drumlineSameDatesUnique & drumline$CaribbeanReef == 1), # rows
           # columns are only the traits we want
           c("Sex", "Mature", "PCL", "FL", "TL", "STL", "Girth")] <- NApitSharkOneDate %>%
    filter(Date %in% drumlineSameDatesUnique) %>%
    select(Sex, Mature, PCL, FL, TL, STL, Girth) # adds 3 STLs @ 2023-12-18

  # Remove the previously processed fish above, retains those already joined with Casey
  NApitSharkToDo <- NApitShark %>%
    filter(!Date %in% drumlineSameDatesUnique)

  # index drumline with this then filter drumline for isna PIT & isna casey which we can't do in shark
  mergedates <- drumline %>%
    filter(Date %in% unique(NApitSharkToDo$Date),
           CaribbeanReef == 1,
           is.na(PIT_Tag_Full_ID_no),
           is.na(Casey),
           !Bite_off) %>%
    pull(Date)
  # this created an extra 2021-03-04: bite-off in drumline not present in shark
  # hook numbers don't match: shark 4 5 2 2, drumline 4 1 4 5 2 (i.e. no second 2 in drumline, instead 1 & 4)
  # added !Bite_off above
  # why isn't that extra shark in the drumline database to be joined to though?
  # don't remove bite-off and let it join normally, adding no data for that shark?
  # because there's no data TO join, this row doesn't exist in the shark table, so remove the row as a merge option option from drumline

  # feed those dates back into shark to get a same-nrow table of traits to feed into drumline
  drumline[which(drumline$Date %in% unique(NApitSharkToDo$Date) &
                   drumline$CaribbeanReef == 1 &
                   is.na(drumline$PIT_Tag_Full_ID_no) &
                   is.na(drumline$Casey) &
                   !drumline$Bite_off), # rows
           c("Sex", "Mature", "PCL", "FL", "TL", "STL", "Girth")] <- NApitSharkToDo %>%
    filter(Date %in% mergedates) %>%
    select(Sex, Mature, PCL, FL, TL, STL, Girth) # adds 7 STLs @ 2023-12-18

  drumline %<>% droplevels() # R will plot all levels even if not populated. This drops unpopulated factor levels

  # Save sheets####
  write.csv(x = drumline,
            file = paste0("../../Data/", today(), "_drumline_reefs.csv"),
            row.names = F)
  saveRDS(object = drumline,
          file = paste0("../../Data/", today(), "_drumline_reefs.rds"))
  rm(list = c("bothdbs", "caseyjoinimport", "badPITs", "caseybothdbs", "caseybothdbsnopit", "dupePITs", "shark", "drumlineSameDates", "NApitShark", "NApitSharkOneDate", "NApitSharkToDo", "drumlineSameDatesUnique", "mergedates"))
} # finish processing runall

# NOW: include distance to dropoff as independent variable:
# -Presumably this will correlate with depth and potentially longitude (and maybe latitude)
# -include variable(s) that are most influential (hopefully distance to dropoff based on the work you've been doing to get this data)
# /home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bathymetry/2023-02-13 Bathy process.txt bottom section:

# 1. Open /home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Maps & Surveys/Bathymetry/STB_Bathy.qgz. Layer, add delimited text layer: latest drumline_reefs.csv. Check outliers.
# /home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Data/2023-02-14_drumline_reefs_getDistToDropoff.csv
# Add as delimited text layer into STB_Bathy qgis.
# 1.5 check outliers; label by ID to find them in dbase, fix, repeat.
# 2. Processing toolbox, raster analysis, sample raster values. Input: drumline/sharks points just added. Raster: DistanceToDeeps. Rename output column prefix: "DtDeeps_"
# 3. ReRun step2, DtShallows, remove previous Sampled. Rename output column prefix: "DtShallows_"
# 4. Export CSV. Save as /Data/YYYY-MM-DD_drumline_reefs_dropoff.csv
# 5. See bathy.R, section = "2023-02-16 update"

# Then see bathy.R, run 2023-02-16 update section

# drumline <- cbind(drumline, tmp[,c("DeepShallow", "DtDropOff")])
# write.csv(x = drumline,
#           file = paste0("../../Data/", today(), "_drumline_reefs_dropoff.csv"),
#           row.names = F)
# saveRDS(object = drumline,
#         file = paste0("../../Data/", today(), "_drumline_reefs_dropoff.rds"))


# LOAD DATA ####
# read in saved data from above
drumline <- list.files(path = "../../Data/") %>% # list all files in Data folder
  .[matches("_drumline_reefs_dropoff.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
  last() # last one is highest date i.e. latest
drumline <- readRDS(file = paste0("../../Data/", drumline))
# drumline %<>% tidyr::drop_na(CaribbeanReef_CPUE)
# use drumline data to explore CPUE in different habitats, sex and size segregation / overlap.
# Possible seasonal differences in catch rates or habitats caught in due to temp etc?

unique(drumline$Site3)
# Bigwood Channel     Bight               AUTEC Channel       Bristol Galley      Green Cay           Central Cays East   Central Andros East
# Levels: Central Andros East Green Cay Bristol Galley AUTEC Channel Bight Bigwood Channel Central Cays East

# # 2021-09-08 PM remove Somerset suggestion
# drumline %<>%
#   filter(Site3 != "Somerset") %>% #remove Somerset rows, n=103, 9% of data
#   mutate(Site3 = factor(Site3, levels = levels(Site3)[2:6])) %>% # remove Somerset as a factor level
#   tidyr::drop_na(CaribbeanReef_CPUE)
unique(drumline$CaribbeanReef_CPUE)

colnames(drumline)
# Explanatory data:
# habitat variables:
# Site3 - factor
# Habitat - factor
# Substrate - factor [15 factors, could reduce]
# Latitude
# Longitude
# Depth_m
## distance to shore

# oceanographics:
# Temperature_C
# Salinity_ppt
# DO_mg_L
# Tide - factor

# Temporal:
# Minute
# Yearday
# Month
# Season - factor
# daylength
# LunarPhase - factor

# To Get:
# chl?
# any other satellite stuff that's easy to grab? NO, all too coarse spatial res & not spatially dynamic at the static sites.






# Bar plots####
# Loop through factorial variables & barplot against CPUE
for (factorvars in c("Site3", "Habitat", "Substrate", "Substrate2", "Tide", "Season", "LunarPhase")) {
  drumlineEdits <- drumline
  if (factorvars == "Habitat") drumlineEdits <- drumline |> filter(Habitat != "Channel") |> tidyr::drop_na(CaribbeanReef_CPUE) # Too few entries, 93 rows
  if (factorvars == "Site3") drumlineEdits <- drumline |> filter(Site3 != "Central Cays East") |> tidyr::drop_na(CaribbeanReef_CPUE) # Too few entries, 87 rows
  if (factorvars == "Substrate2") drumlineEdits <- drumline |> tidyr::drop_na(Substrate2) |> tidyr::drop_na(CaribbeanReef_CPUE) # Too few entries, 87 rows
  ggplot(data = drumlineEdits) +
    geom_col(mapping = aes(x = .data[[factorvars]], y = CaribbeanReef_CPUE), fill = "black", colour = "black") +
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                     title = element_text(size = rel(2)),
                                     legend.text = element_text(size = rel(1.5)),
                                     legend.position = c(0.03, 0.98),
                                     legend.direction = "horizontal",
                                     legend.title = element_blank(),
                                     panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                     plot.background = element_rect(fill = "white"),
                                     strip.text.x = element_text(size = rel(2)),
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1))
  ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/", today(), "_ColPlot_CPUE_", factorvars, ".png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 7,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)
}








# Scatterplots####
# Loop through numerical variables & scatterplot against CPUE with trendline
for (numvars in c("Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")) {
  ggplot(data = drumline |> filter(CaribbeanReef_CPUE != 0) |> tidyr::drop_na(CaribbeanReef_CPUE)) +
    geom_point(mapping = aes(x = .data[[numvars]], y = CaribbeanReef_CPUE), fill = "black") +
    geom_smooth(mapping = aes(x = .data[[numvars]], y = CaribbeanReef_CPUE)) + # , fill = "black"
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                     title = element_text(size = rel(2)),
                                     legend.text = element_text(size = rel(1.5)),
                                     legend.position = c(0.03, 0.98),
                                     legend.direction = "horizontal",
                                     legend.title = element_blank(),
                                     panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                     plot.background = element_rect(fill = "white"),
                                     strip.text.x = element_text(size = rel(2)),
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1))
  ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/", today(), "_ScatterPlot_CPUE_", numvars, ".png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 7,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)
}






# 2023-05-30 LMplots####
expvars = c(
  # "Site3",
  "Habitat", # Site3 Habitat Substrate2 all similar
  # "Substrate2",
  "Tide",
  "Season",
  "LunarPhase",
  # "Latitude",
  # "Longitude",
  "Depth_m",
  "Temperature_C",
  # "Salinity",
  # "DO_mg_L",
  # "Hour",
  "Yearday",
  # "Month",
  # "daylength" # Season better
  # "Sex",
  # "STL",
  # "RandomVar"
  "DtDropOff"
)
gbm.lmplots(
  samples = as.data.frame(drumline |> filter(CaribbeanReef_CPUE != 0) |> tidyr::drop_na(CaribbeanReef_CPUE) |> select(CaribbeanReef_CPUE, all_of(expvars))),
  expvar = expvars,
  # expvar = c("Habitat", "Tide", "LunarPhase", "Depth_m", "Temperature_C", "Yearday", "DtDropOffLogNeg"),
  # expvar = c("DtDropOffLogNeg"),
  resvar = "CaribbeanReef_CPUE",
  # expvarnames = NULL,
  # resvarname = NULL,
  savedir = paste0("../../Projects/2021-10_Drumline_Reefshark/LMplots PairPlots/")
  # plotname = NULL,
  # pngtype = c("cairo-png", "quartz", "Xlib"),
  # r2line = TRUE,
  # pointtext = FALSE,
  # pointlabs = resvar,
  # pointcol = "black",
  # ...
)
# BUG####
# does Habitat (expvar 1) then NA.png & none more.
# forced by typing em all out & deleting one by one from the front.
# something not closing properly after the first one is saved?
# 2023-12-18 fixed with L65 & 82 in gbm.lmplots?


# 2023-05-30 PairPlots####
source("~/Dropbox/Farallon Institute/FarallonInstitute/R/pairPlots.R")
# expvars = c(
#   # "Site3",
#   # "Habitat", # Site3 Habitat Substrate2 all similar
#   # "Substrate2",
#   # "Tide",
#   # "Season",
#   # "LunarPhase",
#   "Latitude",
#   "Longitude",
#   # "Depth_m",
#   # "Temperature_C",
#   # "Salinity",
#   # "DO_mg_L",
#   # "Hour",
#   # "Yearday",
#   # "Month",
#   # "daylength" # Season better
#   # "Sex",
#   # "STL",
#   # "RandomVar"
#   "DtDropOff"
# )
expvardf <- drumline |> filter(CaribbeanReef_CPUE != 0) |> select(all_of(c("CaribbeanReef_CPUE", expvars))) # select(all_of(expvars))
pairs(expvardf,
      lower.panel = panel.lm,
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      main = "pair plots of variables")
# (manually save)


# Henderson etal 2021 figures ####
options(scipen = 5)
# F3 x seasons y temperatureC dots w/ SDs ####
drumline %>%  # create summary table as data input
  group_by(Season) %>%
  summarise(Temperature = mean(Temperature_C, na.rm = TRUE),
            TempSDmin = mean(Temperature_C, na.rm = TRUE) - sd(Temperature_C, na.rm = TRUE),
            TempSDmax = mean(Temperature_C, na.rm = TRUE) + sd(Temperature_C, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Season,
                           y = Temperature)) +
  geom_errorbar(aes(x = Season,
                    ymin = TempSDmin,
                    ymax = TempSDmax),
                width = .2,
                position = position_dodge(0.05)) +
  ylab("Temperature (C°)") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_DotWhisker_Temp_Season.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)


# F4 x species y CPUE (sharks/hook/hour) columns ####
# warning: these are hardcoded so new sharks have to be coded in
drumline %>%                # create summary table as data input
  summarise(Blacknose = sum(Blacknose, na.rm = T) / sum(Soak_time_Blacknose, na.rm = T),
            Sharpnose = sum(Sharpnose, na.rm = T) / sum(Soak_time_Sharpnose, na.rm = T),
            Blacktip = sum(Blacktip, na.rm = T) / sum(Soak_time_Blacktip, na.rm = T),
            CaribbeanReef = sum(CaribbeanReef, na.rm = T) / sum(Soak_time_CaribbeanReef, na.rm = T),
            Lemon = sum(Lemon, na.rm = T) / sum(Soak_time_Lemon, na.rm = T),
            Nurse = sum(Nurse, na.rm = T) / sum(Soak_time_Nurse, na.rm = T),
            Tiger = sum(Tiger, na.rm = T) / sum(Soak_time_Tiger, na.rm = T),
            Bull = sum(Bull, na.rm = T) / sum(Soak_time_Bull, na.rm = T),
            GreatHammerhead = sum(GreatHammerhead, na.rm = T) / sum(Soak_time_GreatHammerhead, na.rm = T)
  ) %>%
  pivot_longer(cols = everything(), # turn into normal/proper long dataset
               names_to = "Species",
               values_to = "CPUE") %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(Species, -CPUE), # reorder largest to smallest
                         y = CPUE), fill = "black") +
  ylab("CPUE (sharks/minute)") +
  xlab("Species") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_Column_CPUE_Species.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)


# F5 x species y CPUE (sharks/hook/hour) site-shaped dots ####
drumline %>%                # create summary table as data input
  group_by(Habitat2) %>%
  summarise(Blacknose = sum(Blacknose, na.rm = T) / sum(Soak_time_Blacknose, na.rm = T),
            Sharpnose = sum(Sharpnose, na.rm = T) / sum(Soak_time_Sharpnose, na.rm = T),
            Blacktip = sum(Blacktip, na.rm = T) / sum(Soak_time_Blacktip, na.rm = T),
            CaribbeanReef = sum(CaribbeanReef, na.rm = T) / sum(Soak_time_CaribbeanReef, na.rm = T),
            Lemon = sum(Lemon, na.rm = T) / sum(Soak_time_Lemon, na.rm = T),
            Nurse = sum(Nurse, na.rm = T) / sum(Soak_time_Nurse, na.rm = T),
            Tiger = sum(Tiger, na.rm = T) / sum(Soak_time_Tiger, na.rm = T),
            Bull = sum(Bull, na.rm = T) / sum(Soak_time_Bull, na.rm = T),
            GreatHammerhead = sum(GreatHammerhead, na.rm = T) / sum(Soak_time_GreatHammerhead, na.rm = T)
  ) %>%
  pivot_longer(cols = Blacknose:last_col(), # turn into normal/proper long dataset
               names_to = "Species",
               values_to = "CPUE") %>%
  ggplot() +
  geom_point(mapping = aes(x = reorder(Species, -CPUE), # reorder largest to smallest
                           y = CPUE,
                           shape = factor(Habitat2, levels = c("Flats", "Fore reef", "Back reef"))),
             size = 5) +
  scale_shape_manual(values = c(0, 2, 6)) +
  ylab("CPUE (sharks/minute)") +
  xlab("Species") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.9, 0.9),
                                   legend.direction = "vertical",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_DotPlot_CPUE_Species_Habitat2.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)

# F6 x sex y TL facet species boxplots ####
drumline %>%                # create summary table as data input
  select(Species, Sex, STL) %>%
  drop_na() %>% # Sex, Species, STL
  group_by(Species) %>%
  filter(length(unique(Sex)) > 1) %>% # must have at least 1 of both species
  ggplot() +
  geom_boxplot(mapping = aes(x = Sex,
                             y = STL)) +
  ylab("Stretched total length (cm)") +
  facet_wrap(.~ Species,
             scales = "free") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_BoxPlot_STL_Sex_facetSpecies.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)

# F7 x season y CPUE dots by species ####
drumline %>%                # create summary table as data input
  group_by(Season) %>%
  summarise(Blacknose = sum(Blacknose, na.rm = T) / sum(Soak_time_Blacknose, na.rm = T),
            Sharpnose = sum(Sharpnose, na.rm = T) / sum(Soak_time_Sharpnose, na.rm = T),
            Blacktip = sum(Blacktip, na.rm = T) / sum(Soak_time_Blacktip, na.rm = T),
            CaribbeanReef = sum(CaribbeanReef, na.rm = T) / sum(Soak_time_CaribbeanReef, na.rm = T),
            Lemon = sum(Lemon, na.rm = T) / sum(Soak_time_Lemon, na.rm = T),
            Nurse = sum(Nurse, na.rm = T) / sum(Soak_time_Nurse, na.rm = T),
            Tiger = sum(Tiger, na.rm = T) / sum(Soak_time_Tiger, na.rm = T),
            Bull = sum(Bull, na.rm = T) / sum(Soak_time_Bull, na.rm = T),
            GreatHammerhead = sum(GreatHammerhead, na.rm = T) / sum(Soak_time_GreatHammerhead, na.rm = T)
  ) %>%
  pivot_longer(cols = Blacknose:last_col(), # turn into normal/proper long dataset
               names_to = "Species",
               values_to = "CPUE") %>%
  filter(!Species %in% c("Sharpnose", "Bull", "Lemon")) %>% # 0.0002 max sharpnose, 0.00008 bull, 7 lemon
  # appeared in legend but weren't given icons, scores too low, removed for cleanliness
  # Species manually removed here ####
ggplot() +
  geom_point(mapping = aes(x = Season, # reorder largest to smallest
                           y = CPUE,
                           shape = reorder(factor(Species), -CPUE)), # , levels = c("Flats", "Fore reef", "Back reef")
             size = 5) +
  geom_line(mapping = aes(x = Season, # reorder largest to smallest
                          y = CPUE,
                          group = Species)) +
  scale_shape_manual(values = c(0:6)) +
  ylab("CPUE (sharks/minute)") +
  xlab("Season") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.12, 0.87),
                                   legend.direction = "vertical",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_DotPlot_CPUE_Species_Season.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)


# F8 x season y TL facet species boxplots (see F6) ####
drumline %>%                # create summary table as data input
  select(Species, Season, STL) %>%
  drop_na() %>%
  group_by(Species) %>%
  filter(length(unique(Season)) > 1) %>% # must have at least 1 of both species
  ggplot() +
  geom_boxplot(mapping = aes(x = Season,
                             y = STL)) +
  ylab("Stretched total length (cm)") +
  facet_wrap(.~ Species,
             scales = "free") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_BoxPlot_STL_Season_facetSpecies.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)


# F9 x habitat/substrate y TL facet species boxplots (see F6) ####
unique(drumline$Habitat2) # Fore reef  flats   Back reef

drumline %>%                # create summary table as data input
  select(Species, Habitat2, STL) %>%
  drop_na() %>%
  group_by(Species) %>%
  filter(length(unique(Habitat2)) > 1) %>% # must have at least 1 of both species
  ggplot() +
  geom_boxplot(mapping = aes(x = Habitat2,
                             y = STL)) +
  ylab("Stretched total length (cm)") +
  xlab("Habitat type") +
  facet_wrap(.~ Species,
             scales = "free") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_BoxPlot_STL_Habitat2_facetSpecies.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)

unique(drumline$Substrate2) # Reef       Vegetation Bare       <NA>

drumline %>%                # create summary table as data input
  select(Species, Substrate2, STL) %>%
  drop_na() %>%
  group_by(Species) %>%
  filter(length(unique(Substrate2)) > 1) %>% # must have at least 1 of both species
  ggplot() +
  geom_boxplot(mapping = aes(x = Substrate2,
                             y = STL)) +
  ylab("Stretched total length (cm)") +
  xlab("Substrate type") +
  facet_wrap(.~ Species,
             scales = "free") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1))
ggsave(paste0("../../Projects/2021-10_Drumline_Reefshark/Henderson Plots/", today(), "_BoxPlot_STL_Substrate2_facetSpecies.png"),
       plot = last_plot(), device = "png", scale = 1.75, width = 7,
       height = 4, units = "in", dpi = 300, limitsize = TRUE)

# replace theme_minimal with my own with all the changes I make, to save space.


# Subset categories next steps####
# PM: Something that would be interesting to include are size and sex differences –
# any segregation in distributions based on these factors,
# who do we catch more of, and
# is there any seasonality to juveniles or mature individuals that could be indicative of reproductive behaviour (pupping, migration, etc.).







# BRT ####
# 2021-09-08 PM remove Somerset suggestion
drumline %<>%
  filter(Site3 != "Somerset") %>% #remove Somerset rows, n=103, 9% of data
  mutate(Site3 = factor(Site3, levels = levels(Site3)[2:6])) %>% # remove Somerset as a factor level
  tidyr::drop_na(CaribbeanReef_CPUE)

# Fran Farabaugh idea, add random var to compare to BRT outputs
# 2023-02-15 is this now done in gbm.auto as an option
# drumline$RandomVar <- runif(n = nrow(drumline), min = 0, max = 1)

drumline <- as.data.frame(drumline) # fails if tibble

# library(remotes) # install_github # install_github
# install_github("SimonDedman/gbm.auto")
library(gbm.auto) # gbm.bfcheck gbm.auto gbm.loop # gbm.bfcheck gbm.auto gbm.loop
# expvars = c("Site3", "Habitat", "Substrate", "Tide", "Season", "LunarPhase",
#             "Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")
# 2021-09-08 PM Substrate2 suggestion
# drumline %>% select(DO_mg_L, Salinity) %>% summarise_all(funs(sum(is.na(.) / n() * 100))) # 58 & 54% NA, drop, 2022-09-14

expvars = c(
  # "Site3",
  "Habitat", # Site3 Habitat Substrate2 all similar
  # "Substrate2",
  "Tide",
  "Season",
  "LunarPhase",
  # "Latitude",
  # "Longitude",
  "Depth_m",
  "Temperature_C",
  # "Salinity",
  # "DO_mg_L",
  # "Hour",
  "Yearday",
  # "Month",
  # "daylength" # Season better
  # "Sex",
  # "STL",
  # "RandomVar"
  "DtDropOff"
)
gbm.bfcheck(samples = drumline, resvar = "CaribbeanReef")
# "  binary bag fraction must be at least 0.007. n = 3053"
# "Gaussian bag fraction must be at least 0.186. n = 113"
# 0.00687848 0.18584071

(length(which(drumline$CaribbeanReef_CPUE != 0)) / nrow(drumline)) * 100 # 3.7% data presence rows. Have to run bin only, no chance for gaus? 113 rows tho...
gbm.auto(
  grids = NULL,
  samples = drumline,       # [-which(is.na(drumline[resvar])),]
  expvar = expvars,
  resvar = "CaribbeanReef_CPUE",
  randomvar = FALSE,
  tc = c(2,3),
  lr = list(0.05, 0.001), # 0.005
  bf = list(0.6, 0.88),
  n.trees = 50,
  ZI = TRUE,
  fam1 = "bernoulli",
  # fam2 = c("gaussian", "bernoulli", "binomial", "poisson", "laplace"),
  simp = TRUE,
  gridslat = 10,
  gridslon = 11,
  # samplesGridsAreaScaleFactor = 1,
  # multiplot = TRUE,
  # cols = grey.colors(1, 1, 1),
  # linesfiles = TRUE,
  smooth = TRUE, # FALSE
  savedir = "../../Projects/2021-10_Drumline_Reefshark/BRT",
  # savegbm = FALSE,
  # loadgbm = NULL,
  # varint = TRUE,
  # map = TRUE,
  # shape = NULL,
  # RSB = TRUE,
  BnW = FALSE,
  alerts = FALSE,
  # pngtype = c("cairo-png", "quartz", "Xlib"),
  # gaus = TRUE,
  # MLEvaluate = TRUE
)


# Sex maturity size combos####
# Assess sex, maturity and size groups with potential combinations:
# --Male
# --Female
# --Mature
# --Immature
# --Male immature, male mature
# --Female immature, female mature
# --All sharks >164 cm, all sharks <165 cm
# --Male >164 cm, male <165 cm (this cutoff is based on the largest immature male)
# --Female >164, female <165 (based on size of males - ecologically we would expect similar behavior/distribution if size not maturity is mature factor)

# Dataset isn't only positive reefsharks, is also zero catch.
drumlist <- list() # # make these combos into a vector of filters so I can run them all in a loop
drumlinefilt <- drumline %>% select(CaribbeanReef_CPUE, CaribbeanReef, Sex, Mature, STL, all_of(expvars)) %>% filter(!is.na(CaribbeanReef_CPUE))
drumlist[[1]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "M")) # 1911; 2023-05-18 update: 2343
drumlist[[2]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "F")) # 1897: 2331
drumlist[[3]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Mature == 1)) # 1905: 2339
drumlist[[4]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Mature == 0)) # 1898: 2328
drumlist[[5]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "M" & Mature == 1)) # 1894: 2327
drumlist[[6]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "M" & Mature == 0)) # 1873: 2304
drumlist[[7]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "F" & Mature == 1)) # 1869: 2304
drumlist[[8]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "F" & Mature == 0)) # 1881: 2314
drumlist[[9]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & STL <= 165)) # 1909: 2341
drumlist[[10]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & STL > 165)) # 1899: 2333
drumlist[[11]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "M" & STL <= 165)) # 1886: 2319
drumlist[[12]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "M" & STL > 165)) # 1881: 2314
drumlist[[13]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "F" & STL <= 165)) # 1879: 2312
drumlist[[14]] <- drumlinefilt %>% filter(CaribbeanReef == 0 | (CaribbeanReef == 1 & Sex == "F" & STL > 165)) # 1876: 2311
# create list of tc lr bf values to tweak them on a per-loop basis as needed
tclist <- list()
lrlist <- list()
bflist <- list()
tclist[[1]] <- 7
tclist[[2]] <- 7
tclist[[3]] <- 7
tclist[[4]] <- 7
tclist[[5]] <- 7
tclist[[6]] <- 7
tclist[[7]] <- 7
tclist[[8]] <- 7
tclist[[9]] <- 7
tclist[[10]] <- 7
tclist[[11]] <- 7
tclist[[12]] <- 1
tclist[[13]] <- 7
tclist[[14]] <- 7
lrlist[[1]] <- 0.002
lrlist[[2]] <- 0.002
lrlist[[3]] <- 0.002
lrlist[[4]] <- 0.002
lrlist[[5]] <- 0.002
lrlist[[6]] <- 0.002
lrlist[[7]] <- 0.001
lrlist[[8]] <- 0.002
lrlist[[9]] <- 0.002
lrlist[[10]] <- 0.002
lrlist[[11]] <- 0.002
lrlist[[12]] <- 0.00001
lrlist[[13]] <- 0.002
lrlist[[14]] <- 0.002
bflist[[1]] <- 0.8
bflist[[2]] <- 0.8
bflist[[3]] <- 0.8
bflist[[4]] <- 0.8
bflist[[5]] <- 0.8
bflist[[6]] <- 0.8
bflist[[7]] <- 0.8
bflist[[8]] <- 0.8
bflist[[9]] <- 0.8
bflist[[10]] <- 0.8
bflist[[11]] <- 0.8
bflist[[12]] <- 0.92
bflist[[13]] <- 0.8
bflist[[14]] <- 0.8

# for (i in 1:length(drumlist)) {
for (i in 1:length(drumlist)) {
  dir.create(paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", i, "/"))
  print(paste0("running loop ", i, " of 14"))
  print(gbm.bfcheck(samples = as.data.frame(drumlist[[i]]), resvar = "CaribbeanReef_CPUE"))
  gbm.auto(
    grids = NULL,
    samples = as.data.frame(drumlist[[i]]),       # [-which(is.na(drumline[resvar])),]
    expvar = expvars,
    resvar = "CaribbeanReef_CPUE",
    tc = tclist[[i]],
    lr = lrlist[[i]],
    bf = bflist[[i]],
    n.trees = 50,
    fam1 = "bernoulli",
    randomvar = TRUE,
    simp = FALSE,
    smooth = TRUE, # FALSE
    savedir = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", i, "/"),
    savegbm = FALSE,
    gaus = FALSE,
    alerts = FALSE,
    MLEvaluate = TRUE)
}


# 2023-05-19 make results table, scrape folders for results, rerun only best
resultstbl <- data.frame(combo = c("SexM",
                                   "SexF",
                                   "MatureTRUE",
                                   "MatureFALSE",
                                   "SexM_MatureTRUE",
                                   "SexM_MatureFALSE",
                                   "SexF_MatureTRUE",
                                   "SexF_MatureFALSE",
                                   "STL-lessEql-165",
                                   "STL-grtr-165",
                                   "SexM_STL-lessEql-165",
                                   "SexM_STL-grtr-165",
                                   "SexF_STL-lessEql-165",
                                   "SexF_STL-grtr-165"),
                         nsamples = c(nrow(drumlist[[1]]),
                                      nrow(drumlist[[2]]),
                                      nrow(drumlist[[3]]),
                                      nrow(drumlist[[4]]),
                                      nrow(drumlist[[5]]),
                                      nrow(drumlist[[6]]),
                                      nrow(drumlist[[7]]),
                                      nrow(drumlist[[8]]),
                                      nrow(drumlist[[9]]),
                                      nrow(drumlist[[10]]),
                                      nrow(drumlist[[11]]),
                                      nrow(drumlist[[12]]),
                                      nrow(drumlist[[13]]),
                                      nrow(drumlist[[14]])),
                         besttc = c(tclist[[1]],
                                    tclist[[2]],
                                    tclist[[3]],
                                    tclist[[4]],
                                    tclist[[5]],
                                    tclist[[6]],
                                    tclist[[7]],
                                    tclist[[8]],
                                    tclist[[9]],
                                    tclist[[10]],
                                    tclist[[11]],
                                    tclist[[12]],
                                    tclist[[13]],
                                    tclist[[14]]),
                         bestlr =  c(lrlist[[1]],
                                     lrlist[[2]],
                                     lrlist[[3]],
                                     lrlist[[4]],
                                     lrlist[[5]],
                                     lrlist[[6]],
                                     lrlist[[7]],
                                     lrlist[[8]],
                                     lrlist[[9]],
                                     lrlist[[10]],
                                     lrlist[[11]],
                                     lrlist[[12]],
                                     lrlist[[13]],
                                     lrlist[[14]]),
                         bestbf = c(bflist[[1]],
                                    bflist[[2]],
                                    bflist[[3]],
                                    bflist[[4]],
                                    bflist[[5]],
                                    bflist[[6]],
                                    bflist[[7]],
                                    bflist[[8]],
                                    bflist[[9]],
                                    bflist[[10]],
                                    bflist[[11]],
                                    bflist[[12]],
                                    bflist[[13]],
                                    bflist[[14]]),
                         trees = as.numeric(rep(NA, 14)),
                         TrainingDataCorrelation = as.numeric(rep(NA, 14)),
                         TrainingDataAUCscore = as.numeric(rep(NA, 14)),
                         CVAUCscore = as.numeric(rep(NA, 14)),
                         CVAUCse = as.numeric(rep(NA, 14)),
                         Overfitting = as.numeric(rep(NA, 14)),
                         CVMeanDeviance = as.numeric(rep(NA, 14)),
                         CVDevianceSE = as.numeric(rep(NA, 14)),
                         CVDsquared = as.numeric(rep(NA, 14)),
                         CVMeanCorrelation = as.numeric(rep(NA, 14)),
                         CVCorrelationSE = as.numeric(rep(NA, 14)),
                         CVRMSE = as.numeric(rep(NA, 14)),
                         DevExplRelNullTrain = as.numeric(rep(NA, 14)),
                         DevExplRelNullCV = as.numeric(rep(NA, 14)),
                         TSS = as.numeric(rep(NA, 14)),
                         Sensitivity = as.numeric(rep(NA, 14)),
                         Specificity = as.numeric(rep(NA, 14)),
                         Accuracy = as.numeric(rep(NA, 14)),
                         Precision = as.numeric(rep(NA, 14)),
                         Recall = as.numeric(rep(NA, 14)),
                         OverallAccuracy = as.numeric(rep(NA, 14)),
                         BalancedAccuracy = as.numeric(rep(NA, 14)),
                         F1score = as.numeric(rep(NA, 14)),
                         F2score = as.numeric(rep(NA, 14)))

for (i in 1:length(drumlist)) { # i <- 1
  tmp <- read.csv(file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", i, "/CaribbeanReef_CPUE/Report.csv")) |>
    select(Best.Binary.BRT)
  tmp <- tmp[2:15, 1]
  for (j in 1:14) { # j <- 1
    resultstbl[i, 5 + j] <- as.numeric(
      str_sub(tmp[j],
              start = str_locate(string = tmp[j], pattern = ": ")[[2]] + 1,
              end = -1L)
    )
  }
  metrics <- read.csv(file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", i, "/CaribbeanReef_CPUE/MLEvalMetricsBin.csv"))
  resultstbl[i, 20:29] <- metrics$Value[c(7:12, 13:14, 16:17)]
}
write.csv(x = resultstbl,
          # file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/ResultsTable.csv"),
          file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_ResultsTableBestonly-PruneVars.csv"),
          row.names = FALSE)


varsrelinf <- data.frame(var = as.character(),
                         rel.inf = as.numeric(),
                         combo = as.numeric())
for (i in 1:length(drumlist)) { # i <- 1
  tmp <- read.csv(file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", i, "/CaribbeanReef_CPUE/Binary BRT Variable contributions.csv")) |>
    mutate(Combo = i)
  varsrelinf <- rbind(varsrelinf, tmp)
}
write.csv(x = varsrelinf,
          # file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/ResultsTable.csv"),
          file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVarsAllCombos.csv"),
          row.names = FALSE)
varsrelinf |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf)) |>
  arrange(desc(rel.inf)) |>
  write_csv(file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVarsAllCombosSummary.csv"))


varsrelinf <- read_csv(file = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVarsAllCombos.csv")) |>
mutate(Sex = case_match(Combo,
           c(1,5,6,11,12) ~ "Male",
           c(2,7,8,13,14) ~ "Female",
           .default = NA),
       Mature = case_match(Combo,
                           c(3,5,7) ~ "Mature",
                           c(4,6,8) ~ "Immature",
                           .default = NA),
       Size = case_match(Combo,
                        c(10,12,14) ~ "Large",
                        c(9,11,13) ~ "Small",
                        .default = NA),
       ) %T>%
write_csv(paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVarsAllCombos.csv"))

# barplot vars all
library(ggplot2)
# tmp <-
varsrelinf |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf, na.rm = TRUE)) |>
  arrange(desc(rel.inf)) |>
  mutate(var = ordered(var, levels = var)) |>
  ggplot(aes(x = var, y = rel.inf)) +
  geom_col() +
  labs(title = "All data") +
  ggsave(filename = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVars-All.png"))

# barplot vars by sex
varsrelinf |>
  filter(Sex == "Male") |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf, na.rm = TRUE)) |>
  arrange(desc(rel.inf)) |>
  mutate(var = ordered(var, levels = var)) |>
  ggplot(aes(x = var, y = rel.inf)) +
  geom_col() +
  labs(title = "Males") +
  ggsave(filename = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVars-Sex-Male.png"))
varsrelinf |>
  filter(Sex == "Female") |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf, na.rm = TRUE)) |>
  arrange(desc(rel.inf)) |>
  mutate(var = ordered(var, levels = var)) |>
  ggplot(aes(x = var, y = rel.inf)) +
  geom_col() +
  labs(title = "Females") +
  ggsave(filename = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVars-Sex-Female.png"))

# barplot vars by maturity
varsrelinf |>
  filter(Mature == "Mature") |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf, na.rm = TRUE)) |>
  arrange(desc(rel.inf)) |>
  mutate(var = ordered(var, levels = var)) |>
  ggplot(aes(x = var, y = rel.inf)) +
  geom_col() +
  labs(title = "Mature") +
  ggsave(filename = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVars-Maturity-Mature.png"))
varsrelinf |>
  filter(Mature == "Immature") |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf, na.rm = TRUE)) |>
  arrange(desc(rel.inf)) |>
  mutate(var = ordered(var, levels = var)) |>
  ggplot(aes(x = var, y = rel.inf)) +
  geom_col() +
  labs(title = "Immature") +
  ggsave(filename = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVars-Maturity-Immature.png"))

# barplot vars by size
varsrelinf |>
  filter(Size == "Large") |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf, na.rm = TRUE)) |>
  arrange(desc(rel.inf)) |>
  mutate(var = ordered(var, levels = var)) |>
  ggplot(aes(x = var, y = rel.inf)) +
  geom_col() +
  labs(title = "Large, over 165 cm STL") +
  ggsave(filename = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVars-Size-Large.png"))
varsrelinf |>
  filter(Size == "Small") |>
  group_by(var) |>
  summarise(rel.inf = sum(rel.inf, na.rm = TRUE)) |>
  arrange(desc(rel.inf)) |>
  mutate(var = ordered(var, levels = var)) |>
  ggplot(aes(x = var, y = rel.inf)) +
  geom_col() +
  labs(title = "Small, less than or equal to 165 cm STL") +
  ggsave(filename = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/Sex-maturity-size-combos/", today(), "_RelInfVars-Size-Small.png"))






# 2023-05-23 STL as resvar ####
# expvars = c(
#   "Habitat",
#   "Tide",
#   "LunarPhase",
#   "Depth_m",
#   "Temperature_C",
#   "Yearday",
#   "DtDropOffLogNeg"
#   # "Sex", #
#   # "STL" # is resvar
# )
gbm.auto(samples = as.data.frame(drumline |> filter(CaribbeanReef == 1, !is.na(STL))), # filter for only positive samples. n=90
         expvar = expvars,
         resvar = "STL",
         randomvar = TRUE,
         tc = 7, # c(2,3,4,5,6,7)
         lr = 0.001, # c(0.001, 0.0005, 0.0001, 0.00005, 0.00001)
         bf = 0.9, # c(0.5, 0.7, 0.85, 0.87, 0.9, 0.92)
         fam2 = "gaussian", # since there are no zeroes
         gaus = TRUE, # only run bin, gaus isn't working
         smooth = TRUE,
         simp = FALSE,
         savegbm = FALSE,
         MLEvaluate = TRUE, # until it's fixed.
         savedir = paste0("../../Projects/2021-10_Drumline_Reefshark/BRT/"),
         BnW = FALSE)
# I'm SURE I've run just gaus before though? Maybe before I added extra conditions to the above?
# fam1 = leave alone, fam2 = gaus, gaus=true








# Bayesian ####
# Perfect test case?
# brms, https://bookdown.org/connect/#/apps/1850/access
# expvars = c("Site3", "Habitat", "Substrate2", "Tide", "Season", "LunarPhase",
#             "Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")
# drumline <- list.files(path = "../../Data/") %>% # list all files in Data folder
#   .[matches("_drumline_reefs.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
#   last() # last one is highest date i.e. latest
# drumline <- readRDS(file = paste0("../../Data/", drumline))
# drumline %<>%
#   filter(Site3 != "Somerset") %>% #remove Somerset rows, n=103, 9% of data
#   mutate(Site3 = factor(Site3, levels = levels(Site3)[2:6])) # remove Somerset as a factor level

# samples = drumline,       # [-which(is.na(drumline[resvar])),]
# expvar = expvars,
# resvar = "CPUE",
# fam1 = c("bernoulli", "binomial", "poisson", "laplace", "gaussian"),
# fam2 = c("gaussian", "bernoulli", "binomial", "poisson", "laplace"),
# savedir = "../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/BRT",

# install.packages("brms")
library(brms) # brm zero_inflated_poisson marginal_effects
# fit_zinb1 <- brm(count ~ persons + child + camper,
#                  data = zinb,
#                  family = zero_inflated_poisson("log"))

# brm(
#   formula,
#   data,
#   family = gaussian(),
#   prior = NULL,
#   autocor = NULL,
#   data2 = NULL,
#   cov_ranef = NULL,
#   sample_prior = "no",
#   sparse = NULL,
#   knots = NULL,
#   stanvars = NULL,
#   stan_funs = NULL,
#   fit = NA,
#   save_pars = NULL,
#   save_ranef = NULL,
#   save_mevars = NULL,
#   save_all_pars = NULL,
#   inits = "random",
#   chains = 4,
#   iter = 2000,
#   warmup = floor(iter/2),
#   thin = 1,
#   cores = getOption("mc.cores", 1),
#   threads = NULL,
#   opencl = NULL,
#   normalize = getOption("brms.normalize", TRUE),
#   control = NULL,
#   algorithm = getOption("brms.algorithm", "sampling"),
#   backend = getOption("brms.backend", "rstan"),
#   future = getOption("future", FALSE),
#   silent = 1,
#   seed = NA,
#   save_model = NULL,
#   stan_model_args = list(),
#   file = NULL,
#   file_refit = getOption("brms.file_refit", "never"),
#   empty = FALSE,
#   rename = TRUE,
#   ...
# )


fit_zinb1 <- brm(CaribbeanReef ~ Site3 + Habitat + Substrate2 + Tide + Season + # all expvars from above. Could trim based on gbm.auto outputs?
                   LunarPhase + Latitude + Longitude + Depth_m + Temperature_C +
                   Salinity + DO_mg_L + Yearday + Month + daylength,
                 data = drumline,
                 family = zero_inflated_poisson("log")) # CPUE (CaribbeanReef) is ZI according to gbm.auto check
# crashes

# 2022-09-13:
# Warning messages:
# 1: Rows containing NAs were excluded from the model.
# 2: There were 2 divergent transitions after warmup. See
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them.
# 3: There were 3823 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 4: Examine the pairs() plot to diagnose sampling problems
#
# 5: The largest R-hat is 2.3, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat
# 6: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess
# 7: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess


# try best 5 only
fit_zinb1 <- brm(CaribbeanReef_CPUE ~ Depth_m + Season + DO_mg_L + Longitude + Salinity,
                 data = drumline,
                 family = hurdle_negbinomial("log")) # CPUE (CaribbeanReef) is ZI according to gbm.auto check
# Error: Family 'zero_inflated_poisson' requires integer responses.
summary(fit_zinb1)
#  Family: zero_inflated_poisson
#   Links: mu = log; zi = identity
# Formula: CaribbeanReef ~ Depth_m + Season + DO_mg_L + Longitude + Salinity
#    Data: drumline (Number of observations: 738)
#   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#          total post-warmup draws = 4000
#
# Population-Level Effects:
#              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     4302.83   1630.44  1444.34  7914.19 1.00     2455     2068
# Depth_m          0.04      0.03    -0.02     0.11 1.00     2496     2527
# SeasonSpring     1.66      0.49     0.72     2.64 1.00     2565     2517
# SeasonSummer     0.10      0.54    -0.99     1.14 1.00     2412     2661
# SeasonAutumn     0.12      0.48    -0.78     1.07 1.00     2297     2511
# DO_mg_L         -0.36      0.28    -0.91     0.20 1.00     2773     2533
# Longitude       55.23     20.95    18.47   101.57 1.00     2460     2056
# Salinity        -0.32      0.22    -0.74     0.14 1.00     2987     2662
# Family Specific Parameters:
#    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# zi     0.16      0.12     0.01     0.44 1.00     2950     2598
#
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

marginal_effects(fit_zinb1) # marginal_effects(fit_rent1, surface = TRUE)
# Manually save these


# loo(fit_zinb1, fit_zinb2)

# 2023-12-13 reDo Bayesian, best vars####
# chosen from BRT bin best bars
# /home/simon/Documents/Si Work/PostDoc Work/Saving The Blue/Projects/2021-10_Drumline_Reefshark/BRT/CaribbeanReef_CPUE_2023-12-11_OrijVars_tuned/Bin_Bars.png
fit_zinb1 <- brm(CaribbeanReef ~ Depth_m + DtDropOff + Temperature_C + Habitat + Season, #  + Yearday + Tide + LunarPhase,
                 data = drumline,
                 family = zero_inflated_poisson("log")) # CPUE (CaribbeanReef) is ZI according to gbm.auto check
# fit_zinb1 <- brm(CaribbeanReef_CPUE ~ Depth_m + DtDropOff + Temperature_C + Habitat + Season, #  + Yearday + Tide + LunarPhase,
#                  data = drumline,
#                  family = hurdle_negbinomial("log")) # CPUE (CaribbeanReef) is ZI according to gbm.auto check # Family 'hurdle_negbinomial' requires integer responses.
summary(fit_zinb1)
marginal_effects(fit_zinb1) # marginal_effects(fit_rent1, surface = TRUE)
# Manually save these

# With all 7 best vars: Warning messages:
# 1: Rows containing NAs were excluded from the model.
# 2: There were 2889 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 3: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See https://mc-stan.org/misc/warnings.html#bfmi-low
# 4: Examine the pairs() plot to diagnose sampling problems
# 5: The largest R-hat is 2.6, indicating chains have not mixed. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#r-hat
# 6: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#bulk-ess
# 7: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#tail-ess

# 2023-12-13 later: Warning messages: 4 vars
# 1: Rows containing NAs were excluded from the model.
# 2: There were 1 divergent transitions after warmup. See https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup to find out why this is a problem and how to eliminate them.
# 3: There were 236 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 4: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See https://mc-stan.org/misc/warnings.html#bfmi-low
# 5: Examine the pairs() plot to diagnose sampling problems
# 6: The largest R-hat is 1.6, indicating chains have not mixed. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#r-hat
# 7: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#bulk-ess
# 8: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#tail-ess

# 2023-12-14:
# not using all the expvars, nor CPUE resvar
# results are coming out poor, need lots of tuning
# does anyone actually care if I ALSO do Bayesian AS WELL AS extensive BRTs AS WELL AS loads of preliminary analyses?
# Is anyone going to benefit from this other than me as a performative exercise?

# Turn this into a markup doc which can be online
# D. See if they say anything interesting
# E. Write short comms paper if so

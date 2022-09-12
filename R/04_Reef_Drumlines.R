# 2021-06-09 Reef shark drumline analysis
# Simon Dedman simondedman@gmail.com
# With Tristan Guttridge & Phil Matich

# Setup, load, prep data####
library(magrittr) # %>% %<>% # %>% %<>%
library(dplyr) # %>% matches last filter mutate case_when relocate everything rename across last_col bind_cols group_by tally pull summarise n_distinct left_join arrange select bind_rows # %>% matches last filter mutate case_when relocate everything rename across last_col bind_cols group_by tally pull summarise n_distinct left_join arrange select bind_rows
library(lubridate) # minute yday month is.POSIXt today
library(tidyverse) # "No used functions found" # "No used functions found"
library(tidylog) # filter mutate relocate pivot_wider rename replace_na group_by tally summarise left_join drop_na select pivot_longer # filter mutate relocate pivot_wider rename replace_na group_by tally summarise left_join drop_na select pivot_longer
{ # processing runall
  # read in data
  # source('R/01_data-import.R') # run data import if you changed the database.
  drumline <- list.files(path = "../../Data/") %>% # list all files in Data folder
    .[matches("_drumline_data.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
    last() # last one is highest date i.e. latest
  drumline <- readRDS(file = paste0("../../Data/", drumline))
  # unique(drumline$Common)

  # unique(drumline$Site2)
  # drumline %>%
  #   group_by(Site2) %>%
  #   summarise(n = n()) %>%
  #   arrange(desc(n))
  # 1 Green Cay              330  *
  # 2 Bigwood Channel        280  *
  # 3 North Bight             97  Bight
  # 4 AUTEC Channel           86  *
  # 5 Bristol Galley          70  *
  # 6 Shark Hole              35  Bight
  # 7 Somerset                30  Somerset
  # 8 Blackbeard's Channel    29  Bight
  #  9 High Cay                25  Somerset
  # 10 Gibson Cay              11  *
  # 11 Cargill Creek           10  Bight?
  # 12 Isla's Spot              7  Bight?

  drumline %<>%
    filter(!Site2 %in% c("Gibson Cay", "Cargill Creek", "Isla's Spot")) %>% # only fished once
    mutate(Site3 = factor(
      case_when(
        Site2 %in% c("North Bight", "Shark Hole", "Blackbeard's Channel") ~ "Bight", # Make values 1 when sharks caught
        Site2 == "High Cay" ~ "Somerset",
        TRUE ~ as.character(Site2)),
      levels = c("Somerset", "Green Cay", "Bristol Galley", "AUTEC Channel", "Bight", "Bigwood Channel")))

  # unique(drumline$Substrate)
  # drumline %>%
  #   group_by(Substrate) %>%
  #   summarise(n = n()) %>%
  #   arrange(desc(n))
  # 1 Sand & seagrass     315
  # 2 Sand                269
  # 3 Sand & patch reef   146
  # 4 Sand & octocorals   108
  # 5 Reef                 44
  # 6 Patch reef           33
  # 7 Silt                 29
  # 8 Hard bottom          23
  # 9 Sand & coral         19
  # 10 Silt & seagrass      10
  # 11 Sand & algae          7
  # 12 Seagrass              7

  drumline %<>%
    mutate(Substrate2 = factor(case_when(
      Substrate %in% c("Sand & patch reef", "Patch reef", "Sand & octocorals", "Sand & coral") ~ "Reef",
      Substrate %in% c("Silt & seagrass", "Sand & seagrass", "Sand & algae", "Seagrass") ~ "Vegetation",
      Substrate %in% c("Sand", "Silt", "Hard bottom") ~ "Bare",
      TRUE ~ as.character(Substrate)),
      levels = c("Bare", "Vegetation", "Reef")))
  # could then mutate Substrate2 into a factor with levels I specify

  # unique(drumline$Habitat)
  # drumline %>%
  #   group_by(Habitat) %>%
  #   summarise(n = n()) %>%
  #   arrange(desc(n))
  # 1 Back reef   650
  # 2 Channel     143
  # 3 Flats       139
  # 4 Fore reef    68
  # 5 Blue Hole    10
  drumline %<>%
    mutate(Habitat2 = case_when(
      Habitat == "Blue Hole" ~ "Flats",
      Habitat == "Channel" & Site2 %in% c("Blackbeard's Channel", "North Bight", "Shark Hole", "Cargill Creek") ~ "Flats", # Cargill filtered out above
      Habitat == "Channel" & Site3 %in% c("AUTEC Channel", "Bigwood Channel") ~ "Back reef",
      TRUE ~ as.character(Habitat)))






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
    mutate(across("NA":last_col(), ~ replace_na(., 0))) # https://stackoverflow.com/a/63970397/3975144
  # later, grouped calculations for CPUE via: mutate(CPUE = "Caribbean Reef" / Soak_time_CaribbeanReef)


  # temporal####
  # time of day. Midpoint of timein/timeout? Will be a posix tho.
  # can use hour as a lazy proxy. or minute
  drumline %<>%
    mutate(Minute = minute(Time_in),
           Yearday = yday(Date),
           Month = month(Date),
           Season = cut(Month,
                        breaks = c(-Inf, 2, 5, 8, 11, Inf),
                        labels = c("Winter", "Spring", "Summer", "Autumn", "Winter")))

  # Daylength
  source('~/Dropbox/Galway/Analysis/R/daylength/daylength.R')

  dl <- daylength(date = drumline$Date,
                  lat = drumline$Latitude,
                  lon = drumline$Longitude,
                  tzs = "America/New_York")
  #dl has date lat lon sunrise sunset dawn dusk daylength, mostly "POSIXct" "POSIXt"
  drumline %<>% bind_cols(dl[, 4:8]) # append sunrise sunset dawn dusk daylength

  # lunar cycle
  library(lunar) #lunar.phase
  drumline %<>% mutate(LunarPhase = lunar.phase(x = Date, shift = -5, name = TRUE)) # Levels: New Waxing Full Waning


  # Fishery / method:
  # Bait_type
  # Bottom_top

  # Subsetting variables: sex, size.

  # add data from shark table, join by Casey tag####
  shark <- list.files(path = "../../Data/") %>% # list all files in Data folder
    .[matches("_shark_capture_data.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
    last() # last one is highest date i.e. latest
  shark <- readRDS(file = paste0("../../Data/", shark))

  # Initial left_join created 242405 obs = dupes
  # shark 345 obs but 132 unique Caseys
  # length(unique(shark$Casey_Tag_no[!is.na(shark$Casey_Tag_no)])) # 138 non na entries, 131 unique = 7 dupes
  # 398587 398587 398574 398562 398974 398574 406255 (2 doubles, 5 unique tags)

  # from here####
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

  dupePITs <- shark %>%     # create dupecasey list
    filter(!is.na(PIT_Tag_Full_ID_no)) %>% # remove nas
    group_by(PIT_Tag_Full_ID_no) %>%
    tally() %>% # number of rows per group
    filter(n > 1) %>% # remove 1s leaving dupes
    pull(PIT_Tag_Full_ID_no) # length 17, all unique

  # vector of same PITs, different Common
  badPITs <- shark %>%
    filter(PIT_Tag_Full_ID_no %in% dupePITs) %>%
    group_by(PIT_Tag_Full_ID_no) %>%
    summarise(howmany = n_distinct(Common)) %>%
    filter(howmany > 1) %>%
    pull(PIT_Tag_Full_ID_no)

  drumline %<>% left_join(
    shark %>%
      filter(!PIT_Tag_Full_ID_no %in% badPITs) %>% # exclude bad tags
      drop_na(PIT_Tag_Full_ID_no) %>% # else joins NA pits in shark w all NAs pits in drumline, populates same values throughout.
      arrange(Time) %>% # arrange by datetime
      group_by(PIT_Tag_Full_ID_no) %>%
      select(PIT_Tag_Full_ID_no, Sex, Mature, PCL, FL, TL, STL, Girth) %>% # remove unneeded cols
      mutate(across(where(is.factor), as.character)) %>% # else breaks the NA fixes below
      # summarise_all(last) # across numericals want max (na rm = T) else last (na rm = T)
      summarise(across(where(is.numeric), max, na.rm = TRUE),
                across(where(~ is.character(.) | is.POSIXt(.)), last)) %>%
      # convert -Inf (from max/last of 2+ NAs) to NA
      mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
             across(where(~ is.character(.)), ~ ifelse(is.infinite(.), NA, .))) %>% # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
      mutate(Sex = factor(Sex, levels = c("F", "M")))
    # mutate sex back to factor here ####
  ) # Joining, by = "PIT_Tag_Full_ID_no"

  # E9F44 PIT from drum, has full in shark? If so fix those in xlsx. Asked TG, 2021-10-05, should add ~10 fish

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
    pull(Casey) # "406260" "406261" "406262" "406263"

  # Can join on these Caseys. But will wipe out everything else. Unless dplyr has fixed that?

  # can use base R index replacement
  caseyjoinimport <- as.data.frame(shark %>%
                                     rename(Casey = Casey_Tag_no) %>%
                                     filter(Casey %in% caseybothdbsnopit) %>%
                                     select(Casey, Sex, Mature, PCL, FL, TL, STL, Girth) %>%
                                     mutate(across(where(is.factor), as.character)) %>% # fixes Sex
                                     arrange(Casey)
  )

  drumline[which(drumline$Casey %in% caseybothdbsnopit), # rows indexed by Casey column logical check
           c("Casey", "Sex", "Mature", "PCL", "FL", "TL", "STL", "Girth")] <- caseyjoinimport # send data to these columns for those rows. Data sent: that table.

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
  #     summarise(across(where(is.numeric), max, na.rm = TRUE),
  #               across(where(~ is.character(.) | is.POSIXt(.)), last)) %>%
  #     # convert -Inf (from max/last of 2+ NAs) to NA
  #     mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
  #            across(where(~ is.character(.)), ~ ifelse(is.infinite(.), NA, .))) # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
  # ) # Joining, by = "Casey"




  # Save sheets####
  write.csv(x = drumline,
            file = paste0("../../Data/", today(), "_drumline_reefs.csv"),
            row.names = F)
  saveRDS(object = drumline,
          file = paste0("../../Data/", today(), "_drumline_reefs.rds"))
  rm(list = c("bothdbs", "caseyjoinimport", "badPITs", "caseybothdbs", "caseybothdbsnopit", "dupePITs"))
} # finish processing runall







# Analysis: bar charts & scatterplots####
# read in saved data from above
drumline <- list.files(path = "../../Data/") %>% # list all files in Data folder
  .[matches("_drumline_reefs.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
  last() # last one is highest date i.e. latest
drumline <- readRDS(file = paste0("../../Data/", drumline))
# use drumline data to explore CPUE in different habitats, sex and size segregation / overlap.
# Possible seasonal differences in catch rates or habitats caught in due to temp etc?

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
  ggplot(data = drumline) +
    geom_col(mapping = aes(x = .data[[factorvars]], y = CaribbeanReef), fill = "black") +
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
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_ColPlot_CPUE_", factorvars, ".png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 7,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)
}

# Scatterplots####
# Loop through numerical variables & scatterplot against CPUE with trendline
for (numvars in c("Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")) {
  ggplot(data = drumline) +
    geom_point(mapping = aes(x = .data[[numvars]], y = CaribbeanReef), fill = "black") +
    geom_smooth(mapping = aes(x = .data[[numvars]], y = CaribbeanReef)) + # , fill = "black"
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
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_ScatterPlot_CPUE_", numvars, ".png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 7,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)
}





# BRT ####
library(remotes) # install_github # install_github
install_github("SimonDedman/gbm.auto")
library(gbm.auto) # gbm.bfcheck gbm.auto gbm.loop # gbm.bfcheck gbm.auto gbm.loop
# expvars = c("Site3", "Habitat", "Substrate", "Tide", "Season", "LunarPhase",
#             "Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")
# 2021-09-08 PM Substrate2 suggestion
expvars = c("Site3", "Habitat", "Substrate2", "Tide", "Season", "LunarPhase",
            "Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")
gbm.bfcheck(samples = drumline, resvar = "CaribbeanReef")
                            # [1] "  binary bag fraction must be at least 0.018. n = 1179"
                            # [1] "Gaussian bag fraction must be at least 0.262. n = 80"
                            # [1] 0.0178117 0.2625000

# 2021-09-08 PM remove Somerset suggestion
drumline %<>%
  filter(Site3 != "Somerset") %>% #remove Somerset rows, n=103, 9% of data
  mutate(Site3 = factor(Site3, levels = levels(Site3)[2:6])) # remove Somerset as a factor level

# 2021-10-19 now doesn't work since new binomial CPUE by species.
drumline <- as.data.frame(drumline) # fails if tibble

gbm.auto(
  grids = NULL,
  samples = drumline,       # [-which(is.na(drumline[resvar])),]
  expvar = expvars,
  resvar = "CaribbeanReef",
  tc = 2,
  lr = list(0.01, 0.005),
  bf = list(0.5, 0.9),
  n.trees = 50,
  ZI = "CHECK",
  # fam1 = c("bernoulli", "binomial", "poisson", "laplace", "gaussian"),
  fam1 = "bernoulli",
  fam2 = c("gaussian", "bernoulli", "binomial", "poisson", "laplace"),
  simp = TRUE,
  gridslat = 2,
  gridslon = 1,
  multiplot = TRUE,
  cols = grey.colors(1, 1, 1),
  linesfiles = TRUE,
  smooth = TRUE, # FALSE
  savedir = "../../Projects/2021-10_Drumline_Reefshark/BRT",
  savegbm = TRUE,
  loadgbm = NULL,
  varint = TRUE,
  map = TRUE,
  shape = NULL,
  RSB = TRUE,
  BnW = FALSE,
  alerts = TRUE,
  pngtype = c("cairo-png", "quartz", "Xlib"),
  gaus = TRUE, # bin run
  MLEvaluate = TRUE)



# gaus run struggling, only n=80, keep playing with list options
# lr
# 0.005 bin 0.54 gaus 0.64
# 0.001 bin 0.5 gaus 0.66
# 0.0001 bin 0.52 gaus 0.55
# 0.00001 runs bin 0.5 gaus 0.5 (about the same)
# 0.000001 didn't
# 0.0000001 bin 0.5 gaus 0.51
# do gbm.loop

# Gbm.loop####
library(magrittr) # %>% %<>% # %>% %<>%
library(dplyr) # %>% matches last filter mutate case_when relocate everything rename across last_col bind_cols group_by tally pull summarise n_distinct left_join arrange select bind_rows # %>% matches last filter mutate case_when relocate everything rename across last_col bind_cols group_by tally pull summarise n_distinct left_join arrange select bind_rows
library(tidyverse) # "No used functions found" # "No used functions found"
library(tidylog) # filter mutate relocate pivot_wider rename replace_na group_by tally summarise left_join drop_na select pivot_longer # filter mutate relocate pivot_wider rename replace_na group_by tally summarise left_join drop_na select pivot_longer
drumline <- list.files(path = "../../Data/") %>% # list all files in Data folder
  .[matches("_drumline_reefs.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
  last() # last one is highest date i.e. latest
drumline <- readRDS(file = paste0("../../Data/", drumline))
library(remotes) # install_github # install_github
install_github("SimonDedman/gbm.auto")
library(gbm.auto) # gbm.bfcheck gbm.auto gbm.loop # gbm.bfcheck gbm.auto gbm.loop
expvars = c("Site3", "Habitat", "Substrate2", "Tide", "Season", "LunarPhase",
            "Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")
drumline %<>%
  filter(Site3 != "Somerset") %>% #remove Somerset rows, n=103, 9% of data
  mutate(Site3 = factor(Site3, levels = levels(Site3)[2:6])) # remove Somerset as a factor level

gbm.loop(savedir = "../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/BRT",
         samples = drumline,
         expvar = expvars,
         resvar = "CaribbeanReef",
         lr = list(0.01, 0.0001), #0.005
         bf = list(0.5, 0.9),
         runautos = FALSE)


# Henderson etal 2021 figures ####
# F3 x seasons y temperatureC dots w/ SDs ####
drumline %>%                # create summary table as data input
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
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_DotWhisker_Temp_Season.png"),
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
                         y = CPUE)) +
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
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_Column_CPUE_Species.png"),
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
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_DotPlot_CPUE_Species_Habitat2.png"),
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
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_BoxPlot_STL_Sex_facetSpecies.png"),
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
                                   legend.position = c(0.1, 0.8),
                                   legend.direction = "vertical",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   plot.background = element_rect(fill = "white"),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_DotPlot_CPUE_Species_Season.png"),
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
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_BoxPlot_STL_Season_facetSpecies.png"),
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
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_BoxPlot_STL_Habitat2_facetSpecies.png"),
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
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0("../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/", today(), "_BoxPlot_STL_Substrate2_facetSpecies.png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 7,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)

# replace theme_minimal with my own with all the changes I make, to save space.


# Subset categories next steps####
# PM: Something that would be interesting to include are size and sex differences –
# any segregation in distributions based on these factors,
# who do we catch more of, and
# is there any seasonality to juveniles or mature individuals that could be indicative of reproductive behaviour (pupping, migration, etc.).





# Bayesian ####
# Perfect test case?
# brms, https://bookdown.org/connect/#/apps/1850/access
# expvars = c("Site3", "Habitat", "Substrate2", "Tide", "Season", "LunarPhase",
#             "Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")
drumline <- list.files(path = "../../Data/") %>% # list all files in Data folder
  .[matches("_drumline_reefs.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
  last() # last one is highest date i.e. latest
drumline <- readRDS(file = paste0("../../Data/", drumline))
drumline %<>%
  filter(Site3 != "Somerset") %>% #remove Somerset rows, n=103, 9% of data
  mutate(Site3 = factor(Site3, levels = levels(Site3)[2:6])) # remove Somerset as a factor level

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
# try best 5 only
fit_zinb1 <- brm(CaribbeanReef ~ Depth_m + Season + DO_mg_L + Longitude + Salinity,
                 data = drumline,
                 family = zero_inflated_poisson("log")) # CPUE (CaribbeanReef) is ZI according to gbm.auto check
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
# Intercept     3929.38   1477.88  1389.22  7292.94 1.00     2891     2097
# Depth_m          0.03      0.03    -0.03     0.10 1.00     3149     2671
# SeasonSpring     1.84      0.51     0.86     2.84 1.00     3128     2465
# SeasonSummer    -0.11      0.54    -1.15     0.96 1.00     2698     2958
# SeasonAutumn    -0.25      0.53    -1.31     0.80 1.00     2607     2682
# DO_mg_L         -0.44      0.29    -1.00     0.14 1.00     3159     3069
# Longitude       50.27     19.01    17.65    93.28 1.00     2900     2065
# Salinity        -0.63      0.31    -1.24    -0.04 1.00     2924     2702
#
# Family Specific Parameters:
#    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# zi     0.15      0.12     0.01     0.44 1.00     2724     2220
#
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


marginal_effects(fit_zinb1) # marginal_effects(fit_rent1, surface = TRUE)





# loo(fit_zinb1, fit_zinb2)










# Turn this into a markup doc which can be online
# D. See if they say anything interesting
# E. Write short comms paper if so

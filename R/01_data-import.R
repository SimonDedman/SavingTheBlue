# Excel data import, processing, save
# Simon Dedman simondedman@gmail.com
# 2021-03-03

{ # run everything in one click
  library(openxlsx) # read.xlsx convertToDateTime
  library(stringr) # "No used functions found"
  library(dplyr)
  library(magrittr) # "No used functions found"
  library(data.table) # "No used functions found"
  library(lubridate) # today
  library(tidylog) # verbose version of tidyverse
  # getwd() # Saving The Blue/Code/SavingTheBlue

  # latestdbase <- "../../Data/Database_2021-06-25.xlsx"
  latestdbase <- "../../Data/Database_2023-12-01.xlsx"
  # Shark Data ####
  shark <- read.xlsx(xlsxFile = latestdbase,
                     sheet = 1,
                     detectDates = TRUE,
                     check.names = TRUE,
                     na.strings = c("NA", "xxx")) %>% # various cols are "xxx" instead of NA or blank, why?
    # cut off after last date, given there are now extra rows for vlookups to prevent data entry errors
    filter(!is.na(Date))

  tmp <- as.POSIXct(rep(NA, nrow(shark)), tz = "America/New_York")
  for (i in 1:nrow(shark)) {
    tmp[i] <- convertToDateTime(shark$Time[i], origin = shark$Date[i], tz = "America/New_York")
    # convertToDateTime can't vectorise over >len1 origins
  }
  shark$Time <- tmp # can't output directly to sharks$Time since it's numeric and thus converts the outputs
  rm(tmp)

  # convert yes no to T F in "Mature" "Fin.genetics"         "Fin.isotopes"         "Muscle.isotopes"      "Whole.blood.isotopes" "Plasma.isotopes" "RBC.isotopes"
  # Fixed in Excel sheet with find replace

  # list unique for all cols, done in excel
  # "Pit Tag #" Unknown xxx None NA [blank]
  # "PIT Tag Full ID #" xxx None [blank]
  # "Casey Tag #" xxx NA [blank]
  # Various have NA & [blank] or xxx & [blank]
  #
  # “xxx” was included to indicate data that could/should have been collected but
  # was not (e.g. for animal welfare, sampling limitations, forgot), while “NA” indicates
  # data that we do not collect (e.g. fork length for nurse sharks, casey tag for
  # nurse sharks). For the purposes of data analysis I think we can integrate these
  # two and use a single code or include as a blank cell – most of the blanks
  # correspond to NA, but some do correspond to missing data (e.g. pit tag for C.
  # limbatus 398545).
  ## xxx NA blank = NA

  # STL has ">200"
  #This was a while ago, but I’m guessing this was a shark that spit the hook and
  # we estimated the size. For the sake of accuracy, I think it’s best to leave this
  # blank rather than guess on a specific number.
  shark$STL[which(shark$STL == ">200")] <- NA
  shark$STL <- as.numeric(shark$STL)

  # Fin_genetics Fin_isotopes Muscle_isotopes: TRUE FALSE "1-day recap"
  # This indicates that samples were not collected because the shark was captured
  # the day before, so these are “False”.
  # shark$Fin_genetics[which(shark$Fin_genetics == "1-day recap")] <- FALSE
  # shark$Fin_isotopes[which(shark$Fin_isotopes == "1-day recap")] <- FALSE
  # shark$Muscle_isotopes[which(shark$Muscle_isotopes == "1-day recap")] <- FALSE # now done in dbase
  shark$Fin_genetics <- as.logical(shark$Fin_genetics)
  shark$Fin_isotopes <- as.logical(shark$Fin_isotopes)
  shark$Muscle_isotopes <- as.logical(shark$Muscle_isotopes)

  # I think adding a recapture column would be beneficial, and also a shark # column,
  # the latter starting at 1 and continuing indefinitely as we sample more sharks. [done in excel sheet]
  # The recapture column could then include the shark number from when it was last caught.
  # Other options are also possible, but good to have data on recaps for growth rates
  # and changes in distributions as we continue to catch more of them.

  # Depth has "<1" (& xxx)
  # PM: These depth relate to gill net sampling with BAMSI – Tristan can provide
  shark$Depth_m[which(shark$Depth_m == "<1")] <- 0.5
  shark$Depth_m <- as.numeric(shark$Depth_m)

  # Factorial columns
  shark$Sex <- factor(shark$Sex, levels = c("F", "M"))
  unique(shark$Species)
  # "G. cirratum"     "C. perezi"       "C. falciformis"  "N. brevirostris" "C. acronotus"
  # "C. limbatus"     "G. cuvier"       "C. obscurus"     "C. leucas"       "R. terraenovae"
  # "S. mokarran"     "C. brevipinna"   "H. americana"    "C. plumbeus"     "S. lewini"       "S. zygaena"
  # Will need to have something that warns us if there are new species which aren't in the existing levels list. Ditto for all of these.

  # PM: pelagic vs inshore then size then rays makes the most sense:
  levels_Species <- c("C. falciformis", "C. obscurus", # silky (Carcharhinus falciformis) and dusky (Carcharhinus obscurus)
                      "C. acronotus", "R. terraenovae", # blacknose (Carcharhinus acronotus) and sharpnose (Rhizoprionodon terraenovae)
                      "C. brevipinna", "S. lewini", "S. zygaena", # Spinner C. brevipinna, scalloped hammer, smooth hammer
                      "C. limbatus", "C. perezi", "N. brevirostris", "G. cirratum", # blacktip (Carcharhinus limbatus), reef (Carcharhinus perezi), lemon (Negaprion brevirostris),  nurse (Ginglymostoma cirratum)
                      "C. plumbeus", "G. cuvier", "C. leucas", "S. mokarran", # sandbar ("C. plumbeus"), tiger (Galeocerdo cuvier), bull (Carcharhinus leucas), hammerhead (Sphyrna mokarran)
                      "H. americanus") # Hypanus americanus = southern stingray
  shark$Species <- factor(shark$Species, levels = levels_Species)

  unique(shark$Common)
  # "Nurse"                "Caribbean reef"       "Silky"                "Lemon"                "Blacknose"            "Blacktip"             "Tiger"                "Dusky"
  # "Bull"                 "Sharpnose"            "Great hammerhead"     "Spinner"              "Southern"             "Sandbar"              "Scalloped Hammerhead" "Smooth hammerhead"
  # "Atlantic sharpnose"
  # upper case second names / name fix
  shark %<>% mutate(Common = case_match(Common,
                                        "Caribbean reef" ~ "Caribbean Reef",
                                        "Great hammerhead" ~ "Great Hammerhead",
                                        "Southern" ~ "Southern Stingray",
                                        c("Atlantic sharpnose", "Sharpnose") ~ "Atlantic Sharpnose",
                                        "Smooth hammerhead" ~ "Smooth Hammerhead",
                                        .default = Common))
  levels_Common <- c("Silky", "Dusky", # silky (Carcharhinus falciformis) and dusky (Carcharhinus obscurus)
                     "Blacknose", "Atlantic Sharpnose", # blacknose (Carcharhinus acronotus) and sharpnose (Rhizoprionodon terraenovae)
                     "Spinner", "Scalloped Hammerhead", "Smooth Hammerhead",
                     "Blacktip", "Caribbean Reef", "Lemon", "Nurse", # blacktip (Carcharhinus limbatus), reef (Carcharhinus perezi), and nurse (Ginglymostoma cirratum)
                     "Sandbar", "Tiger", "Bull", "Great Hammerhead", # tiger (Galeocerdo cuvier), bull (Carcharhinus leucas), hammerhead (Sphyrna mokarran)
                     "Southern Stingray")
  shark$Common <- factor(shark$Common, levels = levels_Common)



  unique(shark$Substrate)
  # "Sand & seagrass"   NA                  "Silt & seagrass"   "Patch reef"        "Sand & Rock"
  # "Sand"              "Sand & octocorals" "Silt"              "Reef"              "Seagrass"
  # "Hard bottom"       "Sand & mud"        "Sand & patch reef"
  shark %<>% mutate(Substrate = case_match(Substrate,
                                           "Sand & mud" ~ "Sand & Mud",
                                           .default = Substrate))
  # PM: will be good to try and match with
  # sand, silt/mud, or rock
  # seagrass, algae, coral
  # coral: soft, hard
  levels_Substrate <- c("Sand & Mud", "Silt", "Sand", "Sand & Rock", "Hard bottom",
                        "Silt & seagrass", "Sand & seagrass", "Seagrass", # 2023-12-15 removed "Sand & algae", not present
                        "Sand & patch reef", "Sand & octocorals", "Patch reef", "Reef", NA) # 2021-10-05 removed "Coral Reef", dupe of Reef, in dbase. 2023-12-15 removed "Sand & coral" not present, added NA (pelagic)
  shark$Substrate <- factor(shark$Substrate, levels = levels_Substrate)


  unique(shark$Habitat)
  # "Channel"         "Back reef"       "Pelagic"         "Flats"           "Fore reef"       "Blue Hole"       "Artificial Reef"
  levels_Habitat <- c("Flats", "Channel", "Artificial Reef", "Back reef", "Blue Hole", "Fore reef", "Pelagic")
  shark$Habitat <- factor(shark$Habitat, levels = levels_Habitat)


  shark$Site2 <- shark$Site
  unique(shark$Site2)
  # "Gibson Cay"             "Green Cay"              "TOTO Navy Buoy"         "Blackbeard's Channel"   "Bigwood Channel"        "Fresh Creek"            "Middle Bight"           "AUTEC Channel"          "Isla's Spot"            "Bristol Galley"
  # "AUTEC Channel Reef"     "Middle Bight – Upper"   "Middle Bight - MB4"     "Shark Hole"             "Behring Point"          "Somerset"               "North Bight – Upper"    "High Cay"               "Somerset reef"          "Bristol Galley dropoff"
  # "Jupiter Florida"        "Cargill Creek"          "Pye's Harbour"          "Princess Anne Wreck"    "Bahia Honda"            "Sandy Key"              "Mary Rock Channel"      "Port Cay Channel"       "Rat Cay Channel"        "Driggs Hill"
  # "Mangrove Cay"
  shark[which(shark$Site2 == "Somerset reef"), "Site2"] <- "Somerset"
  shark[which(shark$Site2 == "AUTEC Channel Reef"), "Site2"] <- "AUTEC Channel"
  shark[which(shark$Site2 == "Princess Anne Wreck"), "Site2"] <- "Jupiter Florida"
  shark[which(shark$Site2 %in% c("North Bight – Upper",
                                 "Middle Bight – Upper",
                                 "Middle Bight",
                                 "Middle Bight - MB4",
                                 "Pye's Harbour",
                                 "Rat Cay Channel",
                                 "Mary Rock Channel",
                                 "Port Cay Channel",
                                 "Behring Point")), "Site2"] <- "North Bight"
  shark[which(shark$Site2 %in% c("Bahia Honda",
                                 "Sandy Key")), "Site2"] <- "Florida Keys"
  shark[which(shark$Site2 == "Gibson Channel"), "Site2"] <- "Gibson Cay"
  shark[which(shark$Site2 == "Bristol Galley dropoff"), "Site2"] <- "Bristol Galley"

  shark |>
    select(Site2, Latitude) |>
    group_by(Site2) |>
    summarise_all(mean, na.rm = TRUE) |>
    arrange(desc(Latitude))

  levels_Site2 <- c(
    "Jupiter Florida",
    "Fresh Creek",
    "Somerset",
    "High Cay",
    "Florida Keys",
    "Green Cay",
    "TOTO Navy Buoy",
    "Bristol Galley",
    "AUTEC Channel",
    "Cargill Creek",
    "North Bight",
    "Bigwood Channel",
    "Shark Hole",
    "Blackbeard's Channel",
    "Isla's Spot",
    "Gibson Cay",
    "Mangrove Cay",
    "Driggs Hill"
  )
  shark$Site2 <- factor(shark$Site2, levels = levels_Site2)


  unique(shark$Gear)
  # "Block-rig" "By-hand" "Polyball" "Handline" "Drumline-bottom" "Gillnet" "Drumline-top" "Polespear" "Dipnet"
  shark$Gear2 <- shark$Gear
  shark[which(shark$Gear2 %in% c("Handline", "By-hand", "Polespear", "Dipnet")), "Gear2"] <- "Hand"
  levels_Gear2 <- c("Hand",
                    "Polyball",
                    "Drumline-bottom",
                    "Drumline-top",
                    "Gillnet")

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

  shark %<>%
    mutate(Site3 = factor(
      case_match(
        Site2,
        c("Cargill Creek", "North Bight", "Shark Hole", "Blackbeard's Channel",  "Isla's Spot") ~ "Bight", # Make values 1 when sharks caught
        c("High Cay", "Somerset", "Fresh Creek") ~ "Central Andros East",
        c("Gibson Cay", "Mangrove Cay", "Driggs Hill") ~ "Central Cays East",
        c("Jupiter Florida", "Florida Keys") ~ "USA",
        .default = as.character(Site2)),
      levels = c("USA", "Central Andros East", "Green Cay", "TOTO Navy Buoy", "Bristol Galley", "AUTEC Channel", "Bight", "Bigwood Channel", "Central Cays East")))

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
  # 10 Silt & seagrass     10
  # 11 Sand & algae         7
  # 12 Seagrass             7

  unique(shark$Substrate)
  # Sand & seagrass   <NA>              Silt & seagrass   Patch reef        Sand & Rock       Sand              Sand & octocorals Silt              Reef              Seagrass          Hard bottom       Sand & mud        Sand & patch reef
  shark %<>%
    mutate(Substrate2 = factor(case_when(
      Substrate %in% c("Sand & patch reef", "Patch reef", "Sand & octocorals", "Sand & coral", "Reef") ~ "Reef",
      Substrate %in% c("Silt & seagrass", "Sand & seagrass", "Sand & algae", "Seagrass") ~ "Vegetation",
      Substrate %in% c("Sand & Mud", "Silt", "Sand", "Sand & Rock", "Hard bottom") ~ "Bare",
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

  unique(shark$Habitat)
  # Channel         Back reef       Pelagic         Flats           Fore reef       Blue Hole       Artificial Reef
  shark %<>%
    mutate(Habitat2 = case_when(
      Habitat == "Blue Hole" ~ "Flats",
      Habitat == "Channel" & Site2 %in% c("Blackbeard's Channel", "North Bight", "Shark Hole", "Cargill Creek") ~ "Flats", # Cargill filtered out above
      Habitat == "Channel" & Site3 %in% c("AUTEC Channel", "Bigwood Channel") ~ "Back reef",
      TRUE ~ as.character(Habitat)))



  # temporal####
  # time of day. Midpoint of timein/timeout? Will be a posix tho.
  # can use hour as a lazy proxy. or minute
  shark %<>%
    mutate(Minute = lubridate::minute(Time),
           Hour = lubridate::hour(Time),
           Yearday = lubridate::yday(Date),
           Month = lubridate::month(Date),
           Season = cut(Month,
                        breaks = c(-Inf, 2, 5, 8, 11, Inf),
                        labels = c("Winter", "Spring", "Summer", "Autumn", "Winter")))

  # Daylength
  source('~/Dropbox/Galway/Analysis/R/daylength/daylength.R')

  dl <- daylength(date = shark$Date,
                  lat = shark$Latitude,
                  lon = shark$Longitude,
                  tzs = "America/New_York")
  #dl has date lat lon sunrise sunset dawn dusk daylength, mostly "POSIXct" "POSIXt"
  shark %<>% bind_cols(dl[, 4:8]) # append sunrise sunset dawn dusk daylength
  rm(dl)
  # lunar cycle
  library(lunar) #lunar.phase
  shark %<>% mutate(LunarPhase = lunar.phase(x = Date, shift = -5, name = TRUE)) # Levels: New Waxing Full Waning

  write.csv(x = shark,
            file = paste0("../../Data/", today(), "_shark_capture_data.csv"),
            row.names = F)
  saveRDS(object = shark,
          file = paste0("../../Data/", today(), "_shark_capture_data.rds"))









  # Drumline Data ####
  drumline <- read.xlsx(xlsxFile = latestdbase,
                        sheet = "Drumline sampling data",
                        detectDates = TRUE, # fails
                        check.names = TRUE,
                        na.strings = c("NA", "xxx")) %>% # various cols are "xxx" instead of NA or blank, why?
    # cut off after last date, given there are now extra rows for vlookups to prevent data entry errors
    filter(!is.na(Date))
  drumline$Date <- as.Date(drumline$Date, origin = "1899-12-30")
  unique(drumline$Depth_m)
  drumline$Depth_m <- as.numeric(drumline$Depth_m)
  tmp <- as.POSIXct(rep(NA, nrow(drumline)), tz = "America/New_York")
  for (i in 1:nrow(drumline)) {
    tmp[i] <- convertToDateTime(drumline$Time_in[i], origin = drumline$Date[i], tz = "America/New_York")
    # convertToDateTime can't vectorise over >len1 origins
  }
  drumline$Time_in <- tmp # can't output directly to drumline$Time_in since it's numeric and thus converts the outputs
  rm(tmp)
  tmp <- as.POSIXct(rep(NA, nrow(drumline)), tz = "America/New_York")
  for (i in 1:nrow(drumline)) {
    tmp[i] <- convertToDateTime(drumline$Time_out[i], origin = drumline$Date[i], tz = "America/New_York")
    # convertToDateTime can't vectorise over >len1 origins
  }
  drumline$Time_out <- tmp # can't output directly to drumline$Time_out since it's numeric and thus converts the outputs
  rm(tmp)
  drumline$Soak_time <- drumline$Time_out - drumline$Time_in # time diff in mins
  # don't add blank rows to break up sheets
  # no adding spaces to the end of cells

  # "Bait present" = T F "bite-off"
  # Bite offs would constitute no bait thus “False”, but I think keeping records
  # of bite offs is valuable for data analysis (i.e. we’re quite sure there was a
  # shark on, we just didn’t get there in time). Thus, adding a separate column
  # for this would be good, which can also be true/false, and if true probably
  # should not be included in analyses.
  ## Done in excel sheet.
  drumline$Bait_present <- as.logical(drumline$Bait_present)
  drumline[drumline$Bite_off == "1", "Bite_off"] <- "TRUE"
  drumline$Bite_off <- as.logical(drumline$Bite_off)

  # convert things to factors with defined levels
  # convert substrate to sand grain size? Or just spatial lookup against the databases
  unique(drumline$Tide)
  # "High"     "Falling"  "Rising"   "Low"
  drumline$Tide <- factor(drumline$Tide, levels = c("Low", "Rising", "High", "Falling"))

  unique(drumline$Bait_type)
  # "Bonito"    "Ladyfish"  "Barracuda" "Blacknose" "Mackerel"
  drumline$Bait_type <- factor(drumline$Bait_type, levels = c("Bonito", "Ladyfish", "Barracuda", "Blacknose", "Mackerel"))

  unique(drumline$Substrate)
  # Same as for Shark tab, check they have the same levels e.g. hard bottom. Include all in both.

  drumline %<>%
    mutate(Substrate = factor(
      case_match(
        Substrate,
        "Seagrass & Mud" ~ "Seagrass & mud", # , "Ray Cay Channel", "Port Cay Channel", "Mary Rock Channel" # fails if present in case_match but no values in dbase
        "Silt and Seagrass" ~ "Silt and seagrass",
        .default = as.character(Substrate))))


    unique(drumline$Substrate)[which(!unique(drumline$Substrate) %in% unique(shark$Substrate))]
    # "Sand & coral" "Sand & algae"  "Seagrass & Mud"    "Silt and Seagrass" "Rock" # present in drumline not shark but that's fine, included in levels just not seen yet @ 2021-03-05
    # not included in levels 2032-12-15
    # drumline$Substrate <- factor(drumline$Substrate, levels = levels_Substrate)
    # 2023-12-15 removed for now, can't deal, not used in drumline sampling, return to this when we've had a proper reorg of data input & validation


    unique(drumline$Habitat)
    # "Fore reef" "Flats"     "Back reef" "Backreef"  "Channel"
    # Same as for Shark tab, check they have the same levels. Include all in both.
    # unique(drumline$Habitat)[which(!unique(drumline$Habitat) %in% unique(shark$Habitat))] # character(0)
    drumline$Habitat <- factor(drumline$Habitat, levels = levels_Habitat)

    unique(drumline$Site)
    # Same as for Shark tab, check they have the same levels. Include all in both.
    # "Bigwood Channel"      "Middle Bight"         "AUTEC Channel"        "Isla's Spot"
    # "Blackbeard's Channel" "Cargill Creek"        "Bristol Galley"       "Green Cay"
    # "AUTEC Channel Reef"   "Middle Bight - Upper" "Middle Bight - MB4"   "North Bight - Upper"
    # "Gibson Channel"       "North Bight"          "High Cay"             "Shark Hole"
    # "AUTEC Reef"           "Somerset reef"        "Pye's Harbour"        "Cargill creek"
    # "Mary Rock Channel"    "Port Cay Channel"     "Rat Cay Channel"
    # "Driggs Hill"          "Mangrove Cay"         "Gibson Cay"

    drumline$Site2 <- drumline$Site
    drumline[which(drumline$Site2 == "Somerset reef"), "Site2"] <- "Somerset"
    drumline[which(drumline$Site2 == "AUTEC Channel Reef"), "Site2"] <- "AUTEC Channel"
    drumline[which(drumline$Site2 == "Princess Anne Wreck"), "Site2"] <- "Jupiter Florida"
    drumline[which(drumline$Site2 == "AUTEC Reef"), "Site2"] <- "AUTEC Channel"
    drumline[which(drumline$Site2 %in% c("North Bight - Upper",
                                         "Middle Bight - Upper",
                                         "Middle Bight",
                                         "Middle Bight - MB4",
                                         "Pye's Harbour",
                                         "Rat Cay Channel",
                                         "Mary Rock Channel",
                                         "Port Cay Channel",
                                         "Behring Point")), "Site2"] <- "North Bight"
    drumline[which(drumline$Site2 %in% c("Bahia Honda",
                                         "Sandy Key")), "Site2"] <- "Florida Keys"
    drumline[which(drumline$Site2 == "Gibson Channel"), "Site2"] <- "Gibson Cay"
    drumline[which(drumline$Site2 == "Bristol Galley dropoff"), "Site2"] <- "Bristol Galley"
    # 2023-12-15 FROMHERE next line kills site2 for Rat Gibson Mangrove Isla's Blackbeards etc.
    drumline$Site2 <- factor(drumline$Site2, levels = levels_Site2)
    unique(drumline$Site2)


    unique(drumline$Bottom_top)
    # "Top"      "Bottom"   NA         "Flats"    "Backreef" "Channel"
    drumline$Bottom_top <- factor(drumline$Bottom_top, levels = c("Bottom", "Top"))


    # unique(drumline$Pit_Tag_no)
    #   sort(drumline$Pit_Tag_no)[which(duplicated(sort(drumline$Pit_Tag_no)))]
    # # "B024" # nurses subsequent days 10/11 nov 19. Same full PIT in shark tab
    # # "B05B" # both reefs, different years 15nov18 20jun19. No 15 nov 18 rows in shark dbase
    # # "F084" # recap, is ok
    # # "F087" # reef & hammerhead different 12mar20 12jul20. Same full PIT in shark tab
    # # "F092" # reefs 2 days apart 11/13 mar 20. 13 mar should be F0A2 3DD.003D44F0A2 ?
    #
    #   # 13 mar:
    #   # Caribbean reef	C. perezi	F084	3DD.003D44F084
    #   # Caribbean reef	C. perezi	F0A2	3DD.003D44F0A2
    #   # Caribbean reef	C. perezi	F066	3DD.003D44F066
    #   # Great hammerhead	S. mokarran	xxx	xxx
    #   # Caribbean reef	C. perezi	F090	3DD.003D44F090
    #   # Caribbean reef	C. perezi	F08B	3DD.003D44F08B
    #   # Caribbean reef	C. perezi	F06E	3DD.003D44F06E
    #   # Caribbean reef	C. perezi	F078	3DD.003D44F078
    #
    #
    # pitjoin <- shark %>%
    #   select(PIT_Tag_Full_ID_no) %>%
    #   mutate(Pit_Tag_no = str_sub(PIT_Tag_Full_ID_no, -4, -1))
    #
    # setDT(pitjoin) # convert to data.table without copy
    # setDT(drumline) # convert to data.table without copy
    # # join and update "df_i" by reference, i.e. without copy
    # # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
    # # If you want to return only df_nonai that have a matching df_i (i.e. rows where the key is in both tables), set the nomatch argument of data.table to 0.
    # # nomatch isn't relevant together with :=, ignoring nomatch
    # drumline[pitjoin, on = "Pit_Tag_no", PIT_Tag_Full_ID_no := i.PIT_Tag_Full_ID_no]
    #
    # pitjoin <- shark %>%
    #   select(PIT_Tag_Full_ID_no) %>%
    #   mutate(Pit_Tag_no = str_sub(PIT_Tag_Full_ID_no, -5, -1))
    #
    # drumline[pitjoin, on = "Pit_Tag_no", PIT_Tag_Full_ID_no := i.PIT_Tag_Full_ID_no]
    #
    # # copy full PITs from Pit_Tag_no to PIT_Tag_Full_ID_no
    # drumline[which(nchar(drumline$Pit_Tag_no) > 5), "PIT_Tag_Full_ID_no"] <- drumline[which(nchar(drumline$Pit_Tag_no) > 5), "Pit_Tag_no"]
    #
    # # PIT tag but no full ID
    # drumline[which(is.na(drumline$PIT_Tag_Full_ID_no)), "Pit_Tag_no"]
    # NULL = none = good
    # drop_na(drumline[which(is.na(drumline$PIT_Tag_Full_ID_no)), "Pit_Tag_no"])


    unique(drumline$Site2)
    # Bigwood Channel      North Bight          AUTEC Channel        Isla's Spot
    # Blackbeard's Channel Cargill Creek        Bristol Galley       Green Cay
    # Gibson Cay           High Cay             Shark Hole           Somerset
    # Driggs Hill          Mangrove Cay

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
      mutate(Site3 = factor(
        case_match(
          Site2,
          c("North Bight", "Shark Hole", "Blackbeard's Channel", "Cargill Creek", "Isla's Spot") ~ "Bight", # , "Ray Cay Channel", "Port Cay Channel", "Mary Rock Channel" # fails if present in case_match but no values in dbase
          c("High Cay", "Somerset", "Fresh Creek") ~ "Central Andros East",
          c("Gibson Cay", "Mangrove Cay", "Driggs Hill") ~ "Central Cays East",
          # c("Jupiter Florida", "Florida Keys") ~ "USA",
          .default = as.character(Site2)),
        levels = c("USA", "Central Andros East", "Green Cay", "TOTO Navy Buoy", "Bristol Galley", "AUTEC Channel", "Bight", "Bigwood Channel", "Central Cays East")))


    unique(drumline$Substrate)
    # "Reef"              "Sand & seagrass"   "Sand & octocorals" "Silt"
    # "Sand"              "Patch reef"        "Sand & coral"      "Hard bottom"
    # "Silt & seagrass"   "Sand & patch reef" "Sand & algae"      "Seagrass"
    # "Seagrass & mud"    "Silt and seagrass" "Rock"

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
        Substrate %in% c("Sand & patch reef", "Patch reef", "Sand & octocorals", "Sand & coral", "Reef") ~ "Reef",
        Substrate %in% c("Seagrass & mud", "Silt & seagrass", "Sand & seagrass", "Sand & algae", "Seagrass") ~ "Vegetation",
        Substrate %in% c("Sand", "Silt", "Hard bottom", "Rock") ~ "Bare",
        TRUE ~ as.character(Substrate)),
        levels = c("Bare", "Vegetation", "Reef")))
    # could then mutate Substrate2 into a factor with levels I specify

    unique(drumline$Habitat)
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


    # temporal####
    # time of day. Midpoint of timein/timeout? Will be a posix tho.
    # can use hour as a lazy proxy. or minute
    drumline %<>%
      mutate(Minute = lubridate::minute(Time_in),
             Hour = lubridate::hour(Time_in),
             Yearday = lubridate::yday(Date),
             Month = lubridate::month(Date),
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



    write.csv(x = drumline,
              file = paste0("../../Data/", today(), "_drumline_data.csv"),
              row.names = F)
    saveRDS(object = drumline,
            file = paste0("../../Data/", today(), "_drumline_data.rds"))
    rm(list = ls()) #remove all objects
    } # close runeverything


# What is the point of "summary for reports" tab in dbase?
# We should be able to autogenerate whatever this is, automatically in a simple script. LMK what it is and I'll do it.
# Can thus also automate report elements like plots etc., in a markdown. I'd be keen to do this.
# Same Q about Caribbean reef tab.

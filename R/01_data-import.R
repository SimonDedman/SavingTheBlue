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
  latestdbase <- "../../Data/SharkCapture.xlsx"
  # read xlsx & munge ####
  shark <- read.xlsx(xlsxFile = latestdbase,
                     sheet = 1,
                     detectDates = TRUE,
                     check.names = TRUE,
                     na.strings = c("NA", "xxx")) %>% # various cols are "xxx" instead of NA or blank, why?
    # cut off after last date, given there are now extra rows for vlookups to prevent data entry errors
    filter(!is.na(event_dt)) |>
    dplyr::mutate(
      dplyr::across(c(id, set_no, hook_bouy_no, acoustic_tag_serial), ~ as.integer(.x)), # force integer format
      dplyr::across(c(depth_m, temperature_C, cam_tag_length_hrs, pcl), ~ as.numeric(.x)), # force numeric format
      dplyr::across(c(lost), ~ as.logical(.x)), # force logical format # removed: fin_genetics, rbc_isotopes,
      # upper case second names / name fix
      common = stringr::str_to_title(common),
      common = stringr::str_remove(common, c(" Shark")),
      substrate = stringr::str_to_title(substrate),
      tide = stringr::str_to_title(tide)
    )

  tmp <- data.frame(
    event_ts = as.POSIXct(rep(NA, nrow(shark)), tz = "America/New_York"),
    time_in = as.POSIXct(rep(NA, nrow(shark)), tz = "America/New_York"),
    time_out = as.POSIXct(rep(NA, nrow(shark)), tz = "America/New_York")
  )
  for (i in 1:nrow(shark)) {
    tmp[i, "event_ts"] <- openxlsx::convertToDateTime(shark$event_ts[i], origin = shark$event_dt[i], tz = "America/New_York")
    tmp[i, "time_in"] <- openxlsx::convertToDateTime(shark$time_in[i], origin = shark$event_dt[i], tz = "America/New_York")
    tmp[i, "time_out"] <- openxlsx::convertToDateTime(shark$time_out[i], origin = shark$event_dt[i], tz = "America/New_York")
    # convertToDateTime can't vectorise over >len1 origins
  }
  shark$event_ts <- tmp[, "event_ts"] # can't output directly to sharks$event_ts since it's numeric and thus converts the outputs
  shark$time_in <- tmp[, "time_in"] # can't output directly to sharks$event_ts since it's numeric and thus converts the outputs
  shark$time_out <- tmp[, "time_out"] # can't output directly to sharks$event_ts since it's numeric and thus converts the outputs
  rm(tmp)
  rm(i)
  # event_ts, time_in, time_out are now POSIXct not "HH:MM:SS" character/other.
  # Can convert to hms but IDK if that's more useful?
  # shark %<>% dplyr::mutate(Time = lubridate::hms(as.character(stringr::str_sub(string = event_ts, start = 12))))



  # new data check ####
  # check for new unique data in any column before anything else runs
  # sort(unique(shark$event_dt))
  # last 10 unique dates
  lasttendates <- sort(unique(shark$event_dt))[(length(unique(shark$event_dt)) - 10):(length(unique(shark$event_dt)))]

  lastten <- (length(sort(unique(shark$event_dt))) - 10):length(sort(unique(shark$event_dt)))
  lastten <- shark |> dplyr::filter(event_dt %in% sort(unique(shark$event_dt))[lastten])

  checkcols <- c("gear", "site", "habitat", "substrate", "tide", "common", "species", "sex")

  for (thiscol in checkcols) { # thiscol <- checkcols[1]   # thiscol <- "site"
    print(thiscol)
    if (!all((shark |> filter(event_dt %in% lasttendates) |> select(all_of(thiscol)) |> unique() |> pull()) %in% (shark |> filter(!event_dt %in% lasttendates) |> select(all_of(thiscol)) |> unique() |> pull()))) {
      # FROM HERE ####
      # Is this complete? Add new data & check
      print(paste0("Present in last 10 dates, absent before: ", sort(unique(lastten[, thiscol]))[which(!sort(unique(lastten[, thiscol])) %in% sort(unique(shark[, thiscol])))]))
    } # close if
  } # close for

  # last 10 dates values for this col
  shark |> filter(event_dt %in% lasttendates) |> select(all_of(thiscol)) |> unique() |> pull()

  # TODO lat lon depth outside range ####
  # lat lon outside existing range?
  # ditto depth


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
  # shark$stl[which(shark$stl == ">200")] <- NA # none left
  # shark$stl <- as.numeric(shark$stl)

  # Fin_genetics Fin_isotopes Muscle_isotopes: TRUE FALSE "1-day recap"
  # This indicates that samples were not collected because the shark was captured
  # the day before, so these are “False”.
  # shark$Fin_genetics[which(shark$Fin_genetics == "1-day recap")] <- FALSE
  # shark$Fin_isotopes[which(shark$Fin_isotopes == "1-day recap")] <- FALSE
  # shark$Muscle_isotopes[which(shark$Muscle_isotopes == "1-day recap")] <- FALSE # now done in dbase
  # shark$Fin_genetics <- as.logical(shark$Fin_genetics)
  # shark$Fin_isotopes <- as.logical(shark$Fin_isotopes)
  # shark$Muscle_isotopes <- as.logical(shark$Muscle_isotopes)

  # I think adding a recapture column would be beneficial, and also a shark # column,
  # the latter starting at 1 and continuing indefinitely as we sample more sharks. [done in excel sheet]
  # The recapture column could then include the shark number from when it was last caught.
  # Other options are also possible, but good to have data on recaps for growth rates
  # and changes in distributions as we continue to catch more of them.

  # Depth has "<1" (& xxx)
  # PM: These depth relate to gill net sampling with BAMSI – Tristan can provide
  # shark$depth_m[which(shark$depth_m == "<1")] <- 0.5
  # shark$depth_m <- as.numeric(shark$depth_m)



  ## Factorial columns ####
  shark$sex <- factor(shark$sex, levels = c("F", "M"))


  ## Default NA/blanks to FALSE ####
  shark <- shark |>
    dplyr::mutate(
      dplyr::across(c(ultrasound,
                      fin_genetics,
                      fin_isotopes,
                      muscle_isotopes,
                      whole_blood_isotopes,
                      plasma_isotopes,
                      rbc_isotopes,
                      external_tag_recap,
                      stb_recap,
                      estimate,
                      lost,
                      cut_hook,
                      not_standardised,
                      depredation),~ ifelse(is.na(.x), FALSE, .x)))


  ## species names levels ####
  sort(unique(shark$species))
  # 2024-12-16
  # "Alopias vulpinus"           "Caranx latus"               "Carcharhinus acronotus"     "Carcharhinus brevipinna"    "Carcharhinus falciformis"   "Carcharhinus leucas"        "Carcharhinus limbatus"      "Carcharhinus obscurus"      "Carcharhinus perezii"       "Carcharhinus plumbeus"
  # "Carcharhinus sorrah"        "Galeocerdo cuvier"          "Ginglymostoma cirratum"     "Hypanus americanus"         "Isurus oxyrinchus"          "Lutjanus analis"            "Lutjanus cyanopterus"       "Negaprion brevirostris"     "Remora"                     "Rhizoprionodon terraenovae"
  # "Serranidae"                 "Sphyraena barracuda"        "Sphyrna lewini"             "Sphyrna mokarran"           "Sphyrna zygaena"            "Torpedo fuscomaculata"

  # PM: pelagic vs inshore then size then rays makes the most sense:
  levels_species <- c("Carcharhinus falciformis", "Carcharhinus obscurus", "Alopias vulpinus", "Isurus oxyrinchus", # silky (Carcharhinus falciformis) and dusky (Carcharhinus obscurus) and common thresher (Alopias vulpinus), Shortfin mako shark
                      "Carcharhinus acronotus", "Rhizoprionodon terraenovae", # blacknose (Carcharhinus acronotus) and sharpnose (Rhizoprionodon terraenovae)
                      "Carcharhinus brevipinna", "Sphyrna lewini", "Sphyrna zygaena", # Spinner C. brevipinna, scalloped hammer, smooth hammer
                      "Carcharhinus limbatus", "Carcharhinus perezii", "Negaprion brevirostris", "Ginglymostoma cirratum", # blacktip (Carcharhinus limbatus), reef (Carcharhinus perezi), lemon (Negaprion brevirostris),  nurse (Ginglymostoma cirratum)
                      "Carcharhinus plumbeus", "Galeocerdo cuvier", "Carcharhinus leucas", "Sphyrna mokarran", # sandbar ("C. plumbeus"), tiger (Galeocerdo cuvier), bull (Carcharhinus leucas), hammerhead (Sphyrna mokarran)
                      "Hypanus americanus", # Hypanus americanus = southern stingray
                      "Caranx latus", "Lutjanus analis", "Lutjanus cyanopterus", "Remora", "Serranidae", "Sphyraena barracuda") # Horse-eye jack, Mutton Snapper, Cuberra Snapper, Remora, Grouper, Barracuda

  # warn user if new species aren't in the levels list
  if (!length(sort(unique(shark$species))[which(!sort(unique(shark$species)) %in% levels_species)]) == 0) stop(paste0("New species found which aren't present in levels_species; add the following to levels_species: ", sort(unique(shark$species))[which(!sort(unique(shark$species)) %in% levels_species)]))

  # species we've caught which are not present in the levels:
  # sort(unique(shark$species))[which(!sort(unique(shark$species)) %in% levels_species)]
  # Get their common names
  # dplyr::left_join(
  #   x = data.frame(species = sort(unique(shark$species))[which(!sort(unique(shark$species)) %in% levels_species)]),
  #   y = shark |> group_by(species) |> summarise(common = first(common))
  #   ) |> pull(common)

  shark$species <- factor(shark$species, levels = levels_species)

  ## common names levels ####
  sort(unique(shark$common))
  # "Nurse"                "Caribbean reef"       "Silky"                "Lemon"                "Blacknose"            "Blacktip"             "Tiger"                "Dusky"
  # "Bull"                 "Sharpnose"            "Great hammerhead"     "Spinner"              "Southern"             "Sandbar"              "Scalloped Hammerhead" "Smooth hammerhead"
  # "Atlantic sharpnose"

  # create common levels from species lookup
  levels_common <- dplyr::left_join(
    x = data.frame(species = levels_species),
    y = shark |> dplyr::group_by(species) |> dplyr::summarise(common = dplyr::first(common))
  ) |> dplyr::pull(common)

  shark$common <- factor(shark$common, levels = levels_common)




  ## site2 ####
  shark$site2 <- shark$site
  sort(unique(shark$site2))
  # "AUTEC Channel"                     "AUTEC Channel Reef"                "AUTEC Reef"                        "Bahia Honda"                       "Behring Point"                     "Bigwood Channel"                   "Blackbeard's Channel"              "Bristol Galley"
  # "Bristol Galley dropoff"            "Cabo San Lucas - Offshore"         "Cargill Creek"                     "Driggs Hill"                       "Fresh Creek"                       "Gibson Cay"                        "Gibson Channel"                    "Gordo Banks - Outer Bank"
  # "Green Cay"                         "High Cay"                          "Isla's Spot"                       "Jupiter Florida"                   "Mangrove Cay"                      "Mary Rock Channel"                 "NEW_ENTRY"                         "North Bight"
  # "North Bight â€“ Upper"             "Port Cay Channel"                  "Princess Anne Wreck"               "Pye's Harbour"                     "Rat Cay Channel"                   "Salvador Channel"                  "Sandy Key"                         "Shark Hole"
  # "Somerset"                          "Somerset reef"                     "South of Cerralvo Island - La Paz" "TOTO Navy Buoy"

  shark[which(shark$site2 == "Somerset reef"), "site2"] <- "Somerset"
  shark[which(shark$site2 %in% c("AUTEC Channel Reef", "AUTEC Reef")), "site2"] <- "AUTEC Channel"
  shark[which(shark$site2 == "Princess Anne Wreck"), "site2"] <- "Jupiter Florida"
  shark[which(shark$site2 %in% c("North Bight – Upper",
                                 "Middle Bight – Upper",
                                 "Middle Bight",
                                 "Middle Bight - MB4",
                                 "Pye's Harbour",
                                 "Rat Cay Channel",
                                 "Mary Rock Channel",
                                 "Port Cay Channel",
                                 "Behring Point")), "site2"] <- "North Bight"
  shark[which(shark$site2 %in% c("Bahia Honda",
                                 "Sandy Key")), "site2"] <- "Florida Keys"
  shark[which(shark$site2 == "Gibson Channel"), "site2"] <- "Gibson Cay"
  shark[which(shark$site2 == "Bristol Galley dropoff"), "site2"] <- "Bristol Galley"

  # mean latitude by site2, N to S, for checking
  shark |>
    select(site2, latitude) |>
    group_by(site2) |>
    summarise_all(mean, na.rm = TRUE) |>
    arrange(desc(latitude)) |>
    print(n = 22)

  levels_site2 <- c(
    "Jupiter Florida",
    "Fresh Creek",
    "Somerset",
    "High Cay",
    "Florida Keys",
    "Green Cay",
    "TOTO Navy Buoy",
    "Salvador Channel",
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
    "Driggs Hill",
    "South of Cerralvo Island - La Paz",
    "Gordo Banks - Outer Bank",
    "Cabo San Lucas - Offshore"
  )
  # warn user if new substrate aren't in the levels list
  if (!length(sort(unique(shark$site2))[which(!sort(unique(shark$site2)) %in% levels_site2)]) == 0) stop(paste0("New site(s) found which aren't present in levels_site2; add the following to levels_site2: ", sort(unique(shark$site2))[which(!sort(unique(shark$site2)) %in% levels_site2)]))
  # sort(unique(shark$site2))[which(!sort(unique(shark$site2)) %in% levels_site2)]


  shark |>
    group_by(site2) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    print(n = 22)
  # 01 Bigwood Channel                    1222
  # 02 Green Cay                           778
  # 03 Shark Hole                          620
  # 04 Jupiter Florida                     308
  # 05 AUTEC Channel                       286
  # 06 North Bight                         268
  # 07 Bristol Galley                      219
  # 08 Cargill Creek                       167
  # 09 Gibson Cay                          137
  # 10 TOTO Navy Buoy                      110
  # 11 Blackbeard's Channel                102
  # 12 High Cay                             66
  # 13 Somerset                             35
  # 14 Salvador Channel                     33
  # 15 Gordo Banks - Outer Bank             20
  # 16 Driggs Hill                          13
  # 17 Isla's Spot                          12
  # 18 Florida Keys                          8
  # 19 Mangrove Cay                          7
  # 20 South of Cerralvo Island - La Paz     2
  # 21 Fresh Creek                           1
  # 22 Cabo San Lucas - Offshore             1




  ## site3 ####
  shark %<>%
    mutate(site3 = factor(
      case_match(
        site2,
        c("Jupiter Florida", "Florida Keys") ~ "USA",
        c("High Cay", "Somerset", "Fresh Creek") ~ "Central Andros East",  # Make values 1 when sharks caught
        # Green Cay
        # TOTO Navy Buoy
        c("Bristol Galley", "AUTEC Channel", "Salvador Channel") ~ "East Andros Cays",
        c("Cargill Creek", "North Bight", "Shark Hole", "Blackbeard's Channel",  "Isla's Spot") ~ "Bight",
        # Bigwood Channel
        c("Gibson Cay", "Mangrove Cay", "Driggs Hill") ~ "Central Cays East",
        c("South of Cerralvo Island - La Paz", "Gordo Banks - Outer Bank", "Cabo San Lucas - Offshore ") ~ "Baja California",
        .default = as.character(site2)),
      levels = c("USA", "Central Andros East", "Green Cay", "TOTO Navy Buoy", "East Andros Cays", "Bight", "Bigwood Channel", "Central Cays East", "Baja California")))

  # site2 needs to be character for site3 creation, factorise after:
  shark$site2 <- factor(shark$site2, levels = levels_site2)




  ## habitat ####
  shark |>
    group_by(habitat) |>
    summarise(n = n()) |>
    arrange(desc(n))
  # Back reef        1774
  # Flats            1464
  # Fore reef         451
  # Pelagic           440
  # Channel           172
  # Blue Hole         105
  # Artificial Reef     9
  levels_habitat <- c("Flats", "Channel", "Artificial Reef", "Back reef", "Blue Hole", "Fore reef", "Pelagic")
  if (!length(sort(unique(shark$habitat))[which(!sort(unique(shark$habitat)) %in% levels_habitat)]) == 0) stop(paste0("New habitat(s) found which aren't present in levels_substrate; add the following to levels_site2: ", sort(unique(shark$habitat))[which(!sort(unique(shark$habitat)) %in% levels_habitat)]))
  shark$habitat <- factor(shark$habitat, levels = levels_habitat)




  ## habitat2 ####
  # point of this? simplify categories, change from Channel to Flats/Back reef
  # shark |> group_by(habitat, site2) |> summarise(n = n()) |> arrange(habitat, site2) |> print(n = 50)
  shark %<>%
    mutate(habitat2 = case_when(
      habitat == "Blue Hole" ~ "Flats",
      habitat == "Channel" & site2 %in% c("Blackbeard's Channel", "North Bight", "Shark Hole", "Cargill Creek") ~ "Flats", # Cargill filtered out above
      habitat == "Channel" & site3 %in% c("AUTEC Channel", "Bigwood Channel") ~ "Back reef",
      TRUE ~ as.character(habitat)))
  # shark |> group_by(habitat, habitat2, site2) |> summarise(n = n()) |> arrange(habitat, habitat2, site2) |> print(n = 50)


  ## substrate ####
  sort(unique(shark$substrate))
  # "Hard Bottom"       "Patch Reef"        "Reef"              "Sand"              "Sand & Coral"      "Sand & Mud"        "Sand & Octocorals" "Sand & Patch Reef" "Sand & Rock"       "Sand & Seagrass"   "Seagrass"
  # "Seagrass & Silt"   "Silt"

  # PM: will be good to try and match with
  # sand, silt/mud, or rock
  # seagrass, algae, coral
  # coral: soft, hard
  levels_substrate <- c("Sand & Mud", "Silt", "Sand", "Sand & Rock", "Hard Bottom",
                        "Seagrass & Silt", "Sand & Seagrass", "Seagrass", # 2023-12-15 removed "Sand & Algae", not present
                        "Sand & Coral", "Sand & Patch Reef", "Sand & Octocorals", "Patch Reef", "Reef", # 2021-10-05 removed "Coral Reef", dupe of Reef, in dbase. 2023-12-15 removed "Sand & coral" not present, added NA (pelagic)
                        "Pelagic")  # added Pelagic

  # warn user if new substrate aren't in the levels list
  if (!length(sort(unique(shark$substrate))[which(!sort(unique(shark$substrate)) %in% levels_substrate)]) == 0) stop(paste0("New substrate(s) found which aren't present in levels_substrate; add the following to levels_site2: ", sort(unique(shark$substrate))[which(!sort(unique(shark$substrate)) %in% levels_substrate)]))
  # sort(unique(shark$substrate))[which(!sort(unique(shark$substrate)) %in% levels_substrate)]
  shark$substrate <- factor(shark$substrate, levels = levels_substrate)




  ## substrate2 ####
  shark |>
    group_by(substrate) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    print(n = 20)
  # Sand & Seagrass    1182
  # Sand                616
  # Patch Reef          530
  # Seagrass & Silt     447
  # Sand & Patch Reef   435
  # NA                  359 # remove NAs in excel
  # Seagrass            356
  # Reef                162
  # Sand & Coral        131
  # Sand & Rock         113
  # Hard Bottom          42
  # Silt                 38
  # Sand & Mud            2
  # Sand & Octocorals     2

  shark %<>%
    mutate(substrate2 = factor(case_when(
      substrate %in% c("Sand & Patch Reef", "Patch Reef", "Sand & Octocorals", "Sand & Coral", "Reef") ~ "Reef",
      substrate %in% c("Seagrass & Silt", "Sand & Seagrass", "Sand & Algae", "Seagrass") ~ "Vegetation",
      substrate %in% c("Sand & Mud", "Silt", "Sand", "Sand & Rock", "Hard Bottom") ~ "Bare",
      TRUE ~ as.character(substrate)),
      levels = c("Bare", "Vegetation", "Reef")))




  ## gear ####
  sort(unique(shark$gear))
  # "Block-rig" "By-hand" "Dipnet" "Drumline-bottom" "Drumline-top" "Gillnet" "Handline" "Polespear" "Polyball"
  shark$gear2 <- shark$gear
  shark[which(shark$gear2 %in% c("Handline", "By-hand", "Polespear", "Dipnet")), "gear2"] <- "Hand"
  shark[which(shark$gear2 %in% c("Block-rig")), "gear2"] <- "Drumline-top"
  sort(unique(shark$gear2))
  levels_gear2 <- c("Hand",
                    "Polyball",
                    "Drumline-bottom",
                    "Drumline-top",
                    "Gillnet")
  if (!length(sort(unique(shark$gear2))[which(!sort(unique(shark$gear2)) %in% levels_gear2)]) == 0) stop(paste0("New site(s) found which aren't present in levels_site2; add the following to levels_site2: ", sort(unique(shark$gear2))[which(!sort(unique(shark$gear2)) %in% levels_gear2)]))

  # sort(unique(shark$gear2))[which(!sort(unique(shark$gear2)) %in% levels_gear2)]
  shark$gear2 <- factor(shark$gear2, levels = levels_gear2)



  ## drumline bottom-top ####
  # unique(shark$bottom_top) # doesn't exist any more
  # # "Top"      "Bottom"   NA         "Flats"    "Backreef" "Channel"
  # shark$Bottom_top <- factor(shark$Bottom_top, levels = c("Bottom", "Top"))




  ## bait type ####
  unique(shark$bait_type)
  # "Bonito"    "Ladyfish"  "Barracuda" "Blacknose" "Mackerel"
  shark$bait_type <- factor(shark$bait_type, levels = c("Bonito", "Ladyfish", "Barracuda", "Blacknose", "Mackerel", NA))




  ## temporal####
  ### times of day ####
  # time of day. Midpoint of timein/timeout? Will be a posix tho.
  # can use hour as a lazy proxy. or minute
  shark %<>%
    dplyr::mutate(Minute = lubridate::minute(event_ts),
                  Hour = lubridate::hour(event_ts),
                  Yearday = lubridate::yday(event_ts),
                  Month = lubridate::month(event_ts),
                  Season = cut(Month,
                               breaks = c(-Inf, 2, 5, 8, 11, Inf),
                               labels = c("Winter", "Spring", "Summer", "Autumn", "Winter")))




  ### daylength ####
  source('~/Dropbox/Galway/Analysis/R/daylength/daylength.R')

  dl <- daylength(date = shark$event_dt,
                  lat = shark$latitude,
                  lon = shark$longitude,
                  tzs = "America/New_York")
  #dl has date lat lon sunrise sunset dawn dusk daylength, mostly "POSIXct" "POSIXt"
  shark %<>% bind_cols(dl[, 4:8]) # append sunrise sunset dawn dusk daylength
  rm(dl)




  ### lunar cycle ####
  library(lunar) #lunar.phase
  shark %<>% mutate(LunarPhase = lunar.phase(x = event_dt, shift = -5, name = TRUE)) # Levels: New Waxing Full Waning




  ### tidal cycle ####
  unique(shark$tide)
  # "High"     "Falling"  "Rising"   "Low"
  shark$tide <- factor(shark$tide, levels = c("Low", "Rising", "High", "Falling", NA))
  # REMOVE NA WHEN FIXED i.e. all tides populated in sheet ####

  # Lookup NA tide against XYT ####
  # install.packages("earthtide")
  # library(earthtide)
  # tmp <- shark |> dplyr::filter(is.na(tide))
  # tmp2 <- apply(X = tmp,
  #               MARGIN = 1, # rows
  #               FUN = function(x) earthtide::calc_earthtide(utc = x$event_ts,
  #                                                           latitude = tmp$latitude,
  #                                                           longitude = tmp$longitude)
  # )
  #
  # tms <- as.POSIXct("1990-01-01", tz = "UTC") + c(0, 3600)
  # wave_groups <- data.frame(start = 0, end = 8, multiplier = 1.5)
  #
  # et <- calc_earthtide(
  #   utc = tms,
  #   do_predict = TRUE,
  #   method = c("tidal_potential", "lod_tide", "pole_tide"),
  #   latitude = 52.3868,
  #   longitude = 9.7144,
  #   elevation = 110,
  #   gravity = 9.8127,
  #   cutoff = 1.0e-5,
  #   catalog = "ksm04",
  #   wave_groups = wave_groups,
  #   n_thread = 1
  # )
  #
  # install.packages("phenology")
  # library(phenology)
  # phenology::getTide(
  #   year = lubridate::year(shark$event_ts),
  #   latitude = shark$latitude,
  #   longitude = shark$longitude,
  #   tz = "America/New_York"
  #   )
  #
  # install.packages("HelpersMG")
  # library(HelpersMG)
  # HelpersMG::tide.info(
  #   year = lubridate::year(shark$event_ts[1]),
  #   latitude = shark$latitude[1],
  #   longitude = shark$longitude[1]
  # )




  # Save objects ####
  write.csv(x = shark |> dplyr::filter(!gear %in% c("Drumline-top", "Drumline-bottom")),
            file = paste0("../../Data/", today(), "_shark_capture_data.csv"),
            row.names = F)
  saveRDS(object = shark |> dplyr::filter(!gear %in% c("Drumline-top", "Drumline-bottom")),
          file = paste0("../../Data/", today(), "_shark_capture_data.rds"))
  rm(list = ls()) #remove all objects
} # close runeverything


# FINAL CHECK SST VALIDITY EXCEL ####
# Tide populate from XYT vital/anyone
# depth=NA in sheet, map interpolate those values into excel
# New data check
# remove old code

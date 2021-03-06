# Excel data import, processing, save
# Simon Dedman simondedman@gmail.com
# 2021-03-03

# install.packages("openxlsx")
library(openxlsx)
library(stringr)
library(magrittr)
library(data.table)
library(lubridate)
library(tidylog) # verbose version of tidyverse
# getwd() # Saving The Blue/Code/SavingTheBlue

latestdbase <- "../../Data/Database_2021-03-20.xlsx"
# Shark Data####
shark <- read.xlsx(xlsxFile = latestdbase,
                   sheet = 1,
                   detectDates = TRUE, # fails
                   check.names = TRUE,
                   na.strings = c("NA", "xxx")) # various cols are "xxx" instead of NA or blank, why?
# date wrong, is excel format
shark$Date <- as.Date(shark$Date, origin = "1899-12-30")
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
# "S. mokarran"
# Will need to have something that warns us if there are new species which aren't in the existing levels list. Ditto for all of these.

# PM: pelagic vs inshore then size makes the most sense:
levels_Species <- c("C. falciformis", "C. obscurus", # silky (Carcharhinus falciformis) and dusky (Carcharhinus obscurus)
                    "C. acronotus", "R. terraenovae", # blacknose (Carcharhinus acronotus) and sharpnose (Rhizoprionodon terraenovae)
                    "C. limbatus", "C. perezi", "G. cirratum", "N. brevirostris", # blacktip (Carcharhinus limbatus), reef (Carcharhinus perezi), and nurse (Ginglymostoma cirratum)
                    "G. cuvier", "C. leucas", "S. mokarran") # tiger (Galeocerdo cuvier), bull (Carcharhinus leucas), hammerhead (Sphyrna mokarran)
shark$Species <- factor(shark$Species, levels = levels_Species)

unique(shark$Common)
levels_Common <- c("Silky", "Dusky", # silky (Carcharhinus falciformis) and dusky (Carcharhinus obscurus)
                   "Blacknose", "Sharpnose", # blacknose (Carcharhinus acronotus) and sharpnose (Rhizoprionodon terraenovae)
                   "Blacktip", "Caribbean reef", "Lemon", "Nurse", # blacktip (Carcharhinus limbatus), reef (Carcharhinus perezi), and nurse (Ginglymostoma cirratum)
                   "Tiger", "Bull", "Great hammerhead") # tiger (Galeocerdo cuvier), bull (Carcharhinus leucas), hammerhead (Sphyrna mokarran)
shark$Common <- factor(shark$Common, levels = levels_Common)



unique(shark$Substrate)
# PM: will be good to try and match with
# sand, silt/mud, or rock
# seagrass, algae, coral
# coral: soft, hard
levels_Substrate <- c("Sand & mud", "Silt", "Sand", "Sand & Rock", "Hard bottom",
                      "Silt & seagrass", "Sand & algae", "Sand & seagrass", "Seagrass",
                      "Sand & patch reef", "Sand & coral", "Sand & octocorals", "Patch reef", "Coral reef", "Reef")
shark$Substrate <- factor(shark$Substrate, levels = levels_Substrate)


unique(shark$Habitat)
levels_Habitat <- c("Flats", "Channel", "Back reef", "Blue Hole", "Fore reef", "Pelagic")
shark$Habitat <- factor(shark$Habitat, levels = levels_Habitat)


shark$Site2 <- shark$Site
unique(shark$Site2)
shark[which(shark$Site2 == "Somerset reef"), "Site2"] <- "Somerset"
shark[which(shark$Site2 == "AUTEC Channel Reef"), "Site2"] <- "AUTEC Channel"
shark[which(shark$Site2 %in% c("North Bight – Upper",
                               "Middle Bight – Upper",
                               "Middle Bight",
                               "Middle Bight - MB4",
                               "Behring Point")), "Site2"] <- "North Bight"
shark[which(shark$Site2 == "Gibson Channel"), "Site2"] <- "Gibson Cay"
levels_Site2 <- c("Fresh Creek",
                  "Somerset",
                  "High Cay",
                  "Green Cay",
                  "Bristol Galley",
                  "AUTEC Channel",
                  "Cargill Creek",
                  "North Bight",
                  "Shark Hole",
                  "Isla's Spot",
                  "Bigwood Channel",
                  "TOTO Navy Buoy",
                  "Blackbeard's Channel",
                  "Gibson Cay")
shark$Site2 <- factor(shark$Site2, levels = levels_Site2)

# toadd####
# Cargill Creek, Gibson Channel


unique(shark$Gear)
# "Block-rig" "By-hand" "Polyball" "Handline" "Drumline-bottom" "Gillnet" "Drumline-top"
shark$Gear2 <- shark$Gear
shark[which(shark$Gear2 %in% c("Handline", "By-hand")), "Gear2"] <- "Hand"
# fromhere which blockrig####
levels_Gear2 <- c("Hand",
                  "Polyball",
                  "Drumline-bottom",
                  "Drumline-top",
                  "Gillnet")


write.csv(x = shark,
          file = paste0("../../Data/", today(), "_shark_capture_data.csv"),
          row.names = F)
saveRDS(object = shark,
        file = paste0("../../Data/", today(), "_shark_capture_data.rds"))


# Drumline Data####

drumline <- read.xlsx(xlsxFile = latestdbase,
                      sheet = 2,
                      detectDates = TRUE, # fails
                      check.names = TRUE,
                      na.strings = c("NA", "xxx")) # various cols are "xxx" instead of NA or blank, why?
drumline$Date <- as.Date(drumline$Date, origin = "1899-12-30")
drumline$Depth_m <- as.numeric(drumline$Depth_m)

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
drumline$Bite_off <- as.logical(drumline$Bite_off)
unique(drumline$Depth_m)

# convert things to factors with defined levels
# convert substrate to sand grain size? Or just spatial lookup against the databases
unique(drumline$Tide)
# "High"     "Falling"  "Rising"   "Low"      "Bonito"   NA         "Incoming"
drumline$Tide <- factor(drumline$Tide, levels = c("Low", "Rising", "High", "Falling"))

unique(drumline$Bait_type)
# "Bonito"    "Ladyfish"  "Barracuda" "Blacknose" NA
drumline$Bait_type <- factor(drumline$Bait_type, levels = c("Bonito", "Ladyfish", "Barracuda", "Blacknose"))

unique(drumline$Substrate)
# Same as for Shark tab, check they have the same levels e.g. hard bottom. Include all in both.
unique(drumline$Substrate)[which(!unique(drumline$Substrate) %in% unique(shark$Substrate))]
# "Sand & coral" "Sand & algae" # present in drumline not shark but that's fine, included in levels just not seen yet @ 2021-03-05
drumline$Substrate <- factor(drumline$Substrate, levels = levels_Substrate)


unique(drumline$Habitat)
# Same as for Shark tab, check they have the same levels. Include all in both.
# unique(drumline$Habitat)[which(!unique(drumline$Habitat) %in% unique(shark$Habitat))] # character(0)
drumline$Habitat <- factor(drumline$Habitat, levels = levels_Habitat)

unique(drumline$Site)
# Same as for Shark tab, check they have the same levels. Include all in both.

drumline$Site2 <- drumline$Site
unique(drumline$Site2)
drumline[which(drumline$Site2 == "Somerset reef"), "Site2"] <- "Somerset"
drumline[which(drumline$Site2 == "AUTEC Channel Reef"), "Site2"] <- "AUTEC Channel"
drumline[which(drumline$Site2 == "AUTEC Reef"), "Site2"] <- "AUTEC Channel"
drumline[which(drumline$Site2 %in% c("North Bight - Upper",
                                     "Middle Bight - Upper",
                                     "Middle Bight",
                                     "Middle Bight - MB4",
                                     "Behring Point")), "Site2"] <- "North Bight"
drumline[which(drumline$Site2 == "Gibson Channel"), "Site2"] <- "Gibson Cay"
drumline$Site2 <- factor(drumline$Site2, levels = levels_Site2)

unique(drumline$Bottom_top)
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


write.csv(x = drumline,
          file = paste0("../../Data/", today(), "_drumline_data.csv"),
          row.names = F)
saveRDS(object = drumline,
        file = paste0("../../Data/", today(), "_drumline_data.rds"))
rm(list = ls()) #remove all objects


# What is the point of "summary for reports" tab in dbase?
# We should be able to autogenerate whatever this is, automatically in a simple script. LMK what it is and I'll do it.
# Can thus also automate report elements like plots etc., in a markdown. I'd be keen to do this.
# Same Q about Caribbean reef tab.

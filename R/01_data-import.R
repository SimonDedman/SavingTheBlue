# Excel data import, processing, save
# Simon Dedman simondedman@gmail.com
# 2021-03-03

# install.packages("openxlsx")
library(openxlsx)
library(tidylog) # verbose version of tidyverse
# getwd() # Saving The Blue/Code/SavingTheBlue


# Shark Data####
shark <- read.xlsx(xlsxFile = "../../Data/Shark capture data_NEW_Jan 2021_UPDATED.xlsx",
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
shark$Fin_genetics[which(shark$Fin_genetics == "1-day recap")] <- FALSE
shark$Fin_isotopes[which(shark$Fin_isotopes == "1-day recap")] <- FALSE
shark$Muscle_isotopes[which(shark$Muscle_isotopes == "1-day recap")] <- FALSE
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
#TODO####
shark$Depth_m <- as.numeric(shark$Depth_m) # NAs introduced by coercion because of the above

# Factorial columns
shark$Sex <- factor(shark$Sex,levels = c("F", "M"))
unique(shark$Species)
# "G. cirratum"     "C. perezi"       "C. falciformis"  "N. brevirostris" "C. acronotus"
# "C. limbatus"     "G. cuvier"       "C. obscurus"     "C. leucas"       "R. terraenovae"
# "S. mokarran"
# Will need to have something that warns us if there are new species which aren't in the existing levels list. Ditto for all of these.

unique(shark$Substrate)
# "Sand & seagrass"   NA  "Silt & seagrass"  "Patch reef" "Sand & Rock" "Sand" "Sand & octocorals" "Silt" "Reef"
# "Seagrass" "Hard bottom" "Sand & mud" "Coral reef" "Sand & patch reef"

unique(shark$Habitat)
# "Channel"    "Back reef"  "Pelagic"    "Flats"      "Fore reef"  "Blue Hole "

unique(shark$Site)
# "Gibson Cay" "Green Cay" "TOTO Navy Buoy" "Blackbeard's Channel" "Bigwood Channel" "Fresh Creek"
# "Middle Bight" "AUTEC Channel" "Isla's Spot" "Bristol Galley" "AUTEC Channel Reef" "Middle Bight - upper"
# "Middle Bight - reciever MB4" "Shark Hole" "Behring Point" "Somerset" "North Bight-upper" "High Cay"
# "Somerset reef"

unique(shark$Gear)
# "Block-rig" "By-hand" "Polyball" "Handline" "Drumline-bottom" "Gillnet" "Drumline-top"



# Drumline Data####

drumline <- read.xlsx(xlsxFile = "../../Data/Shark capture data_NEW_Jan 2021_UPDATED.xlsx",
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

unique(drumline$Substrate)
# Same as for Shark tab, check they have the same levels e.g. hard bottom. Include all in both.
unique(drumline$Habitat)
# Same as for Shark tab, check they have the same levels. Include all in both.
unique(drumline$Site)
# Same as for Shark tab, check they have the same levels. Include all in both.
unique(drumline$Bottom_top)
drumline$Bottom_top <- factor(drumline$Bottom_top, levels = c("Bottom", "Top"))

# What is the point of "summary for reports" tab in dbase?
# We should be able to autogenerate whatever this is, automatically in a simple script. LMK what it is and I'll do it.
# Can thus also automate report elements like plots etc., in a markdown. I'd be keen to do this.
# Same Q about Caribbean reef tab.

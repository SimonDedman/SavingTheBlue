# Excel data import, processing, save
# Simon Dedman simondedman@gmail.com
# 2021-03-03
# install.packages("openxlsx")
library(openxlsx)
library(tidylog) # verbose version of tidyverse
# getwd() # Saving The Blue/Code/SavingTheBlue
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
# STL has ">200"
# isotopes: TRUE FALSE "1-day recap"
# Depth has "<1" (& xxx)
shark$Depth_m <- as.numeric(shark$Depth_m) # NAs introduced by coercion because of the above

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

unique(drumline$Depth..m.)

# convert things to factors with defined levels
# convert substrate to sand grain size? Or just spatial lookup against the databases


# What is the point of "summary for reports" tab in dbase?
# We should be able to autogenerate whatever this is, automatically in a simple script. LMK what it is and I'll do it.
# Can thus also automate report elements like plots etc., in a markdown. I'd be keen to do this.
# Same Q about Caribbean reef tab.

# 2021-10-11 simple reports, summaries, odds and ends
# Simon Dedman simondedman@gmail.com


# 2021-10-11 summary of shark captures####
# Summary Table:
# Common Name
# Species
# Total #
# Male / Female # or ratio
# STL range (min - max)

library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tidylog)

shark <- list.files(path = "../../Data/") %>% # list all files in Data folder
  .[matches("_shark_capture_data.rds", vars = .)] %>% # filter for the right ones, all today() variants, will be ordered rising by date
  last() # last one is highest date i.e. latest
shark <- readRDS(file = paste0("../../Data/", shark))

shark %>%
  group_by(Common) %>%
  summarise(Species = first(Species),
            TotalNo = n(),
            NoFemale = length(which(Sex == "F")),
            NoMale = length(which(Sex == "M")),
            PctFemale = round(length(which(Sex == "F")) / n() * 100, 2),
            PctMale = round(length(which(Sex == "M")) / n() * 100, 2),
            STLmin = min(STL, na.rm = TRUE),
            STLmax = max(STL, na.rm = TRUE)) %>%
  arrange(desc(TotalNo)) %>%
  write_csv(file = paste0("../../Projects/Reports_Misc/", today(), "_SharkCaptureSummary.csv"))

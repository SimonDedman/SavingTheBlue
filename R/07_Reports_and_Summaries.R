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

# Shark counts sex % STL ####
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

# Shark counts sex % STL by year ####
shark %>%
  mutate(Site3 = case_match(
           Site3,
           "USA" ~ "USA",
           .default = "Andros"
         )) |>
  group_by(Site3, Common) %>%
  summarise(Species = first(Species),
            TotalNo = n(),
            NoFemale = length(which(Sex == "F")),
            NoMale = length(which(Sex == "M")),
            PctFemale = round(length(which(Sex == "F")) / n() * 100, 2),
            PctMale = round(length(which(Sex == "M")) / n() * 100, 2),
            STLmin = min(STL, na.rm = TRUE),
            STLmax = max(STL, na.rm = TRUE)) %>%
  arrange(Site3, desc(TotalNo)) %>%
  write_csv(file = paste0("../../Projects/Reports_Misc/", today(), "_SharkCaptureSummaryByCountry.csv"))


# N.sharks caught by year per species ####
shark |>
  mutate(Year = year(Date)) |>
  group_by(Year, Common) |>
  summarise(n = n()) |>
  pivot_wider(names_from = Year,
              values_from = n) |>
  write_csv(file = paste0("../../Projects/Reports_Misc/", today(), "_SharksByYear.csv"))

# N.sharks caught by year per species per country ####
shark |>
  mutate(Year = year(Date),
         Site3 = case_match(
           Site3,
           "USA" ~ "USA",
           .default = "Andros"
         )) |>
  group_by(Site3, Year, Common) |>
  summarise(n = n()) |>
  pivot_wider(names_from = Year,
              values_from = n) |>
  write_csv(file = paste0("../../Projects/Reports_Misc/", today(), "_SharksByYearByCountry.csv"))

# N acoustic tags deployed per species per country ####
shark |>
  mutate(Year = year(Date),
         Site3 = case_match(
           Site3,
           "USA" ~ "USA",
           .default = "Andros"
         )) |>
  filter(is.na(Shark_recapture_no),
         !is.na(Acoustic_Tag_ID_no)) |>
  group_by(Site3, Year, Common) |>
  summarise(n = n()) |>
  pivot_wider(names_from = Year,
              values_from = n) |>
  write_csv(file = paste0("../../Projects/Reports_Misc/", today(), "_AcousticTagsByYearByCountry.csv"))

# N satellite tags deployed per species per country ####
shark |>
  mutate(Year = year(Date),
         Site3 = case_match(
           Site3,
           "USA" ~ "USA",
           .default = "Andros"
         )) |>
  filter(is.na(Shark_recapture_no),
         !is.na(Satellite_Tag_ID_no)) |>
  group_by(Site3, Year, Common) |>
  summarise(n = n()) |>
  pivot_wider(names_from = Year,
              values_from = n) |>
  write_csv(file = paste0("../../Projects/Reports_Misc/", today(), "_SatelliteTagsByYearByCountry.csv"))

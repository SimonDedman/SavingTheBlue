# 2021-06-09 Reef shark drumline analysis
# Simon Dedman simondedman@gmail.com
# With Tristan Guttridge & Phil Matich

# Setup, load, prep data####
library(magrittr)
library(dplyr)
library(tidyverse)
library(tidylog)
# read in data
# source('R/01_data-import.R') # run data import if you changed the database.
drumline <- readRDS("../../Data/2021-08-09_drumline_data.rds")
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
  mutate(Substrate2 = case_when(
    Substrate %in% c("Sand & patch reef", "Patch reef", "Sand & octocorals") ~ "Reef",
    Substrate %in% c("Silt & seagrass", "Sand & seagrass", "Sand & algae") ~ "Vegetation",
    Substrate %in% c("Sand", "Silt", "Hard bottom") ~ "Bare",
    TRUE ~ as.character(Substrate)))
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


# # don't do this? Catching other sharks = soaktime & no reef caught?
# drumline %<>%
#   filter(Common %in% c("Caribbean Reef", NA)) # reef sharks, or nothing caught

# Response data####
# reef shark CPUE on drumline deployments.
# species or common, vs NA, per...soak time
drumline %<>%
  mutate(Catch2 = case_when(
    Common == "Caribbean Reef" ~ 1, # Make values 1 when sharks caught
    TRUE ~ 0)) # else 0
# Total sharks caught per site, divided by total soak time per site?


# Soak time adjustment ####
# Bait present = TRUE: full soak, response = 0. Nothing to do.
# Bait present = false + species = reef: full soak, response = 1. Nothing to do.
drumline %<>%
  mutate(Soak_time2 = case_when(
    # Bait present = false + species = not-reef: half soak, response = 0
    Bait_present == FALSE & (Common != "Caribbean Reef" | is.na(Common)) ~ Soak_time / 2,
    # Bait present = false + species = NA + biteoff = false: half soak, response = 0. Covered by above
    # Bait present = false + species = NA + biteoff = true: half soak, response = 0. Covered by above
    TRUE ~ Soak_time)) # else 0

drumline %<>% mutate(CPUE = Catch2 / as.numeric(Soak_time2))


# temporal####
# time of day. Midpoint of timein/timeout? Will be a posix tho.
# can use hour as a lazy proxy. or minute
drumline$Minute <- minute(drumline$Time_in)
# Yearday
drumline$Yearday <- yday(drumline$Date)
# Season
drumline$Month <- month(drumline$Date)
drumline$Season <- cut(drumline[,"Month"],
                       breaks = c(-Inf, 2, 5, 8, 11, Inf),
                       labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"))
# Daylength
source('~/Dropbox/Galway/Analysis/R/daylength/daylength.R')

dl <- daylength(date = drumline$Date,
                lat = drumline$Latitude,
                lon = drumline$Longitude,
                tzs = "America/New_York")
#dl has date lat lon sunrise sunset dawn dusk daylength, mostly "POSIXct" "POSIXt"
drumline <- cbind(drumline, dl[, 4:8]) # append sunrise sunset dawn dusk daylength
rm(dl)
rm(daylength)

# lunar cycle
library(lunar) #lunar.phase
drumline$LunarPhase <- lunar.phase(x = drumline$Date, shift = -5, name = TRUE)
# Levels: New Waxing Full Waning



# Fishery / method:
# Bait_type
# Bottom_top



# Subsetting variables: sex, size.


# Save sheets####
write.csv(x = drumline,
          file = paste0("../../Data/", today(), "_drumline_reefs.csv"),
          row.names = F)
saveRDS(object = drumline,
        file = paste0("../../Data/", today(), "_drumline_reefs.rds"))


# Analysis: bar chats & scatterplots####

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


# Loop through factorial variables & barplot against CPUE
for (factorvars in c("Site3", "Habitat", "Substrate", "Tide", "Season", "LunarPhase")) {
  ggplot(data = drumline) +
    geom_col(mapping = aes(x = .data[[factorvars]], y = CPUE), fill = "black") +
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

# Loop through numerical variables & scatterplot against CPUE with trendline
for (numvars in c("Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")) {
  ggplot(data = drumline) +
    geom_point(mapping = aes(x = .data[[numvars]], y = CPUE), fill = "black") +
    geom_smooth(mapping = aes(x = .data[[numvars]], y = CPUE)) + # , fill = "black"
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
library(gbm.auto)
expvars = c("Site3", "Habitat", "Substrate", "Tide", "Season", "LunarPhase",
            "Latitude", "Longitude", "Depth_m", "Temperature_C", "Salinity", "DO_mg_L", "Yearday", "Month", "daylength")
gbm.bfcheck(samples = drumline, resvar = "CPUE")
# [1] "  binary bag fraction must be at least 0.018. n = 1179"
# [1] "Gaussian bag fraction must be at least 0.262. n = 80"
# [1] 0.0178117 0.2625000
gbm.auto(
  grids = NULL,
  samples = drumline, # [-which(is.na(drumline[resvar])),]
  expvar = expvars,
  resvar = "CPUE",
  tc = c(2),
  lr = list(0.01, 0.00001),
  bf = list(0.5, 0.9),
  n.trees = 50,
  ZI = FALSE, # "CHECK"
  fam1 = c("bernoulli", "binomial", "poisson", "laplace", "gaussian"),
  fam2 = c("gaussian", "bernoulli", "binomial", "poisson", "laplace"),
  simp = TRUE,
  gridslat = 2,
  gridslon = 1,
  multiplot = TRUE,
  cols = grey.colors(1, 1, 1),
  linesfiles = TRUE,
  smooth = FALSE,
  savedir = "../../Projects/2021_06 Reef shark drumline CPUE/Results_Plots/BRT",
  savegbm = TRUE,
  loadgbm = NULL,
  varint = TRUE,
  map = TRUE,
  shape = NULL,
  RSB = TRUE,
  BnW = F,
  alerts = TRUE,
  pngtype = c("cairo-png", "quartz", "Xlib"),
  gaus = FALSE, # TRUE
  MLEvaluate = TRUE)

# gaus run struggling, only n=80, keep playing with list options




# Bayesian! Perfect test case



# D. See if they say anything interesting
# E. Write short comms paper if so

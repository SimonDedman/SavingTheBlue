### ====================================================================================================
### Project:    Satellite Telemetry - Data filtering using state space models
### Analysis:   Processing and cleaning satellite telemetry data of fin-mounted SPOT tags for further steps
### Script:     ~SavingTheBlue/R/06B_CTCRW_SSM_SPOT_using_aniMotum.R
### Author:     Vital Heim
### Version:    1.0
### ====================================================================================================

### ....................................................................................................
### Content: this R script contains the code to filter, clean and process raw data collected by fin-
###          mounted Smart Position and Temperature (SPOT) transmitters.
###          To do so we fit a continuous time correlated random walk model (CTCRW) in a state space
###          model (SSM) framework using sda-prefiltered data.
###          The code is based on the functions provided within the animotum (Jonsen et al. 2023) package.
###
###          > citation("aniMotum")
###          Ian Jonsen, W James Grecian, Lachlan Phillips, Gemma Carroll, Clive R. McMahon,
###          Robert G. Harcourt, Mark A. Hindell, and Toby A. Patterson (2023) aniMotum, an R
###          package for animal movement data: Rapid quality control, behavioural estimation
###          and simulation.  Methods in Ecology and Evolution DOI: 10.1111/2041-210X.14060
###          The script contains additional code for pre-fitting processing and cleaning of the data.
### ....................................................................................................

### ....................................................................................................
### [TO-DOs] Setwd, paths and parameters ----
### - is re-routing worth it? Some weird re-routes due to small land mass
### - change 10 day segment cutoff to five? looping in predicted locations.
### - if cutoff 5 days we might lose too much data?
### ....................................................................................................

### ....................................................................................................
### [A] Setwd, paths and parameters ----
### ....................................................................................................

# A1: clear memory ----

rm(list = ls())

# A2: load necessary packages ----

library("remotes")
## install if first time
# install.packages("tidyverse")
# install.packages("magrittr")
# #install.packages("TMB", type = "source") # if package version inconsistency detected during loading of foieGras
# install.packages("aniMotum",
#                  repos = c("https://cloud.r-project.org",
#                            "https://ianjonsen.r-universe.dev"),
#                  dependencies = "Suggests") #foiegras was removed from CRAN and replaced with Animotum on 12-12-2022
# install.packages("patchwork")
# install.packages("sf")
# install.packages("sp")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# remotes::install_github("ropensci/rnaturalearthhires")
# install.packages("ggplot2")
# install.packages("xts")
# install.packages("trip")
# install.packages("ggspatial")
# install.packages("virids")
# install.packages("tidylog")
# install.packages("ggpubr")

## load
library("tidyverse")
library("magrittr")
library("aniMotum")
library("patchwork")
library("sf")
library("sp")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
library("ggplot2")
library("xts")
library("trip")
library("ggspatial")
library("viridis")
library("tidylog")
library("ggpubr")

# A3: define needed functions, general paremeters, etc. ----

# *A3.1: general parameters ----
## Define first which sda filter you used to prepare the here analysed data
speedfilter <- "Argosfilter" # choices: "Argosfilter" or "TripSDA"

# *A3.2: CTCRW specific parameters ----

## Prefilter
## You can use the trip package sda-filter algorithm within the ssm function call.
## Here, we set those to options to F as we used ther argosfilter package in a previous script
## to filter out spurious locations
spdf = F # speedfilter
pf = F # use trip::sda() filtered data from function call to fit CTRW


## Model type
model = "crw" # choose between rw, crw, mp. mpm and jpmp available in fit_mpm

## Time steps for fitted or predicted location estimates

fit_step <- NA # use NA as a time step in models where you only want to calculate most probable location estimates at original observation times
pred_step <- 12 # us this time step to predict locations at standardized time period (e.g. 12 h)

### optimizers
optim = "optim"
maxit = 1000
verbose = 2

# A4: Specify data and saveloc ----
saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/OutputData/CTCRW/" #Adjust this

### ....................................................................................................
### [B] SSM with prefiltered data - data prep ----
### ....................................................................................................

# Remember: if you use prefiltered data, this means raw argos data was filtered to remove
# spurious detections based on  a speed-distance-angle algorithm. Based on the script/file either
# the sda filter from the package "argosfilter" or "trip" was used.
# In either case, the files were already adjusted for near duplicated observations so this does not
# need to be done again here in section B.

# B1: Import the filtered data ----

mydets_f <- list.files(path = "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/OutputData/Initial_filter_data/",
                     pattern = paste0(speedfilter,"_.+.R"), # important: make sure that there is only one speedifltered datasheet in path
                     full.names = TRUE ) %>%
  purrr::map_dfr(readRDS)

# B2: housekeeping, tidy data and define column classes ----

## the fit_ssm and fit_mpm() functions in the foiegras package expect a data.frame or tibble in
## the following format if (!) error elipse information is available. If the data was collected
## using other methods than the CLS Argos' Kalman filter model other formats might be needed.
## Check: https://cran.r-project.org/web/packages/foieGras/vignettes/basics.html

## We need:
## 'id'
## 'date'
## 'loc' -location class
## 'lon'
## 'lat'
## 'smaj' - error semi-major axis
## 'smin' - error Semi-minor axis
## 'eor' - error ellipse orientation

## Basic housekeeping
det_f <-mydets_f %>% # This is only needed if you used a different script than 06A_Filter_SPOT_data_argosfilter.R
  dplyr::select( # select relevant columns, here: id, date, location class (lc), lon, lat,
    id,
    date,
    lc,
    lon,
    lat,
    smaj,
    smin,
    eor,
    )

## check if there are Z locations left (should not!)
print(paste0("There are ", length(which(det_f$lc == "Z")), " Z-locations left."))

# B4: Known/tagging locations ----

## we already added tagging locations in a previous step. If you have no FastLoc data
## and no GPS location classes, then the tagging locations can be filtered
## by going for lc == G. If there is GPS data this section needs to be updated using the
## type == "user" argument.

det_f <- det_f %>%
  dplyr::mutate(
    smaj = ifelse(lc == 'G', 50,
                  smaj),
    smin = ifelse(lc == 'G', 50,
                  smin),
    eor = ifelse(lc == 'G', 0,
                 eor)
  )

# B5: Deal with parametrically logical but biologically unreasonable locations ----

## Only execute this section if above speedfilters do not exclude all spurious
## detections satisfyingly.

## Plot filtered tracks individually. Plot the tracks of the desired speedfilter.

### Basemap
esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
### Colors
nb.cols <- length(unique(det_f$id))
mycolors <- viridis(nb.cols)

### Tracks
sf_locs <- sf::st_as_sf(det_f, coords = c("lon","lat")) %>%
  sf::st_set_crs(4326)

sf_lines <- sf_locs %>%
  dplyr::arrange(id, date) %>%
  sf::st_geometry() %>%
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs$id))) %>%
  sf::st_cast("MULTILINESTRING") %>%
  sf::st_sf(id = as.factor(unique(det_f$id)))

sf_points <- sf_locs %>%
  dplyr::arrange(id, date) %>%
  sf::st_geometry() %>%
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs$id))) %>%
  sf::st_sf(id = as.factor(unique(det_f$id)))

ptt_list <- sort(unique(det_f$id))

for (i in 1:length(unique(det_f$id))) {

  # plot the filtered locations by individual
  ggplot() +
    annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none", cachedir = saveloc) +
    layer_spatial(sf_points[i,], size = 0.5) +
    layer_spatial(sf_lines[i,], size = 0.75,aes(color = id)) +
    #scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
    scale_colour_manual(values = mycolors[i]) +
    theme() +
    ggtitle("Speedfiltered (argosfilter::sdafilter()) Argos Location Paths",
            subtitle = paste0("S. mokarran from Andros (n = ", length(unique(mydets_f$id)), ")"))

  # save it
  ptt_id <- ptt_list[[i]]
  ggsave(paste0(saveloc,"Speedfiltered_Argos_data_individual_", ptt_id, ".tiff"),
         # TO DO: USE i as an index for the shark PTT ID rather than shark number
         width = 21, height = 15, units = "cm", device = "tiff", dpi = 300)
}

## Notes code: the code throws some errors/warning at you. However, there are not impacting
## the output, especially since the output only serves finding locations that are biologically
## unreasonable.

## Notes from comparing individual plots.
## Most individuals are fine. Some have locations on land in Andros, but given that
## the island is small in relation to available instrument accuracy, these can be left in for now.

## 235283 has 1 spurious location at around 35°W and 34°N
## 2023-07-22 00:57:32 at lon. -36.06180 and lat: 34.09160

## Filter out these segments manually
det_f <- det_f %>%
  filter(!(
    # 235283
    (id == "235283" & date == as.POSIXct("2023-07-22 00:57:32", tz = "UTC")) #|
    # (id == "179472" & date == as.POSIXct("2019-11-22 11:51:16", tz = "UTC"))
  ))

## Visualise manually improved tracks again:
sf_locs_clean <- sf::st_as_sf(det_f, coords = c("lon","lat")) %>%
  sf::st_set_crs(4326)

sf_lines_clean <- sf_locs_clean %>%
  dplyr::arrange(id, date) %>%
  sf::st_geometry() %>%
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(det_f$id))) %>%
  sf::st_cast("MULTILINESTRING") %>%
  sf::st_sf(id = as.factor(unique(det_f$id)))

sf_points_clean <- sf_locs_clean %>%
  dplyr::arrange(id, date) %>%
  sf::st_geometry() %>%
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs_clean$id))) %>%
  sf::st_sf(id = as.factor(unique(det_f$id)))

#esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
#'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

### ALL
ggplot() +
  annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
  layer_spatial(sf_points_clean, size = 0.5) +
  layer_spatial(sf_lines_clean, size = 0.75,aes(color = id)) +
  scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
  scale_fill_manual(values = mycolors) +
  theme() +
  ggtitle("Speedfiltered (argosfilter::sdafilter()) and manually checked Argos Location Paths",
          subtitle = paste0("S. mokarran from Andros (n = ", length(unique(mydets_f$id)), ")"))

ggsave(paste0(saveloc,"Speedfiltered_Argos_data_all_sharks_clean.tiff"),
       # TO DO: USE i as an index for the shark PTT ID rather than shark number
       width = 21, height = 15, units = "cm", device = "tiff", dpi = 300)

### IND
for (i in 1:length(unique(det_f$id))) {

  # plot the filtered locations by individual
  ggplot() +
    annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none", cachedir = saveloc) +
    layer_spatial(sf_points[i,], size = 0.5) +
    layer_spatial(sf_lines[i,], size = 0.75,aes(color = id)) +
    #scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
    scale_colour_manual(values = mycolors[i]) +
    theme() +
    ggtitle("Speedfiltered (argosfilter::sdafilter()) and manually checked Argos Location Paths",
            subtitle = paste0("S. mokarran from Andros (n = ", length(unique(mydets_f$id)), ")"))

  # save it
  ptt_id <- ptt_list[[i]]
  ggsave(paste0(saveloc,"Speedfiltered_Argos_data_individual_", ptt_id, "_clean.tiff"),
                width = 21, height = 15, units = "cm", device = "tiff", dpi = 300)
}


# B6: calculate the time difference between detections in days and segment tracks ----

## The predictive power of CTCRW models is reduced for long time gaps betweend detections and/or for short track durations
## we need to find large time gaps between locations to split movement track into segments and asses the length of segments
## and remove segments of short duration to improve model fit.

## Depending if all sharks survived and/or all tasg were deployed there might be NA values in the dataset
## check
check <- det_f[is.na(det_f$date),] # if these are NAs coming from post-release mortality sharks or undeplyoed tags, delete them

## Calculate the time difference between detections in days
det_f$tdiff.days <- unlist(tapply(det_f$date, INDEX = det_f$id,
                                 FUN = function(x) c(0, `units<-`(diff(x), "days"))))

## Find the maximum values
max(det_f$tdiff.days) # 57.43806

## Assign different segments if the time difference is larger than the cutoff
det_seg <- det_f %>%
  group_by(id) %>%
  mutate(id = paste0(id,"_", 1+cumsum(tdiff.days > 10))) #MM section: cutoff for segments is > 10 days - following Logan et a. 2020


## Define track lengths
tracklengths <- det_seg %>%
  group_by(id) %>%
  summarise(
    num_locs = n(),
    start_date = min(as.Date(date)),
    end_date = max(as.Date(date)),
  ) %>%
  mutate(tracklength_in_days = as.numeric(end_date - start_date) + 1)

print(tracklengths, n = 100)

#View(tracklengths)

## Check what short track segments would be deleted
del_obs <- dplyr::filter(tracklengths, num_locs < 1  | tracklength_in_days < 20) # Based on Logan et al. 2020, segments of < 20 days should be removed
print(del_obs, n = 100); nrow(del_obs); sum(del_obs$num_locs)
# A tibble: 23 × 5
# id        num_locs start_date end_date   tracklength_in_days
# <chr>        <int> <date>     <date>                   <dbl>
# 1 200368_2         1 2021-07-27 2021-07-27                   1
# 2 200368_3         1 2021-08-07 2021-08-07                   1
# 3 200369_1        12 2020-10-27 2020-11-01                   6
# 4 200369_11        2 2021-08-27 2021-08-28                   2
# 5 200369_12        6 2021-09-23 2021-10-08                  16
# 6 200369_2         1 2020-11-13 2020-11-13                   1
# 7 200369_6        12 2021-03-22 2021-04-02                  12
# 8 200369_7         2 2021-04-18 2021-04-22                   5
# 9 200369_8         1 2021-05-21 2021-05-21                   1
# 10 200369_9         3 2021-06-09 2021-06-19                  11
# 11 209020_4         1 2022-02-10 2022-02-10                   1
# 12 209020_5         4 2022-02-23 2022-02-24                   2
# 13 209020_6         1 2022-03-08 2022-03-08                   1
# 14 209020_7         4 2022-03-20 2022-03-21                   2
# 15 222133_2         1 2023-07-25 2023-07-25                   1
# 16 235283_1         1 2023-05-31 2023-05-31                   1
# 17 235283_2         3 2023-07-27 2023-08-05                  10
# 18 235283_3         2 2023-08-29 2023-09-05                   8
# 19 235283_4         4 2023-09-15 2023-09-22                   8
# 20 235283_5         3 2023-10-03 2023-10-04                   2
# 21 235283_6         3 2023-11-06 2023-11-08                   3
# 22 235283_7         4 2023-12-02 2023-12-03                   2
# 23 235283_8         1 2024-01-19 2024-01-19                   1
# [1] 23 We lose 23 segments
# [1] 73 # We lose 73 observations/location estimates

## Filter out short track segments
det_seg_tl <- det_seg %>%
  inner_join(., tracklengths) %>%
  #TODO: figure out if we need to also remove segments that are 10 days but have little observations: 20240207
  dplyr::filter( # remove tracks shorter than 10 days and/or less than 1 total observations
    !(tracklength_in_days < 20 | num_locs < 1)
  ) %>%
  dplyr::select( #remove unnecessary columns
    -start_date,
    -end_date
    #-tracklength_in_days
  )

## Find the distribution of time gaps between detections
#ddet_f_cleaner$tdiff.days[ddet_f_cleaner$tdiff.days < 0] <- 0

hist <- hist(det_seg_tl$tdiff.days
             , breaks = c(seq(from = 0, to = 60, by = .5))
             , plot = F)

hist$density <- hist$counts / sum(hist$counts)*100
hist$density
# > hist$density
# [1] 81.70959313 12.24337439  1.53042180  1.34378499  0.52258305  0.52258305

plot(hist, freq = F
     , ylim = c(0, 100)
     , col = "skyblue")


## Since 81.71 % of all detections are less than 12 hours apart, we used 12hours as the predictive
## standardized time period for the CTCRW in section D

### ....................................................................................................
### [C] SSM with prefiltered data fitted locations with unsegmented tracks ----
### ....................................................................................................

# C1: fit SSM to original observation times using aniMotum ----

# ## Pre-filter if you have different species or groups you need to run separately:
# spp.f <- "S.mokarran"

### make a map of your raw data before model fitting
# world <- ne_countries(scale = "medium", returnclass = "sf") # use a sf object to speed things up rather than esri tiles

#tiff(paste0(saveloc, "Initial_filter_data/SPOT_tracks_",spp.f,"_argosfilter_raw.tiff"), width = 20, height = 30, units = "cm",res = 150)
# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = det_f, aes(x = lon, y = lat, colour = id), size = 2, shape = 20) +
#   coord_sf(xlim = c(min(det_seg_tl$lon - 0.5), max(det_seg_tl$lon + 0.5)),
#            ylim = c(min(det_seg_tl$lat - 0.5), max(det_seg_tl$lat + 0.5)), expand = F)
#dev.off()
## Define parameters

## The fit_ssm() expects following parameters
# d = a data frame of observations including Argos KF error ellipse info (when present)
# vmax = max travel rate (m/s) passed to sda to identify outlier locations
# ang = angles (deg) of outlier location "spikes"
# distlim =lengths (m) of outlier location "spikes"
# spdf = (logical) turn trip::sda on (default; TRUE) or off
# min.dt = minimum allowable time difference between observations; dt <= min.dt will be ignored by the SSM
# pf = just pre-filter the data, do not fit the SSM (default is FALSE)
# model =  fit either a simple random walk ("rw") or correlated random walk ("crw") as a continuous-time process model
# time.step = options: 1) the regular time interval, in hours, to predict to; 2) a vector of prediction times, possibly not regular, must be specified as a data.frame with id and
# POSIXt dates; 3) NA - turns off prediction and locations are only estimated at observation times.
# scale = scale location data for more efficient optimization. This should rarely be needed (default = FALSE)
# emf = optionally supplied data.frame of error multiplication factors for Argos location quality classes. Default behaviour is to use the factors supplied in foieGras::emf()
# map =  a named list of parameters as factors that are to be fixed during estimation, e.g., list(psi = factor(NA))
# parameters = a list of initial values for all model parameters and unobserved states, default is to let sfilter specify these. Only play with this if you know what you are doing
# fit.to.subset = fit the SSM to the data subset determined by prefilter (default is TRUE)
# control = list of control settings for the outer optimizer (see ssm_control for details)
# inner.control = list of control settings for the inner optimizer (see MakeADFun for additional details)
# verbose = [Deprecated] use ssm_control(verbose = 1) instead, see ssm_control for details
# optim = [Deprecated] use ssm_control(optim = "optim") instead, see ssm_control for details
# optMeth = [Deprecated] use ssm_control(method = "L-BFGS-B") instead, see ssm_control for details
# lpsi [Deprecated] use ssm_control(lower = list(lpsi = -Inf)) instead, see ssm_control for details

## Data
## fit_ssm expects 'd' to be a dataframe or tiblle or sf-tibble (with projection info) with 5,7, or 8 columns
## The data should have  5 columns in the following order: "id", "date", "lc", "lon", "lat".
## Where "date" can be a POSIX object or text string in YYYY-MM-DD HH:MM:SS format
## Argos Kalman Filter (or Kalman Smoother) data should have 8 columns, including the above 5 plus
## "smaj", "smin", "eor" that contain Argos error ellipse variables (in m for "smaj", "smin" and deg for "eor").
dpf <- det_f

## Specify SSM
mod.crw_f <- fit_ssm(
  as.data.frame(dpf), # somehow getting errors if not ran with 'd' in data.frame class
  #vmax = vmax,
  #ang = ang,
  #distlim = distlim,
  spdf = spdf,
  min.dt = 5, # TODO: should we increase time step in argosfilter to improve model fit?
  pf = pf,
  model = model,
  time.step = fit_step,
  #fit.to.subset = T,
  control = ssm_control(
    optim = optim,
    maxit = maxit,
    verbose = verbose), # shows parameter trace, 0: silent, 1: optimizer trace, 2: parametre trace (default)
  #map = list(psi = factor(NA))
)

##Plot the mp model with normalised values
#tiff(paste0(saveloc, "SSM_MPM_output/SPOT_tracks_",spp.f,"_", model,"_",optim,"_",maxit, "iterations_fitted_with_argosfilter_output.tiff"),
            #height = 30, width = 20, units = "cm", res = 150)
# map(mod.crw_f, what = "f", normalise = TRUE, silent = TRUE)
#dev.off()

# The `normalise` argument rescales the estimates to span the interval 0,1.
# Move persistence estimates from `fit_ssm()` tend to be smoothed more extremely compared
# those obtained from `fit_mpm()` and can lack contrast. Normalising the estimates provides a
# clearer view of changes in movement behaviour along tracks, highlighting regions where animals
# spend disproportionately more or less time. When fitting to a collection of individuals,
# the normalisation can be applied separately to each individual or collectively via
# `normalise = TRUE, group = TRUE`. In the latter case, the relative magnitudes of move persistence
# are preserved across individuals.

# C2: re-route path to account for land lcoations

## Some locations may still on land. Reroute them using route_path()

mod.crw_f_rr <- route_path(mod.crw_f,
                         what = "fitted",
                         map_scale = 50, # scale of rnaturalearth map to use for land mass - if you want 10, you need the package "rnaturalearthhires"
                         dist = 10  # buffer distance (m) to add around track locations. The convex hull of these buffered locations defines the size of land polygon used to aid re-routing of points on land.
                         )
#TODO: FIND MOST APPROPRIATE VALUE FOR BUFFER FOR ANDROS SHARKS

## map the locations to check if it improved
my.aes <- aes_lst(line = T,
                  conf = F,
                  mp = F,
                  obs = T)
m1 <- map(mod.crw_f,
          what = "fitted",
          aes = my.aes,
          ext.rng = c(0.3, 0.1),
          silent = TRUE)

m2 <- map(mod.crw_f_rr,
          what = "rerouted",
          aes = my.aes,
          ext.rng = c(0.3, 0.1),
          silent = TRUE)

### Comparison of locations between fitted vs. fitted & re-routed
# (m1 + labs(title = "SSM-fitted locations")
#     m2 + labs(title = "Fitted & re-routed locations")) &
#   theme(panel.grid = element_line(size = 0.1, colour = "grey60"))
ggarrange(m1, m2,
          labels = c("Fitted", "Rerouted"),
          common.legend = T, legend = "top",
          ncol = 1, nrow = 2)  #TODO: super ugly graph, figure out a nicer solution (low priority)

ggsave(paste0(saveloc,"CTCRW_SSM_fitted_vs_re-routed_locations_all_sharks.tiff"),
       # TO DO: USE i as an index for the shark PTT ID rather than shark number
       width = 20, height = 30, units = "cm", device = "tiff", dpi = 300)

### Detailview fitted & re-routed locations
m2 + labs(title = "Fitted & re-routed locations") &
  theme(panel.grid = element_line(size = 0.1, colour = "grey60"))


# B7: check goodness of fit of your ssm/mpm model ----

# Validating SSM fits to tracking data is an essential part of any analysis.
# SSM fits can be visualized quickly using the generic `plot` function on model fit objects.

# Both fitted and predicted locations (gold) can be plotted as 1-D time-series on top of the observations
# (blue) to visually detect any lack of fit. Observations that failed to pass the `prefilter` stage prior
# to SSM fitting (black x's) can be included (default) or omitted with the `outlier` argument.
# Uncertainty is displayed as a $\pm$ 2 SE envelope (pale gold) around estimates. A rug plot along the
#x-axis aids detection of data gaps in the time-series. Note, in second plot, the larger standard errors for
#predicted locations through small data gaps.

#plot(mod.crw_pf, what = "fitted", type = 2)
#plot(mod.crw_pf[1,], what = "predicted")

## As XY-plots (type 1) and 2-D tracks (type 2)
IDs <- (mod.crw_f_rr$id) ## change accordingly

for (i in 1:length(IDs)) {

  # Plottype 1
  plottype <- 1

  ## open plot window
  png(file = paste0(saveloc,"Validation/Goodness_of_fitted_rerouted_", model, "_locations_", IDs[i],"_fitted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter_type_",plottype,"_plot.png"), res = 150, height = 20, width = 30, units = "cm")

  ## plot the fitted locations by individual
  print(plot(mod.crw_f_rr[i,], "rerouted", type = plottype, alpha = 0.1)) # "f" for fitted and type=2 for 2-D

  ## save it
  dev.off()

  # Plottype 2
  plottype <- 2

  ## open plot window
  png(file = paste0(saveloc,"Validation/Goodness_of_fitted_rerouted_", model, "_locations_", IDs[i],"_fitted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter_type_",plottype,"_plot.png"), res = 150, height = 20, width = 30, units = "cm")

  ## plot the fitted locations by individual
  print(plot(mod.crw_f_rr[i,], "rerouted", type = plottype, alpha = 0.1)) # "f" for fitted and type=2 for 2-D

  ## save it
  dev.off()
}

# Residual plots are important for validating models, but classical Pearson residuals,
# for example, are not appropriate for state-space models. Instead, a one-step-ahead prediction
# residual, provides a useful if computationally demanding alternative.
# In `aniMotum`, prediction residuals from state-space model fits are calculated using the
# `osar` function and can be visualized as time-series plots, Q-Q plots, or autocorrelation
# functions:

## calculate & plot residuals

for (i in 1:length(IDs)) {

  # subset individual tracks
  res.s <- osar(mod.crw_f_rr[i,]) ## change accordingly

  # open plot window
  png(file = paste0(saveloc,"Validation/Fitted_rerouted_", model,"_residuals_for_validation_", IDs[i],"_fitted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter.png"), res = 150, height = 20, width = 30, units = "cm")

  #plot
  print((plot(res.s, type = "ts") | plot(res.s, type = "qq")) /
          (plot(res.s, type = "acf") | plot_spacer()))

  # save it
  dev.off()
}

# B7: export the model ouput ----

## fitted locations
### non-projected form
floc <- grab(mod.crw_f, what = "fitted")

### projected form
floc.proj <- grab(mod.crw_f, what = "fitted", as_sf = T)

# ## predicted locations
# ### non-projected form
# ploc<- grab(mod.crw_pf, what = "predicted")
#
# ### projected form
# ploc.proj <- grab(mod.crw_pf, what = "predicted", as_sf = T)

## fitted & rerouted locations
### non-projected form
floc_r <- grab(mod.crw_f_rr, what = "rerouted")

### projected form
floc_r.proj <- grab(mod.crw_f_rr, what = "rerouted", as_sf = T)

## normalie the gamma_t value if you fitted an MPM (NOTE: WITH NEW UPDATE THIS CAN BE DONE USING GRAB)
#floc$g_normalized = (floc$g-min(floc$g))/(max(floc$g)-min(floc$g))

# B8: save the model output as .csv (non-projected form) and as R file (both) ----

# fitted
write.table(floc, paste0(saveloc, "Data_aniMotum_CTCRW_output_fitted_non-projected_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(floc, paste0(saveloc,"Data_aniMotum_CTCRW_output_fitted_non-projected_with_",speedfilter, "_data.rds"))
saveRDS(floc.proj, paste0(saveloc,"Data_aniMotum_CTCRW_output_fitted_projected_with_",speedfilter, "_data.rds"))

## fitted re-routed
write.table(floc_r, paste0(saveloc, "Data_aniMotum_CTCRW_output_fitted_rerouted_non-projected_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(floc_r, paste0(saveloc,"Data_aniMotum_CTCRW_output_fitted_rerouted_non-projected_with_",speedfilter, "_data.rds"))
saveRDS(floc_r.proj, paste0(saveloc,"Data_aniMotum_CTCRW_output_fitted_rerouted_projected_with_",speedfilter, "_data.rds"))

### ....................................................................................................
### [C] SSM with prefiltered data - predicted locations with segmented tracks ----
### ....................................................................................................

## According to Lea et al. 2013

## "because each raw position has a different error field according to its Argos location class, we needed to
## decide the most probable location for each point within its error field. We achieved this by using a Bayesian
## state-space model (SSM) that adjusted the filtered tracks by producing regular positions based on the Argos location class,
## mean turning angle, and autocorrelation in speed and direction, producing the most probable track through the error fields"

## "Argos tracks only have locations for when the sharks were at the surface; consequently there is high
## variability in the number of locations in a given area, as a result of the shark’s varied surfacing behaviour
## rather than because of its actual location. This would introduce a bias into the analysis of time spent in different
## areas. To correct this bias, linear interpolation was used to normalise the transmission fre- quency by generating
## points at 12 hour intervals along track gaps of <20 days."


# C1: fit SSM using aniMotum ----

# ## Pre-filter if you have different species or groups you need to run separately:
# spp.f <- "S.mokarran"

### make a map of your raw data before model fitting
# world <- ne_countries(scale = "medium", returnclass = "sf")
#
# #tiff(paste0(saveloc, "Initial_filter_data/SPOT_tracks_",spp.f,"_argosfilter_raw.tiff"), width = 20, height = 30, units = "cm",res = 150)
# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = det_seg_tl, aes(x = lon, y = lat, colour = id), size = 2, shape = 20) +
#   coord_sf(xlim = c(min(det_seg_tl$lon - 0.5), max(det_seg_tl$lon + 0.5)),
#            ylim = c(min(det_seg_tl$lat - 0.5), max(det_seg_tl$lat + 0.5)), expand = F)
#dev.off()

## Define parameters

## The fit_ssm() expects following parameters
# d = a data frame of observations including Argos KF error ellipse info (when present)
# vmax = max travel rate (m/s) passed to sda to identify outlier locations
# ang = angles (deg) of outlier location "spikes"
# distlim =lengths (m) of outlier location "spikes"
# spdf = (logical) turn trip::sda on (default; TRUE) or off
# min.dt = minimum allowable time difference between observations; dt <= min.dt will be ignored by the SSM
# pf = just pre-filter the data, do not fit the SSM (default is FALSE)
# model =  fit either a simple random walk ("rw") or correlated random walk ("crw") as a continuous-time process model
# time.step = options: 1) the regular time interval, in hours, to predict to; 2) a vector of prediction times, possibly not regular, must be specified as a data.frame with id and
# POSIXt dates; 3) NA - turns off prediction and locations are only estimated at observation times.
# scale = scale location data for more efficient optimization. This should rarely be needed (default = FALSE)
# emf = optionally supplied data.frame of error multiplication factors for Argos location quality classes. Default behaviour is to use the factors supplied in foieGras::emf()
# map =  a named list of parameters as factors that are to be fixed during estimation, e.g., list(psi = factor(NA))
# parameters = a list of initial values for all model parameters and unobserved states, default is to let sfilter specify these. Only play with this if you know what you are doing
# fit.to.subset = fit the SSM to the data subset determined by prefilter (default is TRUE)
# control = list of control settings for the outer optimizer (see ssm_control for details)
# inner.control = list of control settings for the inner optimizer (see MakeADFun for additional details)
# verbose = [Deprecated] use ssm_control(verbose = 1) instead, see ssm_control for details
# optim = [Deprecated] use ssm_control(optim = "optim") instead, see ssm_control for details
# optMeth = [Deprecated] use ssm_control(method = "L-BFGS-B") instead, see ssm_control for details
# lpsi [Deprecated] use ssm_control(lower = list(lpsi = -Inf)) instead, see ssm_control for details

## Data
## fit_ssm expects 'd' to be a dataframe or tiblle or sf-tibble (with projection info) with 5,7, or 8 columns
## The data should have  5 columns in the following order: "id", "date", "lc", "lon", "lat".
## Where "date" can be a POSIX object or text string in YYYY-MM-DD HH:MM:SS format
## Argos Kalman Filter (or Kalman Smoother) data should have 8 columns, including the above 5 plus
## "smaj", "smin", "eor" that contain Argos error ellipse variables (in m for "smaj", "smin" and deg for "eor").

## Use the segmented data for the predicted movements
dpf <- det_seg_tl

## Specify SSM
mod.crw_p <- fit_ssm(
  as.data.frame(dpf), # somehow getting errors if not ran with 'd' in data.frame class
  #vmax = vmax,
  #ang = ang,
  #distlim = distlim,
  spdf = spdf,
  min.dt = 5,
  pf = pf,
  model = model,
  time.step = 12,
  #fit.to.subset = T,
  control = ssm_control(
    optim = optim,
    maxit = maxit,
    verbose = verbose), # shows parameter trace, 0: silent, 1: optimizer trace, 2: parametre trace (default)
  map = list(psi = factor(NA)) # If optimiser failed. Try simplifying the model with this line
)

##Plot the mp model with normalised values
#tiff(paste0(saveloc, "SSM_MPM_output/SPOT_tracks_",spp.f,"_", model,"_",optim,"_",maxit, "iterations_fitted_with_argosfilter_output.tiff"),
#height = 30, width = 20, units = "cm", res = 150)
# map(mod.crw_p, what = "p", normalise = TRUE, silent = TRUE)
#dev.off()

# The `normalise` argument rescales the estimates to span the interval 0,1.
# Move persistence estimates from `fit_ssm()` tend to be smoothed more extremely compared
# those obtained from `fit_mpm()` and can lack contrast. Normalising the estimates provides a
# clearer view of changes in movement behaviour along tracks, highlighting regions where animals
# spend disproportionately more or less time. When fitting to a collection of individuals,
# the normalisation can be applied separately to each individual or collectively via
# `normalise = TRUE, group = TRUE`. In the latter case, the relative magnitudes of move persistence
# are preserved across individuals.

# C2: re-route path to account for land lcoations

## Some locations may still be on land. Reroute them using route_path()

mod.crw_p_rr <- route_path(mod.crw_p,
                           what = "predicted",
                           map_scale = 10, # scale of rnaturalearth map to use for land mass - if you want 10, you need the package "rnaturalearthhires"
                           buffer = 10 # buffer distance (m) to add around track locations. The convex hull of these buffered locations defines the size of land polygon used to aid re-routing of points on land.
)

## map the locations to check if it improved
my.aes <- aes_lst(line = T,
                  conf = F,
                  mp = F,
                  obs = T)
m1 <- map(mod.crw_p,
          what = "predicted",
          aes = my.aes,
          ext.rng = c(0.3, 0.1),
          silent = TRUE)

m2 <- map(mod.crw_p_rr,
          what = "rerouted",
          aes = my.aes,
          ext.rng = c(0.3, 0.1),
          silent = TRUE)

### Comparison of locations between predicted vs. predicted & re-routed
# (m1 + labs(title = "SSM-predicted locations") | m2 + labs(title = "Predicted & re-routed locations")) &
#   theme(panel.grid= element_line (size=0.1, colour="grey60"))
ggarrange(m1, m2,
          labels = c("Predicted", "Rerouted"),
          common.legend = T, legend = "top",
          ncol = 1, nrow = 2)  #TODO: super ugly graph, figure out a nicer solution (low priority)

ggsave(paste0(saveloc,"CTCRW_SSM_predcited_vs_re-routed_locations_all_sharks.tiff"),
       # TO DO: USE i as an index for the shark PTT ID rather than shark number
       width = 20, height = 30, units = "cm", device = "tiff", dpi = 300)


### Detail view predicted & re-routed locations
m2 + labs(title = "Predicted & re-routed locations") &
  theme(panel.grid= element_line (size=0.1, colour="grey60"))

# C3: check goodness of fit of your ssm/mpm model ----

# Validating SSM fits to tracking data is an essential part of any analysis.
# SSM fits can be visualized quickly using the generic `plot` function on model fit objects.

# Both fitted and predicted locations (gold) can be plotted as 1-D time-series on top of the observations
# (blue) to visually detect any lack of fit. Observations that failed to pass the `prefilter` stage prior
# to SSM fitting (black x's) can be included (default) or omitted with the `outlier` argument.
# Uncertainty is displayed as a $\pm$ 2 SE envelope (pale gold) around estimates. A rug plot along the
#x-axis aids detection of data gaps in the time-series. Note, in second plot, the larger standard errors for
#predicted locations through small data gaps.

#plot(mod.crw_pf, what = "fitted", type = 2)
#plot(mod.crw_pf[1,], what = "predicted")

## As XY-plots (type 1) and 2-D tracks (type 2)

IDs <- (mod.crw_p_rr$id) # or mod.crw_p if no re-routing needed

for (i in 1:length(IDs)){

  # Plottype 1
  plottype <- 1

  # open plot window
  png(file = paste0(saveloc,"Validation/Goodness_of_predicted_rerouted_", model, "_locations_", IDs[i],"_fitted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter_type_",plottype,"_plot.png"), res = 150, height = 20, width = 30, units = "cm")

  # plot the fitted locations by individual
  print(plot(mod.crw_p_rr[i,], "rerouted", type = plottype, alpha = 0.1)) # "f" for fitted and type=2 for 2-D

  # save it
  dev.off()

  # Plottype 2
  plottype <- 2

  # open plot window
  png(file = paste0(saveloc,"Validation/Goodness_of_predicted_rerouted_", model, "_locations_", IDs[i],"_fitted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter_type_",plottype,"_plot.png"), res = 150, height = 20, width = 30, units = "cm")

  # plot the fitted locations by individual
  print(plot(mod.crw_p_rr[i,], "rerouted", type = plottype, alpha = 0.1)) # "f" for fitted and type=2 for 2-D

  # save it
  dev.off()
}

# Residual plots are important for validating models, but classical Pearson residuals,
# for example, are not appropriate for state-space models. Instead, a one-step-ahead prediction
# residual, provides a useful if computationally demanding alternative.
# In `aniMotum`, prediction residuals from state-space model fits are calculated using the
# `osar` function and can be visualized as time-series plots, Q-Q plots, or autocorrelation
# functions:

## calculate & plot residuals

for (i in 1:length(IDs)){

  # susbet individual tracks
  res.s <- osar(mod.crw_p_rr[i,])

  # open plot window
  png(file = paste0(saveloc,"Validation/Prediction_", model,"_residuals_for_validation_", IDs[i],"_predicted_rerouted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter.png"), res = 150, height = 20, width = 30, units = "cm")

  #plot
  print((plot(res.s, type = "ts") | plot(res.s, type = "qq")) /
          (plot(res.s, type = "acf") | plot_spacer()))

  # save it
  dev.off()
}

# B7: export the model ouput ----

## fitted locations
### non-projected form
ploc <- grab(mod.crw_p, what = "predicted")

### projected form
ploc.proj <- grab(mod.crw_p, what = "predicted", as_sf = T)

# ## predicted locations
# ### non-projected form
# ploc<- grab(mod.crw_pf, what = "predicted")
#
# ### projected form
# ploc.proj <- grab(mod.crw_pf, what = "predicted", as_sf = T)

## predicted & rerouted locations
### non-projected form
ploc_r <- grab(mod.crw_p_rr, what = "rerouted")

### projected form
ploc_r.proj <- grab(mod.crw_p_rr, what = "rerouted", as_sf = T)

## normalie the gamma_t value if you fitted an MPM (NOTE: WITH NEW UPDATE THIS CAN BE DONE USING GRAB)
#floc$g_normalized = (floc$g-min(floc$g))/(max(floc$g)-min(floc$g))

# B8: save the model output as .csv (non-projected form) and as R file (both) ----

# predicted
write.table(ploc, paste0(saveloc, "Data_aniMotum_CTCRW_output_predicted_non-projected_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(ploc, paste0(saveloc,"Data_aniMotum_CTCRW_output_predicted_non-projected_with_",speedfilter, "_data.rds"))
saveRDS(ploc.proj, paste0(saveloc,"Data_aniMotum_CTCRW_output_predicted_projected_with_",speedfilter, "_data.rds"))

## oredicted re-routed
write.table(ploc_r, paste0(saveloc, "Data_aniMotum_CTCRW_output_predicted_rerouted_non-projected_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(ploc_r, paste0(saveloc,"Data_aniMotum_CTCRW_output_predicted_rerouted_non-projected_with_",speedfilter, "_data.rds"))
saveRDS(ploc_r.proj, paste0(saveloc,"Data_aniMotum_CTCRW_output_predicted_rerouted_projected_with_",speedfilter, "_data.rds"))

### ....................................................................................................
### [D] Calculate CI for location estimates and convert to lat/lon ----
### ....................................................................................................

# The animotum output offers a "merc" location (x and y) as well as the standard error of predicted
# locations (in merc projection also). In case we want to calculate UDs or similar down the line,
# we need a CI for each loaction estimate that then can be used as a estimate for the location error.

# includes individual id, date, longitude, latitude, x and y (typically from the default Mercator
# projection) and their standard errors (`x.se`, `y.se` in km), `u`, `v` (and their standard errors,
# `u.se`, `v.se` in km/h) are estimates of signed velocity in the x and y directions. The `u`, `v`
# velocities should generally be ignored as their estimation uses time intervals between consecutive
# locations, whether they are observation times or prediction times. The columns `s` and `s.se`
# provide a more reliable 2-D velocity estimate, although standard error estimation is turned off
# by default as this generally increases computation time for the `crw` SSM. Standard error
# estimation for `s` can be turned on via the `control` argument to `fit_ssm`
# (i.e. `control = ssm_control(se = TRUE)`, see `?ssm_control` for futher details).


# To calculate a CI in decimal degress, we need to first calculate upper and lower (2.5% and 97.5%) limits
# of our locations. We can do this by adding/substracting 1.96xSE from the x and y coordinates.

# After that, we need to transform the coordinates from the output projection of foiegras to
# decimal degrees.

# D1: find the projection of your fitted ssm/mpm object ----

## You likely will only need to this section for one datatype (predicted vs. fitted) depending on your downstream analyses
## chose your data
mod_data <- ploc_r
mod_data_proj <- ploc_r.proj

head(mod_data_proj)
# CRS:           +proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs

# D2: calculate upper and lower limits (2.5 and 97.5%) of location estimates ----

## Make new columns for longitude/x
mod_data$x.025 <- mod_data$x - (1.96*mod_data$x.se)
mod_data$x.975 <- mod_data$x + (1.96*mod_data$x.se)

## Make new columns for latitude/y
mod_data$y.025 <- mod_data$y - (1.96*mod_data$y.se)
mod_data$y.975 <- mod_data$y + (1.96*mod_data$y.se)

# D3: coordinates into a WGS84 lat/lon projection ----

## general
cord.merc <- SpatialPoints(cbind(mod_data$x, mod_data$y), proj4string=CRS("+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")) # define SpatialPoints object with output projection
cord.dec <- spTransform(cord.merc, CRS("+proj=longlat +datum=WGS84")) # transform the coordinates to a lat/lon projection
### physical check:
par(mfrow = c(1, 2))
plot(cord.merc, axes = TRUE, main = "Merc Coordinates", cex.axis = 0.95)
plot(cord.dec, axes = TRUE, main = "Lat-Lon Coordinates", col = "red", cex.axis = 0.95)
### create dataframe that can be added to mod.t object
coords.new <- as.data.frame(cord.dec)
colnames(coords.new) <- c("lon.new", "lat.new")

## Lower CI
cord.lowCI <- SpatialPoints(cbind(mod_data$x.025, mod_data$y.025), proj4string=CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs ")) # define SpatialPoints object with output projection
cord.025 <- spTransform(cord.lowCI, CRS("+proj=longlat +datum=WGS84")) # transform the coordinates to a lat/lon projection
### physical check:
par(mfrow = c(1, 2))
plot(cord.lowCI, axes = TRUE, main = "Merc Coordinates", cex.axis = 0.95)
plot(cord.025, axes = TRUE, main = "Lat-Lon Coordinates", col = "red", cex.axis = 0.95)
### create dataframe that can be added to mod.t object
CI025 <- as.data.frame(cord.025)
colnames(CI025) <- c("lon025", "lat025")

## upper CI
cord.upCI <- SpatialPoints(cbind(mod_data$x.975, mod_data$y.975), proj4string=CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs ")) # define SpatialPoints object with output projection
cord.975 <- spTransform(cord.upCI, CRS("+proj=longlat +datum=WGS84")) # transform the coordinates to a lat/lon projection
### physical check:
par(mfrow = c(1, 2))
plot(cord.upCI, axes = TRUE, main = "Merc Coordinates", cex.axis = 0.95)
plot(cord.975, axes = TRUE, main = "Lat-Lon Coordinates", col = "red", cex.axis = 0.95)
### create dataframe that can be added to mod.t object
CI975 <- as.data.frame(cord.975)
colnames(CI975) <- c("lon975", "lat975")

# D4: create df with lower and upper CI for lcoation estiamtes in [dd] ----

mod_CIs <- as.data.frame(cbind(mod_data, coords.new, CI025, CI975, mod_data$lon, mod_data$lat))

## double check that conversion was correct.
check <- mod_CIs
conversion.lon <- check$lon.new == check$mod.t.dd$lon
length(conversion.lon[conversion.lon == F]) # needs to be 0
conversion.lat <- check$lat.new == check$mod.t.dd$lat
length(conversion.lat[conversion.lat == F]) # needs to be 0
## if 0 twice continue

## create df and prepare for saving
## if SSM
mod.final <- dplyr::select(mod_CIs, id, date, lon.new, lon025, lon975, lat.new, lat025, lat975)
colnames(mod.final) <- c("id", "date", "lon", "lon025", "lon975", "lat", "lat025", "lat975")

## if MP
#mod.final <- dplyr::select(mod_CIs, id, date, lon.new, lon025, lon975, lat.new, lat025, lat975,g,g_normalized)
#colnames(mod.final) <- c("id", "date", "lon", "lon025", "lon975", "lat", "lat025", "lat975", "gamma_t", "gamma_t_normalized")

### ....................................................................................................
### [E] Save movement data for further analyses ----
### ....................................................................................................

# if based on speedfilter data
write.table(mod.final, paste0(saveloc, "Data_aniMotum_CRW_final_output_proj_WGS84_converted_with_coord_CIs_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(mod.final, paste0(saveloc,"Data_aniMotum_CRW_final_output_proj_WGS84_converted_with_coord_CIs_with_",speedfilter, "_data.rds"))


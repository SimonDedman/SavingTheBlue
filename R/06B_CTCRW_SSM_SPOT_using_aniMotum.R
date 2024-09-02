### ====================================================================================================
### Project:    Andros hammerheads - space use and trophic ecology
### Analysis:   Processing and cleaning satellite telemetry data of fin-mounted SPOT tags for further steps
### Script:     ~SavingTheBlue/R/06B_CTCRW_SSM_SPOT_using_aniMotum.R
### Author:     Vital Heim
### Version:    1.0
### ====================================================================================================

### ....................................................................................................
### Content: this R script contains the code to filter, clean and process raw data collected by fin-
###          mounted Smart Position and Temperature (SPOT) transmitters.
###          This script contains the code fit a continuous time correlated random walk model (CTCRW
###          in a state space model (SSM) framework using sda-prefiltered data.
###          The code is based on the functions provided within the animotum (Jonsen et al. 2023) package:
###          > citation("aniMotum")
###          Ian Jonsen, W James Grecian, Lachlan Phillips, Gemma Carroll, Clive R. McMahon,
###          Robert G. Harcourt, Mark A. Hindell, and Toby A. Patterson (2023) aniMotum, an R
###          package for animal movement data: Rapid quality control, behavioural estimation
###          and simulation.  Methods in Ecology and Evolution DOI: 10.1111/2041-210X.14060
###          The script contains additional code for pre-fitting processing and cleaning of the data.
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
# install.packages("TMB", type = "source") # if package version inconsistency detected during loading of foieGras
install.packages("aniMotum",
                 repos = c("https://cloud.r-project.org",
                           "https://ianjonsen.r-universe.dev"),
                 dependencies = TRUE) #foiegras was removed from CRAN and replaced with Animotum on 12-12-2022
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

# A3: Specify data and saveloc ----
saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/OutputData/CTCRW/" #Adjust this

### ....................................................................................................
### [B] SSM with prefiltered data ----
### ....................................................................................................

# Remember: if you use prefiltered data, this means raw argos data was filtered to remove
# spurious detections based on  a speed-distance-angle algorithm. Based on the script/file either
# the sda filter from the package "argosfilter" or "trip" was used.
# In either case, the files were already adjusted for near duplicated observations so this does not
# need to be done again here in section B.

# Define first which sda filter you used

speedfilter <- "Argosfilter" # choices: "Argosfilter" or "TripSDA"

# B1: Import the filtered data ----

mydets_f <- list.files(path = "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/CTCRW/",
                       pattern = paste0(speedfilter,"_.+.R"),
                       full.names = TRUE ) %>%
  purrr::map_dfr(readRDS) # make sure that there is only the most up to date speedfilter file in the directory

## check the data
# print(n = 1000, mydets_f %>% group_by(id) %>% dplyr::summarise(n = n()))
# A tibble: 9 × 2
# id         n
# <chr>  <int>
# 1 183623   799
# 2 200368   657
# 3 200369   166
# 4 209020   132
# 5 222133   577
# 6 235283    73
# 7 244607    96
# 8 244608   582
# 9 261743    21

# B2: housekeeping, tidy data and define column classes ----

## the fit_ssm and functions in the animotum package expect a data.frame or tibble in
## the following format if (!) error elipse information is available. If the data was collected
## using other methods than the CLS Argos' Kalman filter model other formats might be needed.
## Check: https://ianjonsen.github.io/aniMotum/articles/Overview.html

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
det_f <-mydets_f %>%
  dplyr::select( # select relevant columns, here: id, date, location class (lc), lon, lat,
    id,
    date,
    lc,
    lon,
    lat,
    smaj,
    smin,
    eor
  )

## check if there are Z locations left (should not!)
print(paste0("There are ", length(which(det_f$lc == "Z")), " Z-locations left."))

# B4: Known/tagging locations ----

## we already added tagging locations in a previous step. If you have no FastLoc data
## and no GPS location classes, then the tagging locations can be filtered
## by going for lc == G. If there is GPS data this section needs to be updated using the
## type == "user" argument.

## If this is not the case and the smaj, smin, eor data is already present from the previous
## script this can be commented out

# det_f <- det_f %>%
#   dplyr::mutate(
#     smaj = ifelse(lc == 'G', 50,
#                   smaj),
#     smin = ifelse(lc == 'G', 50,
#                   smin),
#     eor = ifelse(lc == 'G', 0,
#                  eor)
#   )

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

for (i in 1:length(unique(det_f$id))){

  # plot the filtered locations by individual
  ggplot() +
    annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
    layer_spatial(sf_points[i,], size = 0.5) +
    layer_spatial(sf_lines[i,], size = 0.75,aes(color = id)) +
    #scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
    scale_colour_manual(values = mycolors[i]) +
    theme() +
    ggtitle("Speedfiltered (argosfilter::sdafilter()) Argos Location Paths",
            subtitle = paste("S. mokarran from Andros (n = ", length(unique(det_f$id)), ")"))

  # save it
  ggsave(paste0(saveloc,"Speedfiltered_Argos_data_individual_", i, ".tiff"),
         width = 21, height = 15, units = "cm", device ="tiff", dpi=300)
}

## Notes code: if the code throws some errors/warning at you. However, there are not impacting
## the output, especially since the output only serves finding locations that are biologically
## unreasonable.

## Notes from comparing individual plots.
## Most individuals are fine. Some have locations on land in Andros, but given that
## the island is small in relation to available instrument accuray, these can be left in for now.
## Locations on land will later be dealt with by re-routing the tracks around land barriers

## 235283 has 1 spurious location at around 35°W and 34°N
## 2023-07-22 00:57:32 at lon. -36.06180 and lat: 34.09160

## Filter out these segments manually
det_f <- det_f %>%
  filter(!(
    # 235283
    (id == "235283" & date == as.POSIXct("2023-07-22 00:57:32", tz = "UTC"))
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
          subtitle = paste("S. mokarran from Andros (n = ", length(unique(det_f$id)), ")"))
ggsave(paste0(saveloc,"Speedfiltered_Argos_data_all_individuals_clean.tiff"),
       width = 21, height = 15, units = "cm", device ="tiff", dpi=150)

### IND
for (i in 1:length(unique(det_f$id))){

  # plot the filtered locations by individual
  ggplot() +
    annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
    layer_spatial(sf_points_clean[i,], size = 0.5) +
    layer_spatial(sf_lines_clean[i,], size = 0.75,aes(color = id)) +
    #scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
    scale_colour_manual(values = mycolors[i]) +
    theme() +
    ggtitle("Speedfiltered (argosfilter::sdafilter()) and manually checked Argos Location Paths",
            subtitle = paste("S. mokarran from Andros (n = ", length(unique(det_f$id)), ")"))

  # save it
  ggsave(paste0(saveloc,"Speedfiltered_Argos_data_individual_", i, "_clean.tiff"),
         width = 21, height = 15, units = "cm", device ="tiff", dpi=150)
}


# B6: Calculate the time difference between detections in days and segment tracks ----

## Depending if all sharks survived and/or all tasg were deployed there might be NA values in the dataset
## check
check <- det_f[is.na(det_f$date),] # if these are NAs coming from post-release mortality sharks or undeployed tags, delete them
nrow(check) # if this is 0 you are good to continue

## Calculate the time difference between detections in days
det_f$tdiff.days <- unlist(tapply(det_f$date, INDEX = det_f$id,
                                  FUN = function(x) c(0, `units<-`(diff(x), "days"))))

## Find the maximum values
max(det_f$tdiff.days)
# [1] 57.43806

## Assign different segments if the time difference is larger than the cutoff
## we choose a cut off of 10 days
## Make sure to retain original ptt info and arrange df by timestamp
det_seg <- det_f %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(id = paste0(id,"_", 1+cumsum(tdiff.days >= 10))) %>%
  dplyr::mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1))) %>%
  dplyr::arrange( # arrange by timestamp by individual so df can be used for fit_() functions
    shark,
    date
  )

## Let's check if there are sharks that have the tagging data as single segment
print(n = 1000,
      det_seg %>%
        dplyr::group_by(shark)%>%
        dplyr::slice(1:2)
)
## currently two sharks: 235823 and 261743 that have those issue
## we need to add the tagging event manually later on
## to do so we extract the tagging location of these sharks
extra_tagging <- det_seg %>%
  # dplyr::filter(
  #   shark %in% c(235283, 261743)
  # ) %>%
  dplyr::group_by(
    shark
  ) %>%
  dplyr::slice(1) # only get first row, i.e. tagging date

## Based on Logan et al. 2020, segments of <20 days should be removed
## Count number of rows per id
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

### Filter out short track segments
del_obs <- dplyr::filter(tracklengths, num_locs < 8 | tracklength_in_days < 12) # find suitable parameters
# del_obs <- dplyr::filter(tracklengths, tracklength_in_days < 15) # find suitable parameters

print(n = 100, del_obs); nrow(del_obs); sum(del_obs$num_locs)
# [1] 33 segments
# [1] 112 detections

det_seg_tl <- det_seg %>%
  inner_join(., tracklengths) %>%
  dplyr::filter( # remove tracks shorter than X days and/or less than Y total observations
    !(tracklength_in_days < 12 | num_locs < 8)
    # !(tracklength_in_days < 12)
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

plot(hist, freq = F
     , ylim = c(0, 100)
     , col = "skyblue") # approximately 80 % of detections are less or equal to 12 hours apart. Use this info for track predictions.

# B7: fit SSM using aniMotum ----

### make a map of your raw data before model fitting
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_point(data = det_seg_tl, aes(x = lon, y = lat, colour = id), size = 2, shape = 20) +
  coord_sf(xlim = c(min(det_seg_tl$lon - 0.5), max(det_seg_tl$lon + 0.5)),
           ylim = c(min(det_seg_tl$lat - 0.5), max(det_seg_tl$lat + 0.5)), expand = F)

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

## Do you want to calculate fitted observations across entire track?
entire_track <- "No" # change between "Yes" and "No", "No" means you are predicting observations at a given time interval using the segmented tracks

if (entire_track == "Yes"){ ## DO NOT change this
  dpf <- det_f
} else (dpf <- det_seg_tl)

## Remember: According to Lea et al. 2013

## "because each raw position has a different error field according to its Argos location class, we needed to
## decide the most probable location for each point within its error field. We achieved this by using a Bayesian
## state-space model (SSM) that adjusted the filtered tracks by producing regular positions based on the Argos location class,
## mean turning angle, and autocorrelation in speed and direction, producing the most probable track through the error fields"

## "Argos tracks only have locations for when the sharks were at the surface; consequently there is high
## variability in the number of locations in a given area, as a result of the shark’s varied surfacing behaviour
## rather than because of its actual location. This would introduce a bias into the analysis of time spent in different
## areas. To correct this bias, linear interpolation was used to normalise the transmission fre- quency by generating
## points at 12 hour intervals along track gaps of <20 days."

## Speedfilter
## For the prefiltering of the data we need a speedfilter. While other Sphyrna papers use the same speed
## as Vaudo et al. 2017 for mako sharks, this possibly is too high.
## Based on Ryan et al. 2015: https://link.springer.com/article/10.1007/s00227-015-2670-4
## we could use speed as function of FL, i.e. speed = 1xFL [m] * s^-1
## Or we could use modelled cruising speed from Payne et al. 2017, i.e. 2.1 m/s

## Prefilter
#vmax = 2.1 # based on Payne et al. 2016
#ang = c(15,25) # Vaudo et al. 2017, values are internal spikes, i.e. for values you need to 165 = 180 - 15, 155 = 180 - 25
#distlim = c(5000, 8000) # Vaudo et al. 2017
spdf = F
pf = F


## Model
model = "crw" # choose between rw, crw, mp. mpm and jpmp available in fit_mpm

## Fitted vs. predicted, i.e. choice of normalisation time-step
if(entire_track == "Yes"){
  time.step = NA # NA turns the time step off and estimates locations at observation times only
} else (time.step = 12)


## Optimizers
optim = "optim"
maxit = 2000
verbose = 2

## Specify SSM
mod.crw_pf <- aniMotum::fit_ssm(
  as.data.frame(dpf), # somehow getting errors if not ran with 'd' in data.frame class
  #vmax = vmax,
  #ang = ang,
  #distlim = distlim,
  spdf = spdf,
  min.dt = 0, # we have taken care of this during sda filtering steps
  pf = pf,
  model = model,
  time.step = time.step,
  #fit.to.subset = T,
  control = ssm_control(
    optim = optim,
    maxit = maxit,
    verbose = verbose), # shows parameter trace, 0: silent, 1: optimizer trace, 2: parametre trace (default)
  #map = list(psi = factor(NA))
)
# saveRDS(mod.crw_pf, paste0(saveloc, "SSM_fitted.R"))
## we retain 17 segments

##Plot the mp model with normalised values
#tiff(paste0(saveloc, "SSM_MPM_output/SPOT_tracks_",spp.f,"_", model,"_",optim,"_",maxit, "iterations_fitted_with_argosfilter_output.tiff"),
#height = 30, width = 20, units = "cm", res = 150)
map(mod.crw_pf, what = if(entire_track == "Yes"){"f"} else ("p"), normalise = TRUE, silent = TRUE)
#dev.off()

# The `normalise` argument rescales the estimates to span the interval 0,1.
# Move persistence estimates from `fit_ssm()` tend to be smoothed more extremely compared
# those obtained from `fit_mpm()` and can lack contrast. Normalising the estimates provides a
# clearer view of changes in movement behaviour along tracks, highlighting regions where animals
# spend disproportionately more or less time. When fitting to a collection of individuals,
# the normalisation can be applied separately to each individual or collectively via
# `normalise = TRUE, group = TRUE`. In the latter case, the relative magnitudes of move persistence
# are preserved across individuals.

# B8: re-route path to account for land locations

## Some locations may still on land. Reroute them using route_path()
mod.crw_pf_rr <- route_path(mod.crw_pf,
                            what = if(entire_track == "Yes"){"fitted"} else ("predicted"),
                            map_scale = 10, # scale of rnaturalearth map to use for land mass - if you want 10, you need the package "rnaturalearthhires"
                            dist = 10000, # buffer distance (m) to add around track locations. The convex hull of these buffered locations defines the size of land polygon used to aid re-routing of points on land.
                            buffer = 2,  # buffer distance > 0 moves the track a bit further away from any land barriers, which can (sometimes) result in a better rerouted solution. Some trial and error with the buffer distance (units in km) is usually needed
                            centroids = TRUE, # setting centroids = TRUE essentially adds more resolution (& therefore more possible solutions) to the underlying mesh structure used to reroute the track.
                            append = T
                            )

## check if re-routing track changes nr. observations
check_1 <- grab(mod.crw_pf, what = if(entire_track == "Yes"){"fitted"} else ("predicted"))
check_1 <- check_1[,c(1:2)]
check_rr <- grab(mod.crw_pf_rr, what = "rerouted")
check_rr <- check_rr[,c(1:2)]
comparison <- anti_join(check_1, check_rr) # shows rows that are not present in check_rr , we lose 20 land locations

## regarding data loss issue: see response ianjonsen
## https://github.com/ianjonsen/aniMotum/discussions/67#discussioncomment-10418664

## map the locations to check if it improved
my.aes <- aes_lst(line = T,
                  conf = F,
                  mp = F,
                  obs = T)
m1 <- map(mod.crw_pf,
          what = if(entire_track == "Yes"){"fitted"} else ("predicted"),
          aes = my.aes,
          ext.rng = c(0.3, 0.1),
          silent = TRUE)

m2 <- map(mod.crw_pf_rr,
          what = "rerouted",
          aes = my.aes,
          ext.rng = c(0.3, 0.1),
          silent = TRUE)


### Comparison of locations between fitted vs. fitted & re-routed
# (m1 + labs(title = "SSM locations") | m2 + labs(title = "SSM re-routed locations")) &
#   theme(panel.grid = element_line(size=0.1, colour="grey60"))

### Detailview fitted & re-routed locations
m1 + labs(title = "Fitted/predicted locations") &
  theme(panel.grid= element_line (size=0.1, colour="grey60"))
ggsave(paste0(saveloc,"Predicted_locations_time_step_", time.step,"_h.tiff"), width = 15, height = 12, units = "cm", dpi = 150)

### Detailview fitted & re-routed locations
m2 + labs(title = "Re-routed locations") &
  theme(panel.grid= element_line (size=0.1, colour="grey60"))
ggsave(paste0(saveloc,"Rerouted_locations_time_step_", time.step,"_h.tiff"), width = 15, height = 12, units = "cm", dpi = 150)

# B8: check goodness of fit of your ssm/mpm model ----

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
for (i in 1:2){

  plottype = i # change accordingly, save plottype 1 and 2

  IDs <- (mod.crw_pf_rr$id) ## change accordingly

  for (i in 1:length(IDs)){

    # open plot window
    png(file = paste0(saveloc,"Validation/Goodness_of_fitted_rerouted_", model, "_locations_", IDs[i],"_fitted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter_type_",plottype,"_plot.png"), res = 150, height = 20, width = 30, units = "cm")

    # plot the fitted locations by individual
    print(plot(mod.crw_pf_rr[i,], "rerouted", type = plottype, alpha = 0.1)) # "f" for fitted and type=2 for 2-D

    # save it
    dev.off()
  }
}

# Residual plots are important for validating models, but classical Pearson residuals,
# for example, are not appropriate for state-space models. Instead, a one-step-ahead prediction
# residual, provides a useful if computationally demanding alternative.
# In `aniMotum`, prediction residuals from state-space model fits are calculated using the
# `osar` function and can be visualized as time-series plots, Q-Q plots, or autocorrelation
# functions:

## calculate & plot residuals

for (i in 1:length(IDs)){

  # subset individual tracks
  res.s <- osar(mod.crw_pf_rr[i,]) ## change accordingly

  # open plot window
  png(file = paste0(saveloc,"Validation/Prediction_", model,"_fitted","_rerouted_residuals_for_validation_", IDs[i],"_fitted_with_", optim, "_",maxit,"iterations_", speedfilter, "_filter.png"), res = 150, height = 20, width = 30, units = "cm")

  #plot
  (plot(res.s, type = "ts") | plot(res.s, type = "qq")) /
          (plot(res.s, type = "acf") | plot_spacer())

  # save it
  dev.off()
}

# B9: export the model output ----

## what model did you calculate and want to export

m_type <- if(entire_track == "Yes"){"fitted"} else ("predicted") # choose between "Fitted", "Predicted", and "Rerouted" case sensitive

## fitted & predicted locations
### non-projected form
loc <- grab(mod.crw_pf, what = if(entire_track == "Yes"){"fitted"} else ("predicted"))

### projected form
loc.proj <- grab(mod.crw_pf, what = if(entire_track == "Yes"){"fitted"} else ("predicted"), as_sf = T)

## rerouted locations, either fitted or predicted
### non-projected form
locRR <- grab(mod.crw_pf_rr, what = "rerouted")

### projected form
locRR.proj <- grab(mod.crw_pf_rr, what = "rerouted", as_sf = T)

# B10: save the model output as .csv (non-projected form) and as R file (both) ----

# fitted & predicted
write.table(loc, paste0(saveloc, m_type, "/Data_aniMotum_CTCRW_output_",m_type,"_non-projected_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(loc, paste0(saveloc, m_type, "/Data_aniMotum_CTCRW_output_",m_type,"_non-projected_with_",speedfilter, "_data.rds"))
saveRDS(loc.proj, paste0(saveloc, m_type, "/Data_aniMotum_CTCRW_output_",m_type,"_projected_with_",speedfilter, "_data.rds"))
## fitted only
saveRDS(loc, paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/kmeans/Data_aniMotum_CRW_output_","fitted","_with_",speedfilter, "_data.rds"))

# rerouted
write.table(locRR, paste0(saveloc, "rerouted", "/Data_aniMotum_CTCRW_output_",m_type, "_rerouted","_non-projected_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(locRR, paste0(saveloc, "rerouted", "/Data_aniMotum_CTCRW_output_",m_type,"_rerouted","_non-projected_with_",speedfilter, "_data.rds"))
saveRDS(locRR.proj, paste0(saveloc, "rerouted", "/Data_aniMotum_CTCRW_output_",m_type,"_rerouted","_projected_with_",speedfilter, "_data.rds"))

### ....................................................................................................
### [C] Calculate CI for location estimates and convert to lat/lon ----
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

# chose your data
mod_data <- locRR
mod_data_proj <- locRR.proj

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


## if you have predicted locations you need to add the tagging locations again
tagging_selected <- extra_tagging %>%
  dplyr::select(any_of(names(mod.final)))
tagging_selected$date <- strptime(tagging_selected$date, format = "%Y-%m-%d %H", tz = "UTC")

if (entire_track == "No")  {
  mod.final <- mod.final %>%
    dplyr::full_join(tagging_selected, by = c("id", "date"), suffix = c("", "_new")
  ) %>%
    dplyr::mutate(
      lon = ifelse(is.na(lon), lon_new, lon),
      lon025 = ifelse(is.na(lon025), lon, lon025), # if tagging date got removed, lon025 will be NA, make sure this is tagging loc
      lon975 = ifelse(is.na(lon975), lon, lon975), # see above
      lat = ifelse(is.na(lat), lat_new, lat),     # see above
      lat025 = ifelse(is.na(lat025), lat, lat025), # see above
      lat975 = ifelse(is.na(lat975), lat, lat975)  # see above
    ) %>%
    dplyr::select( # drop columns that are not needed
      -lon_new,
      -lat_new,
      -shark
    )
} else (mod.final <- mod.final)

## make sure that the df is showing dates in ascending order

mod.final %<>%
  dplyr::mutate(shark = as.numeric(str_sub(id, start = 1, end = str_locate(id, "\\_")[,1] - 1))) %>%
  dplyr::arrange(shark, date) %>%
  dplyr::select(-shark)

### ....................................................................................................
### [E] Save movement data for further analyses ----
### ....................................................................................................

# if based on speedfilter data
write.table(mod.final, paste0(saveloc, "rerouted", "/Data_aniMotum_CRW_output_","rerouted","_proj_WGS84_converted_with_coord_CIs_with_",speedfilter, "_data.csv"),row.names=F,sep=",",dec=".")
saveRDS(mod.final, paste0(saveloc, "rerouted", "/Data_aniMotum_CRW_output_","rerouted","_proj_WGS84_converted_with_coord_CIs_with_",speedfilter, "_data.rds"))

saveRDS(mod.final, paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/kmeans/Data_aniMotum_CRW_output_","rerouted","_proj_WGS84_converted_with_coord_CIs_with_",speedfilter, "_data.rds"))
saveRDS(mod.final, paste0("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Andros_Hammerheads/InputData/dBBMM/Data_aniMotum_CRW_output_","rerouted","_proj_WGS84_converted_with_coord_CIs_with_",speedfilter, "_data.rds"))

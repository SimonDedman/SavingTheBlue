### ====================================================================================================
### Project:    Movement (behavior) analysis
### Analysis:   Fitting continuous-time state-space models and quality control of SPOT tag data with foieGras
### Script:     Rscript_satellite_movement_data_cleaning_and_modelling_with_foieGras.R
### Author:     Vital Heim
### Version:    1.0
### ====================================================================================================

### ....................................................................................................
### Content: this script contains the code to clean and filter satellite-linked geolocator data
### using the foiegras package. The package allows the calculation of continuous-time random
### and correlated random walk state-space models for quality control of animal tracking data. 
### Latent variable models are provided to estimate move persistance along tracks as an index
### behaviour.
### For further information: 
### https://esajournals.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/ecy.2566
### ....................................................................................................

### ....................................................................................................
### [A] Ready environment, load packages ----
### ....................................................................................................

# A1: clear memory ----

rm(list = ls())

# A2: load necessary packages ----

library("dplyr")
library("magrittr")

#install.packages("TMB", type = "source") # if package version inconsistency detected during loading of foieGras
#install.packages("foieGras")# if first time
library("foieGras")

#install.packages("patchwork")
library("patchwork")

library("sf")
install.packages("sp")
library("sp")


# A3: Specify data and saveloc ----
saveloc <- "C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/OutputData/SSM_MPM_output/" #Adjust this

### ....................................................................................................
### [B] Data import and housekeeping ----
### ....................................................................................................

# The 'foieGras' package allows the data to be pre-filtered during the calculation of a state-space
# model. Therefore, there should (check!) be no need to import data previously filtered using e.g.
# the 'argosfilter'package.
# We therefore import the raw data from the satellite-linked geolocators.
# Import the "\\-Locations.csv" files as they contain the actual locations based on multiple transmissions so you
# do not ahve to filter duplicates.

# We need some metadata of the sharks, namely tagging time and location so that these a) can be
# added to the dataframe and b) used to filter out detections that happened during tag initialization
# and pre-deployment.

# B1: Import satellite-derived movement data and shark metadata ----

## Movement data
mov_all ="C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/InputData"
all_csv = dir(mov_all, recursive=T, full.names=T, pattern="\\-Locations.csv$") # import files in folders in path directory all at once
mydata = lapply(all_csv, read.csv,sep=",",dec=".",stringsAsFactor=F,header=T) # import all .csv files containing TAT-Hiso data, but skip header lines
mydets <- do.call("rbind",mydata)

## Shark metadata
tags <- read.table("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/InputData/Datasheet_Bahamas_Smok_Tagging_Metadata.csv",sep=",",dec=".",header=T,na.strings=c(""," ",NA))
tags$tagging <- as.POSIXct(tags$tagging,format="%Y-%m-%d %H:%M",tz="US/Eastern")
tags <- dplyr::select(tags, id, tagging, lat, lon)
colnames(tags) <- c("id", "date", "lat", "lon")
tags$lc <- "3" # we add a location class criteria for later joining with the movement data
tags$id <- as.character(tags$id)

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
ddet <-mydets %>%
        dplyr::select( # select relevant columns, here: id, date, location class (lc), lon, lat, 
          Ptt,
          Date,
          Type,
          Quality,
          Longitude,
          Latitude,
          Error.Semi.major.axis,
          Error.Semi.minor.axis,
          Error.Ellipse.orientation
        ) %>%
        filter( # locations that were user specified within the Wildlife Data Portal
          !Type %in% c("User")
        ) %>%
        mutate( # define Date format
          Date = as.POSIXct(Date,format="%H:%M:%S %d-%b-%Y", tz="UTC"),
          Date.EST = as.POSIXct(format(Date, tz = "US/Eastern")), # SPOT tag timestamps are in UTC, Bahamas and US visited areas of our sharks are in EST
          Ptt = as.character(Ptt) # define tag id as character class
        ) %>%
        dplyr::rename( # rename the columns so they fit the requirements for the fit functions
          id = Ptt,
          date = Date.EST,
          lc = Quality,
          lon = Longitude,
          lat = Latitude,
          smaj = Error.Semi.major.axis,
          smin = Error.Semi.minor.axis,
          eor = Error.Ellipse.orientation
        ) %>%
        dplyr::select( # remove UTC time column
          -Date,
          -Type
        ) 

## add tagging date for subsequent filtering
ddet <- dplyr::left_join(ddet, tags, by = "id")

## remove detections pre-tag deployment and and tagging location as data point
ddet %<>%
  filter( # remove occurences that happened before the release time
    date.x >= date.y
  ) %>%
  dplyr::select( #get rid of unneeded columns
    -date.y,
    -lat.y,
    -lon.y,
    -lc.y
  ) %>% 
  dplyr::rename(
    date = date.x,
    lc = lc.x,
    lon = lon.x,
    lat = lat.x,
  ) %>%
  full_join( # add tagging info
    tags
    ) %>%
  arrange( # arrange by timestamp by individual so df can be used for fit_() functions
    id,
    date
  )

# B3: split movement tracks into segments if time gaps too large ----

## hSSMs experience difficulties and can produce spurious results if there are large detection gaps
## in the tracking data. Tracks should therefore be broken into multiple segments if there are long detection gaps

## Based on Logan et al. 2020: we will break tracks into multiple segments if the gaps between
## Argos locations are >10 days.

## Resulting segments <20 days will be excluded

## *B3.1: calculate the time difference between detections in days ----
ddet$tdiff.days <- unlist(tapply(ddet$date, INDEX = ddet$id,
                               FUN = function(x) c(0, `units<-`(diff(x), "days"))))

## Find the maximum values
max(ddet$tdiff.days)

# 2nd step: assign different segments if the time difference is larger than the cutoff
ddet_clean <- ddet %>%
  group_by(id) %>%
  mutate(id = paste0(id,".", 1+cumsum(tdiff.days >= 10)))

## *B3.2: Find segments shorter than 20 days and remove them ----

### Based on Logan et al. 2020, segments of <20 days should be removed
### Count number of rows per id
tracklengths <- ddet_clean %>% count(id)
#View(tracklengths)

### Add to the df in a new column
ddet_clean %<>% 
  inner_join(., tracklengths)

### Remove all segments shorther than 20 days
ddet_cleaner <- ddet_clean[!ddet_clean$n < 20, ]

### check
table(ddet_cleaner$id)

# B4: Find the distribution of time gaps between detections ----

ddet_cleaner$tdiff.days[ddet_cleaner$tdiff.days < 0] <- 0

hist <- hist(ddet_cleaner$tdiff.days
             , breaks = c(seq(from = 0, to = 20, by = .5))
             , plot = F)

hist$density <- hist$counts / sum(hist$counts)*100   
#hist$density

plot(hist, freq = F
     , ylim = c(0, 100)
     , col = "skyblue")

## Based on how many detections were made within which time gap, you might want to adjust the time step,
## i.e. the 'time.step =' argument within the 'fit_' functions



### ....................................................................................................
### [C] Fitting a foieGras ssm model ----
### ....................................................................................................

# C1: define parameters

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

d <- ddet_cleaner %>%
           dplyr::select( #get rid of unneeded columns
               id,
               date,
               lc,
               lon,
               lat,
               smaj,
               smin,
               eor
           )
  

## Prefilter
vmax = 1.2 # based on Logan et al. 2020, Wells et al. 2018 and Vaudo et al. 2017
ang = c(15,25) # Vaudo et al. 2017, values are internal spikes, i.e. for values you need to 165 = 180 - 15, 155 = 180 - 25
distlim = c(5000, 8000) # Vaudo et al. 2017
spdf = TRUE
pf = F


## Model
model = "crw"
time.step = 12 # NA turns the time step off and estimates locations at observation times

## Specify SSM
mod.s <- fit_ssm(
  as.data.frame(d), # somehow getting errors if not ran with 'd' in data.frame class
  vmax = vmax,
  ang = ang,
  distlim = distlim,
  spdf = spdf,
  #min.dt = 20,
  pf = pf,
  model = model,
  time.step = time.step,
  fit.to.subset = T,
  control = ssm_control(verbose = 2), # shows parameter trace, 0: silent, 1: optimizer trace, 2: parametre trace (default)
)

ind.s <- grab(mod.s, as_sf = T)

## Assessing goodness of fit using prediciton residuals, or one-step-ahead residuals
res.s <- osar(mod.s[9,]) # calculate predicition residuals - do for one animal at a time as computationally v. demanding

(plot(res.s, type = "ts") | plot(res.s, type = "qq")) /
  (plot(res.s, type = "acf") | plot_spacer()) # visual assesment of time-series, q-q- and autocorrelation function plots

### ....................................................................................................
### [D] Fitting a foieGras mpm model ----
### ....................................................................................................

# The foigras package allows you to fit a randowm walk with time-varying move persistence to temporally regular
# or irregular location data

# The fit_mpm() function expects the following arguments
# x = a fG_ssm fit object or a data frame of observations (see details)
# what = if a fG_ssm fit object is supplied then what determines whether fitted or predicted (default) values are mapped; ignored if x is a data frame
# model = mpm model to fit; either mpm with unpooled random walk variance parameters (sigma_(g,i)) or jmpm with a single, pooled random variance parameter (sigma_g)
# coords = column numbers of the location coordinates (default = 3:4)
# control = list of control settings for the outer optimizer (see mpm_control for details)
# inner.control = list of control parameters for the inner optimization
# optim [Deprecated] = use ssm_control(optim = "optim") instead, see ssm_control for details
# optMeth [Deprecated] = use ssm_control(method = "L-BFGS-B") instead, see ssm_control for details
# verbose [Deprecated] = use ssm_control(verbose = 1) instead, see ssm_control for details

## Define parameters
x = mod.s
what = "predicted" # default is "predicted", but that only works if you used a regularised time step, if you just fit the observations, you need to choose "fitted" here
model = "jmpm" # parameter estimation is improved when conducted jointly across multiple individual datasets
coords = c(3:4) # defaul is 3:4 and corresponds to if x = fSSM object, if x = dataframe, you need to adjust
controlmpm = mpm_control(verbose = 2)

## Fit move persistance model (mpm)
mod.m <- fit_mpm(
  x,
  what = what,
  model = model,
  coords = coords,
  control = controlmpm
)

ind.m <- grab(mod.m, as_sf = T)

### ....................................................................................................
### [E] Combine state-space and move-persistence models ----
### ....................................................................................................

# The join() joins ssm-predicted locations and mpm-estimated behaviorual index into a single tibble. 
# if the predicted tibble is a projected sf object, so will be the output. This can be avoided if as_sf =? F.

mod.t.dd <- join(mod.s, mod.m, what.ssm = "predicted", as_sf = F)
mod.t.sf <- join(mod.s, mod.m, what.ssm = "predicted", as_sf = T)

### ....................................................................................................
### [F] Calculate CI for location estimates and convert to lat/lon ----
### ....................................................................................................

# The foiegras output offers a "merc" location (x and y) as well as the standard error of predicted
# locations (in merc projection also). In case we want to calculate UDs or similar down the line,
# we need a CI for each loaction estimate that then can be used as a estimate for the location error.

# To calculate a CI in decimal degress, we need to first calculate upper and lower (2.5% and 97.5%) limits
# of our locations. We can do this by adding/substracting 1.96xSE from the x and y coordinates.

# After that, we need to transform the coordinates from the output projection of foiegras to
# decimal degrees.

# F1: find the projection of your fitted ssm/mpm object

#> head(mod.t)
# CRS:           +proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs

# F2: calculate upper and lower limits (2.5 and 97.5%) of location estimates ----

## Make new columns for longitude/x
mod.t.sf$x.025 <- mod.t.sf$x - (1.96*mod.t.sf$x.se)
mod.t.sf$x.975 <- mod.t.sf$x + (1.96*mod.t.sf$x.se)

## Make new columns for latitude/y
mod.t.sf$y.025 <- mod.t.sf$y - (1.96*mod.t.sf$y.se)
mod.t.sf$y.975 <- mod.t.sf$y + (1.96*mod.t.sf$y.se)

# F3: coordinates into a WGS84 lat/lon projection ----

## general
cord.merc <- SpatialPoints(cbind(mod.t.sf$x, mod.t.sf$y), proj4string=CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs ")) # define SpatialPoints object with output projection
cord.dec <- spTransform(cord.merc, CRS("+proj=longlat +datum=WGS84")) # transform the coordinates to a lat/lon projection
### physical check:
par(mfrow = c(1, 2))
plot(cord.merc, axes = TRUE, main = "Merc Coordinates", cex.axis = 0.95)
plot(cord.dec, axes = TRUE, main = "Lat-Lon Coordinates", col = "red", cex.axis = 0.95)
### create dataframe that can be added to mod.t object
coords.new <- as.data.frame(cord.dec)
colnames(coords.new) <- c("lon.new", "lat.new")

## Lower CI
cord.lowCI <- SpatialPoints(cbind(mod.t.sf$x.025, mod.t.sf$y.025), proj4string=CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs ")) # define SpatialPoints object with output projection
cord.025 <- spTransform(cord.lowCI, CRS("+proj=longlat +datum=WGS84")) # transform the coordinates to a lat/lon projection
### physical check:
par(mfrow = c(1, 2))
plot(cord.lowCI, axes = TRUE, main = "Merc Coordinates", cex.axis = 0.95)
plot(cord.025, axes = TRUE, main = "Lat-Lon Coordinates", col = "red", cex.axis = 0.95)
### create dataframe that can be added to mod.t object
CI025 <- as.data.frame(cord.025)
colnames(CI025) <- c("lon025", "lat025")

## upper CI
cord.upCI <- SpatialPoints(cbind(mod.t.sf$x.975, mod.t.sf$y.975), proj4string=CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs ")) # define SpatialPoints object with output projection
cord.975 <- spTransform(cord.upCI, CRS("+proj=longlat +datum=WGS84")) # transform the coordinates to a lat/lon projection
### physical check:
par(mfrow = c(1, 2))
plot(cord.upCI, axes = TRUE, main = "Merc Coordinates", cex.axis = 0.95)
plot(cord.975, axes = TRUE, main = "Lat-Lon Coordinates", col = "red", cex.axis = 0.95)
### create dataframe that can be added to mod.t object
CI975 <- as.data.frame(cord.975)
colnames(CI975) <- c("lon975", "lat975")

# F3: create df with lower and upper CI for lcoation estiamtes in [dd]

mod.dd.df <- as.data.frame(cbind(mod.t.sf, coords.new, CI025, CI975, mod.t.dd$lon, mod.t.dd$lat))

## double check that conversion was correct.
#check <- dplyr::select(mod.dd.df, id, date, lon.new, lon025, lon975, lat.new, lat025, lat975, mod.dd.df[,26], mod.dd.df[,27])
#conversion.lon <- check$lon.new == check$mod.t.dd$lon
#length(conversion.lon[conversion.lon == F]) # needs to be 0
#conversion.lat <- check$lat.new == check$mod.t.dd$lat
#length(conversion.lat[conversion.lat == F]) # needs to be 0
## if 0 twice continue

## create df and prepare for saving
mod.final <- dplyr::select(mod.dd.df, id, date, lon.new, lon025, lon975, lat.new, lat025, lat975)
colnames(mod.final) <- c("id", "date", "lon", "lon025", "lon975", "lat", "lat025", "lat975")

### ....................................................................................................
### [F] Save movement data for further analyses ----
### ....................................................................................................

write.table(mod.t.sf, paste0(saveloc, "Data_foiegras_SSM_MPM_output_12hrs_predicted_raw_sf_output_merc_proj.csv"), row.names=F,sep=",",dec=".")
write.table(mod.t.dd, paste0(saveloc, "Data_foiegras_SSM_MPM_output_12hrs_predicted_raw_non-sf_output_merc_proj.csv"), row.names=F,sep=",",dec=".")
write.table(mod.final, paste0(saveloc, "Data_foiegras_SSM_MPM_output_12hrs_predicted_proj_WGS84_converted_with_coord_CIs.csv"), row.names=F,sep=",",dec=".")


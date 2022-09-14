### ====================================================================================================
### Project:    PhD - Satellite Telemetry
### Analysis:   Processing and cleaning satellite telemetry data of fin-mounted SPOT tags for further steps
### Script:     Rscript_Filtering_SPOT_tag_data_(4)
### Author:     Vital Heim
### Version:    3.0
### ====================================================================================================

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Content: this R script contains the code to filter, clean and process raw data collected by fin-
###          mounted Smart Position and Temperature (SPOT) transmitters. We use the argosfilter package
###          by Freitas et al. 2008.
###          Here processed data can then be used in further scripts and steps to model and analyze 
###          SPOT tag data.
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [A] Setwd, paths and parameters ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A1: Clear memory ----

rm(list = ls())

# A2: Set paths and define some parameters ----

setwd("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/") # make sure that you have the newest files in the directory
loc="C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/InputData/"
saveloc=paste("C:/Users/Vital Heim/switchdrive/Science/Projects_and_Manuscripts/Bahamas_Hammerheads_2022/OutputData/Initial_filter_data/",sep="")

sep=","
dec="."
dat.TZ="US/Eastern"

# 3rd step: load necessary packages

library("dplyr")       #R swiss army knife for data manipulation
library("lubridate")   #For manipulating time/date objects

#install.packages("argosfilter") # if first time
library("argosfilter") # to filter raw Argos data, i.e. SPOT tag data

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [B] Data import and initial check and filtering of missing information ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# B1: import data file(s) ----

# ............................................................... DON'T CHANGE
## define the animal-id
choice <- sort(unique(list.dirs(loc, full.names = F, recursive = F)))
spp <- sort(unique(DET$studyperiod))
# > choice
# [1] "177940" "177941" "177942" "183623" "200368" "200369" "209020" "23596" 
# ....................................................................... NEXT

# ........................................................ ADJUST ACCORDINGLY
## choose your shark
animalid <- choice[7]
# ....................................................................... NEXT

# ............................................................... DON'T CHANGE
## read in data files (implement for multiple IDs at some point)  
det <-read.csv(paste0(loc,animalid,"/", animalid,"-All.csv"), header = TRUE) # movement data
tag <- read.csv(paste0(loc, "Datasheet_Bahamas_Smok_Tagging_Metadata.csv"), header = T) # tagging info

## define tagging metadata
deploy.local <- as.POSIXct(tag[tag$id == animalid,4], tz = dat.TZ)
deploy.UTC <- as.POSIXct(format(deploy.local, tz = "UTC", usetz = T)) # change local time to UTC as SPOT tag data is in UTC
deployLat <- tag[tag$id == animalid, 5]
deployLon <- tag[tag$id == animalid, 6]

# B2: housekeeping ----

head(det)
str(det)
#View(det)

## Define Loc..date and msg.date
det$Loc..date<-as.POSIXct(det$Loc..date,format="%m/%d/%Y %H:%M:%S",tz="UTC")
det$Msg.Date<-as.POSIXct(det$Msg.Date,format="%m/%d/%Y %H:%M:%S",tz="UTC")
#View(det)

## Remove data from prior of tag deployment
r <- which(det$Platform.ID.No.== animalid & det$Loc..date < deploy.UTC)
if (length(r)>0){
  det <- det[-r,]
}

## remove rows with missing values in lat or long

sum(is.na(det$Latitude)) # if there are, delete them
sum(is.na(det$Longitude)) # if there are, delete them

det <- det[!is.na(det$Latitude), ] # check if all gone

## reclass location quality column for later filtering

class(det$Loc..quality)
det$Loc..quality <- as.character(det$Loc..quality)

unique(det$Loc..quality)

# B3: add tagging location and date

det.i <- det %>% 
           bind_rows(list(Latitude = deployLat,
                          Longitude = deployLon,
                          Loc..quality = "3",
                          Loc..date = deploy.UTC,
                          Error.radius = 50),.)
# ....................................................................... NEXT
 
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [C] Filter data based on speed and turning angles ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## To filter the track based on speed and turning angles, we use the package 
## "argosfilter" from Freitas et al. 2008

## To filter data based on turning angles, speed, distance etc. one can use the sdafilter()
## function. The locations are filtered using the Freitas et al. 2008 algorithm.

## Locations are filtered using the algorithm described in Freitas et al. (2008). The algorithm first
## removes all locations with location class Z (-9), which are the points for which the location process
## failed. Then all locations requiring unrealistic swimming speeds are removed, using the MacConnell et al. (1992) algorithm, unless the point is located at less than 5 km from the previous
## location. This procedure enables retaining good quality locations for which high swimming speeds
## result from location being taken very close to each other in time. The default maximum speed
## threshold is 2 m/s. The last step is optional, and enables to remove unlikely spikes from the animal’s path. The angles of the spikes should be specified in ang, and their respective length in
## distlim. The default is c(15,25) for ang and c(2500,5000) for distlim, meaning that all spikes
## with angles smaller than 15 and 25 degrees will be removed if their extension is higher than 2500
## m and 5000 m respectively. No spikes are removed if ang=-1. ang and distlim vectors must have
## the same length

## The output will be a vector with the following elements:
## "removed" = location removed by filter
## "not" = location not removed
## "end_location" = location a tht end of the track where the algorithm could not be applied

## The function needs the following arguments
## lat = numeric vector of latitudes in decimal degrees
## lon = numeric vector of longitudes in decimal degrees
## dtime = a vector of class POSIXct with date and time for each location
## lc = a numeric or character vector of Argos location classes. Z classes can be entered as "Z", "z", or -9
## vmax = speed threshold in ms-1. Default is 2ms-1
## ang = anles of the spikes to be removed. Default is c(15,25), No spikes are removed if ang = -1.
## distlim = lengths of the above spikes, in meters. Default is c(2500,5000).

# ............................................................... DON'T CHANGE
# C1: define parameters for filtering ----

lat = det.i$Latitude
lon = det.i$Longitude
dtime = det.i$Loc..date
lc = det.i$Loc..quality # remove Z locations
vmax = 4.5 # based on Logan et al. 2020, Wells et al. 2018 and Vaudo et al. 2017
ang = c(15,25) # Vaudo et al. 2017, values are internal spikes, i.e. for values you need to 165 = 180 - 15, 155 = 180 - 25
distlim = c(5000, 8000) # Vaudo et al. 2017

# C2: apply filter ----

## Plot unfiltered data
plot(lon,lat,col="lightgrey",type="l"
     ,xlim=c(min(lon)-0.5,max(lon)+0.5)
     ,ylim=c(min(lat)-0.5,max(lat)+0.5)
     ,xlab="Longitude",ylab="Latitude")

## Filter the data using sdafilter()
filter <- sdafilter(lat = lat, lon = lon, dtime = dtime, lc = lc, vmax = vmax, ang = ang, distlim = distlim)

## Check number of of locations by location class removed by the filter
table(lc,filter)
table(filter)

## Plot the filtered data
lines(lon[which(filter=="not")],lat[which(filter=="not")],col="blue")

# C3: remove the spurious locations from the dataset ----

## The output of the sdafilter() is a vector with three types of locations: 
## "removed", "not", "end_location"
## Since it is a vector, we can cbind() it to the detections dataset and then filter
## the rows that contain spurious detections

#View(filter)
length(filter); nrow(det.i)
clean <- cbind(det.i, filter)

#View(clean)

## Only keep rows where clean$filter == "not"
cleaner <- clean[clean$filter == "not",]
#View(cleaner)
# ....................................................................... NEXT

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [D] Save the filtered tracks for further analysis ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ............................................................... DON'T CHANGE
start <- as.Date(min(cleaner$Loc..date), format = "%Y-%m")
end <- as.Date(max(cleaner$Loc..date), format = "%Y-%m")

## csv
write.table(cleaner, file=paste(saveloc, animalid, "_filtered_SPOT_track_",start,"_",end,".csv",sep=""),row.names=F,sep=sep,dec=dec)
## txt
write.table(cleaner, file=paste(saveloc, animalid, "_filtered_SPOT_track_",start,"_",end,".txt",sep=""),row.names=F,sep=sep,dec=dec)
# ....................................................................... NEXT

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### [E] Filter out first 24hrs of data to eliminate bias from tagging event ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# E1: import individual data file to acquire tagging date ----

tagday <- det.i

## Only keep first row as this should contain the tagging date
tagday <- tagday[1,]

## adjust class of loc.date column
tagday$Loc..date <- as.POSIXct(tagday$Loc..date,format="%m/%d/%Y %H:%M:%S",tz="UTC")

# E2: filter out 24 hrs of data post tagging to elimiante bias from tagging event ----

tagday[,7]<-tagday[,7]+3600*24 # adjust
str(tagday)

a<-numeric()
a<-which(cleaner$Loc..date<tagday$Loc..date)


if (length(a)>0){
  cleaner24<-cleaner[-a,]
} else {
    cleaner24 <- cleaner
}

View(cleaner24)

# E3: save filtered data ----

## csv
write.table(cleaner24, file=paste(saveloc, animalid, "_filtered_SPOT_track_24hrs_filter",".csv",sep=""),row.names=F,sep=sep,dec=dec)
## txt
write.table(cleaner24, file=paste(saveloc, animalid, "_filtered_SPOT_track_24hrs_filter",start,"_",end,".txt",sep=""),row.names=F,sep=sep,dec=dec)
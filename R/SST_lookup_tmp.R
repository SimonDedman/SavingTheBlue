remotes::install_github('IMOS-AnimalTracking/remora', build_vignettes = TRUE, dependencies = TRUE)
library(remora)
library(ncdf4)
sst <- nc_open("C:/Users/simon/Downloads/20220402141456-NCEI-L3C_GHRSST-SSTskin-AVHRR_Pathfinder-PFV5.3_NOAA19_G_2022092_day-v02.0-fv01.0.nc",
               verbose = T)

# 24.55471, -77.6792
# April 2nd 2022
# 10.26 PM

#Subset to extents # -98 36, 8 65
LonIdx <- which(sst$dim$lon$vals > 24.55 & sst$dim$lon$vals < 24.6)
LatIdx <- which(sst$dim$lat$vals > -77.7 & sst$dim$lat$vals < -77.65)
# see https://stackoverflow.com/questions/58631421/ncvar-get-cannot-allocate-vector-of-size-for-netcdf4-subset-no-matter-how-smal
z <- ncvar_get(nc = sst, #get Z data for those extents
               varid = sst$var$sea_surface_temperature,
               start = c(LonIdx[1],
                         LatIdx[1],
                         1),
               count = c(length(LonIdx),
                         length(LatIdx),
                         1),
               verbose = T)
lon <- sst$dim$lon$vals[LonIdx]  # longitude array indexed by subset zone only
lat <- sst$dim$lat$vals[LatIdx]   # latitude array
nc_close(sst) #close connection
rm(list = c("sst", "LatIdx", "LonIdx"))
rownames(z) <- as.character(lon)
colnames(z) <- as.character(lat)
saveRDS(object = z, file = "z.Rds")
saveRDS(object = lon, file = "lon.Rds")
saveRDS(object = lat, file = "lat.Rds")
library(tidyr) # pivot longer
library(magrittr) # %<>%
ztbl <- as_tibble(z, rownames = "lon")
ztbl %<>% pivot_longer(-lon, names_to = "lat", values_to = "depth") #don't know if I did this originally, but this makes it a 3 column df
detach("package:tidyr", unload = TRUE) # else loads df_i as tibble which ruins things later
detach("package:magrittr", unload = TRUE) # else loads df_i as tibble which ruins things later
saveRDS(object = ztbl, file = "ztbl.Rds")
rm(list = c("z", "lat", "lon"))

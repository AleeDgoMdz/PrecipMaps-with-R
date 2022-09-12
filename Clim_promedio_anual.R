library(ncdf4)
library(sf)
library(tmap)                # Vizualizacion de datos
library(rgdal)               # Proyeccion de datos
library(spatialEco)          # Eliminacion de NA data
library(gstat)
library(raster)
source("functions.R")

netcdfs <- list.files("datos","*.nc$")
netcdfs <- as.list(netcdfs)
#netcdfName <-"CC_Temps_Vars_2000.nc" VALOR DE PRUEBA
shpPath <-"shapefileMX/mx.shp"
crsProj <-CRS("+proj=merc +a=6378137 +b=6378137
                            +lat_ts=0   +lon_0=0   +x_0=0
                            +y_0=0      +k=1       +units=m
                            +nadgrids=@null
                            +wktext     +no_defs")
resolution <- 200000
listados <- tail(netcdfs,22)
AnnualMeans <-list()
for (netcdfName in listados){
  precip <- rasterStack(netcdfName,shpPath,crsProj,resolution)
  precipMean <- calc(precip,fun = mean)
  print("-----FIN DEL PROMEDIO")
  AnnualMeans <-append(AnnualMeans,precipMean)
}
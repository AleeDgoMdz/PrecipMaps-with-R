#ESTE CODIGO ESTA DISENIADA PARA PRUEBAS UNITARIAS
library(ncdf4)
library(sf)
library(tmap)                # Vizualizacion de datos
library(rgdal)               # Proyeccion de datos
library(spatialEco)          # Eliminacion de NA data
library(gstat)
library(raster)
library(akima)
source("functions.R")

data <- nc_open("datos/CC_Temps_Vars_2000.nc")
geomMx   <- st_read("shapefileMX/mx.shp")
geomMx   <- as_Spatial(geomMx) #conversion a objeto reconocido en R

# CON ESTA FUNCION SE ASEGURA QUE NO HAYA NA EN LOS CAMPOS ESPACIALES
df <- coordsFilterNan(data,1)[c(1,2)]  
#Generacion del SpatialPoint object  y definicion de extension----
dfSpatial <-SpatialPoints(df,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
dfSpatial@bbox <- geomMx@bbox #Se asegura la extension de mx
#proyeccion de los datos ----
dfSpatial <-spTransform(dfSpatial,
                        CRS("+proj=merc +a=6378137 +b=6378137
                            +lat_ts=0   +lon_0=0   +x_0=0
                            +y_0=0      +k=1       +units=m
                            +nadgrids=@null
                            +wktext     +no_defs"))

geomMx <- spTransform(geomMx,
                      CRS("+proj=merc +a=6378137 +b=6378137
                            +lat_ts=0   +lon_0=0   +x_0=0
                            +y_0=0      +k=1       +units=m
                            +nadgrids=@null
                            +wktext     +no_defs"))
dfSpatial$precip <- coordsFilterNan(data,1)[3]
dfSpatial@bbox <- resizeMatrix(2000,dfSpatial@bbox)





xgrd <- seq(dfSpatial@bbox[1,1]+1000,dfSpatial@bbox[1,2],2000)
ygrd <- seq(dfSpatial@bbox[2,1]+1000,dfSpatial@bbox[2,2],2000)

#inter <- interp(dfSpatial,z="precip",xo=xgrd,yo=ygrd,duplicate = "strip",extrap = TRUE)
#rastInter <- raster(inter)
#clip <- mask(rastInter,geomMx)
#plot(clip)
#writeRaster(Clip,"akima","GTiff")

# --- Interpolacion con datos como lista y como como spatialdataframe----

xdata <- dfSpatial@coords[,1]
ydata <- dfSpatial@coords[,2]
zdata <- dfSpatial$precip
# Interpolacion y generacion de spatialPointDataFrame
interdos <- interp(xdata,ydata,zdata,xo=xgrd,yo=ygrd,duplicate = "mean",extrap=TRUE)
dataInter <- interp2xyz(interdos,data.frame = TRUE)
coordinates(dataInter) <- ~x+y
# generacion del raster
rasterEm <- raster(xmn=dfSpatial@bbox[1,1],
                   xmx=dfSpatial@bbox[1,2],
                   ymn=dfSpatial@bbox[2,1],
                   ymx=dfSpatial@bbox[2,2],
                   crs="+proj=merc +a=6378137 +b=6378137
                            +lat_ts=0   +lon_0=0   +x_0=0
                            +y_0=0      +k=1       +units=m
                            +nadgrids=@null
                            +wktext     +no_defs",
                   resolution=c(2000,2000),
                   vals=NA)
rasterFull <- rasterize(dataInter,rasterEm,"z",update=TRUE)
rastClip <- mask(rasterFull,geomMx)
plot(rastClip)

writeRaster(rastClip,"akimaLinear","GTiff")
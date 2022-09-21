#ESTE CODIGO ESTA DISENIADA PARA PRUEBAS UNITARIAS
library(ncdf4)
library(sf)
library(tmap)                # Vizualizacion de datos
library(rgdal)               # Proyeccion de datos
library(spatialEco)          # Eliminacion de NA data
library(gstat)
library(raster)
source("functions.R")

#carga de datos y shp de mx ----
data2000 <- nc_open("datos/CC_Temps_Vars_2000.nc")
geomMx   <- st_read("shapefileMX/mx.shp")
geomMx   <- as_Spatial(geomMx) #conversion a objeto reconocido en R

# CON ESTA FUNCION SE ASEGURA QUE NO HAYA NA EN LOS CAMPOS ESPACIALES
df <- coordsFilterNan(data2000,1)[c(1,2)]

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
#creacion del SpatialGrid object ----
#DEFINIR AQUI LA RESOLUCION DEL RESTER FINAL
grd<- createSpatialGrid(2000,dfSpatial)
#Obtencion de todos las interpolaciones por tiempo
IDWList <- list()
# ptm<-proc.time() ESTADISTICAS DE TIEMPO


for (ti in 1:length(ncvar_get(data2000,"Time"))){
  dfaux <- spatialandProyection(data2000,ti)
  rasterIDWClip <- IDWinterpolation(data2000,dfaux,ti,grd,geomMx)
  # dfaux es para tener la lista de coordenadas siempre vacias
  # grd grid a interporlar
  # 
  IDWList <-append(IDWList,rasterIDWClip)
}
IWDStack <- stack(IDWList,quick=TRUE)
meanIDW <- calc(IWDStack,fun=mean)
#plot(meanIDW)


writeRaster(meanIDW,"IDW_100_4","GTiff")

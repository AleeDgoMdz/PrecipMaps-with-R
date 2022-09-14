# Esta funcion ajusta el min y max de los limites de las capas espaciales
# con la finalidad de que la resolucion deseada se ajuste sin problemas
resizeMatrix <- function(resolution,bbox){
  xDist <- bbox[1,2]-bbox[1,1]
  yDist <- bbox[2,2]-bbox[2,1]
  xres  <- xDist%%resolution
  yres  <- yDist%%resolution
  xdif  <- resolution -xres #positivo
  ydif  <- resolution -yres #positivo
  bbox[1,1] <- bbox[1,1]-xdif  #suma algebraica (- - + = suma)
  bbox[2,2] <-bbox[2,2]+ydif #(+ +  = suma)
  return(bbox)
}  

# Esta funcion obtien el numero de celdas del SpatialGrid
ncells<- function(dfspatial,resolution){
  xDist    <- dfspatial@bbox[1,2]-dfspatial@bbox[1,1]
  yDist    <- dfspatial@bbox[2,2]-dfspatial@bbox[2,1]
  nRows    <- yDist/resolution
  nColumns <- xDist/resolution
  return(nRows*nColumns)
}
# Esta funcion genera un grid con determinada resolucion
# a partir de la extension de un SpatialPointsDataFrame
createSpatialGrid <- function(resolution,dfspatial){
  dfspatial@bbox <- resizeMatrix(resolution,dfspatial@bbox)
  totalCells <- ncells(dfspatial,resolution)
  grd <- as.data.frame(spsample(dfspatial,"regular",n = totalCells ))
  names(grd) <- c("x","y")
  coordinates(grd) <- c("x","y")
  gridded(grd)<- TRUE
  fullgrid(grd) <- TRUE
  proj4string(dfspatial)<- proj4string(dfspatial) #LINEA MODIFICADA, S MAYUSCULA,CAMBIO APROBADO
  proj4string(grd)<- proj4string(dfspatial)      #LINEA MODIFICADA, S MAYUSCULA, CAMBIO APROBADO
  return(grd)
}

# Esta funcion limpia los na values en los campos de coordenadas, diseniada para la 
# la construccion del primer dataframe y ser utilizada en la funcion IDWinterpolation
# ESTA FUNCION REGRESA UN DATAFRAME, NO UN DATAFRAMESPATIAL
coordsFilterNan <-function(netcdf,ti){
  df  <- data.frame(
    "long"=-ncvar_get(netcdf,"Longitude"),
    "lat" =ncvar_get(netcdf,"Latitude")
  )
  df$precip <- ncvar_get(netcdf,"PRECIP")[ti,]
  df <- na.omit(df)
  return (df)
}
# Esta funcion asgina el sistema de coordenadas y la proyeccion del netcdf
# Utilizar esta funcion evita problemas en la funcion IDWinterpolation
# Diseniada especificamente para su uso en la funcion IDWinterpolation
spatialandProyection <- function(netcdf,ti){
  dfspatial <- coordsFilterNan(netcdf,ti)[c(1,2)]
  dfspatial <- SpatialPoints(dfspatial,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  dfspatial <- spTransform(dfspatial,
                          CRS("+proj=merc +a=6378137 +b=6378137
                            +lat_ts=0   +lon_0=0   +x_0=0
                            +y_0=0      +k=1       +units=m
                            +nadgrids=@null
                            +wktext     +no_defs"))
  return(dfspatial)
}
#Esta funcion genera la IDW interpolation cortada a la forma de mexico(RasterLayer).
#Diseaniada para ser utilizada en un loop
#netcdf    --- archivo netcdf utilizado en este momento(se iterara sobre cada netcdf)
#dfspatial --- SpatialPoints object sin variables (unicamente coords)
#ti        --- Tiempo n para obtener valores de precipitaciones del netcdf
#grd       --- SpatialGrid para la interpolacion
#geomClip  --- SpatialPolygonsDataframe para cortar el raster
# FUNCION MODIFICADA. SE AGREGA EL PARAMETRO frameaux PARA SUSTITUIR LA EXTRACCION DE DATOS DEL NETCDF!!!!
IDWinterpolation <- function(netcdf,dfspatial,ti,grd,geomClip){
  #dfspatial$precip <- ncvar_get(netcdf,"PRECIP")[ti,] #Generacion de raster con timepo N ---LINEA COMENTADA, DESCOMENTAR EN CASO DE EXPLOTAR
  # LA LINEA ANTERIOR ERA UTILIZADA CUANDO SE OBTENIAN LOS VALORES DE PRECIP DIRECTAMENTE DEL NETCDF
  # DEPRECADO AL NO COINCIDIR CON EL NUMERO DE ROWS PUES HAN SIDO FILTRADOS
  dfspatial$precip <- coordsFilterNan(netcdf,ti)[c(3)]
  #dfspatial <- sp.na.omit(dfspatial,margin = 1) #Eliminacion de estaciones con NaN values- LOS VALORES YA HAN SIDO LIMPIADOS--
  interIDW<- gstat::idw(precip~1,dfspatial,newdata=grd,idp=4.0)  
  rastIWD <- raster(interIDW)
  rastIWDClip <- mask(rastIWD,geomClip)
  return (rastIWDClip)
}




#Esta funcion crea un RasterStack de un conjunto de rasters (uno por dia)
# a partir de un archivo netcdf que almacena los datos de precipitacion
#crsProj --- CRS object
rasterStack <- function(netcdfName,shpPath,crsProj,resolution){
  data <- nc_open(paste("datos/",netcdfName,sep =""))
  geomMx <- st_read(shpPath)
  geomMx <- as_Spatial(geomMx)
  #generacion de frame ----
  #df  <- data.frame(
  #  "long"=-ncvar_get(data,"Longitude"),
  #  "lat" =ncvar_get(data,"Latitude"))
  df <- coordsFilterNan(data,1)[c(1,2)]  
  
  dfspatial <-SpatialPoints(df,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  dfspatial@bbox <- geomMx@bbox #Se asegura la extension de mx
  #proyeccion de los datos ----
  dfspatial <-spTransform(dfspatial,crsProj)
  geomMx <- spTransform(geomMx,crsProj)
  #creacion del SpatialGrid object ----
  #DEFINIR AQUI LA RESOLUCION DEL RASTER PARA INTERPOLACION
  grd<- createSpatialGrid(resolution,dfspatial)
  #Obtencion de todos las interpolaciones por tiempo
  IDWList <- list()
  for (ti in 1:length(ncvar_get(data,"Time"))){
    dfaux <- spatialandProyection(data,ti)
    rasterIDWClip <- IDWinterpolation(data,dfaux,ti,grd,geomMx)
    IDWList <-append(IDWList,rasterIDWClip)
  }
  IWDStack <- stack(IDWList,quick=TRUE)
  return(IWDStack)
}

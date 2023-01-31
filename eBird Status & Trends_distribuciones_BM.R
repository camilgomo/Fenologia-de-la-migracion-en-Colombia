####### eBird status and trends - Descarga de distribuciones reproductivas de aves migratorias

##Instalar los paquetes necesarios
install.packages(c("tidyverse", "raster", "sf", "ebirdst", "rnaturalearth"))
remotes::install_github("CornellLabofOrnithology/ebirdst")
install.packages("ebirdst")
install.packages("rworldmap")


## Cargar los paquetes
library(rnaturalearth)
library(ebirdst)
library(raster)
library(dplyr)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(rworldmap)
library(terra)


#directorio

setwd("C:/Users/Usuario/Documents/SELVA/Migration_Phenology")
getwd()

#Mapa

worldMap <- getMap()

## Generar unos paletas de color que vamos a usar mas tarde
FineGray <- c("gray80", "gray72", "gray64", "gray56", "gray48", "gray42", "gray34", "gray26", "gray18", "gray10", "gray2", "black")
Ceruleanblue4 <- c("#40E0D04B", "cyan")
Forest <- c("#FFFFFF0A", "lightgreen")
Map <- c("#FFFFFF0A", "orange", "lightgreen", "darkgreen")
Baseline <- c("#FFFFFF0A", "#40E0D06E" )

## Llamar la capa de elevacion que vamos a usar como base y cortarlo a nuestra region de interes.
Ele <- raster("AltWorld.tif")
template <- Ele
extent(template)<- c(-200, -20, -60, 100)
Elevation <- crop(Ele,template)
plot(Elevation)

####### Activar la llave para aceder a las capas de eBird 
##expires KEY 18 July 2023 - f0spt9d4kstc
set_ebirdst_access_key("f0spt9d4kstc", overwrite = TRUE)


#####  Blackburnian Warbler ########################################################        
dl_path <- ebirdst_download(species = "Blackburnian Warbler") # Para bajar los shapes de cada especie se cambia el nombre aqui.

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez
dl_path
dl_path <-  "C:\\Users\\Usuario\\AppData\\Roaming\\R\\data\\R\\ebirdst\\2021\\bkbwar" 
#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año
BKWA <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.

#Periodo de invierno (TOMA UNOS MINUTOS EN COMPILAR)
BKWA1 <- BKWA[[1]]
BKWA2 <- BKWA[[2]]
BKWA3 <- BKWA[[3]]
BKWA4 <- BKWA[[4]]
BKWA5 <- BKWA[[5]]
BKWA6 <- BKWA[[6]]
BKWA7 <- BKWA[[7]]
BKWA8 <- BKWA[[8]]
BKWA49 <- BKWA[[49]]
BKWA50 <- BKWA[[50]]
BKWA51 <- BKWA[[51]]
BKWA52 <- BKWA[[52]]

#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

BKWA25 <- BKWA[[25]]
BKWA26 <- BKWA[[26]]
BKWA27 <- BKWA[[27]]
BKWA28 <- BKWA[[28]]
BKWA29 <- BKWA[[29]]
BKWA30 <- BKWA[[30]]
BKWA31 <- BKWA[[31]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

BKWA_Breeding <- BKWA25+BKWA26+BKWA27+BKWA28+BKWA29+BKWA30+BKWA31
plot(BKWA_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

BKWA_Breeding_WGS <- project(BKWA_Breeding, Elevation@crs)
plot(BKWA_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- BKWA_Breeding_WGS
ext(template)<- c(-200, -20, 0, 60)
CW2 <- crop(BKWA_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "BKWA_breeding.tif", overwrite =T)
BKWA_Breeding <- raster("BKWA_Breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

BKWA_Breeding
hist(BKWA_Breeding, ylim=c(0,10000))
quantile(BKWA_Breeding, c(.95, .975, .99)) 

## 99 quantile es 4.419968  y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

BKWA_Breeding1 <- BKWA_Breeding/4.419968 
BKWA_Breeding2 <- calc(BKWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
BKWA_Breeding3 <- calc(BKWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(BKWA_Breeding3, "BKWA_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(BKWA_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Summer Tanager ########################################################        
dl_path <- ebirdst_download(species = "Summer Tanager") # Para bajar los shapes de cada especie se cambia el nombre aqui.

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path
dl_path <-  "C:\\Users\\Usuario\\AppData\\Roaming\\R\\data\\R\\ebirdst\\2021\\sumtan" 

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

SMTN <- load_raster(product = "abundance", path = dl_path)

#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

SMTN25 <- SMTN[[25]]
SMTN26 <- SMTN[[26]]
SMTN27 <- SMTN[[27]]
SMTN28 <- SMTN[[28]]
SMTN29 <- SMTN[[29]]
SMTN30 <- SMTN[[30]]
SMTN31 <- SMTN[[31]]
SMTN32 <- SMTN[[32]]
SMTN33 <- SMTN[[33]]
SMTN34 <- SMTN[[34]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

SMTN_Breeding <- SMTN25+SMTN26+SMTN27+SMTN28+SMTN29+SMTN30+SMTN31+SMTN32+SMTN33+SMTN34
plot(SMTN_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

SMTN_Breeding_WGS <- project(SMTN_Breeding, Elevation@crs)
plot(SMTN_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- SMTN_Breeding_WGS
ext(template)<- c(-200, -20, 0, 60)
CW2 <- crop(SMTN_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "SMTN_breeding.tif", overwrite =T)
SMTN_Breeding <- raster("SMTN_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

SMTN_Breeding
hist(SMTN_Breeding, ylim=c(0,10000))
quantile(SMTN_Breeding, c(.95, .975, .99)) 

## 99 quantile es 6.970659   y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.
SMTN_Breeding1 <- SMTN_Breeding/6.970659  
SMTN_Breeding2 <- calc(SMTN_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
SMTN_Breeding3 <- calc(SMTN_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(SMTN_Breeding3, "SMTN_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(SMTN_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  American Redstart ########################################################        

dl_path <- ebirdst_download(species = "American Redstart")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

AMRE <- load_raster(product = "abundance", path = dl_path)

#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

AMRE26 <- AMRE[[26]]
AMRE27 <- AMRE[[27]]
AMRE28 <- AMRE[[28]]
AMRE29 <- AMRE[[29]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

AMRE_Breeding <- AMRE26+AMRE27+AMRE28+AMRE29
plot(AMRE_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

AMRE_Breeding_WGS <- project(AMRE_Breeding, Elevation@crs)
plot(AMRE_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- AMRE_Breeding_WGS
ext(template)<- c(-200, -25, 0, 90)
CW2 <- crop(AMRE_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "AMRE_breeding.tif")
AMRE_Breeding <- raster("AMRE_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

AMRE_Breeding
hist(AMRE_Breeding, ylim=c(0,10000))
quantile(AMRE_Breeding, c(.95, .975, .99)) 

## 99 quantile es 4.924448    y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

AMRE_Breeding1 <- AMRE_Breeding/4.924448   
AMRE_Breeding2 <- calc(AMRE_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
AMRE_Breeding3 <- calc(AMRE_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(AMRE_Breeding3, "AMRE_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(AMRE_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Yellow Warbler ########################################################        

dl_path <- ebirdst_download(species = "Yellow Warbler") # Para bajar los shapes de cada especie se cambia el nombre aqui.

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

YEWA <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

YEWA26 <- YEWA[[26]]
YEWA27 <- YEWA[[27]]
YEWA28 <- YEWA[[28]]
YEWA29 <- YEWA[[29]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

YEWA_Breeding <- YEWA26+YEWA27+YEWA28+YEWA29
plot(YEWA_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

YEWA_Breeding_WGS <- project(YEWA_Breeding, Elevation@crs)
plot(YEWA_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- YEWA_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(YEWA_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "YEWA_breeding.tif")
YEWA_Breeding <- raster("YEWA_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

YEWA_Breeding
hist(YEWA_Breeding, ylim=c(0,10000))
quantile(YEWA_Breeding, c(.95, .975, .99)) 

## 99 quantile es 6.422353     y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

YEWA_Breeding1 <- YEWA_Breeding/6.422353    
YEWA_Breeding2 <- calc(YEWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
YEWA_Breeding3 <- calc(YEWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(YEWA_Breeding3, "YEWA_Breeding3.tif", format = "GTiff", overwrite=TRUE)

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(YEWA_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)




#####  Bay-breasted Warbler ########################################################        

dl_path <- ebirdst_download(species = "Setophaga castanea")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

BBWA <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

BBWA25 <- BBWA[[25]]
BBWA26 <- BBWA[[26]]
BBWA27 <- BBWA[[27]]
BBWA28 <- BBWA[[28]]
BBWA29 <- BBWA[[29]]
BBWA30 <- BBWA[[30]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

BBWA_Breeding <- BBWA25+BBWA26+BBWA27+BBWA28+BBWA29+BBWA30
plot(BBWA_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

BBWA_Breeding_WGS <- project(BBWA_Breeding, Elevation@crs)
plot(BBWA_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- BBWA_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(BBWA_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "BBWA_breeding.tif")
BBWA_Breeding <- raster("BBWA_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

BBWA_Breeding
hist(BBWA_Breeding, ylim=c(0,10000))
quantile(BBWA_Breeding, c(.95, .975, .99)) 

## 99 quantile es 2.5549213      y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

BBWA_Breeding1 <- BBWA_Breeding/2.5549213     
BBWA_Breeding2 <- calc(BBWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
BBWA_Breeding3 <- calc(BBWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(BBWA_Breeding3, "BBWA_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(BBWA_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Northern Waterthrush ########################################################        

dl_path <- ebirdst_download(species = "Northern Waterthrush")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

NOWA <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

NOWA25 <- NOWA[[25]]
NOWA26 <- NOWA[[26]]
NOWA27 <- NOWA[[27]]
NOWA28 <- NOWA[[28]]
NOWA29 <- NOWA[[29]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

NOWA_Breeding <- NOWA25+NOWA26+NOWA27+NOWA28+NOWA29
plot(NOWA_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

NOWA_Breeding_WGS <- project(NOWA_Breeding, Elevation@crs)
plot(NOWA_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- NOWA_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(NOWA_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "NOWA_breeding.tif")
NOWA_Breeding <- raster("NOWA_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

NOWA_Breeding
hist(NOWA_Breeding, ylim=c(0,10000))
quantile(NOWA_Breeding, c(.95, .975, .99)) 

## 99 quantile es 3.549133 y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

NOWA_Breeding1 <- NOWA_Breeding/3.549133      
NOWA_Breeding2 <- calc(NOWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
NOWA_Breeding3 <- calc(NOWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(NOWA_Breeding3, "NOWA_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(NOWA_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Black and White Warbler ########################################################        

dl_path <- ebirdst_download(species = "Mniotilta varia")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

BWWA <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

BWWA25 <- BWWA[[25]]
BWWA26 <- BWWA[[26]]
BWWA27 <- BWWA[[27]]
BWWA28 <- BWWA[[28]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

BWWA_Breeding <- BWWA25+BWWA26+BWWA27+BWWA28
plot(BWWA_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

BWWA_Breeding_WGS <- project(BWWA_Breeding, Elevation@crs)
plot(BWWA_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- BWWA_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(BWWA_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "BWWA_breeding.tif")
BWWA_Breeding <- raster("BWWA_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

BWWA_Breeding
hist(BWWA_Breeding, ylim=c(0,10000))
quantile(BWWA_Breeding, c(.95, .975, .99)) 

## 99 quantile es 3.608405 y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

BWWA_Breeding1 <- BWWA_Breeding/3.608405       
BWWA_Breeding2 <- calc(BWWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
BWWA_Breeding3 <- calc(BWWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(BWWA_Breeding3, "BWWA_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(BWWA_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Tennessee Warbler ########################################################        

dl_path <- ebirdst_download(species = "Leiothlypis peregrina")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

TEWA <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

TEWA26 <- TEWA[[26]]
TEWA27 <- TEWA[[27]]
TEWA28 <- TEWA[[28]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

TEWA_Breeding <- TEWA26+TEWA27+TEWA28
plot(BWWA_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

TEWA_Breeding_WGS <- project(TEWA_Breeding, Elevation@crs)
plot(TEWA_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- TEWA_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(TEWA_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "TEWA_breeding.tif")
TEWA_Breeding <- raster("TEWA_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

TEWA_Breeding
hist(TEWA_Breeding, ylim=c(0,10000))
quantile(TEWA_Breeding, c(.95, .975, .99)) 

## 99 quantile es 4.806944 y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

TEWA_Breeding1 <- TEWA_Breeding/4.806944        
TEWA_Breeding2 <- calc(TEWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
TEWA_Breeding3 <- calc(TEWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(TEWA_Breeding3, "TEWA_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(TEWA_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Mourning Warbler ########################################################        

dl_path <- ebirdst_download(species = "Geothlypis philadelphia")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

MOWA <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

MOWA25 <- MOWA[[25]]
MOWA26 <- MOWA[[26]]
MOWA27 <- MOWA[[27]]
MOWA28 <- MOWA[[28]]
MOWA29 <- MOWA[[29]]
MOWA30 <- MOWA[[30]]
MOWA31 <- MOWA[[31]]
MOWA32 <- MOWA[[32]]
MOWA33 <- MOWA[[33]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

MOWA_Breeding <- MOWA25+MOWA26+MOWA27+MOWA28+MOWA29+MOWA30+MOWA31+MOWA32+MOWA33
plot(MOWA_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

MOWA_Breeding_WGS <- project(MOWA_Breeding, Elevation@crs)
plot(MOWA_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- MOWA_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(MOWA_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "MOWA_breeding.tif", overwrite =T)
MOWA_Breeding <- raster("MOWA_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

MOWA_Breeding
hist(MOWA_Breeding, ylim=c(0,10000))
quantile(MOWA_Breeding, c(.95, .975, .99)) 

## 99 quantile es 3.429353  y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

MOWA_Breeding1 <- MOWA_Breeding/3.429353           
MOWA_Breeding2 <- calc(MOWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
MOWA_Breeding3 <- calc(MOWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(MOWA_Breeding3, "MOWA_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(MOWA_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Swainson's Thrush ########################################################        

dl_path <- ebirdst_download(species = "Catharus ustulatus")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

SWTH <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

SWTH26 <- SWTH[[26]]
SWTH27 <- SWTH[[27]]
SWTH28 <- SWTH[[28]]
SWTH29 <- SWTH[[29]]
SWTH30 <- SWTH[[30]]
SWTH31 <- SWTH[[31]]
SWTH32 <- SWTH[[32]]
SWTH33 <- SWTH[[33]]
SWTH34 <- SWTH[[34]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

SWTH_Breeding <- SWTH26+SWTH27+SWTH28+SWTH29+SWTH30+SWTH31+SWTH32+SWTH33+SWTH34
plot(SWTH_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

SWTH_Breeding_WGS <- project(SWTH_Breeding, Elevation@crs)
plot(SWTH_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- SWTH_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(SWTH_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "SWTH_breeding.tif")
SWTH_Breeding <- raster("SWTH_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

SWTH_Breeding
hist(SWTH_Breeding, ylim=c(0,10000))
quantile(SWTH_Breeding, c(.95, .975, .99)) 

## 99 quantile es 16.445336  y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

SWTH_Breeding1 <- SWTH_Breeding/16.445336           
SWTH_Breeding2 <- calc(SWTH_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
SWTH_Breeding3 <- calc(SWTH_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(SWTH_Breeding3, "SWTH_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(SWTH_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Great Crested Flycatcher ########################################################        

dl_path <- ebirdst_download(species = "Myiarchus crinitus")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

GCFL <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

GCFL24 <- GCFL[[24]]
GCFL25 <- GCFL[[25]]
GCFL26 <- GCFL[[26]]
GCFL27 <- GCFL[[27]]
GCFL28 <- GCFL[[28]]
GCFL29 <- GCFL[[29]]
GCFL30 <- GCFL[[30]]
GCFL31 <- GCFL[[31]]
GCFL32 <- GCFL[[32]]
GCFL33 <- GCFL[[33]]


## Combinar las semanas para generar una sola capa para el periodo de invierno

GCFL_Breeding <- GCFL24+GCFL25+GCFL26+GCFL27+GCFL28+GCFL29+GCFL30+GCFL31+GCFL32+GCFL33
plot(GCFL_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

GCFL_Breeding_WGS <- project(GCFL_Breeding, Elevation@crs)
plot(GCFL_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- GCFL_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(GCFL_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "GCFL_breeding.tif")
GCFL_Breeding <- raster("GCFL_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

GCFL_Breeding
hist(GCFL_Breeding, ylim=c(0,10000))
quantile(GCFL_Breeding, c(.95, .975, .99)) 

## 99 quantile es 6.159040  y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

GCFL_Breeding1 <- GCFL_Breeding/6.159040          
GCFL_Breeding2 <- calc(GCFL_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
GCFL_Breeding3 <- calc(GCFL_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(GCFL_Breeding3, "GCFL_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(GCFL_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)





#####  Acadian Flycatcher ########################################################        

dl_path <- ebirdst_download(species = "Empidonax virescens")

## Llamar al dl_patch y grabar el resultado para ir directo a las capas la proxima vez

dl_path

#Este es el nombre de la ruta para acceder a las capas. Cambiarlo a la ruta de tu compu.

#### Cargar los rasters que tienen la superficie de abundancia para cada semana del año

ACFL <- load_raster(product = "abundance", path = dl_path)

## Definir las semanas que queremos para el periodo reproductivo o de INVIERNO (estacionario). Este ejemplo solo toma las semanas de invierno, pero se puede cambiar de acuerdo al periodo que se quiera comparar.
#Periodo reproductivo (TOMA UN TIEMPO EN COMPILAR)

ACFL22 <- ACFL[[22]]
ACFL23 <- ACFL[[23]]
ACFL24 <- ACFL[[24]]
ACFL25 <- ACFL[[25]]
ACFL26 <- ACFL[[26]]
ACFL27 <- ACFL[[27]]
ACFL28 <- ACFL[[28]]
ACFL29 <- ACFL[[29]]
ACFL30 <- ACFL[[30]]
ACFL31 <- ACFL[[31]]

## Combinar las semanas para generar una sola capa para el periodo de invierno

ACFL_Breeding <- ACFL22+ACFL23+ACFL24+ACFL25+ACFL26+ACFL27+ACFL28+ACFL29+ACFL30+ACFL31
plot(ACFL_Breeding)

## Reprojectar usando la capa de elevacion para convertirlo a WGS84

ACFL_Breeding_WGS <- project(ACFL_Breeding, Elevation@crs)
plot(ACFL_Breeding_WGS)

## Cortar a la region de interes, se puede ajustar para captar solo uno o dos paises

template <- ACFL_Breeding_WGS
ext(template)<- c(-200, -20, 0, 90)
CW2 <- crop(ACFL_Breeding_WGS,template)
plot(CW2)

## Guardar el raster para no tener que hacer los pasos arriba cada vez

writeRaster(CW2, "ACFL_breeding.tif")
ACFL_Breeding <- raster("ACFL_breeding.tif")

## Exploracion de los datos para estandarizar (este paso podria cambiar)

ACFL_Breeding
hist(ACFL_Breeding, ylim=c(0,10000))
quantile(ACFL_Breeding, c(.95, .975, .99)) 

## 99 quantile es 7.421115  y vamos a usar esto para estandarizar la capa en valores entre 0 y 1 

## Volver todos los valores de la capa entre 0 y 1. Los 10% de los valores m?s bajos > NA.

ACFL_Breeding1 <- ACFL_Breeding/7.421115           
ACFL_Breeding2 <- calc(ACFL_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
ACFL_Breeding3 <- calc(ACFL_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Guardar para usos futuros, capa transformada. Estas son las capas que luego utilizaremos para sobreponer las especies y estimar si son o no similares.

writeRaster(ACFL_Breeding3, "ACFL_Breeding3.tif", format = "GTiff")

## Crear Mapa

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(ACFL_Breeding3, col = Ceruleanblue4, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(50, xy=c(-150, -50), below = "Kms", type='bar', divs=2)

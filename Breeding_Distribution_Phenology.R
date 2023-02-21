## Comparación entre RASTERS de distribución ##################

library("terra")

setwd("C:/Users/Usuario/Documents/SELVA/Migration_Phenology/Breeding_TIF_Graphs")

## after #####

# BWWA vs AMRE

BWWA_rast <- terra::rast("BWWA_Breeding3.tif")
AMRE_rast <- terra::rast("AMRE_Breeding3.tif")

# BWWA vs BKWA

BBWA_rast <- terra::rast("BWWA_Breeding3.tif")
BKWA_rast <- terra::rast("BKWA_Breeding3.tif")

# BKWA vs AMRE

BKWA_rast <- terra::rast("BKWA_Breeding3.tif")
AMRE_rast <- terra::rast("AMRE_Breeding3.tif")

# BWWA vs YEWA

BBWA_rast <- terra::rast("BWWA_Breeding3.tif")
YEWA_rast <- terra::rast("YEWA_Breeding3.tif")

rm <- mask(BBWA_rast, YEWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# BWWA vs YEWA

BKWA_rast <- terra::rast("BKWA_Breeding3.tif")
YEWA_rast <- terra::rast("YEWA_Breeding3.tif")

rm <- mask(BKWA_rast, YEWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# AMRE vs YEWA

AMRE_rast <- terra::rast("AMRE_Breeding3.tif")
YEWA_rast <- terra::rast("YEWA_Breeding3.tif")

rm <- mask(AMRE_rast, YEWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)


## before fall ######

# GCFL vs MOWA

GCFL_rast <- terra::rast("GCFL_Breeding3.tif") 
MOWA_rast <- terra::rast("MOWA_Breeding3.tif") 


rm <- mask(GCFL_rast, MOWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)


## before spring ####

# ACFL vs NOWA

ACFL_rast <- terra::rast("ACFL_Breeding3.tif")
NOWA_rast <- terra::rast("NOWA_Breeding3.tif")

rm <- mask(ACFL_rast, NOWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)



## no difference ####

# SWTH vs TEWA

SWTH_rast <- terra::rast("SWTH_Breeding3.tif")
TEWA_rast <- terra::rast("TEWA_Breeding3.tif")

rm <- mask(SWTH_rast, TEWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# SWTH vs SMTN

SWTH_rast <- terra::rast("SWTH_Breeding3.tif")
SMTN_rast <- terra::rast("SMTN_Breeding3.tif")

rm <- mask(SWTH_rast, SMTN_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# SWTH vs BBWA

SWTH_rast <- terra::rast("SWTH_Breeding3.tif")
BBWA_rast <- terra::rast("BBWA_Breeding3.tif")

rm <- mask(SWTH_rast, BBWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# SWTH vs BBWA

SWTH_rast <- terra::rast("SWTH_Breeding3.tif")
BBWA_rast <- terra::rast("BBWA_Breeding3.tif")

rm <- mask(SWTH_rast, BBWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# TEWA vs SMTN

TEWA_rast <- terra::rast("TEWA_Breeding3.tif")
SMTN_rast <- terra::rast("SMTN_Breeding3.tif")

rm <- mask(TEWA_rast, SMTN_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# TEWA vs BBWA

TEWA_rast <- terra::rast("TEWA_Breeding3.tif")
BBWA_rast <- terra::rast("BBWA_Breeding3.tif")

rm <- mask(TEWA_rast, BBWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

# SMTN vs BBWA

SMTN_rast <- terra::rast("SMTN_Breeding3.tif")
BBWA_rast <- terra::rast("BBWA_Breeding3.tif")

rm <- mask(SMTN_rast, BBWA_rast)
plot(rm)
area_shared <- expanse(rm, transform=F)

format(area_shared, scientific = FALSE)

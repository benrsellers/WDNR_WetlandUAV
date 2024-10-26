# Install necessary packages
# install.packages("randomForest")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("caret")
# install.packages("readxl")
# install.packages("sp")
# install.packages("raster")
# install.packages("terra")

# Load libraries
library(terra)

###WHAT WE WANT THIS SCRIPT TO DO###
#1. Calculate RGB Vegetation Indices outlined here https://plantmethods.biomedcentral.com/articles/10.1186/s13007-019-0418-8/tables/1
#2. Calculate structural metrics like TPI, Roughness, TRI
#3. Stack all the rasters together to be used in model building



###########################################
###       Step 1: Calculate RGB VIs     ###
###########################################

#Reading in Drone imagery 
getwd()
#drone_dtm <- rast("data/tif/drone_dtm_focal.tif") #This layer was made in Moffat_Random_Forest_Sellers to remove NAs using focal smoothing
drone_dsm <- rast("../../PB_data/overview_rgb/PheasantBranch_DEM.tif")
drone_rgb <- rast("../../PB_data/overview_rgb/PheasantBranch_RGB3cm.tif") #This layer was mad3 in Moffat_Random_Forest_Sellers to resample RGB to the resolution of DSM and DTM (10cm)

#resample RGB to match resolution of DSM
#drone_rgb <- resample(drone_rgb, drone_dsm, threads = T)
#writeRaster(drone_rgb, "../../PB_data/overview_rgb/PheasantBranch_RGB3cm.tif")

# Renaming Bands
names(drone_rgb) <- c("red","green", "blue", "alpha")
#names(drone_dtm) <- "dtm"
names(drone_dsm) <- "dsm"

# Defining RGB Vegetation Indices
VDVI = (2*drone_rgb$green-drone_rgb$red - drone_rgb$blue)/(2*drone_rgb$green+drone_rgb$red+drone_rgb$blue)
#writeRaster(VDVI, "data/tif/drone_VDVI.tif", overwrite = TRUE)
NGRDI = (drone_rgb$green-drone_rgb$red)/(drone_rgb$green+drone_rgb$red)
#writeRaster(NGRDI, "data/tif/drone_NGRDI.tif", overwrite = TRUE)
VARI = (drone_rgb$green-drone_rgb$red)/(drone_rgb$green+drone_rgb$red-drone_rgb$blue)
#writeRaster(VARI, "data/tif/drone_VARI.tif", overwrite = TRUE)
GRRI = drone_rgb$green/drone_rgb$red
#writeRaster(GRRI, "data/tif/drone_GRRI.tif", overwrite = TRUE)

#CHM Calculations
CHM <- drone_dsm - drone_dtm
#writeRaster(CHM, "data/tif/drone_CHM.tif", overwrite = TRUE)

###########################################
### Step 2: Calculate Structural Indices###
###########################################

# Using terra's built in terrain characteristics function to get TPI, TRI, and Roughness from drone DSM
terrain_metrics <- terrain(drone_dsm, v=c("TRI", "TPI", "roughness"), neighbors = 8)

###########################################
###     Step 3: Stack  Rasters          ###
###########################################

# Stack all of the layers together
drone_stack <- c(drone_rgb, drone_dsm, terrain_metrics$TRI, terrain_metrics$TPI,
                 terrain_metrics$roughness, VDVI, NGRDI, VARI, GRRI)

#rename the drone layer names
names(drone_stack) <- c("r", "g", "b", "alpha", "dsm", "tri", "tpi",
                        "roughness", "vdvi", "ngrdi", "vari", "grri")

# Write Drone stack raster
writeRaster(drone_stack, "../../PB_data/overview_rgb/dronestack_3cm.tif", overwrite = TRUE)

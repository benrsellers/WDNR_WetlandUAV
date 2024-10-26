# Install necessary packages
# install.packages("terra")
# install.packages("sf")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("dplyr")

# Load libraries
library(terra)
library(sf)
library(dplyr)
library(randomForest)
library(ggplot2)
library(caret)

###WHAT WE WANT THIS SCRIPT TO DO###
#1. Read in all shapefile data, assign classes, and merge into one shapefile.
#2. Split training polygons into calibration/validation sets and extract pixels from drone data for both sets
#3. Train an RF Model
#4. Run RF Model on the entire Camblin Ranch drone data stack
#5. Calculate error metrics from classification maps

### Input file paths

#path to Develop Data folder (seperate from github repo due to size of tif files & privacy)
develop_data <- normalizePath(file.path(wd, "..", "..", "DEVELOP_data"), winslash = "/")

#path to training polygon folders
bareground_shapefiles <- file.path(develop_data, "shp", "TrainingPolygons", "1_BareGroundTrainingData", winslash = "/")
grass_shapefiles <- file.path(develop_data, "shp", "TrainingPolygons", "2_GrassTrainingData", winslash = "/")
shrub_shapefiles <- file.path(develop_data, "shp", "TrainingPolygons", "3_ShrubTrainingData", winslash = "/")

#path to drone map files
drone_stack <- rast(file.path(develop_data, "Drone", "camblin_stack.tif"))

### Output File Paths

RF_outputs <- file.path("data/RF_outputs/Camblin_full")

# Variable for todays dat to label RF outputs
current_date <- format(Sys.Date(), "%Y-%m-%d")


################################################
### Step 1. Merging Training Data Shapefiles ###         
################################################

# Source the Drone_funcitons.R script which contains the read_and_merge_polygons() function
getwd()
source("Drone_functions.R")

# Create a list of each training polygon type (Shrubs, Grass, Bare Ground) 
bareground_files <- list.files("C:/Ben_wd/DEVELOP/repos/DEVELOP_data/shp/TrainingPolygons/1_BareGroundTrainingData", pattern = "\\.shp$", full.names = TRUE)
grass_files <- list.files("C:/Ben_wd/DEVELOP/repos/DEVELOP_data/shp/TrainingPolygons/2_GrassTrainingData", pattern = "\\.shp$", full.names = TRUE)
shrub_files <- list.files("C:/Ben_wd/DEVELOP/repos/DEVELOP_data/shp/TrainingPolygons/3_ShrubTrainingData", pattern = "\\.shp$", full.names = TRUE)
grass_files

#use the read_and_merge_polygons function to create a single training polygon layer for each class using the list of shapefiles provided
bareground_poly <- read_and_merge_training_polygons(bareground_files)
grass_poly <- read_and_merge_training_polygons(grass_files)
shrub_poly <- read_and_merge_training_polygons(shrub_files)

# Assign classes to landcover types
bareground_poly$class <- 1
grass_poly$class <- 2
shrub_poly$class <- 3

#select only the geometry and class columns from shapefiles
bareground_poly_filtered <- bareground_poly[, c("geometry", "class")]
grass_poly_filtered <- grass_poly[, c("geometry", "class")]
shrub_poly_filtered <- shrub_poly[, c("geometry", "class")]

# Combine shapefiles into one sf collection
training_poly <- rbind(bareground_poly_filtered, grass_poly_filtered, shrub_poly_filtered)
st_write(training_poly, "data/TrainingData/shp/TrainingPoly.shp", append = FALSE)
plot(training_poly)

#make a column for row number so we can join to training data later
training_poly <- training_poly %>% mutate(index = rownames(.))
training_poly

# Remove invalid geometries (a couple of the polygons had degenerated edges, duplicate vertices, and were invalid)
training_poly <- st_make_valid(training_poly)
training_poly

#########################################
### Step 2. Training/Validation Split ###
#########################################


# Select 70% of polygons of class to use for calibration (training data)
calibration_set <- training_poly %>%
  group_by(class) %>%
  slice_sample(prop = 0.70, replace = FALSE) %>%
  ungroup() %>% 
  mutate(ID = seq_len(nrow(.)))
calibration_set
plot(calibration_set$class)

# Select the remaining 30% for validation (test data)
validation_set <- training_poly %>%
  filter(!st_equals(., calibration_set, sparse = FALSE) %>% rowSums()) %>% 
  mutate(ID = seq_len(nrow(.)))
plot(validation_set$class)

#extract pixels from calibration polygon subset
extracted_cal_values <- terra::extract(drone_stack, calibration_set, xy= TRUE)#, bind = TRUE)#, sp = TRUE)# %>% na.omit(extracted_cal_values)
extracted_cal_values <- extracted_cal_values  %>% left_join(calibration_set, by = "ID", select(ID))
table(extracted_cal_values$class)

#extract pixels from validation polygon subset
extracted_val_values <- terra::extract(drone_stack, validation_set, xy= TRUE)# %>% na.omit(extracted_val_values)
extracted_val_values <- extracted_val_values  %>% left_join(validation_set, by = "ID", select(ID))
table(extracted_val_values$class)

####################################
### Step 3. Creation of RF Model ###
####################################

#Making the class column a factor
extracted_cal_values$class <- factor(extracted_cal_values$class)
extracted_val_values$class <- factor(extracted_val_values$class)
#checking structure 
str(extracted_val_values)
extracted_cal_values

# Define predictor variables for modeling
x_data <- extracted_cal_values[1:14]
x_data <- subset(x_data, select= -c(ID,alpha))
head(x_data)

#define dependent variable for modeling
y_data <- extracted_cal_values$class
y_data

#check out corellation
#might want to remove one of each pair that has a corellation above .7
cor(x_data)

#set seed to be reproducible 
set.seed(0)

#train model
rf_Camblin <- randomForest(x=x_data, y=y_data, mtry=7, ntree=50, importance=TRUE, keep.forest=TRUE)

#view the model
rf_Camblin
plot(rf_Camblin)

#see and plot the importance of each variable 
importance(rf_Camblin)
varImpPlot(rf_Camblin, main = 'Band Importance')

#################################################################
### Step 4. Classifying Camblin Ranch Drone Map with RF Model ###
#################################################################

# Setting up variables so that we can run models in a loop and keep track of their run number
tif_count <- length(list.files(RF_outputs, pattern = "\\.tif$", full.names = TRUE))+1
tif_count
### Running RF on the entire camblin ranch drone map
output_filename <- file.path(RF_outputs, paste0("CamblinFull", tif_count,'_', current_date, ".tif"))
output_filename
map <- predict(drone_stack, type='response', rf_Camblin, filename=output_filename, format="GTiff", overwrite=TRUE)
plot(map)
#if getting error that NAs exitst, run code below then re run line 160
#drone_stack[is.na(drone_stack)] <- 0
#writeRaster(drone_stack, "data/tif/camblin_stack_noNA.tif", overwrite = TRUE)

#######################################################
### Step 5. Accuracy assement using validation data ###
#######################################################

# Defining Calibration and Validation shapefiles
validation <- vect(extracted_val_values[, c("class", "x", "y")],geom = c("x", "y"))
calibration <- vect(extracted_cal_values[, c("class", "x", "y")], geom = c("x", "y"))
#add validation points to map
plot(validation, add = T)

# Extracting pixel values from the classified map using the training sample validation points
validation$reference <- as.factor(validation$class)
validation$reference
prediction <- extract(map, validation)
prediction

# adding the predicted pixel values to the df with the known pixel values (from training polygons)
validation$prediction <- as.factor(prediction$class)
plot(validation$reference)

# Creating a confusion matrix using the caret package
cm <- confusionMatrix(as.factor(validation$prediction), as.factor(validation$reference))
print(cm)
hm <- as.data.frame(as.table(cm))
hm

#plot confusion matric in ggplot
plot <- ggplot(hm, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = Freq), color = "black", size = 10) +
  # following lines only increase text size (optional)
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))

# reorder quadrants
plot +
  scale_x_discrete(limits = c("1", "2", "3"),
                   labels = c("Bare Ground", "Grass/Forb", "Shrub")) +
  scale_y_discrete(limits = c("1", "2", "3"),
                   labels = c("Bare Ground", "Grass/Forb", "Shrub")) +
  # following lines only increase text size (optional)
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))
ggsave("ConfusionMatrix.png")


ggplot(data =  error_matrix_df, mapping = aes(x = prediction, y = reference)) +
  geom_tile(aes(fill = n), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", n)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")
###Calculating error metrics

# number of total cases/samples
n <- sum(error_matrix)
n

# number of correctly classified cases per class
diag <- diag(error_matrix)

# Calculate Overall Accuracy
(OA <- sum(diag) / n)

#Kappa Index

# observed (true) cases per class
rowsums <- apply(error_matrix, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(error_matrix, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
# Calculate kappa score
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa

# Calculate Producer accuracy
(PA <- diag / colsums)
# User accuracy
(UA <- diag / rowsums)
(outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA))

# Calculate Precision (True Positives divided by sum of true and false positives )
precision = diag / colsums 

# Calculate recall
recall = diag / rowsums 

#Calculate F1 Score
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

# Macro (average) Precision 
(macroPrecision = mean(precision))

# Calculate
(macroRecall = mean(recall))


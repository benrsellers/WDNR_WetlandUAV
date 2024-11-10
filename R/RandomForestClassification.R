# Install necessary packages
# install.packages("terra")
# install.packages("sf")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("whitebox")
## Need this line to run whitebox, downloades files locally
# whitebox::install_whitebox()


# Load libraries
library(terra)
library(sf)
library(dplyr)
library(randomForest)
library(ggplot2)
library(caret)
library(whitebox)

###WHAT WE WANT THIS SCRIPT TO DO###
#1. Read in all shapefile data, assign classes, and merge into one shapefile.
#2. Split training polygons into calibration/validation sets and extract pixels from drone data for both sets
#3. Train an RF Model
#4. Run RF Model on the entire Camblin Ranch drone data stack
#5. Calculate error metrics from classification maps

### Input file paths

#path to UAV Data folder (seperate from github repo due to size of tif files & privacy)
wd <- getwd()
wd
UAV_data <- file.path(wd, "..", "..", "PB_data/overview_rgb")

#path to training polygon folders
LBVeg_poly <- st_read(file.path(UAV_data, "shp", "PB_training", "LightBrownVeg", winslash = "/"))
LGVEG_poly <- st_read(file.path(UAV_data, "shp", "PB_training", "LightGreenVeg", winslash = "/"))
Shrubs_poly <- st_read(file.path(UAV_data, "shp", "PB_training", "Shrubs", winslash = "/"))
EmVEG_poly <- st_read(file.path(UAV_data, "shp", "PB_training", "EmergentVeg", winslash = "/"))
Water_poly <- st_read(file.path(UAV_data, "shp", "PB_training", "Water", winslash = "/"))
phrag_poly <- st_read(file.path(UAV_data, "shp", "PB_training", "Phrag", winslash = "/"))
rice_poly <-  st_read(file.path(UAV_data, "shp", "PB_training", "Rice", winslash = "/"))



#shrub_shapefiles <- file.path(develop_data, "shp", "TrainingPolygons", "3_ShrubTrainingData", winslash = "/")

#path to drone map files
drone_stack <- rast(file.path(UAV_data, "tif", "dronestack_3cm.tif"))

### Output File Paths

RF_outputs <- file.path(UAV_data, "tif", "RF_outputs")

# Variable for todays dat to label RF outputs
current_date <- format(Sys.Date(), "%Y-%m-%d")


################################################
### Step 1. Merging Training Data Shapefiles ###         
################################################

# Assign classes to landcover types
LBVeg_poly$class <- 1
LGVEG_poly$class <- 2
Shrubs_poly$class <- 3
EmVEG_poly$class <- 4
Water_poly$class <- 5
phrag_poly$class <- 6
rice_poly$class <- 7
#select only the geometry and class columns from shapefiles (could make this a for loop)
LBVeg_poly <- LBVeg_poly[, c("geometry", "class")]
LGVEG_poly <- LGVEG_poly[, c("geometry", "class")]
Shrubs_poly <- Shrubs_poly[, c("geometry", "class")]
EmVEG_poly <- EmVEG_poly[, c("geometry", "class")]
Water_poly <- Water_poly[, c("geometry", "class")]
phrag_poly <- phrag_poly[, c("geometry", "class")]
rice_poly <- rice_poly[, c("geometry", "class")]


# Combine shapefiles into one sf collection
training_poly <- rbind(LBVeg_poly, LGVEG_poly, Shrubs_poly, EmVEG_poly, Water_poly, rice_poly, phrag_poly)
#st_write(training_poly, "data/TrainingData/shp/TrainingPoly.shp", append = FALSE)
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
extracted_cal_values
extracted_val_values
#Making the class column a factor
extracted_cal_values$class <- factor(extracted_cal_values$class)
extracted_val_values$class <- factor(extracted_val_values$class)
#checking structure 
str(extracted_val_values)
extracted_cal_values

# Define predictor variables for modeling
x_data <- extracted_cal_values[1:13]
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
output_filename <- file.path(RF_outputs, paste0("PheasantBranch", tif_count,'_', current_date, ".tif"))
output_filename
#if getting error that NAs exitst, run code below
drone_stack[is.na(drone_stack)] <- 0
map <- predict(drone_stack, type='response', rf_Camblin, filename=output_filename, format="GTiff", overwrite=TRUE)
plot(map)

# Define a majority function that handles NA values
whitebox::wbt_majority_filter(map, file.path(UAV_data, "tif", "RF_outputs", "majority_filter.tif"), filterx = 11, filtery = 11,
                verbose_mode = FALSE)

# Apply the majority filter with focal()
filtered_raster <- focal(map, w = matrix(1, nrow = window_size, ncol = window_size), fun = majority_function)

#writeRaster(drone_stack, "data/tif/camblin_stack_noNA.tif", overwrite = TRUE)

#######################################################
### Step 5. Accuracy assement using validation data ###
#######################################################
validation
calibration
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
  scale_x_discrete(limits = as.character(1:7),
                   labels = c("light green", "light brown", "shrubs", "emergent", "Water", "Phrag", "rice")) +
  scale_y_discrete(limits = as.character(1:7),
                   labels = c("light green", "light brown", "shrubs", "emergent", "Water", "Phrag", "rice")) +
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


# install.packages("sf")
# Load libraries
library(sf)


### Function 1: Reading in all training polygon shapefiles and joining them into one sf polygon

read_and_merge_training_polygons <- function(filepaths) {
  #make an empty sf object to store joined shapefiles
  shapefiles <- NULL

  #loop over shapefiles in the list of filepaths you supply
  for (i in filepaths) {
    shp = st_read(i)
    if (is.null(shapefiles)) {
      shapefiles = shp
    } else {
      shapefiles <- rbind(shapefiles, shp)
    }
  } 
  return(shapefiles)
}


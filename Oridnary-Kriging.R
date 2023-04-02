# Install require packages and load libraries
install.packages("sp")
install.packages("gstat")
install.packages("automap")
install.packages("raster")
install.packages("patchwork")
install.packages("tidyverse")

library(sp)
library(gstat)
library(automap)
library(raster)
library(patchwork)
library(tidyverse)

## Import and visualize the data
# Importing raw data and storing in a variable, a link is used instead of a local file to make it reproducible 
temperature <- read_csv("https://raw.githubusercontent.com/km3smith/GG-618-Term-Project/main/bcMeanTemp.csv")
# Converting csv to Spatial Dataframe(SPDF) using specified long and lat columns
coordinates(temperature) = ~long+lat
coordinates(temperature)[1:24,]

# Plotting January temperature data with a colour gradient representing temperature variation
spplot(temperature, "Jan", colorkey = TRUE, main = "Jan Avg Temp")
# Plotting January temperature data with sizes representing temperature variation
bubble(temperature, "Jan", col="blue", main = "Jan Avg Temp")

## Create the grid
# Creating a grid for raster based analysis#
bbox(temperature) #shows extent of dataset
# Extracting min and maximum X and Y values from dataset
x <- seq(min(temperature$long), max(temperature$long), length.out = 50)
y <- seq(min(temperature$lat), max(temperature$lat), length.out = 50)
# Generating Grid
grid <- expand.grid(long = x, lat = y)
# Convert grid to a spatial data frame
coordinates(grid) <- c("long", "lat")

## Ordinary Kriging for January 
# Running Ordinary Kriging for January
January <- autoKrige(Jan~1, temperature, grid)
plot(January, sp.layout = list(pts = list("sp.points", temperature)))
# Cast the Spatial object to a data.frame
ggplot_data1<- as.data.frame(January$krige_output)
# Plotting mean temperature values for January
ggplot(ggplot_data1, aes(x = long, y = lat, fill = var1.pred)) +
  geom_raster() + coord_fixed() +
  scale_fill_gradient(low = 'blue', high = 'red') + ggtitle("January 2015 Mean Temperature")
# Cross-validate
kr.cv <- autoKrige.cv(Jan~1, temperature, model = c("Ste"), nfold = 5)
kr.cv

## Export the predictions as a raster
# Convert Kriging output to a data frame
Jan_krige_df <- as.data.frame(January$krige_output)
# Create a raster object from the data frame
raster_prediction <- rasterFromXYZ(Jan_krige_df, crs=projection(temperature))
# Write raster to file (saves file to R Project location, can be specified)
writeRaster(raster_prediction, filename = "Jan_krige_prediction.tif", format = "GTiff", overwrite = TRUE)

# Repeat for February
### Februray
February <- autoKrige(Feb~1, temperature, grid)
plot(February, sp.layout = list(pts = list("sp.points", temperature)))

ggplot_data2 <- as.data.frame(February$krige_output)
ggplot(ggplot_data2, aes(x = long, y = lat, fill = var1.pred)) +
  geom_raster() + coord_fixed() +
  scale_fill_gradient(low = 'blue', high = 'red') + ggtitle("February 2015 Mean Temperature")

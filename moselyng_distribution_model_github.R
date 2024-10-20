library(sf)
library(terra)
library(predicts)
library(geodata)
library(tidymodels)
library(tidyverse)
library(pROC)
library(tmap)
library(corrplot)

Sys.setlocale(locale='no_NB.utf8') 
options(scipen = 999)

# https:tmap# https://jcoliver.github.io/learn-r/011-species-distribution-models.html


# Performance -------------------------------------------------------------

# Use all available cores
library(parallel)
options(mc.cores = detectCores())

# Adjust garbage collection
gc()

# Set memfrac to a desired value if needed
terraOptions(memfrac = 0.9)  # Use 95% of available RAM

# Raster data -------------------------------------------------------------

# Path
continous_rasters <- rast("C:/Privat/data_analysis/distribution_modelling_trond/to_github/data/continous_rasters.tif")
cat_rasters <- rast("C:/Privat/data_analysis/distribution_modelling_trond/to_github/data/categorical_rasters.tif")

plot(continous_rasters)
plot(cat_rasters)

# Stack the aggregated categorical raster with continuous rasters
rasters <- c(continous_rasters, cat_rasters)

# Base map ----------------------------------------------------------------

#norge <- st_read("C:/Privat/spatial_trond/Norge_polygon/norge_oversikt.shp")

my_map <- st_read("C:/Privat/data_analysis/distribution_modelling_trond/to_github/data/norge.gpkg")
# Transform vector data to match the raster CRS

# Use tmap to plot the rasters
tm_shape(rasters$bioclim10) + 
  tm_raster(palette = "-RdYlBu", midpoint = NA, style = "cont") +
  tm_layout(title = "Downsampled Raster Visualization", legend.outside = TRUE)+
  tm_shape(my_map)+
  tm_borders()

tm_shape(rasters$ar50) + 
  tm_raster() +
  tm_layout(title = "Arealdekke", legend.outside = TRUE)+
  tm_shape(my_map)+
  tm_borders()

# Occurence data ----------------------------------------------------------

# file paths --------------------------------------------------------------

# Define the file paths
path_training <- "C:/Privat/data_analysis/distribution_modelling_trond/to_github/data/train.gpkg"
#path_test <- "C:/Privat/data_analysis/distribution_modelling_trond/to_github/data/test_data.gpkg"
path_ind_test <- "C:/Privat/data_analysis/distribution_modelling_trond/to_github/data/ind_test.gpkg"


# Read training_data from GeoPackage
training <- st_read(path_training, layer = "train")
ind_testing <- st_read(path_ind_test, layer = "ind_test")
#training_data_original <- training_data_original |> select(occ, folds, geom)
# 
# # Read test_data from GeoPackage
# test_data_original <- st_read(path_test, layer = "test_data")
# test_data_original <- st_transform(test_data_original, crs = 32633)
# test_data_original
# #test_data_original <- test_data_original |> select(occ, folds, geom)
# 
# # Read test_data (blocked cv) from GeoPackage
# ind_test_original <- st_read(path_ind_test)
# ind_test_original
# ind_test_original <- st_transform(ind_test_original, crs = 32633)
# # ind_test_original <- ind_test_original|> select(RV, geom) |> 
# #   rename(occ = RV)
# ind_test_original <- ind_test_original|> rename(occ = RV)
# ind_test_original
# 
# # training <- training_data_original |> st_as_sf()
# # testing <- test_data_original|> dplyr::select(-folds)
# # ind_testing <- ind_test_original
# 
# training <- rbind(training_data_original, test_data_original)
# training
# table(training$occ)
# unique(training$ar50)
# 
# # Changing value from 1 to 0 for observations that are obviously wrong, e.g.,  observations within the following land cover categories: 
# # 70	Bre: Is og snø som ikke smelter i løpet av sommeren
# # 81	Ferskvann: Elv og innsjø
# # 82	Hav
# # 99	Ikke kartlagt
# # Update 'occ' to 0 where 'occ' is 1 and 'ar50' is one of the specified categories
# # Update occ to 0 for category "81"
# training$occ[training$occ == 1 & training$ar50 == "81"] <- 0
# # Update occ to 0 for category "70"
# training$occ[training$occ == 1 & training$ar50 == "70"] <- 0
# # Update occ to 0 for category "82"
# training$occ[training$occ == 1 & training$ar50 == "82"] <- 0
# # Update occ to 0 for category "99"
# training$occ[training$occ == 1 & training$ar50 == "99"] <- 0
# 
# table(training$occ)


# 
# training <- training|> dplyr::select(-folds)
# ind_testing <- ind_test_original


train <- st_drop_geometry(training)
ind_test <- st_drop_geometry(ind_testing)

train$ar50 <- as.factor(train$ar50)
ind_test$ar50 <- as.factor(ind_test$ar50)

train$berggrunn <- as.factor(train$berggrunn)
ind_test$ar50 <- as.factor(ind_test$berggrunn)

pairs(train[,2:6], cex=0.1)

ncol(train)
cor_p <- cor(train[,2:8])
corrplot(cor_p, method = 'number') # colorful number

# Separate the predictor variables and the response variable
predictors_train <- train %>% select(-occ)
predictors_train
response_train <- as.numeric(train$occ)

# predictors_test <- test %>% select(-occ)
# response_test <-as.numeric(test$occ)

predictors_ind_test <- ind_test %>% select(-occ)
response_ind_test <-as.numeric(ind_test$occ)

presvals <- train |> filter(occ == 1) #|> 
#select(-occ)

colnames (presvals)
str(presvals)

# Plotting occurence data -------------------------------------------------
train_occ_pts <- training |> filter(occ == 1)
train_abs_pts <- training |> filter(occ == 0)

# # Create a custom legend
# legend_labels <- c("Bakgrunnspunkt", "Forekomst")
# legend_col <- c("grey20", "red")
# legend_values <- c(0, 1)

# # Plot with tmap
# tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
#   tm_shape(my_map) +
#   tm_borders() +
#   tm_graticules()+
#   tm_shape(train_abs_pts) +
#   tm_dots(col = "grey30", legend.show = FALSE) +
#   tm_shape(train_occ_pts) +
#   tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "Treningsdata", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)
# 
# # Use tmap to plot the downsampled raster
# tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "-RdYlBu", midpoint = NA, style = "cont") +
#   tm_layout(title = "Downsampled Raster Visualization", legend.outside = TRUE)+
#   tm_shape(my_map)+
#   tm_borders() +
#   tm_graticules()
#   # tm_shape(train_abs_pts) +
#   # tm_dots(col = "grey20", legend.show = FALSE) +
#   tm_shape(train_occ_pts) +
#   tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "Treningsdata", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)

# test_occ_pts <- testing |> filter(occ == 1)
# test_abs_pts <- testing |> filter(occ == 0)

ind_test_occ_pts <- ind_testing |> filter(occ == 1)
ind_test_abs_pts <- ind_testing |> filter(occ == 0)

pts1 <- train_occ_pts 
#pts2 <- test_occ_pts #|> select(-folds)
pts3 <- ind_test_occ_pts #|> select(-folds)
# all_occs_pts <- rbind(pts1, pts2, pts3)
# plot(all_occs_pts)

#all_train_pts <- rbind(pts1, pts2)


# # Use tmap to plot the downsampled raster
# map1 <- tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
#   tm_shape(my_map) +
#   tm_borders() +
#   tm_shape(train_abs_pts) +
#   tm_dots(col = "grey20", legend.show = FALSE) +
#   tm_shape(train_occ_pts) +
#   tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "a) Treningsdata", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE,
#             outer.margins = c(0, 0, 0, 0), inner.margins = c(0, 0, 0, 0))
# 
# map1
# 
# # map2 <- tm_shape(rasters$bioclim10) + 
# #   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
# #   tm_layout(title = "Testdata", legend.outside = FALSE)+
# #   tm_shape(my_map)+
# #   tm_borders()+
# #   tm_shape(test_abs_pts)+
# #   tm_dots(col = "grey20", legend.show = FALSE) +  
# #   tm_shape(test_occ_pts)+
# #   tm_dots(col = "red", size = 0.1)+
# #   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
# #   tm_layout(title = "b) Testdata", legend.position = c("left", "top"), 
# #             legend.frame = FALSE, frame = FALSE,
# #             outer.margins = c(0, 0, 0, 0), inner.margins = c(0, 0, 0, 0))
# 
# legend_labels2 <- c("Fravær", "Forekomst")
# 
# map3 <- tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
#   tm_layout(title = "Uavhengige testdata", legend.outside = FALSE)+
#   tm_shape(my_map)+
#   tm_borders()+
#   tm_shape(ind_test_abs_pts)+
#   tm_dots(col = "grey20", legend.show = FALSE) +  
#   tm_shape(ind_test_occ_pts)+
#   tm_dots(col = "red", size = 0.15)+
#   tm_add_legend(type = "symbol", labels = legend_labels2, col = legend_col) +
#   tm_layout(title = "c) Uavhengige\n evalueringsdata", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE,
#             outer.margins = c(0, 0, 0, 0), inner.margins = c(0, 0, 0, 0))
#   
# map3
# 
# tmap_arrange(map1, map3, ncol = 2)
# 
# # Adjust text sizes
# map1 <- map1 + tm_layout(title.size = 1.3, legend.text.size = 1)
# #map2 <- map2 + tm_layout(title.size = 1.3, legend.text.size = 1)
# map3 <- map3 + tm_layout(title.size = 1.3, legend.text.size = 1)
# 
# # Save the arranged maps again with adjusted text sizes
# combined_maps <- tmap_arrange(map1, map3, ncol = 2)
# combined_maps

# # Define the output path
# output_path <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/plots_tables/tmap_combined2.png"
# 
# tmap_save(combined_maps, filename = output_path, width = 30, height = 10, units = "cm", dpi = 600, bg = "white")



# Adjusted plot with hack -------------------------------------------------


# Define legend labels and colors
legend_labels <- c("Bakgrunnspunkt", "Forekomst")
legend_col <- c("grey40", "red")

legend_labels2 <- c("Fravær", "Forekomst")
legend_col2 <- c("grey20", "red")

# Create a rectangle polygon in latitude and longitude
rect_coords <- matrix(c(-10, 68, 
                        9, 68, 
                        9, 75, 
                        -10, 75, 
                        -10, 68), 
                      ncol = 2, byrow = TRUE)
rect_polygon <- st_polygon(list(rect_coords))

# Convert the polygon to an sf object with EPSG:4326
rect_sf <- st_sfc(rect_polygon, crs = 4326)

# Transform the polygon to EPSG:32633
rect_sf_transformed <- st_transform(rect_sf, crs = 32633)

# Plot the maps with the white polygon and graticules
map1 <- tm_shape(rasters$bioclim10) +
  tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  tm_shape(train_abs_pts) +
  tm_dots(col = "grey40", legend.show = FALSE) +
  tm_shape(train_occ_pts) +
  tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
  tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "a) Treningsdata", title.bg.color = "white", legend.bg.color = "white",
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map1
# 
# # map2 <- tm_shape(rasters$bioclim10) + 
# #   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
# #   tm_layout(title = "Testdata", legend.outside = FALSE) +
# #   tm_shape(my_map) +
# #   tm_borders() +
# #   tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
# #   tm_shape(test_abs_pts) +
# #   tm_dots(col = "grey20", legend.show = FALSE) +  
# #   tm_shape(test_occ_pts) +
# #   tm_dots(col = "red", size = 0.1) +
# #   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
# #   tm_shape(rect_sf_transformed) +
# #   tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
# #   tm_layout(title = "b) Testdata", title.bg.color = "white", legend.bg.color = "white", 
# #             legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE, 
# #             inner.margins = c(-0.05, -0.05, -0.05, -0.05))
# # 
# legend_col2 <- c("grey20", "red")
# legend_labels2 <- c("Fravær", "Forekomst")
# 
map3 <- tm_shape(rasters$bioclim10) +
  tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
  #tm_layout(title = "Uavhengige testdata", legend.outside = FALSE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  tm_shape(ind_test_abs_pts) +
  tm_dots(col = "grey20", legend.show = FALSE, size = 0.08) +
  tm_shape(ind_test_occ_pts) +
  tm_dots(col = "red", size = 0.15) +
  tm_add_legend(type = "symbol", labels = legend_labels2, col = legend_col2) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "b) Testdata", title.bg.color = "white", legend.bg.color = "white",
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map3
# 
# Adjust text sizes
map1 <- map1 + tm_layout(title.size = 1.3, legend.text.size = 1)
#map2 <- map2 + tm_layout(title.size = 1.3, legend.text.size = 1)
map3 <- map3 + tm_layout(title.size = 1.3, legend.text.size = 1)

# Arrange the maps together
combined_maps <- tmap_arrange(map1, map3, ncol = 2)
combined_maps

# Define the output path
output_path <- "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/trening_test.png"

# Save the combined maps
tmap_save(combined_maps, filename = output_path, width = 38, height = 16, units = "cm", dpi = 600, bg = "white")


# Figur with occurences and environmental variables -----------------------


# # Plot the maps with the white polygon and graticules
# map1 <- tm_shape(rasters$bioclim10) +
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
#   tm_shape(my_map) +
#   tm_borders() +
#   tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
#   tm_shape(train_abs_pts) +
#   tm_dots(col = "grey20", legend.show = FALSE) +
#   tm_shape(train_occ_pts) +
#   tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_shape(rect_sf_transformed) +
#   tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
#   tm_layout(title = "a) Treningsdata", title.bg.color = "white", legend.bg.color = "white",
#             legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
#             inner.margins = c(-0.05, -0.05, -0.05, -0.05))

# map1

# map2 <- tm_shape(rasters$bioclim10) +
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
#   tm_layout(title = "Testdata", legend.outside = FALSE) +
#   tm_shape(my_map) +
#   tm_borders() +
#   tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
#   tm_shape(test_abs_pts) +
#   tm_dots(col = "grey20", legend.show = FALSE) +
#   tm_shape(test_occ_pts) +
#   tm_dots(col = "red", size = 0.1) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_shape(rect_sf_transformed) +
#   tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
#   tm_layout(title = "b) Testdata", title.bg.color = "white", legend.bg.color = "white",
#             legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
#             inner.margins = c(-0.05, -0.05, -0.05, -0.05))
#
# legend_labels2 <- c("Fravær", "Forekomst")

# map3 <- tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
#   #tm_layout(title = "Uavhengige testdata", legend.outside = FALSE) +
#   tm_shape(my_map) +
#   tm_borders() +
#   tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
#   tm_shape(ind_test_abs_pts) +
#   tm_dots(col = "grey20", legend.show = FALSE) +  
#   tm_shape(ind_test_occ_pts) +
#   tm_dots(col = "red", size = 0.15) +
#   tm_add_legend(type = "symbol", labels = legend_labels2, col = legend_col) +
#   tm_shape(rect_sf_transformed) +
#   tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
#   tm_layout(title = "b) Uavhengige\n evalueringsdata", title.bg.color = "white", legend.bg.color = "white", 
#             legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE, 
#             inner.margins = c(-0.05, -0.05, -0.05, -0.05))
# 
# map3
# 
# # Adjust text sizes
# map1 <- map1 + tm_layout(title.size = 1.3, legend.text.size = 1)
# #map2 <- map2 + tm_layout(title.size = 1.3, legend.text.size = 1)
# map3 <- map3 + tm_layout(title.size = 1.3, legend.text.size = 1)
# 
# # Arrange the maps together
# combined_maps <- tmap_arrange(map1, map2, map3, ncol = 3)
# combined_maps
# 
# # Define the output path
# output_path <- "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/tmap_combined4.png"
# 
# # Save the combined maps
# tmap_save(combined_maps, filename = output_path, width = 38, height = 16, units = "cm", dpi = 600, bg = "white")


# Plot a and b and c -----------------------------------------------------------------

legend_labels_simple <- c("Registrert forekomst")
legend_col_simple <- c("red")

all_train_pts <- pts1

map1 <- tm_shape(rasters$bioclim10) + 
  tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  # tm_shape(train_abs_pts) +
  # tm_dots(col = "grey20", legend.show = FALSE) +
  tm_shape(all_train_pts) +
  tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
  tm_add_legend(type = "symbol", labels = legend_labels_simple, col = legend_col_simple) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "a) Responsvariabel", title.bg.color = "white", legend.bg.color = "white", 
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE, 
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map1

legend_labels_simple <- c("Gjennomsnitts\nTemperatur")

map2 <- tm_shape(rasters$bioclim10) + 
  tm_raster(palette = "-RdYlBu", midpoint = NA, style = "cont", title = "Gjennomsnittstemperatur,\nvarmeste kvartal") +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "b) Forklaringsvariabel 1", title.bg.color = "white", legend.bg.color = "white", 
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE, 
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map2


# # Use tmap to plot the downsampled raster
# tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "-RdYlBu", midpoint = NA, style = "cont") +
#   tm_layout(title = "Downsampled Raster Visualization", legend.outside = TRUE)+
#   tm_shape(my_map)+
#   tm_borders()
# 
# rasters$ar50
# unique(rasters$ar50)


# tm_shape(arealdekke) + 
#   tm_raster() +
#   tm_layout(title = "Arealdekke", legend.outside = TRUE)+
#   tm_shape(my_map)+
#   tm_borders()

# Define the Arealdekke layer (assuming it is already loaded as 'arealdekke')
arealdekke <- rasters$ar50

# 10	Bebygd: Boligfelt, tettsted, by, samferdsel, industriområde o.l.
# 20	Jordbruk: Fulldyrka jord, overflatedyrka jord og innmarksbeite
# 30	Skog: Skogdekt areal
# 50	Snaumark: Fastmark med naturlig vegetasjonsdekke som ikke er skog
# 60	Myr: Areal som på overflata har preg av myr
# 70	Bre: Is og snø som ikke smelter i løpet av sommeren
# 81	Ferskvann: Elv og innsjø
# 82	Hav
# 99	Ikke kartlagt

# Define the categories and corresponding colors
categories <- c(10, 20, 30, 50, 60, 99)
labels <- c("10 Bebygd", "20 Jordbruk", "30 Skog", "50 Snaumark", "60 Myr", "99 Annet")
# colors <- c("#e31a1c", "#ffd16e", "#00ad3b", "#cfcc91", "#d1d1ff", "#fafafa")
# colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
colors <- c("#e31a1c", "#FFBF00", "#33a02c", "#cfcc91", "#d1d1ff", "#fafafa")


# Plot the Arealdekke layer with custom categories and colors
# tm_shape(arealdekke) + 
#   tm_raster(style = "cat", palette = colors, labels = labels, title = "AR50 arealtype") +
#   tm_layout(title = "Arealdekke", legend.outside = TRUE) +
#   tm_shape(my_map) +
#   tm_borders()

map3 <- tm_shape(arealdekke) + 
  tm_raster(style = "cat", palette = colors, labels = labels, title = "AR50 arealtype") +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "c) Forklaringsvariabel 2", title.bg.color = "white", legend.bg.color = "white", 
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE, 
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map3

# Adjust text sizes
map1 <- map1 + tm_layout(title.size = 1.3, legend.text.size = 0.9)
map2 <- map2 + tm_layout(title.size = 1.3, legend.text.size = 0.9)
map3 <- map3 + tm_layout(title.size = 1.3, legend.text.size = 0.9)


# Arrange the maps together
combined_maps <- tmap_arrange(map1, map2, map3, ncol = 3)
combined_maps

# Define the output path
output_path <- "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/respons_og_forklaring.png"

# Save the combined maps
tmap_save(combined_maps, filename = output_path, width = 38, height = 16, units = "cm", dpi = 600, bg = "white")


# Plot FOP ----------------------------------------------------------------
# Notice the difference in the scales of 
# the FOP axes. EVs showing a larger interval 
# on the FOP axis typically carry more explanatory power.
library(MIAmaxent)


# Funksjon for å plotte FOP på norsk --------------------------------------

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("EV", "int", "PRO", "RV", "n"))
}

#' Creates infinitely weighted logistic regression model (equivalent to maxent)
#' without regularization
#'
#' @param formula Object of class "formula": a symbolic description of the model
#'   to be fitted. Do not use '.' term, as weights are concatenated to the data
#'   object.
#' @param data Data frame containing the variables in the model. Response
#'   variable as (1/NA).
#'
#' @keywords internal
#' @noRd

.runIWLR <- function(formula, data) {
  RV <- all.vars(formula)[1]
  data[, RV][is.na(data[, RV])] <- 0
  padd <- data[data[, RV]==1, ]
  padd[, RV] <- 0
  padddata <- rbind(data, padd)
  # Code below this line was modified from the MIT-licensed 'maxnet' library
  # Copyright (c) 2016, Steven Phillips
  wgts <- padddata[, RV] + (1 - padddata[, RV])*100
  glmdata <- cbind(padddata, wgts)
  
  withCallingHandlers({
    model <- stats::glm(formula=formula, family="binomial", data=glmdata,
                        weights=wgts)
  }, warning = function(w) {
    if(grepl("fitted probabilities numerically 0 or 1", conditionMessage(w)) ||
       grepl("glm.fit: algorithm did not converge", conditionMessage(w))){
      invokeRestart("muffleWarning")
    }
  })
  
  if (any(is.na(model$coefficients))) {
    nacoef <- names(model$coefficients)[is.na(model$coefficients)]
    model$formula.narm <- stats::update(model$formula,
                                        paste("~ . -", paste(nacoef, collapse = " - ")))
    model$betas <- model$coefficients[-1][!is.na(model$coefficients[-1])]
  } else {
    model$formula.narm <- model$formula
    model$betas <- model$coefficients[-1]
  }
  bkg <- stats::model.matrix(model$formula.narm,
                             padddata[padddata[, RV]==0, ])[, -1, drop=FALSE]
  model$alpha <- 0
  link <- (bkg %*% model$betas) + model$alpha
  rr <- exp(link)
  raw <- rr / sum(rr)
  model$entropy <- -sum(raw * log(raw), na.rm = TRUE)
  model$alpha <- -log(sum(rr))
  class(model) <- c("iwlr", class(model))
  return(model)
  # Code above this line was modified from the MIT-licensed 'maxnet' library
  # Copyright (c) 2016, Steven Phillips
}



#' Predict method for infinitely-weighted logistic regression
#'
#' Returns model predictions for new data in "PRO" or "raw" format.
#'
#' @param object Model of class "iwlr"
#' @param newdata Data frame containing variables with which to predict
#' @param type Type of model output: "PRO" or "raw"
#'
#' @keywords internal
#'
#' @export
#'
#' @method predict iwlr

predict.iwlr <- function(object, newdata, type="PRO", ...) {
  mmformula <- stats::update.formula(object$formula.narm, NULL ~ . - 1)
  newdata <- stats::model.matrix(mmformula, newdata)
  raw <- exp((newdata %*% object$betas) + object$alpha)
  RV <- all.vars(object$formula)[1]
  N <- sum(object$data[,RV] == 0)
  PRO <- raw * N
  if (type == "PRO") {return(PRO)} else {return(raw)}
}


#' checks the validity of RV values
#'
#' Presence-only data should be coded as: 1/NA (preferred) or 1/0 (danger of
#' misinterpretation as presence/absence data)
#'
#' @param rv Vector of response variable values
#' @keywords internal
#' @noRd

.binaryrvcheck <- function(rv) {
  if (class(rv) != "numeric" && class(rv) != "integer") {
    stop("The response variable must be numeric or integer class: presence (1)
         and either background or absence (NA/0)", call. = FALSE)
  }
  if (anyNA(rv) && !all(levels(as.factor(rv)) %in% "1")) {
    stop("The response variable must contain exactly 2 levels: presence (1)
       and either background or absence (NA/0)", call. = FALSE)
  }
  if (!anyNA(rv) && !all(levels(as.factor(rv)) %in% c("1", "0"))) {
    stop("The response variable must contain exactly 2 levels: presence (1)
       and either background or absence (NA/0)", call. = FALSE)
  }
}



#' checks representation of dvs in tranformations
#'
#' @param dvnamesni Names of DVs in model (no interaction terms)
#' @param alltransf List of transformation functions
#' @keywords internal
#' @noRd

.check.dvs.in.transf <- function(dvnamesni, alltransf) {
  for (i in dvnamesni) {
    a <- paste0(i, "_transf")
    if (sum(names(alltransf) == a) != 1) {
      stop(paste(i, "must be represented in 'transformations' (exactly once)"),
           call. = FALSE)
    }
  }
}



#' calculates optimum EV value based on FOP
#'
#' The optimum that is returned is based on the loess-smoothed data (for
#' continuous variables).
#'
#' @param data Dataframe containing the response variable in the first column
#'   and explanatory variables in the second column. The response variable
#'   should represent presence or background, coded as: 1/NA.
#' @param span The proportion of FOP points included in the local regression
#'   neighborhood. Should be between 0 and 1. Irrelevant for categorical EVs.
#' @param intervals Number of intervals into which the continuous EV is divided.
#'   Defaults to the minimum of N/10 and 100. Irrelevant for categorical EVs.
#'
#' @return the EV value at which FOP is highest (\code{EVoptimum})
#' @keywords internal
#' @noRd

.fopoptimum <- function(df, span = 0.5, intervals = NULL) {
  
  df <- data.frame(RV = df[, 1], EV = df[, 2])
  .binaryrvcheck(df[, 1])
  df[, 1][is.na(df[, 1])] <- 0
  
  if (class(df[, 2]) %in% c("numeric", "integer")) {
    if (is.null(intervals)) {intervals <- min(c(ceiling(nrow(df)/10), 100))}
    df$int <- cut(df[, 2], breaks=max(2, intervals))
    grouped <- dplyr::group_by(df, int)
    FOPdf <- as.data.frame(dplyr::summarise(grouped, n = dplyr::n(),
                                            intEV = mean(EV),
                                            intRV = mean(RV, na.rm=FALSE)))
    FOPdf$loess <- stats::predict(stats::loess(intRV~intEV, FOPdf,
                                               weights=FOPdf$n, span=span))
    if (any(is.na(FOPdf$loess))) {
      EVoptimum <- FOPdf$intEV[which.max(FOPdf$intRV)]
    } else { EVoptimum <- FOPdf$intEV[which.max(FOPdf$loess)]  }
    
  }
  
  if (class(df[, 2]) %in% c("factor", "character")) {
    grouped <- dplyr::group_by(df, EV)
    FOPdf <- as.data.frame(dplyr::summarise(grouped, n = dplyr::n(),
                                            lvlRV = mean(RV, na.rm=FALSE)))
    EVoptimum <- FOPdf$EV[which.max(FOPdf$lvlRV)]
  }
  
  return(EVoptimum)
}



#' checks the validity of formulas
#'
#' @param formula Formula entered as selection start point
#' @param dvdata List of data frames containing EVs
#' @keywords internal
#' @noRd

.formulacheck <- function(formula, dvdata) {
  if (any(attr(stats::terms(formula), "order") != 1)) {
    stop("The provided formula may contain first-order explanatory variables
      only (no interactions)", call. = FALSE)
  }
  trms <- labels(stats::terms(formula))
  for (i in trms) {
    if (sum(names(dvdata) == i) != 1) {
      stop(paste(i, "must be represented in 'dvdata' (exactly once)"),
           call. = FALSE)
    }
  }
}


#' Loads a transformations object
#'
#' From .Rdata file or from existing object
#'
#' @param transformations transformations object produced by deriveVars
#' @keywords internal
#' @noRd

.load.transf <- function(transformations) {
  if (class(transformations) == "character") {
    alltransf <- get(load(transformations))
  } else {
    alltransf <- transformations
  }
  if (!all(sapply(alltransf[-1], class) == "function")) {
    stop("'transformations' should be a transformations object returned by 'deriveVars'",
         call. = FALSE)
  }
  return(alltransf)
}



#' calculates skewness of a vector
#'
#' Also calculates the constant 'c' needed for zero-skewness transformation in
#' \code{scalex}
#'
#' @param x Vector of data. Must have scale [0,1]!
#' @keywords internal
#' @noRd

.minskew <- function(x) {
  cmin <- min(x)-10*(max(x)-min(x))
  cmax <- max(x)+10*(max(x)-min(x))
  if(e1071::skewness(x, na.rm=TRUE, type=2) >= 0 && cmin < -min(x)) {
    cmin <- -min(x)
  }
  cmid <- (cmin + cmax) / 2
  skew <- e1071::skewness(.scalex(x, x, cmid), na.rm=TRUE)
  while (abs(skew) > 1 * 10^-05 && min(abs(c(cmax, cmin)-cmid)) > 10^-10) {
    sleft <- e1071::skewness(.scalex(x, x, (cmid + cmin) / 2), na.rm = TRUE,
                             type = 2)
    sright <- e1071::skewness(.scalex(x, x, (cmid + cmax) / 2), na.rm = TRUE,
                              type = 2)
    if (abs(sleft) < abs(skew) && abs(sleft) < abs(sright)) {
      cmax <- cmid
      skew <- sleft
    }
    else if (abs(sright) < abs(skew)) {
      cmin <- cmid
      skew <- sright
    }
    else {
      cmin <- (cmin + cmid) / 2
      cmax <- (cmax + cmid) / 2
    }
    cmid <- (cmin + cmax) / 2
  }
  return(list(c = cmid, skew = skew))
}



#' Plotting helper function for testAUC
#'
#' @param fpr false positive rate vector
#' @param tpr true positive rate vector
#' @param AUC AUC value
#' @param x PRO = 1 x-coordinate
#' @param y PRO = 1 y-coordinate
#' @keywords internal
#' @noRd

.plotROC <- function(fpr, tpr, AUC, PROpt, x, y, ...) {
  args1 <- list(xlab="1 - specificity (false positive rate)",
                ylab="Sensitivity (true positive rate)", col="red",
                main=paste("AUC = ", signif(AUC, 3)))
  inargs <- list(...)
  args1[names(inargs)] <- inargs
  do.call(graphics::plot, c(list(x=fpr, y=tpr, xlim=c(0,1), ylim=c(0,1),
                                 type="l"), args1))
  
  graphics::abline(0, 1, lty=3)
  
  if (PROpt == TRUE) {
    args2 <- list(cex=0.8, col="#999999", pch=19)
    inargs <- list(...)
    args2[names(inargs)] <- inargs
    do.call(graphics::points, c(list(x=x, y=y), args2))
    
    args3 <- list(cex=0.8, col="#999999")
    inargs <- list(...)
    args3[names(inargs)] <- inargs
    do.call(graphics::text, c(list(x=x, y=y, labels="PRO = 1", pos=4), args3))
  }
}



#' Reminders when using devtools::release
#'
#' @keywords internal

release_questions <- function() {
  c(
    "Have you reknitted the static vignette and copied the html file into /vignettes?"
  )
}



#' skewness transformation using constant c
#'
#' @param x Vector of data.
#' @param c Constant
#' @keywords internal
#' @noRd

.scalex <- function(xnull, x, c) {
  if(e1071::skewness(xnull, na.rm = TRUE, type = 2) < 0) {
    return(exp(c * x))
  } else {
    return(log(x + c))
  }
}


#' Return output from \code{\link{selectDVforEV}} as if it had been produced
#' under a stricter (lower) alpha. Results will match \code{selectDVforEV(...,
#' alpha = stricter, retest = TRUE)} if \code{list} was also produced with
#' retest = TRUE.
#'
#' @param list Output list from selectDVforEV()
#' @param alpha Stricter alpha than used to produce \code{list}
#' @keywords internal
#' @noRd
#' @importFrom rlang .data

.stricterselectDVforEV <- function(list, alpha) {
  dvdata <- list()
  selection <- list()
  
  for (i in seq_along(list$selection)) {
    evname <- names(list$selection)[i]
    drop <- FALSE
    ctable <- list$selection[[i]]
    bests <- ctable[!duplicated(ctable$round),]
    if (any(bests$P < alpha)) {
      selectedmod <- utils::tail(dplyr::filter(bests, .data$P < alpha), 1)
      lastround <- min(selectedmod$round + 1, max(bests$round))
    } else {
      lastround <- 1
      drop <- TRUE
    }
    selection[[i]] <- dplyr::filter(ctable, round <= lastround)
    names(selection)[i] <- evname
    if (!drop) {
      selectedset <- unlist(strsplit(selectedmod$variables, split=" + ",
                                     fixed=TRUE))
      dvdata[[i]] <- list$dvdata[[evname]][, selectedset, drop = FALSE]
      names(dvdata)[i] <- evname
    }
  }
  RV <- list(RV = list$dvdata$RV)
  dvdata <- c(RV, Filter(Negate(is.null), dvdata))
  return(list(dvdata = dvdata, selection = selection))
}




plotFOPnorsk <- function(data, EV, span = 0.5, intervals = NULL, ranging = FALSE,
                         densitythreshold = NULL, ...) {
  
  if (EV==1) {
    stop("'EV' cannot be the first column of 'data', which must be the response variable")
  }
  df <- data.frame(RV = data[, 1], EV = data[, EV])
  evname <- names(data[, EV, drop = FALSE])
  
  .binaryrvcheck(df[, 1])
  df[, 1][is.na(df[, 1])] <- 0
  
  if (class(df[, 2]) %in% c("numeric", "integer")) {
    if (ranging == T) {
      df[, 2] <- (df[, 2] - min(df[, 2])) / diff(range(df[, 2]))
    }
    if (is.null(intervals)) {intervals <- min(c(ceiling(nrow(df)/10), 100))}
    df$int <- cut(df[, 2], breaks=max(2, intervals))
    
    grouped <- dplyr::group_by(df, int)
    FOPdf <- as.data.frame(dplyr::summarise(grouped, n = dplyr::n(),
                                            intEV = mean(EV), intRV = mean(RV, na.rm=FALSE)))
    FOPdf$loess <- stats::predict(stats::loess(intRV~intEV, FOPdf,
                                               weights=FOPdf$n, span=span))
    
    if (any(is.na(FOPdf$loess))) {
      evoptimum <- FOPdf$intEV[which.max(FOPdf$intRV)]
    } else { evoptimum <- FOPdf$intEV[which.max(FOPdf$loess)]  }
    
    FOP <- list(EVoptimum = evoptimum,
                FOPdata = FOPdf)
    
    op <- graphics::par(mar=(c(5, 4, 4, 4) + 0.3))
    on.exit(graphics::par(op))
    dens <- stats::density(df[, 2])
    graphics::plot(range(dens$x), range(dens$y), type="n", axes=FALSE, ann=FALSE)
    graphics::polygon(x=c(min(dens$x), dens$x, max(dens$x)), y=c(0, dens$y, 0),
                      border=NA, col="grey90")
    graphics::axis(side=4, col="grey60", col.axis="grey60", las=1)
    graphics::mtext("Tetthetsestimat (Kernel density)", side=4, line=3, col="grey60")
    graphics::par(new=TRUE)
    if (is.null(densitythreshold)) {densitythreshold <- 0}
    closedsymbols <- as.numeric(FOPdf$n >= densitythreshold)+1
    args1 <- list(bty="n", main = paste0(" "), xlab = evname,
                  ylab = "Forekomstandel", las=1,
                  pch=c(1,20)[closedsymbols])
    inargs <- list(...)
    args1[names(inargs)] <- inargs
    do.call(graphics::plot, c(list(x=FOPdf$intEV, y=FOPdf$intRV), args1))
    graphics::points(FOPdf$loess ~ FOPdf$intEV, type="l", lwd=2, col="red")
  }
  
  if (class(df[, 2]) %in% c("factor", "character")) {
    grouped <- dplyr::group_by(df, EV)
    FOPdf <- as.data.frame(dplyr::summarise(grouped, n = dplyr::n(),
                                            lvlRV = mean(RV, na.rm=FALSE)))
    
    FOP <- list(EVoptimum = FOPdf$EV[which.max(FOPdf$lvlRV)],
                FOPdata = data.frame(level=FOPdf$EV, n=FOPdf$n,
                                     levelRV=FOPdf$lvlRV))
    
    op <- graphics::par(mar=(c(5, 4, 4, 4) + 0.3))
    on.exit(graphics::par(op))
    graphics::barplot(FOPdf$n, axes=FALSE, ann=FALSE, col="grey90", border=NA)
    graphics::axis(side=4, col="grey60", col.axis="grey60", las=1)
    graphics::mtext("Antall observasjoner i dataene", side=4, line=3, col="grey60")
    graphics::par(new=TRUE)
    args1 <- list(names.arg = FOPdf$EV, main = paste0(" "),
                  xlab = evname, ylab = "Frekvens av observerte forekomster",
                  density=rep(20, nrow(FOPdf)), col="black", las = 2)
    inargs <- list(...)
    args1[names(inargs)] <- inargs
    do.call(graphics::barplot, c(list(FOPdf$lvlRV), args1))
  }
  
  invisible(FOP)
}


plotRespNorsk <- function(model, transformations, EV, logscale = FALSE, ...) {
  
  if (!(class(model)[1] %in% c("iwlr", "lr"))) {
    stop("'model' should be of the class produced by 'selectEV' or 'chooseModel'", call. = FALSE)
  }
  evbetas <- model$betas[grep(paste0(EV, "_"), names(model$betas))]
  evbetasni <- evbetas[!grepl(":", names(evbetas), fixed=TRUE)]
  if (length(evbetasni)==0) {
    stop("The 'EV' specified cannot be found in the model")
  }
  
  alltransf <- .load.transf(transformations)
  evtransfs <- alltransf[match(paste0(names(evbetasni), "_transf"),
                               names(alltransf), nomatch = 0)]
  if (!(length(evtransfs)==length(evbetasni))) {
    stop("The transformation function for at least one DV in the model is missing")
  }
  
  dvdata <- lapply(evtransfs, function(f) {
    x <- environment(f)$xnull
    f(x) })
  names(dvdata) <- names(evbetasni)
  traindata <- data.frame("RV"=alltransf[[1]], dvdata)
  formula <- stats::formula(paste("RV ~", paste(names(evbetasni), collapse = " + ")))
  
  if (class(model)[1] == "iwlr") {
    smodel <- .runIWLR(formula, traindata)
  } else if (class(model)[1] == "lr") {
    smodel <- .runLR(formula, traindata)
  }
  
  evnull <- environment(evtransfs[[1]])$xnull
  if (class(evnull) %in% c("numeric", "integer")) {
    seq <- seq(min(evnull), max(evnull), length.out = 100)
  }
  if (class(evnull) %in% c("factor", "character")) {
    seq <- levels(as.factor(evnull))
  }
  newdata <- as.data.frame(do.call(cbind,
                                   lapply(evtransfs, function(f, x) {
                                     f(x) }, x=seq)))
  names(newdata) <- names(evbetasni)
  type <- ifelse(class(model)[1] == "iwlr", "PRO", "response")
  preds <- stats::predict(smodel, newdata, type)
  resp <- data.frame(EV = seq, preds = preds)
  
  if (logscale == TRUE) { resp$preds <- log10(resp$preds) }
  ylab <- ifelse(type == "Sannsynlighet", "Prediksjon: rel. sannsynlighet for forekomst", "Prediksjon: rel. sannsynlighet for forekomst")
  if (logscale == TRUE) { ylab <- paste("log", ylab) }
  args1 <- list(main = paste0(" "), xlab = EV,
                ylab = ylab, col="red")
  inargs <- list(...)
  args1[names(inargs)] <- inargs
  
  if (class(resp[, 1]) %in% c("numeric", "integer")) {
    do.call(graphics::plot, c(list(x=resp[, 1], y=resp[, 2], type="l"), args1))
  }
  
  if (class(resp[, 1]) %in% c("factor", "character")) {
    do.call(graphics::barplot, c(list(height=resp[, 2], names.arg=resp[, 1]), args1))
  }
  
  if (type == "PRO") {
    if (logscale == TRUE) { graphics::abline(h = 0, lty = 3)
    } else { graphics::abline(h = 1, lty = 3) }
  }
  
}

# if (class(resp[, 1]) %in% c("factor", "character")) {
#   do.call(graphics::barplot, c(list(height=resp[, 2], names.arg=resp[, 1]), args1))
# }


# if (class(resp[, 1]) %in% c("factor", "character")) {
#   # Add 'las = 2' to make labels vertical
#   do.call(graphics::barplot, c(list(height=resp[, 2], 
#                                     names.arg=resp[, 1], 
#                                     las = 2),  # las = 2 makes the labels vertical
#                                args1))
# }

pa_train <- train |> rename(RV = occ)
fop_train <- train |> rename(RV = occ)


# Funksjon FOP plot norsk slutt -------------------------------------------
par(mfrow=c(3,3)) #plotting window with four columns and three rows
for (i in 2:10) plotFOP(fop_train,i,intervals=500)
for (i in 11:19) plotFOP(pa_train,i,intervals=500)
#for (i in 20:28) plotFOP(pa_train,i,intervals=500)
par(mfrow=c(1,1))

glimpse(fop_train)
unique(fop_train$ar50)

# Assuming 'fop_train' is your dataset

# Define the mapping of ar50 values to character strings
ar50_mapping <- c(
  "10" = "10 Bebygd",
  "20" = "20 Jordbruk",
  "30" = "30 Skog",
  "50" = "50 Snaumark",
  "60" = "60 Myr",
  "99" = "99 Annet"
)

lime_mapping <- c(
  "1" = "1 Kalkfattig",
  "2" = "2 Intermediær",
  "3" = "3 Kalkrik"
)

fop_train2 <- fop_train

# Convert ar50 to character based on the mapping
fop_train2$ar50 <- as.character(fop_train2$ar50)
fop_train2$ar50 <- ar50_mapping[fop_train2$ar50]

fop_train2$berggrunn <- as.character(fop_train2$berggrunn)
fop_train2$berggrunn <- lime_mapping[fop_train2$berggrunn]

# Check the updated data
head(fop_train2)

#
# 10 Bebygd
# 20 Jordbruk
# 30 Skog
# 50 Snaumark
# 60 Myr
# 99 Annet

glimpse(fop_train)

#fop_train <- train
plotFOP(fop_train, "bioclim10", xlab = "Gjennomsnittstempetratur, varmeste kvartal")
plotFOPnorsk(fop_train, "bioclim10", xlab = "Gjennomsnittstempetratur, varmeste kvartal")
plotFOPnorsk(fop_train2, "ar50", xlab = " ")
plotFOPnorsk(fop_train, "aspect", xlab = "Helningsretning")
plotFOPnorsk(fop_train2, "berggrunn", xlab = "Berggrunn")


##########################
# MODELLING
##########################

# MIA_Maxent --------------------------------------------------------------

### Transforming explanatory variables
#DVs <- deriveVars(train, algorithm = "maxent", transformtype = c("L", "M", "D", "HF", "HR", "T", "B"))

# You can check the result with
glimpse(train)
train3 <- train |> select(occ, ar50)


rv_train <- train3 |> rename("RV" = "occ")
rv_train <- train |> rename("RV" = "occ")

rv_train

?deriveVars

#transformtype	
# pecifies the types of transformations types to be performed. 
# Default is the full set of the following transformation types: 
# L (linear), M (monotone), D (deviation), HF (forward hinge), 
# HR (reverse hinge), T (threshold), and B (binary).

DVs <- deriveVars(rv_train,
  #transformtype = c("L", "M", "D", "B"),
  transformtype = c("L", "M", "D", "HF", "HR", "T", "B"),
  allsplines = FALSE,
  algorithm = "maxent",
  write = FALSE,
  dir = NULL,
  quiet=FALSE)


# DVs <- deriveVars(pa_train,
#   transformtype = c("L", "M", "D", "HF", "HR", "T", "B"),
#   allsplines = FALSE,
#   algorithm = "maxent",
#   write = FALSE,
#   dir = NULL,
#   quiet=FALSE)
#
## Preparing predictors with all variables
dvdata <- DVs$dvdata
dvdata
summary(dvdata)
head(summary(DVs$transformations))

# plot transformations 

plot(train$bioclim10, DVs$dvdata$bioclim10$bioclim10_L, pch=20, 
     ylab="bioclim10")
plot(train$bioclim10, DVs$dvdata$bioclim10$bioclim10_M, pch=20, 
     ylab="bioclim10")
plot(train$bioclim10, DVs$dvdata$bioclim10$bioclim10_D05, pch=20, 
     ylab="bioclim10")
plot(train$bioclim10, DVs$dvdata$bioclim10$bioclim10_D1, pch=20, 
     ylab="bioclim10")
plot(train$bioclim10, DVs$dvdata$bioclim10$bioclim10_D2, pch=20, 
     ylab="bioclim10")
plot(train$bioclim10, DVs$dvdata$bioclim10$bioclim10_HF9, pch=20, 
     ylab="bioclim10")
plot(train$bioclim10, DVs$dvdata$bioclim10$bioclim10_T12, pch=20, 
     ylab="bioclim10")

plot(train$ar50, DVs$dvdata$ar50$ar50_BX50, pch=20, 
     ylab="ar50")

targetDVselect <- selectDVforEV(DVs$dvdata, alpha = 0.001, quiet = FALSE)
summary(targetDVselect$dvdata)
sum(sapply(targetDVselect$dvdata[-1], length))
targetDVselect$selection$ar50

targettargetEVselect <- selectEV(targetDVselect$dvdata, alpha = 0.001, 
                              interaction = FALSE, quiet = FALSE)


summary(targetDVselect$dvdata)
targettargetEVselect$selectedmodel$formula
best_models <- targettargetEVselect$selection[!duplicated(targettargetEVselect$selection$round), ]
best_models$P <- formatC(as.numeric(best_models$P), format = "f", digits = 4)
best_models

# path <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/dvdata.rds"
# dvdata <-  readRDS(file = path)
# 
# path <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/DVs.rds"
# DVs <- readRDS(file = path)
#?selectDVforEV

### ### Selecting transformed variables for modelling


# DVselect <- selectDVforEV(
#     dvdata,
#   alpha = 0.001,
#   retest = FALSE,
#   test = "Chisq",
#   algorithm = "maxent",
#   write = FALSE,
#   dir = NULL,
#   quiet = FALSE #Suppress progress bar?
# )


# str(DVselect)
# 
# DVselect <- selectEV(
#   dvdata,
#   alpha = 0.01,
#   retest = FALSE,
#   interaction = FALSE,
#   formula = NULL,
#   test = "Chisq",
#   algorithm = "maxent",
#   write = FALSE,
#   dir = NULL,
#   quiet = FALSE
# )
# 
# 
# summary(DVselect$dvdata)
# # path <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/DVselect.rds"
# # DVselect <- readRDS(file = path)
# 
# #DVselect$dvdata$CLGve$CLGve_BX82 <- NULL
# 
# ### DVselect - building parsimonious models
# ### Without extra predictors
# #?selectEV
# targetEVselect <- selectEV(DVselect$dvdata,
#   alpha = 0.1,
#   interaction = FALSE,
#   write = FALSE)
# # # 
# 
# # summary(DVselect$dvdata)
# # 
# # summary(targetEVselect$dvdata)
# 
# # path <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/targetEVselect.rds"
# # #saveRDS(EVselect, file = path)
# # EVselect <- read_rds(file = path)
# 
# 
# 
# length(EVselect$dvdata[-1])

#EVselect$selectedmodel$formula

# # Extract the unique rows based on the 'round' column
# best_models <- EVselect$selection[!duplicated(EVselect$selection$round), ]

# Format the 'P' column to 4 digits after the decimal, preserving scientific notation for small values
#best_models$P <- formatC(as.numeric(best_models$P), format = "f", digits = 4)

# Display the result
#print(best_models)



par(mfrow = c(1,1))
plot(targetEVselect$selection$round, targetEVselect$selection$Dsq, 
     xlab="round", ylab="Dsq")

trail <- targetEVselect$selection
trail
best_models

#write.csv(trail, "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/model_selection_trail.csv")
writexl::write_xlsx(trail, "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/model_selection_trail.xlsx")

# selecting models vith various types of complexity
targetEVselect$selectedmodel$formula

# Best models in each round
targetEVselect$selection[!duplicated(targetEVselect$selection$round), ]

#?chooseModel()

# one predictors
mymodel1 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10"), algorithm = "maxent")
mymodel1
formula(mymodel1)

# two predictors
mymodel2 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim7"),algorithm = "maxent")
# three predictors
mymodel3 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim7 + precip6"),algorithm = "maxent")

# four predictors
mymodel4 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim7 + precip6 + ar50"),algorithm = "maxent")

# five predictors
mymodel5 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim7 + precip6 + ar50 + bioclim8"),algorithm = "maxent")
# seven predictors
mymodel6 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim7 + precip6 + ar50 + bioclim8 + sca7"),algorithm = "maxent")
# eight predictors
mymodel7 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim7 + precip6 + ar50 + bioclim8 + sca7 + berggrunn"),algorithm = "maxent")

mymodel8 <- chooseModel(targetDVselect$dvdata, 
                         formula("~ bioclim10 + bioclim7 + precip6 + ar50 + bioclim8 + sca7 + berggrunn + tri"),algorithm = "maxent")

# simple model predictors
mymodel_simple <- chooseModel(targetDVselect$dvdata, 
                              formula("~ bioclim10 + ar50"),algorithm = "maxent")

# simple model predictors
mymodel_ar <- chooseModel(targetDVselect$dvdata, 
                          formula("~ ar50"),algorithm = "maxent")

mymodel1
mymodel2
mymodel3
mymodel4
mymodel5
mymodel6
mymodel7
mymodel8
mymodel9
mymodel_simple
mymodel_ar

# examine a single-effect response curve for 
# and marginal effect response curves for the
# most important EV included in the model chosen above

par(mfrow=c(3,1))
plotFOP(train, "bioclim10")
plotResp(mymodel1, DVs$transformations, "bioclim10")
plotResp2(mymodel1, DVs$transformations, "bioclim10")

par(mfrow=c(3,1))
plotFOP(train, "ar50")
plotResp(mymodel_simple, DVs$transformations, "ar50")
plotResp2(mymodel_simple, DVs$transformations, "ar50")

plotFOP(train, "aspect")
# plotResp(mymodel9, DVs$transformations, "aspect")
# plotResp2(mymodel9, DVs$transformations, "aspect")

plotFOP(train, "berggrunn")
# plotResp(mymodel9, DVs$transformations, "tmin9")
# plotResp2(mymodel9, DVs$transformations, "tmin9")


# Best models in each round
targetEVselect$selection[!duplicated(targetEVselect$selection$round), ]

calculateFTVA(targetEVselect,  formula(mymodel1))
calculateFTVA(targetEVselect,  formula(mymodel2))
calculateFTVA(targetEVselect,  formula(mymodel3))
calculateFTVA(targetEVselect,  formula(mymodel4))
calculateFTVA(targetEVselect,  formula(mymodel5))
calculateFTVA(targetEVselect,  formula(mymodel6))
calculateFTVA(targetEVselect,  formula(mymodel7))
calculateFTVA(targetEVselect,  formula(mymodel8))
calculateFTVA(targetEVselect,  formula(mymodel9))
calculateFTVA(targetEVselect,  formula(mymodel10))
calculateFTVA(targetEVselect,  formula(mymodel11))
calculateFTVA(targetEVselect,  formula(mymodel12))
calculateFTVA(targetEVselect,  formula(mymodel13))
calculateFTVA(targetEVselect,  formula(mymodel14))
calculateFTVA(targetEVselect,  formula(mymodel15))
calculateFTVA(targetEVselect,  formula(mymodel_simple))
calculateFTVA(targetEVselect,  formula(mymodel_ar))


par(mfrow = c(3,3))

# Evaluate model with PO data
testAUC(model = mymodel1, transformations = DVs$transformations,
        data = train)



# testAUC(model = mymodel2, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel3, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel4, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel5, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel6, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel7, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel8, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel9, transformations = DVs$transformations,
#         data = train)
# 
# testAUC(model = mymodel_simple, transformations = DVs$transformations,
#         data = train, add = TRUE)
# 
# testAUC(model = mymodel_ar, transformations = DVs$transformations,
#         data = train, add = TRUE)


# Evaluate model with external PA data

par(mfrow = c(3,3))

pa_evaluation <-  ind_test

testAUC(model = mymodel1, transformations = DVs$transformations,
        data = pa_evaluation)

testAUC(model = mymodel2, transformations = DVs$transformations,
        data = pa_evaluation)

mymodel2

testAUC(model = mymodel3, transformations = DVs$transformations,
        data = pa_evaluation)

testAUC(model = mymodel4, transformations = DVs$transformations,
        data = pa_evaluation)

testAUC(model = mymodel5, transformations = DVs$transformations,
        data = pa_evaluation)

testAUC(model = mymodel6, transformations = DVs$transformations,
        data = pa_evaluation)

testAUC(model = mymodel7, transformations = DVs$transformations,
        data = pa_evaluation)

testAUC(model = mymodel8, transformations = DVs$transformations,
        data = pa_evaluation)

testAUC(model = mymodel9, transformations = DVs$transformations,
        data = pa_evaluation)


testAUC(model = mymodel9, transformations = DVs$transformations, col = "blue",
        data = pa_evaluation)

testAUC(model = mymodel_simple, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)

#?MIAmaxent::projectModel()

testAUC(model = mymodel_ar, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)

#DVs$transformations



# DATA1 <- projectModel(model=mymodel_simple, transformations=DVs$transformations, data = pa_evaluation)$output 
# auc.roc.plot(DATA1, color=T, smoothing = 1,legend.cex=0.9, main="")

# Spatial prediction
# Run the three following lines to time the operation
# ptm <- proc.time() ## start of clock and then to end
# predictions <- projectModel(model=mymodel, transformations=DVs$transformations, data=rasterstack) # 10 min
# proc.time()-ptm ## end of clock. (10-30 min)
# (round(seconds_to_period(ptm["elapsed"]),0))


#pred = terra::predict(stack, model = mymodel, type = "response")

# Spatial prediction

model <- mymodel_simple
library(raster)
# Converting SpatRaster to RasterStack
rstack <- raster::stack(rasters)
rstack 
names(rstack)
# predictor_data <- rstack[[c(13,44)]]
# predictor_data
# plot(predictor_data)
# Alternatively, using subset
#predictor_data <- subset(rstack, c("bioclim10", "tmax6"))
# predictor_data
# plot(predictor_data)
# single_raster <- rstack[[c(13)]]
# single_raster
# plot(single_raster)
# 
# # Master raster file
# master_raster <- raster("C:/Privat/spatial_trond/norwegian_rasters/categorical/master_raster.tif") #Master DEM (keep this file without changes)
# master_raster
# maskLayer <- single_raster

##############################
##############################
#####                      ###
#####       Spatial        ###
#####     predictions      ###
#####                      ###
##############################
##############################

# ?MIAmaxent::projectModel
# ?MIAmaxent::projectRaster


# Spatial prediction
# Run the three following lines to time the operation
ptm <- proc.time() ## start of clock and then to end
predictions <- projectModel(model= mymodel_simple, transformations= DVs$transformations, data=rstack, rescale = TRUE) # 10 min
proc.time()-ptm ## end of clock. (10-30 min)
#save(predictions, file = "moselyng_spatial_pred.rda")
par(mfrow = c(1,1))
plot(predictions$output)
#writeRaster(predictions$output, "predictions_moselyng.tif", overwrite = T)
prediction_map <- rast(predictions$output)
plot(prediction_map)

predictions2 <- projectModel(model= mymodel_simple, transformations= DVs$transformations, data=ind_test, rescale = TRUE) # 10 min
predictions2_df <- predictions2$output
predictions2_df$occ <- as.factor(predictions2_df$occ)
glimpse(predictions2_df)
boxplot(predictions2_df$PRO~predictions2_df$occ)

predictions8 <- projectModel(model= mymodel8, transformations= DVs$transformations, data=ind_test, rescale = TRUE) # 10 min
predictions8_df <- predictions8$output
predictions8_df$occ <- as.factor(predictions8_df$occ)
glimpse(predictions8_df)

boxplot(predictions8_df$PRO~predictions8_df$occ)
mean(predictions_df$PRO)

ggplot(data = predictions2_df, aes(x = predictions2_df$occ, y = predictions2_df$PRO))+
  geom_boxplot()+
  geom_jitter()


testAUC(model = mymodel_simple, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)

# Calculate ROC and AUC
roc_obj2 <- roc(response = predictions2_df$occ, predictor = predictions2_df$PRO)
roc_obj2

# Print the AUC value
auc_value2 <- auc(roc_obj2)
print(auc_value2)

# Optionally, plot the ROC curve
#plot(roc_obj2, main = paste("AUC:", round(auc_value, 3)))

# Calculate ROC and AUC
roc_obj8 <- roc(response = predictions8_df$occ, predictor = predictions8_df$PRO)
roc_obj8

# Print the AUC value
auc_value8 <- auc(roc_obj8)
print(auc_value8)

# Optionally, plot the ROC curve
plot(roc_obj8, main = paste("AUC:", round(auc_value2, 3)))
plot(roc_obj2, add = TRUE)



# ROCR --------------------------------------------------------------------

library(ROCR)

library(ROCR)

# Create a prediction object for roc_obj8
pred_8 <- prediction(predictions8_df$PRO, predictions8_df$occ)

# Create performance object
perf_8 <- performance(pred_8, "tpr", "fpr")

# Plot ROC curve
plot(perf_8, main = "ROC Curve for Dataset 8")

# Create a prediction object for roc_obj2
pred_2 <- prediction(predictions2_df$PRO, predictions2_df$occ)

# Create performance object
perf_2 <- performance(pred_2, "tpr", "fpr")

# Plot ROC curve
plot(perf_2, main = "ROC Curve for Dataset 2")

# Set the filename and resolution for the output image
#png("C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/ROC_kurver_MaxEnt_modell.png", width = 6.5, height = 7, units = "in", res = 600)

# Plot the ROC curves
plot(perf_2, col = "blue", main = "ROC-kurver for utbredelsesmodell", xlab = "Andel falske positive (1 – spesifisitet)", 
     ylab = "Andel ekte positive (sensitivitet)", 
     cex.lab = 1.2,  # Increase size of x and y axis labels
     cex.main = 1.4) # Increase size of the main title)
plot(perf_8, col = "red", add = TRUE)
#plot(perf_ind_test, col = "forestgreen", add = TRUE)

# Add diagonal line representing AUC = 0.5 (Random Model)
abline(a = 0, b = 1, col = "gray", lty = 2)

# Add line for a perfect model
lines(c(0, 0, 1), c(0, 1, 1), col = "black", lty = 3, lwd = 2)

# Add legend including the perfect and random models
legend("bottomright", legend = c("Enkel modell (AUC = 0,86)",  
                                 "Kompleks modell (AUC = 0,79)", 
                                 "Tilfeldig klassifisering (AUC = 0,5)", "Perfekt modell (AUC = 1,0)"), 
       col = c("blue", "red", "gray", "black"), 
       lty = c(1, 1, 1, 2, 3), lwd = c(1, 1, 1, 1, 2))

text(0.4, 0.6, "Areal under\n kurven\n(AUC)",
     cex = 1.0)

# Close the file device to save the image
dev.off()


# Plot variable importance
plot(me, xlim = c(0,40))
me


# # Print AUC for training and testing data
# cat("AUC for Training Data:", auc(roc_train_me_subset), "\n")
# cat("AUC for Testing Data:", auc(roc_test_me_subset), "\n")
# cat("AUC for Independent Testing Data:", auc(roc_ind_test_me_subset), "\n")
# 
# # Plot ROC curves
# plot(roc_train_me_subset, col = "blue", main = "ROC-kurver for MaxEnt-modell", xlab = "1 – spesifisitet", ylab = "Sensitivitet")
# lines(roc_test_me_subset, col = "red")
# lines(roc_ind_test_me_subset, col = "forestgreen")
# text(0.6, 0.6, "Areal under\n kurven\n(AUC)",
#      cex = 1.8)
# legend("bottomright", legend = c("Train", "Test", "Independent test"), col = c("blue", "red", "forestgreen"), lty = 1)
# 
# # Plot variable importance
# plot(me_subset, xlim = c(0,70))
# #me_subset


# # Envelope Modeling --------------------------------------------------
# # The envelope (a.k.a. “Bioclim”) is an example. 
# # It only uses presence data, so we use ‘presvals’ instead of ‘sdmdata’.
# # Envelope modelling
# 
# envelope_model <- envelope(presvals[,c(2:8)])
# envelope_model
# summary(envelope_model)
# 
# pr <- partialResponse(envelope_model, presvals, "tri")
# plot(pr, type="l")
# 
# pr <- partialResponse(envelope_model, presvals, "bioclim3")
# plot(pr, type="l")
# 
# pr <- partialResponse(envelope_model, presvals, "bioclim7")
# plot(pr, type="l")
# 
# pr <- partialResponse(envelope_model, presvals, "bioclim8")
# plot(pr, type="l")
# 
# pr <- partialResponse(envelope_model, presvals, "bioclim10")
# plot(pr, type="l")
# 
# pr <- partialResponse(envelope_model, presvals, "percip6")
# plot(pr, type="l")
# 
# pr <- partialResponse(envelope_model, presvals, "sca7")
# plot(pr, type="l")
# 
# 
# names(rasters)
# names(envelope_model)
# 
# # Predict using the envelope model
# envelope_prediction <- terra::predict(object = rasters, 
#                                       model = envelope_model, 
#                                       fun = predict, na.rm = TRUE)
# 
# plot(envelope_prediction, main = "Predicted Suitability (Envelope Model)")
# 
# 
# 
# # Predict on training data
# pred_train_env <- predict(envelope_model, predictors_train, na.rm=TRUE)
# 
# # Predict on testing data
# pred_test_env <- predict(envelope_model, predictors_test)
# 
# # Predict on independent testing data
# pred_ind_test_env <- predict(envelope_model, predictors_ind_test)
# 
# # Evaluate the Envelope model
# roc_train_env <- roc(response_train, pred_train_env)
# roc_test_env <- roc(response_test, pred_test_env)
# roc_ind_test_env <- roc(response_ind_test, pred_ind_test_env)
# 
# 
# # Print AUC for Envelope model
# cat("AUC for Envelope Training Data:", auc(roc_train_env), "\n")
# cat("AUC for Envelope Testing Data:", auc(roc_test_env), "\n")
# cat("AUC for Envelope Testing Data:", auc(roc_ind_test_env), "\n")
# 
# 
# # Plot ROC curves for Envelope model
# plot(roc_train_env, col = "blue", main = "ROC Curves for Envelope Model")
# lines(roc_test_env, col = "red")
# lines(roc_ind_test_env, col = "forestgreen")
# legend("bottomright", legend = c("Train", "Test", "Independent test"), col = c("blue", "red", "forestgreen"), lty = 1)
# 
# # Plot predicted values
# par(mfrow = c(1, 1)) # Ensure only one plot is shown
# 
# # Determine minimum threshold for "presence"
# env_eval <- pa_evaluate(p = test[test$occ == 1, ],
#                         a = test[test$occ == 0, ],
#                         model = envelope_model,
#                         type = "response")
# 
# par(mfrow = c(2,2))
# plot(env_eval, "ROC")
# plot(env_eval, "TPR")
# plot(env_eval, "boxplot")
# plot(env_eval, "density")
# par(mfrow = c(1,1))
# 
# # Determine minimum threshold for "presence"
# env_threshold <- env_eval@thresholds$max_spec_sens
# env_threshold
# 
# # Plot the results
# # Plot base map
# #plot(my_map, axes = TRUE, col = "grey95")
# 
# # Only plot areas where probability of occurrence is greater than the threshold
# plot(envelope_prediction > env_threshold, col = c(NA, "olivedrab"))
# 
# plot(envelope_prediction > env_threshold, add = TRUE, legend = FALSE, col = c(NA, "olivedrab"))
# 
# # And add those observations
# points(x = obs_data$longitude, y = obs_data$latitude, col = "black", pch = "+", cex = 0.75)
# 
# # Redraw those country borders
# plot(my_map, add = TRUE, border = "grey5")
# 
# 
# # Modelling GLM ---------------------------------------------------------------
# 
# glimpse(train)
# 
# # Build a model using training data
# glm_model <- glm(occ ~ ar50, data = train, family = binomial())
# glm_model <- glm(occ ~ bioclim10, data = train, family = binomial())
# glm_model <- glm(occ ~poly(bioclim10, 2), data = train, family = binomial())
# glm_model <- glm(occ ~poly(bioclim10, 3), data = train, family = binomial())
# 
# glm_model <- glm(occ~. , data = train, family = binomial())
# 
# #glm_model <- glm(occ ~ bioclim10 + ar50, data = train, family = binomial())
# glm_model <- glm(occ ~ poly(bioclim10, 2) + ar50, data = train, family = binomial())
# glm_model <- glm(occ ~ poly(bioclim10, 2) + bioclim3 + ar50, data = train, family = binomial())
# glm_model <- glm(occ ~ poly(bioclim10, 2) + poly(bioclim3, 2) +poly(sca7, 2) + ar50, data = train, family = binomial())
# glm_model <- glm(occ ~ poly(bioclim10, 2) + poly(bioclim3, 2) + ar50, data = train, family = binomial())
# glm_model <- glm(occ ~ poly(bioclim10, 2) + poly(bioclim3, 2), data = train, family = binomial())
# 
# #glm_model <- glm(occ ~ poly(bioclim10, 3) + ar50, data = train, family = binomial())
# 
# glm_model
# summary(glm_model)
# glance(glm_model)
# 
# # Response plots ----------------------------------------------------------
# rp <- partialResponse(glm_model, data = train, var = "bioclim10", nsteps=30)
# 
# ggplot(rp, aes(x = bioclim10, y = p))+
#   geom_line()+
#   ylab("prob of occurence")+
#   xlab("bioclim10")+
#   xlim(-5,20)
# #rp2 <- partialResponse(glm_model, data = train, var = "bioclim10", var2 = "ar50", nsteps=30)
# 
# 
# # Predict on training and testing data using GLM
# pred_train_glm <- predict(glm_model, data = train, type = "response")
# pred_test_glm <- predict(glm_model, newdata = test, type = "response")
# pred_ind_test_glm <- predict(glm_model, newdata = ind_test, type = "response")
# 
# # Evaluate the GLM model
# roc_train_glm <- roc(train$occ, pred_train_glm)
# roc_test_glm <- roc(test$occ, pred_test_glm)
# roc_ind_test_glm <- roc(ind_test$occ, pred_ind_test_glm)
# 
# # Print AUC for GLM model
# cat("AUC for GLM Training Data:", auc(roc_train_glm), "\n")
# cat("AUC for GLM Testing Data:", auc(roc_test_glm), "\n")
# cat("AUC for GLM Independent Testing Data:", auc(roc_ind_test_glm), "\n")
# 
# 
# # Plot ROC curves for GLM model
# plot(roc_train_glm, col = "blue", main = "ROC Curves for GLM Model")
# lines(roc_test_glm, col = "red")
# lines(roc_ind_test_glm, col = "forestgreen")
# legend("bottomright", legend = c("Train", "Test", "Independent test"), col = c("blue", "red", "forestgreen"), lty = 1)
# 
# 
# # Prediction on spatial rasters -------------------------------------------
# 
# # Step 4: Identify and replace "Hav" in 'rasters$ar50' with NA
# rasters$ar50[rasters$ar50 == "Hav"] <- NA
# 
# # Step 5: Ensure 'ar50' factor levels in 'rasters' match those in 'train'
# #rasters$ar50 <- factor(rasters$ar50, levels = levels(train$ar50))
# 
# # Record the start time
# start_time <- Sys.time()
# 
# # Get predicted values from the model
# glm_predict <- predict(rasters, glm_model, type = "response", na.rm = TRUE)
# 
# # Record the end time
# end_time <- Sys.time()
# 
# # Calculate the elapsed time
# elapsed_time <- end_time - start_time
# 
# # Extract the hours, minutes, and seconds from the elapsed time
# elapsed_seconds <- as.numeric(elapsed_time, units = "secs")
# hours <- floor(elapsed_seconds / 3600)
# minutes <- floor((elapsed_seconds %% 3600) / 60)
# seconds <- round(elapsed_seconds %% 60, 2)
# 
# # Print the time taken in hours, minutes, and seconds
# cat(sprintf("Time taken: %02d:%02d:%05.2f (hours:minutes:seconds)\n", hours, minutes, seconds))
# 
# glm_predict
# plot(glm_predict)
# 
# # # Use tmap to plot the downsampled raster
# # tm_shape(glm_predict) + 
# #   tm_raster(palette = "RdYlBu", midpoint = NA, style = "cont") +
# #   tm_layout(title = "Predictions", legend.outside = TRUE)
# 
# # # Downsample the raster for faster plotting
# # predict_glm_downsampled <- aggregate(glm_predict, fact = 10, fun = max)
# # 
# # tmap_mode("plot")
# # # Use tmap to plot the downsampled raster
# # tm_shape(predict_glm_downsampled) + 
# #   tm_raster(palette = "Reds", midpoint = NA, style = "cont") +
# #   tm_layout(title = "Downsampled Raster Visualization", legend.outside = TRUE)
# 
# 
# # Define the output file paths
# #output_path_glm_predict <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/glm_predict.tif"
# #output_path_glm_pred_error_map <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/glm_pred_error_map.tif"
# 
# # Save the glm_predict raster as a GeoTIFF
# #writeRaster(glm_predict, output_path_glm_predict, overwrite = TRUE)
# 
# 
# # Prediction error GLM ----------------------------------------------------
# 
# # Record the start time
# # start_time <- Sys.time()
# # 
# # # Get predicted values from the model
# # # to get the probability and standard error
# # glm_pred_error_map <- predict(rasters, glm_model, se.fit=TRUE, na.rm = TRUE)
# # 
# # # Record the end time
# # end_time <- Sys.time()
# # 
# # # Calculate the elapsed time
# # elapsed_time <- end_time - start_time
# # 
# # # Extract the hours, minutes, and seconds from the elapsed time
# # elapsed_seconds <- as.numeric(elapsed_time, units = "secs")
# # hours <- floor(elapsed_seconds / 3600)
# # minutes <- floor((elapsed_seconds %% 3600) / 60)
# # seconds <- round(elapsed_seconds %% 60, 2)
# 
# # # Print the time taken in hours, minutes, and seconds
# # cat(sprintf("Time taken: %02d:%02d:%05.2f (hours:minutes:seconds)\n", hours, minutes, seconds))
# # 
# # glm_pred_error_map
# # plot(glm_pred_error_map)
# 
# # Define the output file paths
# output_path_fit <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/fit.tif"
# output_path_se_fit <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/se_fit.tif"
# output_path_residual_scale <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/residual_scale.tif"
# 
# # Save each layer as a GeoTIFF
# # writeRaster(glm_pred_error_map[["fit"]], output_path_fit, overwrite = TRUE)
# # writeRaster(glm_pred_error_map[["se.fit"]], output_path_se_fit, overwrite = TRUE)
# # writeRaster(glm_pred_error_map[["residual.scale"]], output_path_residual_scale, overwrite = TRUE)
# 
# # Confirm the rasters are saved
# cat("Rasters have been saved as GeoTIFF files.")
# 
# # head(training)
# # # Extract the relevant raster layer for ar50
# # ar50_layer <- rasters$ar50
# # 
# # # Convert ar50 to a factor
# # ar50_layer <- as.factor(ar50_layer)
# # 
# # # Set levels 99, 81, and 82 to NA
# # ar50_layer[ar50_layer %in% c("99", "81", "82")] <- NA
# # 
# # # Ensure the factor levels are consistent with the model's levels
# # model_levels <- levels(training$ar50)
# # ar50_layer <- factor(ar50_layer, levels = model_levels)
# # 
# # # Update the rasters object with the new factor layer
# # rasters$ar50 <- ar50_layer
# # rasters$ar50
# # 
# # # Get the predicted values from the model
# # glm_predict <- predict(rasters, glm_model, type = "response")
# # 
# # glm_predict
# # hist(glm_predict)
# # 
# # # Print predicted values
# # plot(glm_predict)
# # 
# # # p = testing[testing$pa == 1, ]: In this case, p stands for presence data, so we pass
# # # all the rows in the testing data that correspond to a location where there was a species
# # # present (that is, the value in the pa column is equal to 1).
# 
# 
# # Use testing data for model evaluation
# # glm_eval <- pa_evaluate(p = test[test$occ == 1, ],
# #                         a = test[test$occ == 0, ],
# #                         model = glm_model,
# #                         type = "response")
# # glm_eval
# 
# glm_eval <- pa_evaluate(p = ind_test[ind_test$occ == 1, ],
#                         a = ind_test[ind_test$occ == 0, ],
#                         model = glm_model,
#                         type = "response")
# glm_eval
# 
# par(mfrow = c(2,2))
# plot(glm_eval, "ROC")
# plot(glm_eval, "TPR")
# plot(glm_eval, "boxplot")
# plot(glm_eval, "density")
# par(mfrow = c(1,1))
# 
# 
# # Determine minimum threshold for "presence"
# glm_threshold <- glm_eval@thresholds$max_spec_sens
# glm_threshold_no_omission <- glm_eval@thresholds$no_omission
# glm_threshold_prev <- glm_eval@thresholds$equal_prevalence
# glm_threshold_kappa <- glm_eval@thresholds$max_kappa
# glm_threshold_equal <- glm_eval@thresholds$equal_sens_spec
# 
# par(mfrow = c(1,1))
# # Plot the results
# # Plot base map
# 
# 
# par(mfrow = c(1,2))
# plot(my_map$geometry, 
#      axes = FALSE, 
#      col = "grey95", 
#      border = "grey50")
# 
# plot(glm_predict > glm_threshold, 
#      add = TRUE, 
#      legend = TRUE, 
#      col = c(NA, "olivedrab")) # only color for second element ("present")
# 
# plot(my_map$geometry, 
#      axes = FALSE, 
#      col = "grey95", 
#      border = "grey50")
# 
# # Only plot areas where probability of occurrence is greater than the threshold
# plot(glm_predict > glm_threshold_no_omission, 
#      add = TRUE, 
#      legend = FALSE, 
#      alpha = 0.5,
#      col = c(NA, "olivedrab")) # only color for second element ("present")
# 
# plot(glm_predict > glm_threshold_prev, 
#      add = TRUE, 
#      legend = FALSE, 
#      alpha = 0.5,
#      col = c(NA, "olivedrab")) # only color for second element ("present")
# 
# plot(glm_predict > glm_threshold_kappa, 
#      add = TRUE, 
#      legend = FALSE, 
#      alpha = 0.5,
#      col = c(NA, "olivedrab")) # only color for second element ("present")
# 
# plot(glm_predict > glm_threshold_equal, 
#      add = TRUE, 
#      legend = FALSE, 
#      alpha = 0.5,
#      col = c(NA, "olivedrab")) # only color for second element ("present")
# 
# 
# 
# # Only plot areas where probability of occurrence is greater than the threshold
# plot(glm_predict > glm_threshold_prev, legend = FALSE, col = c(NA, "olivedrab"))
# 
# 
# # Load the thresholded raster
# #glm_threshold_raster <- glm_predict > glm_threshold_kappa
# glm_threshold_raster <- glm_predict > glm_threshold_prev
# plot(glm_threshold_raster)
# 
# # Convert to a categorical raster: 1 for presence and NA for absence
# values(glm_threshold_raster) <- ifelse(values(glm_threshold_raster), 1, NA)
# 
# # Define levels for the categorical raster
# levels(glm_threshold_raster) <- data.frame(value = 1, presence = "presence")
# 
# # Create custom legends
# legend_labels <- c("Forekomst")
# legend_col <- c("red")
# 
# # Define the palette
# threshold_palette <- c("olivedrab")
# #threshold_palette <- c("black")
# 
# 
# # Plot with tmap
# tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.1, legend.show = FALSE) +
#   tm_shape(glm_threshold_raster) + 
#   tm_raster(palette = threshold_palette, legend.show = FALSE) +
#   tm_shape(my_map) +
#   tm_borders(lwd = 0.01, col = "grey30") +
#   tm_shape(all_occs_pts) +
#   tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "GLM", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)
# 
# 
# # Add the points with colors and sizes based on 'occ'
# training_data_original
# occ_1 <- training_data_original |> dplyr::select(occ) |> 
#   dplyr::filter(occ == 1)
# 
# 
# occ_1
# plot(occ_1, add = TRUE)
# 
# # Plot points with occ == 1 (red and larger)
# plot(st_geometry(occ_1), col = "red", pch = 1, cex = 0.5, add = TRUE)
# 
# varImportance(glm_model, train)
# 
# glm_model
# 
# # Extract residuals from the GLM model
# residuals <- residuals(glm_model, type = "deviance")
# 
# # Create a plot of residuals
# plot(residuals, main = "Residuals of GLM Model", ylab = "Deviance Residuals", xlab = "Index")
# abline(h = 0, col = "red")
# 
# par(mfrow = c(2,2))
# plot(glm_model)
# par(mfrow = c(1,1))
# 
# 

# Maxent ------------------------------------------------------------------

# # Set JAVA_HOME environment variable
# Sys.setenv(JAVA_HOME = "C:/Privat/java/jdk-22.0.1")
# # Load rJava package
# library(rJava)
# MaxEnt()
# 
# # Check if MaxEnt is available
# if (!MaxEnt()) {
#   stop("MaxEnt setup failed. Ensure maxent.jar is in the correct directory and rJava is installed.")
# }
# 
# # Separate the predictor variables and the response variable
# predictors_train <- train %>% select(-occ)
# predictors_train
# response_train <- as.numeric(train$occ)
# 
# predictors_test <- test %>% select(-occ)
# response_test <-as.numeric(test$occ)
# 
# predictors_ind_test <- ind_test %>% select(-occ)
# response_ind_test <-as.numeric(ind_test$occ)
# 
# # Fit MaxEnt model
# me <- MaxEnt(x = predictors_train, p = response_train)
# 
# 
# # Maxent ------------------------------------------------------------------
# # # Set JAVA_HOME environment variable
# # Sys.setenv(JAVA_HOME = "C:/Privat/java/jdk-22.0.1")
# # # Load rJava package
# # library(rJava)
# # MaxEnt()
# # 
# # # Check if MaxEnt is available
# # if (!MaxEnt()) {
# #   stop("MaxEnt setup failed. Ensure maxent.jar is in the correct directory and rJava is installed.")
# # }
# # 
# # 
# # # Separate the predictor variables and the response variable
# # predictors_train <- train %>% dplyr::select(-occ)
# # response_train <- train$occ
# # #predictors_train$ar50 <- as.factor(predictors_train$ar50)
# # #predictors_train$berggrunn <- as.factor(predictors_train$berggrunn)
# # 
# # predictors_test <- test %>% dplyr::select(-occ)
# # #predictors_test$ar50 <- as.factor(predictors_test$ar50)
# # #predictors_test$berggrunn <- as.factor(predictors_test$berggrunn)
# # response_test <- test$occ
# # 
# # predictors_ind_test <- ind_test %>% dplyr::select(-occ)
# # #predictors_ind_test$ar50 <- as.factor(predictors_ind_test$ar50)
# # #predictors_ind_test$berggrunn <- as.factor(predictors_ind_test$berggrunn)
# # response_ind_test <- ind_test$occ
# # 
# # # Set Java memory allocation
# # options(java.parameters = "-Xmx4g")  # Adjust as needed
# # 
# # # Use all available cores
# # library(parallel)
# # options(mc.cores = detectCores())
# # 
# # # Adjust garbage collection
# # gc()
# # 
# # # Fit MaxEnt model
# # me <- predicts::MaxEnt(x = predictors_train, p = response_train)
# 
# 
# # Fit MaxEnt model
# # Set Java memory options
# 
# # View MaxEnt model summary
# summary(me)
# me

# # # Predict on training data
# # pred_train_me <- predict(mymodel_simple, x = predictors_train)
# # 
# # # Predict on testing data
# # pred_test_me <- predict(me, x = predictors_test)
# # 
# # pred_ind_test_me <- predict(me, x = predictors_ind_test)
# # 
# # #?roc()
# # predictions
# # glimpse(predictions)
# # 
# # glimpse(train)
# # glimpse(ind_test)
# # 
# # # Evaluate the model
# # roc_train_me <- roc(response_train, pred_train_me)
# # roc_test_me <- roc(response_test, pred_test_me)
# # roc_ind_test_me <- roc(response_ind_test, pred_ind_test_me)
# # 
# # roc_train_me <- roc(response_train, pred_train_me)
# # roc_test_me <- roc(response_test, pred_test_me)
# # roc_ind_test_me <- roc(response_ind_test, pred_ind_test_me)
# # 
# # 
# # # Print AUC for training and testing data
# # cat("AUC for Training Data:", auc(roc_train_me), "\n")
# # cat("AUC for Testing Data:", auc(roc_test_me), "\n")
# # cat("AUC for Independent Testing Data:", auc(roc_ind_test_me), "\n")
# # 
# # # # Plot ROC curves with reversed x-axis
# # # plot(roc_train_me, col = "blue", main = "ROC-kurver for MaxEnt-modell", xlab = "1 – spesifisitet", ylab = "Sensitivitet", xlim = c(1, 0))
# # # lines(roc_test_me, col = "red")
# # # lines(roc_ind_test_me, col = "forestgreen")
# # # text(0.6, 0.6, "Areal under\n kurven\n(AUC)", cex = 1.8)
# # # legend("bottomright", legend = c("Trening", "Test", "Uavhengige testdata"), col = c("blue", "red", "forestgreen"), lty = 1)
# # # 
# # # 
# # # # Plot ROC curves
# # # plot(roc_train_me, col = "blue", main = "ROC-kurver for MaxEnt-modell", xlab = "1 – spesifisitet", ylab = "Sensitivitet")
# # # lines(roc_test_me, col = "red")
# # # lines(roc_ind_test_me, col = "forestgreen")
# # # text(0.6, 0.6, "Areal under\n kurven\n(AUC)",
# # #      cex = 1.8)
# # # legend("bottomright", legend = c("Trening", "Test", "Uavhengige testdata"), col = c("blue", "red", "forestgreen"), lty = 1)
# # 
# # 
# # # ROCR --------------------------------------------------------------------
# # 
# # library(ROCR)
# # 
# # # Create ROCR prediction objects
# # pred_train_rocr <- prediction(pred_train_me, response_train)
# # pred_test_rocr <- prediction(pred_test_me, response_test)
# # pred_ind_test_rocr <- prediction(pred_ind_test_me, response_ind_test)
# # 
# # # Generate ROC performance objects
# # perf_train <- performance(pred_train_rocr, "tpr", "fpr")
# # perf_test <- performance(pred_test_rocr, "tpr", "fpr")
# # perf_ind_test <- performance(pred_ind_test_rocr, "tpr", "fpr")
# # 
# # # Set the filename and resolution for the output image
# # png("C:/Privat/spatial_trond/distribution_modelling/moselyng/plots_tables/ROC_kurver_MaxEnt_modell.png", width = 6.5, height = 7, units = "in", res = 600)
# # 
# # # Plot the ROC curves
# # plot(perf_train, col = "blue", main = "ROC-kurver for MaxEnt-modell", xlab = "Andel falske positive (1 – spesifisitet)", 
# #      ylab = "Andel ekte positive (sensitivitet)", 
# #      cex.lab = 1.2,  # Increase size of x and y axis labels
# #      cex.main = 1.4) # Increase size of the main title)
# # plot(perf_test, col = "red", add = TRUE)
# # plot(perf_ind_test, col = "forestgreen", add = TRUE)
# # 
# # # Add diagonal line representing AUC = 0.5 (Random Model)
# # abline(a = 0, b = 1, col = "gray", lty = 2)
# # 
# # # Add line for a perfect model
# # lines(c(0, 0, 1), c(0, 1, 1), col = "black", lty = 3, lwd = 2)
# # 
# # # Add legend including the perfect and random models
# # legend("bottomright", legend = c("Treningsdata (AUC = 0,94)", "Testdata (AUC = 0,89)", 
# #                                  "Uavhengige testdata (AUC = 0,86)", 
# #                                  "Tilfeldig klassifisering (AUC = 0,5)", "Perfekt modell (AUC = 1,0)"), 
# #        col = c("blue", "red", "forestgreen", "gray", "black"), 
# #        lty = c(1, 1, 1, 2, 3), lwd = c(1, 1, 1, 1, 2))
# # 
# # text(0.4, 0.6, "Areal under\n kurven\n(AUC)",
# #     cex = 1.0)
# # 
# # # Close the file device to save the image
# # dev.off()
# 
# 
# # Plot variable importance
# plot(me, xlim = c(0,40))
# me



# # Reduced maxent model ----------------------------------------------------
# 
# # Increase Java memory allocation to 16GB
# options(java.parameters = "-Xmx6g")
# 
# # Identify key variables based on variable contribution plot
# selected_vars <- c("bioclim10", "ar50") # example selection
# 
# selected_vars
# 
# # Subset the predictor data for training
# predictors_train_subset <- predictors_train[, selected_vars]
# 
# # Fit MaxEnt model with reduced variables
# me_subset <- MaxEnt(x = predictors_train_subset, p = response_train)
# 
# plot(me_subset)
# 
# # Subset the rasters as well
# rasters_subset <- rasters[[selected_vars]]
# 
# # Predict to raster data
# maxent_predict <- predict(me, x = rasters)
# plot(maxent_predict, main = "Predicted Suitability")
# 
# # Predict to raster data
# 
# options(java.parameters = "-Xmx8g")
# 
# # Record the start time
# start_time <- Sys.time()
# 
# # Get predicted values from the model
# maxent_predict_raster_subset <- predict(me_subset, x = rasters)
# 
# # Record the end time
# end_time <- Sys.time()
# 
# # Calculate the elapsed time
# elapsed_time <- end_time - start_time
# 
# # Extract the hours, minutes, and seconds from the elapsed time
# elapsed_seconds <- as.numeric(elapsed_time, units = "secs")
# hours <- floor(elapsed_seconds / 3600)
# minutes <- floor((elapsed_seconds %% 3600) / 60)
# seconds <- round(elapsed_seconds %% 60, 2)
# # Print the time taken in hours, minutes, and seconds
# cat(sprintf("Time taken: %02d:%02d:%05.2f (hours:minutes:seconds)\n", hours, minutes, seconds))
# 
# # Use testing data for model evaluation
# maxent_eval <- pa_evaluate(p = test[test$occ == 1, ],
#                            a = test[test$occ == 0, ],
#                            model = me_subset,
#                            type = "response")
# 
# par(mfrow = c(2,2))
# plot(maxent_eval, "ROC")
# plot(maxent_eval, "TPR")
# plot(maxent_eval, "boxplot")
# plot(maxent_eval, "density")
# par(mfrow = c(1,1))
# 
# # View MaxEnt model summary
# summary(me_subset)
# 
# # Predict on training data
# pred_train_me_subset <- predict(me_subset, x = predictors_train)
# predictors_train_subset
# predictors_train
# 
# # Predict to raster data
# #maxent_predict8 <- predict(me, x = rasters)
# maxent_predict2 <- predict(me_subset, x = rasters)
# par(mfrow = c(1,2))
# plot(maxent_predict8, main = "Predicted Suitability")
# plot(maxent_predict2, main = "Predicted Suitability")
# par(mfrow = c(1,1))
# 
# 
# # Predict on testing data
# # pred_test_me_subset <- predict(me_subset, x = predictors_test)
# # 
# # pred_ind_test_me_subset <- predict(me_subset, x = predictors_ind_test)
# # pred_ind_test_me_subset
# # 
# # ind_test
# # ind_test_abs_pts
# # ind_test_occ_pts
# 
# # Evaluate the model
# roc_train_me_subset <- roc(response_train, pred_train_me_subset)
# roc_test_me_subset <- roc(response_test, pred_test_me_subset)
# roc_ind_test_me_subset <- roc(response_ind_test, pred_ind_test_me_subset)
# 
# 
# # Print AUC for training and testing data
# cat("AUC for Training Data:", auc(roc_train_me_subset), "\n")
# cat("AUC for Testing Data:", auc(roc_test_me_subset), "\n")
# cat("AUC for Independent Testing Data:", auc(roc_ind_test_me_subset), "\n")
# 
# # Plot ROC curves
# plot(roc_train_me_subset, col = "blue", main = "ROC-kurver for MaxEnt-modell", xlab = "1 – spesifisitet", ylab = "Sensitivitet")
# lines(roc_test_me_subset, col = "red")
# lines(roc_ind_test_me_subset, col = "forestgreen")
# text(0.6, 0.6, "Areal under\n kurven\n(AUC)",
#      cex = 1.8)
# legend("bottomright", legend = c("Train", "Test", "Independent test"), col = c("blue", "red", "forestgreen"), lty = 1)
# 
# # Plot variable importance
# plot(me_subset, xlim = c(0,70))
# #me_subset
# 
# maxent_predict2 <- predict(me_subset, x = rasters)
# 
# # Determine minimum threshold for "presence"
# maxent_threshold <- maxent_eval@thresholds$max_spec_sens
# 
# # Determine minimum thresholds for "presence"
# maxent_threshold <- maxent_eval@thresholds$max_spec_sens
# maxent_threshold
# maxent_threshold_no_omission <- maxent_eval@thresholds$no_omission
# maxent_threshold_no_omission
# maxent_threshold_prev <- maxent_eval@thresholds$equal_prevalence
# maxent_threshold_prev
# maxent_threshold_kappa <- maxent_eval@thresholds$max_kappa
# maxent_threshold_kappa
# maxent_threshold_equal <- maxent_eval@thresholds$equal_sens_spec
# maxent_threshold_equal
# 
# # # Example: Plotting the results with one of the thresholds
# # # Load the Maxent prediction raster
# #maxent_predict_raster <- predict(me_subset, newdata = rasters)  # Replace with your Maxent prediction raster
# 
# # Use one of the thresholds for presence-absence classification
# threshold_raster <- maxent_predict2 > 0.55
# plot(threshold_raster)
# 
# # Convert to a categorical raster: 1 for presence and NA for absence
# values(threshold_raster) <- ifelse(values(threshold_raster), 1, NA)
# 
# # Define levels for the categorical raster
# levels(threshold_raster) <- data.frame(value = 1, presence = "prevalence")
# 
# 
# Plot predictions on maps ------------------------------------------------

# Plot the maps with the white polygon and graticules
map1 <- tm_shape(prediction_map) +
  tm_raster(palette = "-RdYlBu", midpoint = 1, style = "cont", alpha = 0.7,title = "Relativ sannsynlighet\n for forekomst",  legend.show = TRUE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  # tm_shape(train_abs_pts) +
  # tm_dots(col = "grey20", legend.show = FALSE) +
  # tm_shape(train_occ_pts) +
  # tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
  # tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "a) Utbredelsesmodell, moselyng", title.bg.color = "white", legend.bg.color = "white",
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map1

# Plot the maps with the white polygon and graticules
map2 <- tm_shape(prediction_map) +
  tm_raster(palette = "-RdYlBu", midpoint = 1, style = "cont", alpha = 0.7,title = "Relativ sannsynlighet\n for forekomst",  legend.show = TRUE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  # tm_shape(train_abs_pts) +
  # tm_dots(col = "grey20", legend.show = FALSE) +
  # tm_shape(train_occ_pts) +
  # tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
  # tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "a) Utbredelsesmodell, moselyng", title.bg.color = "white", legend.bg.color = "white",
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map2


# # Define the Arealdekke layer (assuming it is already loaded as 'arealdekke')
# threshold_raster <- prediction_threshold_raster
# 
# # Define the categories and corresponding colors
# # categories <- c(10, 20, 30, 50, 60, 99)
# labels <- c("Fravær", "Forekomst")
# # colors <- c("#e31a1c", "#ffd16e", "#00ad3b", "#cfcc91", "#d1d1ff", "#fafafa")
# # colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
# colors <- c("#fafafa", "red")
# colors <- c("#fafafa", "olivedrab")
# 
# 
# legend_col <- c("olivedrab")
# legend_values <- c(1)
# legend_labels3 <- c("Forekomst")
# 
# threshold_raster <- prediction_map > 2
# 
# map2 <- tm_shape(threshold_raster) +
#   tm_raster(style = "cat", palette = colors, labels = labels,
#             title = "Sannsynlig utbredelse",  legend.show = TRUE) +
#   #tm_layout(title = "Moselyng", legend.outside = FALSE) +
#   tm_shape(my_map) +
#   tm_borders() +
#   tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
#   # tm_shape(train_occ_pts) +
#   # tm_dots(col = "red", size = 0.01, legend.show = FALSE) +
#   # tm_shape(test_occ_pts) +
#   # tm_dots(col = "red", size = 0.01) +
#   # tm_shape(ind_test_occ_pts) +
#   # tm_dots(col = "red", size = 0.01) +
#   # tm_add_legend(type = "symbol", labels = legend_labels3, col = legend_col) +
#   tm_shape(rect_sf_transformed) +
#   tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
#   tm_layout(title = "b) Avgrensning av interesseområde", title.bg.color = "white", legend.bg.color = "white",
#             legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
#             inner.margins = c(-0.05, -0.05, -0.05, -0.05))
# 
# map2
# 
# # Adjust text sizes
# map1 <- map1 + tm_layout(title.size = 1.3, legend.text.size = 1)
# map2 <- map2 + tm_layout(title.size = 1.3, legend.text.size = 1)
# 
# # Arrange the maps together
# combined_maps_predictions <- tmap_arrange(map1, map2, ncol = 2)
# combined_maps_predictions
# 
# # Define the output path
# output_path <- "C:/Privat/spatial_trond/distribution_modelling/moselyng/plots_tables/prediction_maps_combined.png"
# 
# # Save the combined maps
# tmap_save(combined_maps_predictions, filename = output_path, width = 38, height = 16, units = "cm", dpi = 600, bg = "white")
# 
# 
# 
# # # -------------------------------------------------------------------------
# # 
# # 
# # 
# 
# # # Plot base map
# plot(my_map,
#      axes = TRUE,
#      col = "grey95")
# 
# 
# # Only plot areas where probability of occurrence is greater than the threshold
# plot(prediction_map, 
#      add = FALSE, 
#      legend = FALSE, 
#      col = c(NA, "olivedrab")) # only color for second element ("present")
# 
# #And add those observations
# points(x = obs_data$longitude,
#        y = obs_data$latitude,
#        col = "black",
#        pch = "+",
#        cex = 0.75)
# 
# 
# #save.image(file = "C:/Privat/spatial_trond/distribution_modelling/moselyng/modelling_environment_23_may_24.RData")
# #glm_threshold_raster <- glm_predict > glm_threshold_kappa
# prediction_threshold_raster <- prediction_map > 2
# plot(prediction_threshold_raster)
# 
# # Convert to a categorical raster: 1 for presence and NA for absence
# values(prediction_threshold_raster) <- ifelse(values(prediction_threshold_raster), 1, NA)
# 
# # Define levels for the categorical raster
# levels(prediction_threshold_raster) <- data.frame(value = 1, presence = "presence")
# 
# # Create custom legends
# legend_labels <- c("Forekomst")
# legend_col <- c("red")
# 
# # Define the palette
# threshold_palette <- c("olivedrab")
# #threshold_palette <- c("black")
# 
# #threshold_palette <- c("black")
# 
# 
# # Plot with tmap
# map2 <- tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.1, legend.show = FALSE) +
#   tm_shape(prediction_threshold_raster) + 
#   tm_raster(palette = threshold_palette, legend.show = FALSE) +
#   tm_shape(my_map) +
#   tm_borders(lwd = 0.01, col = "grey30") +
#   tm_shape(pts1) +
#   tm_dots(col = "red", size = 0.05, legend.show = FALSE) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "maxent", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)
# 
# map2
# 
# 
# tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.1, legend.show = FALSE) +
#   tm_shape(prediction_map) + 
#   tm_raster(palette = "-RdYlBu", midpoint = 1, style = "cont") +
#   # tm_shape(my_map) +
#   # tm_borders(lwd = 0.01, col = "grey30") +
#   # tm_shape(all_occs_pts) +
#   # tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   #tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "maxent", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)
# 
# 
# # Plot with tmap
# map1 <- tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.1, legend.show = FALSE) +
#   tm_shape(prediction_threshold_raster) + 
#   tm_raster(palette = "-RdYlBu", midpoint = 3, style = "cont") +
#   # tm_shape(my_map) +
#   # tm_borders(lwd = 0.01, col = "grey30") +
#   # tm_shape(all_occs_pts) +
#   # tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   #tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "maxent", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)
# 
# map1
# 
# # Plot with tmap
# map1 <- tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.1, legend.show = FALSE) +
#   tm_shape(prediction_threshold_raster) + 
#   tm_raster(palette = "inferno", midpoint = NA, style = "cont") +
#   # tm_shape(my_map) +
#   # tm_borders(lwd = 0.01, col = "grey30") +
#   # tm_shape(all_occs_pts) +
#   # tm_dots(col = "red", size = 0.1, legend.show = FALSE) +
#   #tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "maxent", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)
# 
# map1
# 
# tmap_arrange(map1, map2, ncol = 2)
# 
# # Maxent model one variable ----------------------------------------------------
# 
# # Increase Java memory allocation to 16GB
# options(java.parameters = "-Xmx6g")
# 
# selected_vars <- c("ar50") # example selection
# selected_vars
# 
# 
# # Subset the predictor data for training, ensuring it remains a data frame
# predictors_train_one_var <- predictors_train[, selected_vars, drop = FALSE]
# 
# # Fit MaxEnt model with reduced variables
# me_one_var <- MaxEnt(x = predictors_train_one_var, p = response_train)
# 
# # Proceed with the rest of your code...
# 
# 
# plot(me_one_var)
# 
# # Subset the rasters as well
# rasters_subset <- rasters[[selected_vars]]
# 
# # Predict to raster data
# # maxent_predict <- predict(me, x = rasters)
# # plot(maxent_predict, main = "Predicted Suitability")
# 
# # Predict to raster data
# 
# options(java.parameters = "-Xmx8g")
# 
# # Record the start time
# start_time <- Sys.time()
# 
# # Get predicted values from the model
# prediction_threshold_raster_subset <- predict(me_one_var, x = rasters)
# 
# # Record the end time
# end_time <- Sys.time()
# 
# # Calculate the elapsed time
# elapsed_time <- end_time - start_time
# 
# # Extract the hours, minutes, and seconds from the elapsed time
# elapsed_seconds <- as.numeric(elapsed_time, units = "secs")
# hours <- floor(elapsed_seconds / 3600)
# minutes <- floor((elapsed_seconds %% 3600) / 60)
# seconds <- round(elapsed_seconds %% 60, 2)
# # Print the time taken in hours, minutes, and seconds
# cat(sprintf("Time taken: %02d:%02d:%05.2f (hours:minutes:seconds)\n", hours, minutes, seconds))
# 
# # Use testing data for model evaluation
# maxent_eval <- pa_evaluate(p = test[test$occ == 1, ],
#                            a = test[test$occ == 0, ],
#                            model = me_one_var,
#                            type = "response")
# 
# par(mfrow = c(2,2))
# plot(maxent_eval, "ROC")
# plot(maxent_eval, "TPR")
# plot(maxent_eval, "boxplot")
# plot(maxent_eval, "density")
# par(mfrow = c(1,1))
# 
# # View MaxEnt model summary
# summary(me_one_var)
# 
# # Predict on training data
# pred_train_me_one_var <- predict(me_one_var, x = predictors_train)
# 
# # Predict on testing data
# pred_test_me_one_var <- predict(me_one_var, x = predictors_test)
# 
# pred_ind_test_me_one_var <- predict(me_one_var, x = predictors_ind_test)
# 
# 
# # Evaluate the model
# roc_train_me_one_var <- roc(response_train, pred_train_me_one_var)
# roc_test_me_one_var <- roc(response_test, pred_test_me_one_var)
# roc_ind_test_me_one_var <- roc(response_ind_test, pred_ind_test_me_one_var)
# 
# 
# # Print AUC for training and testing data
# cat("AUC for Training Data:", auc(roc_train_me_one_var), "\n")
# cat("AUC for Testing Data:", auc(roc_test_me_one_var), "\n")
# cat("AUC for Independent Testing Data:", auc(roc_ind_test_me_one_var), "\n")
# 
# # Plot ROC curves
# plot(roc_train_me_one_var, col = "blue", main = "ROC-kurver for MaxEnt-modell", xlab = "1 – spesifisitet", ylab = "Sensitivitet")
# lines(roc_test_me_one_var, col = "red")
# lines(roc_ind_test_me_one_var, col = "forestgreen")
# text(0.6, 0.6, "Areal under\n kurven\n(AUC)",
#      cex = 1.8)
# legend("bottomright", legend = c("Train", "Test", "Independent test"), col = c("blue", "red", "forestgreen"), lty = 1)
# 
# # Plot variable importance
# plot(me_one_var, xlim = c(0,30))
# 
# 
# 
# # Random forest -----------------------------------------------------------
# 
# # loading the libraries
# library(randomForest)
# 
# rf_model <- randomForest(occ ~ ., data=train, importance=TRUE,
#                         proximity=TRUE)
# print(rf_model)
# ## Look at variable importance:
# round(importance(rf_model), 2)
# 
# # Variable importance:
# importance(rf_model,type=1)
# varImpPlot(rf_model)
# 
# # Look at single trees:
# head(getTree(rf_model,1,T))
# 
# #Predict on training and testing data using Random Forest
# pred_train_rf <- predict(rf_model, newdata = predictors_train)
# pred_test_rf <- predict(rf_model, newdata = predictors_test)
# pred_ind_test_rf <- predict(rf_model, newdata = predictors_ind_test)
# 
# 
# # Evaluate the Random Forest model
# roc_train_rf <- roc(response_train, pred_train_rf)
# roc_test_rf <- roc(response_test, pred_test_rf)
# roc_ind_test_rf <- roc(response_ind_test, pred_ind_test_rf)
# 
# 
# # Print AUC for Random Forest model
# cat("AUC for Random Forest Training Data:", auc(roc_train_rf), "\n")
# cat("AUC for Random Forest Testing Data:", auc(roc_test_rf), "\n")
# cat("AUC for Random Forest Independent Testing Data:", auc(roc_ind_test_rf), "\n")
# 
# 
# # Plot ROC curves for Random Forest model
# plot(roc_train_rf, col = "blue", main = "ROC Curves for Random Forest Model")
# lines(roc_test_rf, col = "red")
# lines(roc_ind_test_rf, col = "forestgreen")
# legend("bottomright", legend = c("Train", "Test", "Independent test"), col = c("blue", "red", "forestgreen"), lty = 1)
# 
# rasters_subset <- rasters
# rasters_subset[is.na(rasters_subset)] <- -9999  # Replace NAs with a value not present in the data
# 
# # Predict on the raster stack
# rf_predict_raster <- predict(rasters_subset, model = rf_model, na.rm = TRUE)
# 
# # Plot predicted values - single map
# plot(rf_predict_raster, main = "Predicted Suitability (Random Forest)")
# 
# # Use testing data for model evaluation
# rf_eval <- pa_evaluate(p = test[test$occ == 1, ],
#                        a = test[test$occ == 0, ],
#                        model = rf_model)
# 
# 
# par(mfrow = c(2,2))
# plot(rf_eval, "ROC")
# plot(rf_eval, "TPR")
# plot(rf_eval, "boxplot")
# plot(rf_eval, "density")
# par(mfrow = c(1,1))
# 
# 
# # Determine minimum threshold for "presence"
# rf_threshold <- rf_eval@thresholds$max_spec_sens
# rf_threshold
# 
# # Plot the results
# # Plot base map
# 
# # Only plot areas where probability of occurrence is greater than the threshold
# plot(rf_predict_raster > rf_threshold, legend = FALSE, col = c(NA, "olivedrab"))
# 
# # Load the thresholded raster
# threshold_raster <- rf_predict_raster > rf_threshold
# plot(threshold_raster)
# 
# # Load the thresholded raster
# threshold_raster <- rf_predict_raster > rf_threshold
# 
# # Convert to a categorical raster: 1 for presence and NA for absence
# values(threshold_raster) <- ifelse(values(threshold_raster), 1, NA)
# 
# # Define levels for the categorical raster
# levels(threshold_raster) <- data.frame(value = 1, presence = "presence")
# 
# # Create custom legends
# legend_labels <- c("Forekomst")
# legend_col <- c("red")
# 
# # Define the palette
# threshold_palette <- c("olivedrab")
# #threshold_palette <- c("black")
# 
# 
# # Plot with tmap
# tm_shape(rasters$bioclim10) + 
#   tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.1, legend.show = FALSE) +
#   tm_shape(threshold_raster) + 
#   tm_raster(palette = threshold_palette, legend.show = FALSE) +
#   tm_shape(my_map) +
#   tm_borders(lwd = 0.01, col = "grey30") +
#   tm_shape(all_occs_pts) +
#   tm_dots(col = "red", size = 0.05, legend.show = FALSE) +
#   tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
#   tm_layout(title = "Random forest", legend.position = c("left", "top"), 
#             legend.frame = FALSE, frame = FALSE)
# 
# 
# # Now, we plot the response surface:
# xyz$z <- predict(rf_model, xyz, type='response')
# wireframe(z ~ bioclim3 + bioclim10, data = xyz, zlab = list("Occurrence prob.", rot=90), 
#           drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), 
#           zlim = c(0, 1), main='Random Forest', xlab='bio11', ylab='bio10', 
#           screen=list(z = -120, x = -70, y = 3))
# 
# 
# # Compare Maps ------------------------------------------------------------
# 
# par(mfrow = c(1, 4))
# 
# # Plot GLM results
# #plot(my_map, axes = TRUE, col = "grey95")
# plot(glm_predict > glm_threshold, add = TRUE, legend = FALSE, col = c(NA, "olivedrab"))
# points(x = obs_data$longitude, y = obs_data$latitude, col = "black", pch = "+", cex = 0.75)
# plot(my_map, add = TRUE, border = "grey5")
# 
# # Plot MaxEnt results
# plot(maxent_predict > maxent_threshold, add = TRUE, legend = FALSE, col = c(NA, "olivedrab"))
# points(x = obs_data$longitude, y = obs_data$latitude, col = "black", pch = "+", cex = 0.75)
# plot(my_map, add = TRUE, border = "grey5")
# 
# # Plot Random Forest results
# plot(rf_predict_raster > rf_threshold, add = TRUE, legend = FALSE, col = c(NA, "olivedrab"))
# points(x = obs_data$longitude, y = obs_data$latitude, col = "black", pch = "+", cex = 0.75)
# plot(my_map, add = TRUE, border = "grey5")
# 
# # Plot Envelope Model results
# plot(env_predict > env_threshold, add = TRUE, legend = FALSE, col = c(NA, "olivedrab"))
# points(x = obs_data$longitude, y = obs_data$latitude, col = "black", pch = "+", cex = 0.75)
# plot(my_map, add = TRUE, border = "grey5")
# 
# 
# 
# 
# # Forecast climate data ---------------------------------------------------
# 
# # Download predicted climate data
# forecast_data <- cmip6_world(model = "MPI-ESM1-2-HR",
#                              ssp = "245",
#                              time = "2061-2080",
#                              var = "bioc",
#                              res = 2.5,
#                              path = "data")
# 
# # Use names from bioclim_data
# names(forecast_data) <- names(bioclim_data)
# 
# # Predict presence from model with forecast data
# forecast_presence <- predict(forecast_data, glm_model, type = "response")
# 
# # Plot base map
# plot(my_map, 
#      axes = TRUE, 
#      col = "grey95")
# 
# # Add model probabilities
# plot(forecast_presence, add = TRUE)
# 
# # Redraw those country borders
# plot(my_map, add = TRUE, border = "grey5")
# 
# # Add original observations
# points(x = obs_data$longitude, 
#        y = obs_data$latitude, 
#        col = "black", 
#        pch = "+", 
#        cex = 0.75)
# 
# # Plot base map
# plot(my_map, 
#      axes = TRUE, 
#      col = "grey95")
# 
# # Only plot areas where probability of occurrence is greater than the threshold
# plot(forecast_presence > glm_threshold, 
#      add = TRUE, 
#      legend = FALSE, 
#      col = c(NA, "olivedrab"))
# 
# # And add those observations
# points(x = obs_data$longitude, 
#        y = obs_data$latitude, 
#        col = "black",
#        pch = "+", 
#        cex = 0.6)
# 
# # Redraw those country borders
# plot(my_map, add = TRUE, border = "grey5")
# 

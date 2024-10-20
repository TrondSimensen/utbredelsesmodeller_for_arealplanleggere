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
#options(scipen = 999)

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
colors <- c("#e31a1c", "#FFBF00", "#33a02c", "#cfcc91", "#d1d1ff", "#fafafa")


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
#train3 <- train |> select(occ, ar50)


#rv_train <- train3 |> rename("RV" = "occ")
rv_train <- train |> rename("RV" = "occ")

rv_train

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

targetEVselect <- selectEV(targetDVselect$dvdata, alpha = 0.001, 
                              interaction = FALSE, quiet = FALSE)


summary(targetDVselect$dvdata)
targetEVselect$selectedmodel$formula
best_models <- targetEVselect$selection[!duplicated(targetEVselect$selection$round), ]
best_models$P <- formatC(as.numeric(best_models$P), format = "f", digits = 4)
best_models

targetEVselect
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
                        formula("~ bioclim10 + bioclim3"),algorithm = "maxent")
# three predictors
mymodel3 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim3 + precip6"),algorithm = "maxent")

# four predictors
mymodel4 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim3 + precip6 + ar50"),algorithm = "maxent")

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
                         formula("~ bioclim10 + bioclim3 + precip6 + ar50 + bioclim7 + sca7 + bioclim8 + tri"),algorithm = "maxent")

# simple model predictors
mymodel_simple <- chooseModel(targetDVselect$dvdata, 
                              formula("~ bioclim10 + bioclim3 + ar50"),algorithm = "maxent")

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
formula(mymodel_simple)
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

testAUC(model = mymodel_simple, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)

# two predictors

par(mfrow = c(3,3))
testAUC(model = mymodel1, transformations = DVs$transformations,
        data = pa_evaluation)

mymodel_bio10_bio3 <- chooseModel(targetDVselect$dvdata, 
                        formula("~ bioclim10 + bioclim3"),algorithm = "maxent")

testAUC(model = mymodel_bio10_bio3, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)


mymodel_bio10_ar50 <- chooseModel(targetDVselect$dvdata, 
                                  formula("~ bioclim10 + ar50"),algorithm = "maxent")

testAUC(model = mymodel_bio10_ar50, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)

mymodel_bio10_bio3_ar50 <- chooseModel(targetDVselect$dvdata, 
                                  formula("~ bioclim10 + bioclim3 + ar50"),algorithm = "maxent")

testAUC(model = mymodel_bio10_bio3_ar50, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)

round(0.8049667, 2)

#?MIAmaxent::projectModel()

testAUC(model = mymodel_ar, transformations = DVs$transformations,
        data = pa_evaluation, add = TRUE)




##############################
##############################
#####                      ###
#####       Spatial        ###
#####     predictions      ###
#####                      ###
##############################
##############################

# Spatial prediction

par(mfrow = c(1,1))

model <- mymodel_simple
library(raster)
# Converting SpatRaster to RasterStack
rstack <- raster::stack(rasters)
rstack 
names(rstack)


# Spatial prediction
# Run the three following lines to time the operation
ptm <- proc.time() ## start of clock and then to end
predictions <- projectModel(model= mymodel_simple, transformations= DVs$transformations, data=rstack, rescale = TRUE) # 10 min
proc.time()-ptm ## end of clock. (10-30 min)
#save(predictions, file = "moselyng_spatial_pred.rda")
par(mfrow = c(1,2))
plot(predictions$output)
#writeRaster(predictions$output, "predictions_moselyng.tif", overwrite = T)
prediction_map <- rast(predictions$output)
plot(prediction_map)

predictions3 <- projectModel(model= mymodel_simple, transformations= DVs$transformations, data=ind_test, rescale = TRUE) # 10 min
predictions3_df <- predictions3$output
predictions3_df$occ <- as.factor(predictions3_df$occ)
glimpse(predictions3_df)
boxplot(predictions3_df$PRO~predictions3_df$occ)

predictions8 <- projectModel(model= mymodel8, transformations= DVs$transformations, data=ind_test, rescale = TRUE) # 10 min
plot(predictions8)
prediction_map8 <- projectModel(model= mymodel8, transformations= DVs$transformations, data=rstack, rescale = TRUE) # 10 min
plot(prediction_map8$output)
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
roc_obj3 <- roc(response = predictions3_df$occ, predictor = predictions3_df$PRO)
roc_obj3

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

par(mfrow = c(1,1))

# Create a prediction object for roc_obj8
pred_8 <- prediction(predictions8_df$PRO, predictions8_df$occ)

# Create performance object
perf_8 <- performance(pred_8, "tpr", "fpr")

# Plot ROC curve
plot(perf_8, main = "ROC Curve for Dataset 8")

# Create a prediction object for roc_obj2
pred_3 <- prediction(predictions2_df$PRO, predictions2_df$occ)

# Create performance object
perf_3 <- performance(pred_3, "tpr", "fpr")

# Plot ROC curve
plot(perf_3, main = "ROC Curve for Dataset 2")

# Set the filename and resolution for the output image
png("C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/ROC_kurver_MaxEnt_modell.png", width = 6.5, height = 7, units = "in", res = 600)

# Plot the ROC curves
plot(perf_3, col = "blue", main = "ROC-kurver for utbredelsesmodell", xlab = "Andel falske positive (1 – spesifisitet)", 
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
                                 "Kompleks modell (AUC = 0,80)", 
                                 "Tilfeldig klassifisering (AUC = 0,5)", "Perfekt modell (AUC = 1,0)"), 
       col = c("blue", "red", "gray", "black"), 
       lty = c(1, 1, 1, 2, 3), lwd = c(1, 1, 1, 1, 2))

text(0.4, 0.6, "Areal under\n kurven\n(AUC)",
     cex = 1.0)

# Close the file device to save the image
dev.off()


# Plot predictions on maps ------------------------------------------------
par(mfrow = c(1,1))

# Plot the maps with the white polygon and graticules
map1 <- tm_shape(prediction_map) +
  tm_raster(palette = "-RdYlBu", midpoint = 1, style = "cont", alpha = 1,title = "Relativ sannsynlighet\n for forekomst",  legend.show = TRUE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  # tm_shape(train_abs_pts) +
  # tm_dots(col = "grey20", legend.show = FALSE) +
  # tm_shape(train_occ_pts) +
  # tm_dots(col = "black", size = 0.07, legend.show = FALSE) +
  # tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "a) Utbredelsesmodell, moselyng", title.bg.color = "white", legend.bg.color = "white",
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map1
prediction_threshold <- 
threshold_raster <- prediction_threshold_raster


threshold_raster <- prediction_map > 2
plot(threshold_raster)
 
# Convert to a categorical raster: 1 for presence and NA for absence
values(threshold_raster) <- ifelse(values(threshold_raster), 1, NA)
# 
# Define levels for the categorical raster
levels(threshold_raster) <- data.frame(value = 1, presence = "Forekomst")
# 
# Create custom legends
legend_labels <- c("Forekomst")
legend_col <- c("red")
# 
# Define the palette
threshold_palette <- c("olivedrab")
# #threshold_palette <- c("black")
# 
# #threshold_palette <- c("black")
# 
# 
# Plot with tmap
map2 <- tm_shape(rasters$bioclim10) +
  tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.1, legend.show = FALSE) +
  tm_shape(threshold_raster) +
  tm_raster(palette = threshold_palette, legend.show = FALSE) +
  tm_shape(my_map) +
  tm_borders(lwd = 0.01, col = "grey30") +
  tm_shape(pts1) +
  tm_dots(col = "red", size = 0.05, legend.show = FALSE) +
  tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
  tm_layout(title = "maxent", legend.position = c("left", "top"),
            legend.frame = FALSE, frame = FALSE)

map2

# 
# # Define the categories and corresponding colors
# # categories <- c(10, 20, 30, 50, 60, 99)
labels <- c("Forekomst mindre sannsynlig", "Sannsynlig forekomst")
# # colors <- c("#e31a1c", "#ffd16e", "#00ad3b", "#cfcc91", "#d1d1ff", "#fafafa")
# # colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
# colors <- c("#fafafa", "red")
colors <- c("#fafafa", "olivedrab")
# 
# 
legend_col <- c("olivedrab")
legend_values <- c(1)
legend_labels3 <- c("Forekomst")

threshold_raster <- prediction_map > 2

map2 <- tm_shape(threshold_raster) +
  tm_raster(style = "cat", palette = colors, labels = labels,
            title = "Sannsynlig utbredelse",  legend.show = TRUE) +
  #tm_layout(title = "Moselyng", legend.outside = FALSE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_graticules(x = c(10, 20), y = c(60, 65, 70), projection = 4326) +
  # tm_shape(train_occ_pts) +
  # tm_dots(col = "red", size = 0.01, legend.show = FALSE) +
  # tm_shape(test_occ_pts) +
  # tm_dots(col = "red", size = 0.01) +
  # tm_shape(ind_test_occ_pts) +
  # tm_dots(col = "red", size = 0.01) +
  # tm_add_legend(type = "symbol", labels = legend_labels3, col = legend_col) +
  tm_shape(rect_sf_transformed) +
  tm_polygons(col = "white", border.alpha = 0) + # Add the white polygon
  tm_layout(title = "b) Interesseområde", title.bg.color = "white", legend.bg.color = "white",
            legend.position = c("left", "top"), legend.frame = FALSE, frame = TRUE,
            inner.margins = c(-0.05, -0.05, -0.05, -0.05))

map2

# Adjust text sizes
map1 <- map1 + tm_layout(title.size = 1.3, legend.text.size = 1)
map2 <- map2 + tm_layout(title.size = 1.3, legend.text.size = 1)

# Arrange the maps together
combined_maps_predictions <- tmap_arrange(map1, map2, ncol = 2)
combined_maps_predictions

# Define the output path
output_path <- "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/prediction_maps_combined.png"

# Save the combined maps
tmap_save(combined_maps_predictions, filename = output_path, width = 38, height = 16, units = "cm", dpi = 600, bg = "white")

#

# FOP-plots ---------------------------------------------------------------


# Open a PNG device for high-resolution plots
png(filename = "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/high_res_plots2.png", width = 18 * 300, height = 20 * 300 * 2/3, res = 300)

# Adjust margins
par(mfrow=c(3,2), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(5,5,5,5))
#par(mfrow=c(3,2))


# Open a PNG device for high-resolution plots
png(filename = "C:/Privat/spatial_trond/distribution_modelling/moselyng/plots_tables/high_res_plots2.png",
    width = 18 * 300, height = 20 * 300 * 2/3, res = 300)

# Adjust margins and layout
# Increase the left outer margin for label space (second number in oma)
# Increase the bottom margin (first number in mar) for horizontal space between plots
par(mfrow=c(3,2), mar=c(6, 4.1, 4.1, 2.1), oma=c(6,6,2,2))

# Your plotting code...

# Norwegian labels for 'ar50artype1'
ar50artype1_labels <- c("10"="10 Bebygd", "20"="20 Jordbruk", "30"="30 Skog", "50"="50 Snaumark", 
                        "60"="60 Myr", "70"="70 Bre", "81"="81 Ferskvann", "82"="82 Hav")

# Replace levels with Norwegian labels
#pa_train$ar50artype1 <- factor(pa_train$ar50artype1, labels = ar50artype1_labels)

# Plot with Norwegian axis titles and custom right y-axis
plotFOPnorsk(pa_train, "bioclim10", main="", xlab="Bioklimatisk variabel 10", ylab="Frekvens av forekomst", cex.axis=1, cex.lab=1.5)
mtext("a)", side=3, line=0.5, at=0.5, cex=1.2)
# Custom right axis text for the first plot
#mtext("Kjerneestimert tetthet", side=4, line=3, cex=1)

plotFOPnorsk(pa_train, "ar50artype1", main="", xlab="Arealdekke AR50", ylab="Frekvens av forekomst", cex.axis=1, cex.lab=1.5)
mtext("b)", side=3, line=0.5, at=0.5, cex=1.2)
# Custom right axis text for the second plot
#mtext("Antall observasjoner", side=4, line=3, cex=1)

plotRespNorsk(mymodel_simple, DVs$transformations, "bioclim10", main="", xlab="Bioklimatisk variabel 10", ylab="Enkelteffektsvar", cex.axis=1, cex.lab=1.5)
mtext("c)", side=3, line=0.5, at=0.5, cex=1.2)

plotRespNorsk(mymodel_simple, DVs$transformations, "ar50artype1", main="", xlab="Arealdekke AR50", ylab="Marginal effektsvar", cex.axis=1, cex.lab=1.5)
mtext("d)", side=3, line=0.5, at=0.5, cex=1.2)

# Reset to default single plot layout and close the PNG device
par(mfrow=c(1,1))
dev.off()

# Open a PNG device for high-resolution plots
png(filename = "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/high_res_plots2.png",
    width = 10 * 300, height = 16 * 300 * 2/3, res = 300)

# Adjust margins and layout
# Increase the left outer margin for label space (second number in oma)
# Increase the bottom margin (first number in mar) for horizontal space between plots
par(mfrow=c(3,2), mar=c(6, 4.1, 4.1, 2.1), oma=c(6,6,2,2))


plotFOPnorsk(fop_train, "bioclim10", xlab = "Gjennomsnittstempetratur, varmeste kvartal")
mtext("a) Data: Gjennomsnittstempetratur i varmeste kvartal", side=3, line=0.5, at=0.5, cex=1.2)

plotFOPnorsk(fop_train2, "ar50", xlab = " ")
mtext("b) Data: Arealtyper", side=3, line=0.5, at=0.5, cex=1.2)

plotRespNorsk(mymodel_simple, DVs$transformations, "bioclim10", main="", xlab="Bioklimatisk variabel 10", ylab="Relativ sannsynlighet for forekomst", cex.axis=1, cex.lab=1.5)
mtext("c) Modell: Gjennomsnittstempetratur i varmeste kvartal", side=3, line=0.5, at=0.5, cex=1.2)

plotRespNorsk(mymodel_simple, DVs$transformations, "ar50", main="", xlab="Arealtype AR50", ylab="Relativ sannsynlighet for forekomst", cex.axis=1, cex.lab=1.5)
mtext("d) Modell: arealtyper", side=3, line=0.5, at=0.5, cex=1.2)

plotFOPnorsk(fop_train, "aspect", xlab = "Helningsretning")
mtext("e) Data: Helningsretning", side=3, line=0.5, at=0.5, cex=1.2)

plotFOPnorsk(fop_train2, "berggrunn", xlab = " ")
mtext("f) Data: kalkinnhold i berggrunn", side=3, line=0.5, at=0.5, cex=1.2)


# Reset to default single plot layout and close the PNG device
par(mfrow=c(1,1))
dev.off()

# Open a PNG device for high-resolution plots
png(filename = "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/high_res_plots2.png",
    width = 10 * 300, height = 20 * 300 * 2/3, res = 300)

# Adjust margins and layout
# Increase the left outer margin for label space (second number in oma)
# Increase the bottom margin (first number in mar) for horizontal space between plots
par(mfrow=c(3,2), mar=c(6, 5, 4, 2), oma=c(4, 4, 2, 2))

# Update cex.lab for larger x and y axis labels, and adjust vertical spacing with mar
plotFOPnorsk(fop_train, "bioclim10", xlab = "Gjennomsnittstempetratur, varmeste kvartal", cex.lab=1.6)
mtext("a) Data: Gjennomsnittstempetratur i varmeste kvartal", side=3, line=-1.5, adj=0, cex=1.4)

plotFOPnorsk(fop_train2, "ar50", xlab = " ", cex.lab=1.6)
mtext("b) Data: Arealtyper", side=3, line=-1.5, adj=0, cex=1.4)

plotRespNorsk(mymodel_simple, DVs$transformations, "bioclim10", main="", xlab="Bioklimatisk variabel 10", ylab="Relativ sannsynlighet for forekomst", cex.axis=1.2, cex.lab=1.6)
mtext("c) Modell: Gjennomsnittstempetratur i varmeste kvartal", side=3, line=-1.5, adj=0, cex=1.4)

plotRespNorsk(mymodel_simple, DVs$transformations, "ar50", main="", xlab="Arealtype AR50", ylab="Relativ sannsynlighet for forekomst", cex.axis=1.2, cex.lab=1.6)
mtext("d) Modell: arealtyper", side=3, line=-1.5, adj=0, cex=1.4)

plotFOPnorsk(fop_train, "aspect", xlab = "Helningsretning", cex.lab=1.6)
mtext("e) Data: Helningsretning", side=3, line=-1.5, adj=0, cex=1.4)

plotFOPnorsk(fop_train2, "berggrunn", xlab = " ", cex.lab=1.6)
mtext("f) Data: kalkinnhold i berggrunn", side=3, line=-1.5, adj=0, cex=1.4)

# Reset to default single plot layout and close the PNG device
par(mfrow=c(1,1))
dev.off()


# Open a PNG device for high-resolution plots
png(filename = "C:/Privat/data_analysis/distribution_modelling_trond/to_github/outputs/high_res_plots2.png",
    width = 10 * 300, height = 15 * 300 * 2/3, res = 300)

# Adjust margins and layout
# Increase the left outer margin for label space (second number in oma)
# Increase the bottom margin (first number in mar) for horizontal space between plots
par(mfrow=c(3,2), mar=c(6, 5, 4, 2), oma=c(4, 4, 2, 2))

# Update cex.lab for larger x and y axis labels, and adjust vertical spacing with mar
plotFOPnorsk(fop_train, "bioclim10", xlab = "Gjennomsnittstempetratur i varmeste kvartal", cex.lab=1.6)
mtext("a) Data: gjennomsnittstempetratur", side=3, line=1.0, adj=-0.1, cex=1.4)

plotFOPnorsk(fop_train2, "ar50", xlab = " ", cex.lab=1.6)
mtext("b) Data: arealtyper", side=3, line=1.0, adj=-0.1, cex=1.4)

plotRespNorsk(mymodel_simple, DVs$transformations, "bioclim10", main="", xlab="Gjennomsnittstempetratur i varmeste kvartal", ylab="Relativ sannsynlighet for forekomst", cex.axis=1.2, cex.lab=1.6)
mtext("c) Modell: gjennomsnittstempetratur", side=3, line=1.0, adj=-0.1, cex=1.4)

plotRespNorsk(mymodel_simple, DVs$transformations, "ar50", main="", xlab="Arealtype AR50", ylab="Relativ sannsynlighet for forekomst", cex.axis=1.2, cex.lab=1.6)
mtext("d) Modell: arealtyper", side=3, line=1.0, adj=-0.1, cex=1.4)

plotFOPnorsk(fop_train, "aspect", xlab = "Helningsretning", cex.lab=1.6)
mtext("e) Data: helningsretning", side=3, line=1.0, adj=-0.1, cex=1.4)

plotFOPnorsk(fop_train2, "berggrunn", xlab = " ", cex.lab=1.6)
mtext("f) Data: kalkinnhold i berggrunn", side=3, line=1.0, adj=-0.1, cex=1.4)

# Reset to default single plot layout and close the PNG device
par(mfrow=c(1,1))
dev.off()

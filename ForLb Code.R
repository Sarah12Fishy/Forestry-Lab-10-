install.packages("terra")
install.packages("tmap")
install.packages("sf")

# Point 1. Read inventory data
setwd("~/Purdue/Spring 2025/AGR 333/Lab 10")
getwd()

filepath <- ("C:/Users/sever/OneDrive/Documents/Purdue/Spring 2025/AGR 333/AGLB10/ForL2/files/")

library(terra)
dem <- rast(paste0(filepath, "unit2.img"))
print(dem)

df_dem <- as.data.frame(dem, xy = TRUE)

# Point 2. Extract Slope and Aspect

library(terra)

slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)

aspect <- terrain(dem, v = "aspect", unit = "degrees")

#install.packages(‘tmap’)
library(tmap)
ttm() # Switch to interactive mode
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")

tm_shape(aspect) +
  tm_raster(style = "cont")

# Point 3. Reclassify Aspect

asp_class <- matrix(c(
  0, 45, 1,    # North
  45, 90, 2,   # East
  90, 135, 2,  # East
  135, 180, 3, # South
  180, 225, 3, # South
  225, 270, 4, # West
  270, 315, 4, # West
  315, 360, 1  # North
), ncol = 3, byrow = TRUE)


asp <- classify(aspect, asp_class)

ttm()
tm_shape(asp) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)

# Point 4. Visualize Sample Forest Inventory Plots 

sum_u2 <- read.csv(paste0(filepath, "sum_u2.csv"))
library(sf)
svy_pts <- st_read(paste0(filepath, "HEE_Overstory_Survey_Points_2017.shp"))
svy_pts <- st_transform(svy_pts, 32616) # Project to WGS 84 UTM 16 N
survey_pts <- subset(svy_pts, Unit == '2') # Subset for unit 2

sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)

unique(sum_u2$Plot)
unique(survey_pts$Plot)

sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)
sum_u2

# Point 5. Create Circular Plots

sf_plot <- st_buffer(sum_u2, dist = 17.83)

# Point 6. Unify Coordinate Systems

crs(sf_plot , proj=T)
crs(asp , proj=T)

asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)

# Point 7. Visualization

# a. Dominant Species by Aspec
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South", "West")) +
  tm_shape(sf_plot) +
  tm_polygons('Common.name') +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9)

# b. Dominant Species by Slope

ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominant trees by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)

# c. Basal Area (BA) Distribution

ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominant trees by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)

# d. Trees Per Acre (TPA) Distribution

ttm()
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# e. Biomass Distribution

ttm()
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()


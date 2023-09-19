# Clear Variables
rm(list = ls())

# Load GCP Data
gcp = read.csv(here::here("data","GCP.csv")) # Insert you own ground control point data

# ---- Functions ----
# Calculating Error & RMSE (Random Mean Square Error)
# To calculate RMSE, subset your data first to distinguish between GCP RMSE & Validation Point RMSE
rmse = function(gcp){
  # Create Error Variables
  Northing_Error = (gcp$Data_Northing - gcp$Measured_Northing)
  Easting_Error = (gcp$Data_Easting - gcp$Measured_Easting)
  Elevation_Error = (gcp$Data_Elevation - gcp$Measured_Elevation)
  Lateral_XY_Error = sqrt((Northing_Error^2)+(Easting_Error^2))
  Total_XYZ_Error = sqrt((Northing_Error^2)+(Easting_Error^2)+(Elevation_Error^2))
  Northing_Error_Squared = Northing_Error^2
  Easting_Error_Squared = Easting_Error^2
  Elevation_Error_Squared = Elevation_Error^2
  Lateral_XY_Error_Squared = Lateral_XY_Error^2
  Total_XYZ_Error_Squared = Total_XYZ_Error^2
  
  # Create Error Dataframe
  error = data.frame(
    "Northing_Error" = Northing_Error,
    "Easting_Error" = Easting_Error,
    "Elevation_Error" = Elevation_Error,
    "Lateral_XY_Error" = Lateral_XY_Error,
    "Total_XYZ_Error" = Total_XYZ_Error,
    "Northing_Error_Squared" = Northing_Error_Squared,
    "Easting_Error_Squared" = Easting_Error_Squared,
    "Elevation_Error_Squared" = Elevation_Error_Squared,
    "Lateral_XY_Error_Squared" = Lateral_XY_Error_Squared,
    "Total_XYZ_Error_Squared" = Total_XYZ_Error_Squared
  )
  
  Northing_RMSE = sqrt((sum(error$Northing_Error_Squared))/nrow(error))
  Easting_RMSE = sqrt((sum(error$Easting_Error_Squared))/nrow(error))
  Elevation_RMSE = sqrt((sum(error$Elevation_Error_Squared))/nrow(error))
  Lateral_XY_RMSE = sqrt((sum(error$Lateral_XY_Error_Squared))/nrow(error))
  Total_XYZ_RMSE = sqrt((sum(error$Total_XYZ_Error_Squared))/nrow(error))
  
  RMSE = data.frame("Northing_RMSE" = Northing_RMSE,
                    "Easting_RMSE" = Easting_RMSE,
                    "Elevation_RMSE" = Elevation_RMSE,
                    "Lateral_XY_RMSE" = Lateral_XY_RMSE,
                    "Total_XYZ_RMSE" = Total_XYZ_RMSE)
  return(Northing_Error)
}

# Accuracy of Sample SD
accuracy_sample_sd = function(points){
  # Create a storage vector
  storage = data.frame(Point = c(1:nrow(points)))
  for(i in 1:nrow(points)){
    mni = points[i, "Measured_Northing"]
    mei = points[i, "Measured_Easting"]
    mnj = points[, "Measured_Northing"]
    mej = points[, "Measured_Easting"]
    # Calculate
    calc = data.frame(Name = sqrt((mni - mnj)^2 + (mei - mej)^2))
    name = points[i,"Name"]
    colnames(calc) = name
    storage = cbind(storage, calc)
  }
  output = data.frame(Point = points$Name)
  storage_raw = subset(storage, select = -c(1,2))
  # Calculate Statistics
  max = apply(storage_raw, 1, max)
  min = apply(storage_raw, 1, min)
  avg = apply(storage_raw, 1, mean)
  standev = apply(storage_raw, 1, sd)
  # Add to Output
  stats = cbind(max, min, avg, standev)
  output = cbind(output, stats)
  sd_sample_mean_dist = sd(output$avg) 
  sd_sample_min_dist = sd(output$min) 
  sd_sample_dist = merge(sd_sample_mean_dist, sd_sample_min_dist)
  return(sd_sample_dist)
}

# Accuracy of Selected Points
accuracy_selected = function(points){
  # Create a storage vector
  storage = data.frame(Points = c(1:nrow(points)))
  for(i in 1:nrow(subset(points, Class == "GCP"))){
    mni = subset(points, Class == "GCP")[,"Measured_Northing"][i]
    mei = subset(points, Class == "GCP")[,"Measured_Easting"][i]
    mnj = points[,"Measured_Northing"]
    mej = points[, "Measured_Easting"]
    # Calculate
    calc = data.frame(Name = sqrt((mni - mnj)^2 + (mei - mej)^2))
    name = points[i,"Name"]
    colnames(calc) = name
    storage = cbind(storage, calc)
  }
  output = data.frame(Point = points$Name)
  storage_raw = subset(storage, select = -c(1))
  # Calculate Statistics
  max = apply(storage_raw, 1, max)
  min = apply(storage_raw, 1, min)
  avg = apply(storage_raw, 1, mean)
  standev = apply(storage_raw, 1, sd) # Uses Population Standard Deviation, but sd uses sample!
  # Add to Output
  stats = cbind(max, min, avg, standev)
  output = cbind(output, stats)
  # Calculate Sample SD
  sd_sample_mean_dist = sd(output$avg) # Uses Sample Standard Deviation, which is correct
  sd_sample_min_dist = sd(output$min) # Uses Sample Standard Deviation, which is correct
  sd_sample_dist = data.frame("SD Sample Mean Distance" = sd_sample_mean_dist, 
                              "SD Sample Minimum Distance" = sd_sample_min_dist)
  return(output)
}

# ---- Run All Stats ----
{
  RMSE_GCP = rmse(subset(gcp, Class == "GCP"))
  RMSE_Val = rmse(subset(gcp, Class == "Test Point"))
  accuracy_statistics = accuracy_selected(gcp)
  accuracy_sample_sd = accuracy_sample_sd(gcp) # Returns Sample Standard Deviation for Mean & Min 
}

# ---- Create Histograms
{
  # Create Error Variables
  Northing_Error = data.frame(Name = gcp$Name, Class = gcp$Class, "Northing Error" = (gcp$Data_Northing - gcp$Measured_Northing))
  Easting_Error = data.frame(Name = gcp$Name, Class = gcp$Class, "Easting Error" = (gcp$Data_Easting - gcp$Measured_Easting))
  Elevation_Error = data.frame(Name = gcp$Name, Class = gcp$Class, "Elevation Error" = (gcp$Data_Elevation - gcp$Measured_Elevation))
  Lateral_XY_Error = data.frame(Name = gcp$Name, Class = gcp$Class, "Lateral XY Error" = sqrt((Northing_Error$Northing.Error^2)+(Easting_Error$Easting.Error^2)))
  Total_XYZ_Error = data.frame(Name = gcp$Name, Class = gcp$Class, "Total XYZ Error" = sqrt((Northing_Error$Northing.Error^2)+(Easting_Error$Easting.Error^2)+(Elevation_Error$Elevation.Error^2)))
}

{
  # Create Error Histograms
  require(ggplot2)
  
  # Northing Error Histogram
  ne_hist = ggplot(subset(Northing_Error, Class=="Test Point"), aes(x = Name, y = Northing.Error)) + 
    geom_col(aes(x = Name, fill = factor(Northing.Error))) + 
    labs(title = "Validation Point Northing Error") +
    xlab("Validation Point") +
    theme(legend.position="none") +
    scale_x_continuous(breaks = seq(min(as.numeric(subset(Northing_Error, Class=="Test Point")$Name)), max(as.numeric(subset(Northing_Error, Class=="Test Point")$Name)), by = 1))
  
  # Easting Error Histogram
  ee_hist = ggplot(subset(Easting_Error, Class=="Test Point"), aes(x = Name, y = Easting.Error)) + 
    geom_col(aes(x = Name, fill = factor(Easting.Error))) + 
    labs(title = "Validation Point Easting Error") +
    xlab("Validation Point") +
    theme(legend.position="none") +
    scale_x_continuous(breaks = seq(min(as.numeric(subset(Easting_Error, Class=="Test Point")$Name)), max(as.numeric(subset(Easting_Error, Class=="Test Point")$Name)), by = 1))
  
  # Elevation Error Histogram
  ele_hist = ggplot(subset(Elevation_Error, Class=="Test Point"), aes(x = Name, y = Elevation.Error)) + 
    geom_col(aes(x = Name, fill = factor(Elevation.Error))) + 
    labs(title = "Validation Point Elevation Error") +
    xlab("Validation Point") +
    theme(legend.position="none") +
    scale_x_continuous(breaks = seq(min(as.numeric(subset(Elevation_Error, Class=="Test Point")$Name)), max(as.numeric(subset(Elevation_Error, Class=="Test Point")$Name)), by = 1))
  
  # Lateral XY Error Histogram
  latxy_hist = ggplot(subset(Lateral_XY_Error, Class=="Test Point"), aes(x = Name, y = Lateral.XY.Error)) + 
    geom_col(aes(x = Name, fill = factor(Lateral.XY.Error))) + 
    labs(title = "Validation Point Lateral XY Error") +
    xlab("Validation Point") +
    theme(legend.position="none") +
    scale_x_continuous(breaks = seq(min(as.numeric(subset(Lateral_XY_Error, Class=="Test Point")$Name)), max(as.numeric(subset(Lateral_XY_Error, Class=="Test Point")$Name)), by = 1))
  
  # Total XYZ Error Histogram
  totxyz_hist = ggplot(subset(Total_XYZ_Error, Class=="Test Point"), aes(x = Name, y = Total.XYZ.Error)) + 
    geom_col(aes(x = Name, fill = factor(Total.XYZ.Error))) + 
    labs(title = "Validation Point Total XYZ Error") +
    xlab("Validation Point") +
    theme(legend.position="none") +
    scale_x_continuous(breaks = seq(min(subset(Total_XYZ_Error, Class=="Test Point")$Name, na.rm = TRUE), max(subset(Total_XYZ_Error, Class=="Test Point")$Name, na.rm = TRUE), by = 1))
}

# Call Histogram of Choice
ne_hist
ee_hist
ele_hist
latxy_hist
totxyz_hist

# Saving Error Histograms
ggsave(filename = "northing_error_histogram.png", plot = ne_hist, width = 16, height = 9, units = "in", dpi = 100)
ggsave(filename = "easting_error_histogram.png", plot = ee_hist, width = 16, height = 9, units = "in", dpi = 100)
ggsave(filename = "elevation_error_histogram.png", plot = ele_hist, width = 16, height = 9, units = "in", dpi = 100)
ggsave(filename = "lateral_xy_error_histogram.png", plot = latxy_hist, width = 16, height = 9, units = "in", dpi = 100)
ggsave(filename = "total_xyz_error_histogram.png", plot = totxyz_hist, width = 16, height = 9, units = "in", dpi = 100)















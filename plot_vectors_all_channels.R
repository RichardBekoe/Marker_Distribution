# Script to plot arrows of channel vectors for many channels
library(ggplot2)

library(jpeg)
library(ggplot2)
library(raster)
library(grid)

# **************** Functions *****************************************

# Function to load the data and assign nice column names
load_cellcsv <- function(filename)
{
  data <- read.csv(filename)
  
  # Change col names to things like:
  # "Image", "Object", 
  # "COM_X", "COM_Y", "COM_Z", 
  # "X", "Y",
  # "ZernikeMagnitude_0_0" etc
  # "ZernikePhase_0_0" etc
  colnames(data) <- sub("Number", "", colnames(data))
  colnames(data) <- sub("Location_CenterMassIntensity_X_channels_split_Filtered", "COM_X", colnames(data)) 
  colnames(data) <- sub("Location_CenterMassIntensity_Y_channels_split_Filtered", "COM_Y", colnames(data))
  colnames(data) <- sub("Location_Center_X", "X", colnames(data))
  colnames(data) <- sub("Location_Center_Y", "Y", colnames(data))
  colnames(data) <- sub("RadialDistribution_ZernikeMagnitude_channels_split_Filtered", "ZernikeMagnitude", colnames(data))
  colnames(data) <- sub("RadialDistribution_ZernikePhase_channels_split_Filtered", "ZernikePhase", colnames(data))
  
  return (data)
}

# Function to calculate coordinates for arrow heads based on Zerike 1,1 magnitude and phase
# polar to cartesian conversion
# scale_factor can be a vector
transform_zernike_to_vector_end <- function (data, scale_factor = 0, max_length = 30)
{
  r <- data$ZernikeMagnitude_1_1
  theta <- data$ZernikePhase_1_1
  
  # calculate vector
  data$Zernike_X <- r * cos(theta)
  data$Zernike_Y <- r * sin(theta)
  
  if (scale_factor == 0) {
    # if scale factor is zero, calculate it from the max length
    max_mag <- max(data$ZernikeMagnitude_1_1, na.rm = T)
    scale_factor <- max_length / max_mag
  }
  
  # scale and place vector in the image coord system
  data$Zernike_X <- scale_factor * data$Zernike_X + data$X
  data$Zernike_Y <- scale_factor * data$Zernike_Y + data$Y

  return(data)  
}

# make a subset of cells in an roi
subset_roi <- function (data, x, y, width, height)
{
  x_max <- x + width
  y_max <- y + height
  data <- subset(data, X < x_max)
  data <- subset(data, X > x)
  data <- subset(data, Y < y_max)
  data <- subset(data, Y > y)
  return(data)
}

# **************************************************************************************

# load the data from all cell.csv files in all subfolders, and calculate Zernike vectors
# NB All channels will use the same scale factor for the Zernike vectors, so far
channel_sub_folder = "cp_output/"
channel_list <- list.dirs(path = channel_sub_folder, full.names = FALSE, recursive = FALSE)
for (channel in channel_list){
  data <- load_cellcsv(paste0(channel_sub_folder, channel, "/cell.csv"))
  data <- transform_zernike_to_vector_end(data)
  assign(channel, data)
  rm(data)
}


#image <- jpeg::readJPEG("/home/user/Documents/channels_split_cellprofiler_headless/Output_Channels_Split_Transferred/s0287_Sedigeh-HandN_s0_p5_r1_a1_ac_ilastik_s2_Probabilities_no_gaps__mask.tiff")
#grob <- rasterGrob(image, interpolate=TRUE)

str_name<-'cp_output/Overlay.tiff'    # TO DO Get the right channel overlay for each channel. This file is for testing.

library(tiff)
image <- readTIFF(str_name) 

grob <- rasterGrob(image, interpolate=TRUE)


library(tiff)
image <- readTIFF(str_name)

pdf("plots.pdf", width = 6.9, height = 7)

# all channels  in the list)

# for (channel in channel_list) {
  
  # OR only the correct channel)
  
for (channel in c("Channel_6")){
  
  data <- get(channel)
  
  roi <- subset_roi(data, 100, 400, 200, 200)

  # plot subset with arrows towards the centre of mass

  p <- ggplot(roi, aes(X, Y)) +
    scale_y_reverse() +
    theme_classic() +
    geom_point(color = "black", size = 0.25, stroke = 0) +
    ggtitle(paste("Centre Of Mass:", channel)) +
    geom_segment(aes(x = X, y = Y, xend = COM_X, yend = COM_Y),
                 arrow = arrow(length = unit(0.002, "npc")), size = 0.12,
                 color = "red")
  print(p)

  # plot subset with arrows towards the Zernike coord

  p <- ggplot(roi, aes(X, Y)) +
    scale_y_reverse() +
    theme_classic() +
    geom_point(color = "black", size = 0.25, stroke = 0) +
    ggtitle(paste("Zernike:", channel)) +
    geom_segment(aes(x = X, y = Y, xend = Zernike_X, yend = Zernike_Y),
                 arrow = arrow(length = unit(0.002, "npc")), size = 0.12,
                 color = "red")
  print(p)

  
  
  #plot Centre OF Mass with mask
  
  p <- ggplot(data, aes(X, Y)) +
    annotation_custom(grob = grob, xmin=0, xmax=1000, ymin=0, ymax=-1000) +
    scale_y_reverse() +
    theme_classic() +
    geom_point(color = "yellow", size = 0.25, stroke = 0) +
    ggtitle(paste("Centre Of Mass with Mask:", channel)) +
    geom_segment(aes(x = X, y = Y, xend = COM_X, yend = COM_Y),
                arrow = arrow(length = unit(0.002, "npc")), size = 0.12,
                color  = "red")
    
  print(p)
  # ggsave("ggsave.pdf", width = 6.9, height = 7)  # save individual plot
  
  #plot Zernike with mask
  
  p <- ggplot(data, aes(X, Y)) +
    annotation_custom(grob = grob, xmin=0, xmax=1000, ymin=0, ymax=-1000) +
    scale_y_reverse() +
    theme_classic() +
    geom_point(color = "yellow", size = 0.25, stroke = 0) +
    ggtitle(paste("Zernike with Mask:", channel)) +
    geom_segment(aes(x = X, y = Y, xend = Zernike_X, yend = Zernike_Y),
                 arrow = arrow(length = unit(0.002, "npc")), size = 0.12,
                 color = "red")
  print(p)

}

dev.off()


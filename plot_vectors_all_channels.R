# Script to plot arrows of channel vectors for many channels
library(ggplot2)

library(tiff)
#library(jpeg)
library(ggplot2)
library(raster)
library(grid)

# **************** Functions *****************************************      
source("utilities.R")

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




pdf("plots.pdf", width = 6.9, height = 7)

# all channels  in the list)

for (channel in channel_list) {

# OR only the specified channel)

#for (channel in c("HandN-0006")){
  
  data <- get(channel)
  
  roi <- subset_roi(data, 100, 400, 200, 200)
  
  
  str_name<-paste0(channel_sub_folder, channel, "/Overlay.tiff") 
  image <- readTIFF(str_name) 
  grob <- rasterGrob(image, interpolate=TRUE)
  
  
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


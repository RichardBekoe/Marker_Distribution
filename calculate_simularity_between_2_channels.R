# Calculate simularity or 'partner' score between neighbours between 2 channels
library(tiff)
#library(jpeg)
library(ggplot2)
library(raster)
library(grid)
source("utilities.R")


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

############################################

# calculate simlularity or interaction score

calculate_score1 <- function(o1, o2){
  # dot product of Zernike moments
  score <- o1$ZernikeMagnitude_1_1 * o2$ZernikeMagnitude_1_1 * cos(o1$ZernikePhase_1_1 - o2$ZernikePhase_1_1)
  score <- ifelse(score==0, 0, log10(abs(score * 100E10)) * sign(score))
  return(score)
}

calculate_score2 <- function(o1, o2){
  # dot product of com vector
  score <- (o1$COM_X-o1$X) * (o2$COM_X-o2$X) + (o1$COM_Y-o1$Y) * (o2$COM_Y-o2$Y)
  score <- ifelse(score==0, 0, log10(abs(score)) * sign(score))
  return(score)
}

calculate_score3 <- function(o1, o2){
  # product of Zernike moments projected onto the connecting line

  # theta is the angle of the line connecting the 2 objects
  theta <- atan2((o2$Y - o1$Y), (o2$X - o1$X))
  
  # Threshold the Zernike 0 0 value?????
  threshold = 1E-5
  if(o1$ZernikeMagnitude_0_0 < threshold || o2$ZernikeMagnitude_0_0 < threshold){
    score = NA
    s1 = 0
    s2 = 0
  }
  else{
  
    # project each vector onto this line
    s1 <- o1$ZernikeMagnitude_1_1 * cos(o1$ZernikePhase_1_1 - theta)
    s2 <- o2$ZernikeMagnitude_1_1 * cos(o2$ZernikePhase_1_1 - theta)
    
    # We know if s1 +ve and s2 -ve the vectors point towards each other, vive versa is away from each other
    # Not interested in both +ve or both -ve
    score <- s1 * s2
    if(score >= 0){ # both +ve or both -ve ->   -> or  <-   <-
      score <- 0
    }
    else{
      if(s1 > 0){  # implies s2 < 0
        score <- abs(score) # arrows together  ->   <-
      }
      else{
        score <- -1 * abs(score) # arrows away  <-   ->
      }
    }
    
  }
  
  return(c(score, s1, s2, theta*360/(2*3.142), o1$ZernikePhase_1_1*360/(2*3.142), o2$ZernikePhase_1_1*360/(2*3.142)))
}

#############################################


# load the cell neighbour relationships
# This will be the same for all channels so just load the one in the parent folder
NR <- read.csv("cp_output/Object relationships.csv")

# initialise a col for the scores etc.
NR$score <- NA
NR$s1 <- NA
NR$s2 <- NA
NR$theta <- NA
NR$phase1 <- NA
NR$phase2 <- NA
NR$theta <- NA
NR$obj1_x <- NA
NR$obj1_y <- NA
NR$obj2_x <- NA
NR$obj2_y <- NA

# new data for all image scores (not cell interaction scores)
Channel_A <- vector()
Channel_B <- vector()
Num_Interactions <- vector()


# Calculate scores between cells for 2 given channels

for(channelA in 1:12){
  for(channelB in channelA:12){

    chA_name <- ls(pattern = "HandN*")[channelA]     # relies on corrent file names!
    chB_name <- ls(pattern = "HandN*")[channelB]     # relies on corrent file names!
    chA_data <- get(chA_name)
    chB_data <- get(chB_name)
    
    for (i in 1:length(NR$First.Object.Number)){
      
      obj1 <- NR$First.Object.Number[i]
      obj2 <- NR$Second.Object.Number[i]
      
      if(obj1 > obj2) next    # there are repeats in the data, i.e. 1->12 and 12->1, remove repeats
      
      channelA_obj1 <- chA_data[obj1,]   # relies on one object per row in order in the channel data
      channelB_obj2 <- chB_data[obj2,]
      
      s <- calculate_score3(channelA_obj1, channelB_obj2) # choose which score to calculate
      
      NR$score[i] <- s[1]
      NR$s1[i] <- s[2]
      NR$s2[i] <- s[3]
      NR$theta[i] <- s[4]
      NR$phase1[i] <- s[5]
      NR$phase2[i] <- s[6]
      
      NR$obj1_x[i] <- channelA_obj1$X
      NR$obj1_y[i] <- channelA_obj1$Y
      NR$obj2_x[i] <- channelB_obj2$X
      NR$obj2_y[i] <- channelB_obj2$Y
    }
    
    # Categorise score
    NR$interaction <- ifelse(NR$score > 0, "Y", 
                             ifelse(NR$score == 0, NA, "N"))
    
    # remove repeats
    NR_reduced <- subset(NR, First.Object.Number < Second.Object.Number)
    
    # Make data frame of just the interactions
    NR_interactions <- subset(NR_reduced, interaction == "Y")
    
    if(channelA == channelB){
      # visualise
      name <- paste0("Zernike_with_Mask_", chA_name)
      pdf(name, width = 7, height = 7)
      
      str_name<-paste0(channel_sub_folder, chA_name, "/Overlay.tiff") 
      image <- readTIFF(str_name) 
      grob <- rasterGrob(image, interpolate=TRUE)
      
      #plot Zernike with mask
      p <- ggplot(chA_data, aes(X, Y)) +
        annotation_custom(grob = grob, xmin=0, xmax=1000, ymin=0, ymax=-1000) +
        scale_y_reverse() +
        theme_classic() +
        geom_point(color = "yellow", size = 0.25, stroke = 0) +
        ggtitle(name) +
        geom_segment(aes(x = X, y = Y, xend = Zernike_X, yend = Zernike_Y),
                     arrow = arrow(length = unit(0.002, "npc")), size = 0.12,
                     color = "white") +
        geom_segment(data = NR_interactions, aes(x = obj1_x, y = obj1_y, xend = obj2_x, yend = obj2_y), color = "red",
                    size = 0.15) #+
                #    scale_color_gradient2(low = ("red"), mid = "white",
                #                          high = ("purple"))
      print(p)
      
      dev.off()
    }
    
    # visualise just the interactions
    name <- paste0("Zernike_interactions_", chA_name,  "_vs_", chB_name)
    pdf(name, width = 7, height = 7)
    
    p <- ggplot(chA_data, aes(X, Y)) +
      scale_y_reverse() +
      theme_classic() +
      geom_point(color = "yellow", size = 0.25, stroke = 0) +
      ggtitle(name) +
      geom_segment(data = NR_interactions, aes(x = obj1_x, y = obj1_y, xend = obj2_x, yend = obj2_y), color = "red",
                   size = 0.15)
    
    print(p)
    
    dev.off()
    
    # store the number of interactions
    Channel_A <- c(Channel_A, channelA)
    Channel_B <- c(Channel_B, channelB)
    Num_Interactions <- c(Num_Interactions, dim(NR_interactions)[1])

  }
}

number_of_interactions <- data.frame(Channel_A, Channel_B, Num_Interactions)
write.csv(number_of_interactions, file = "number_of_interactions.csv")



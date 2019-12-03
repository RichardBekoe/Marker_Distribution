# utility functions

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
    # if scale factor is zero, calculate it from the max lengthJ
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
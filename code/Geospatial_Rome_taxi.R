#(a)
#get data
setwd("/Users/apple/Documents/UOW/INFO911 Data Mining and Knowledge Discovery/assignment1")
taxi_data <- read.csv('taxi.csv', header = TRUE, sep = ',', dec = '.')

install.packages("dbscan")  
library(dbscan)    
library(ggplot2)

# Get a representative sample by taking 100 observations from each DriveNo
# Define the number of samples to take from each DriveNo
samples_per_driver <- 100

# Initialize an empty list for samples
sampled_list <- list()

# Get unique DriveNo values
driver_numbers <- unique(taxi_data$DriveNo)

# Stratified sampling
for (driver in driver_numbers) {
  driver_data <- taxi_data[taxi_data$DriveNo == driver, ]
  if (nrow(driver_data) >= samples_per_driver) {
    sampled_data <- driver_data[sample(nrow(driver_data), samples_per_driver), ]
    sampled_list[[as.character(driver)]] <- sampled_data
  }
}

# Combine all sampled data into a single data frame
stratified_sample <- do.call(rbind, sampled_list)
stratified_sample$status <- 'valid'

# Identify missing and invalid points
invalid_points <- is.na(stratified_sample$Latitude) | is.na(stratified_sample$Longitude) |
  stratified_sample$Latitude < 41.8 | stratified_sample$Latitude > 42.05 | 
  stratified_sample$Longitude < 12.35 | stratified_sample$Longitude > 12.65
stratified_sample$status[invalid_points] <- 'invalid'
valid_points <- !invalid_points

# Apply DBSCAN to detect outliers
# Select relevant columns for clustering
coordinates <- stratified_sample[, c("Longitude", "Latitude")]

# Run DBSCAN
dbscan_result <- dbscan(coordinates, eps = 0.01, minPts = 2)

# Add cluster information to the sampled data
stratified_sample$cluster <- dbscan_result$cluster

# Identify outliers (cluster = 0)
outliers <- stratified_sample$cluster == 0
stratified_sample$status[outliers] <- 'outlier'
valid_points2 <- valid_points | !outliers

# Plot the sample
ggplot(stratified_sample, aes(x = stratified_sample$Longitude, y = stratified_sample$Latitude, color = status)) +
  geom_point(alpha = 0.5, size=0.2) +
  scale_color_manual(values = c("valid" = "lightgreen", 
                                "outlier" = "red", 
                                "invalid" = "purple")) +
  labs(title = "Taxi Data in Rome (Feb-Mar 2014)",
       subtitle = paste("Valid: ", table(stratified_sample$status)['valid'],
                        "| Invalid: ", table(stratified_sample$status)['invalid'],
                        "| Outlier: ", table(stratified_sample$status)['outlier']),
       x = "Longitude", 
       y = "Latitude",
       color = "Point Type") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  # Add a point for Rome's center
  geom_point(aes(x = rome_center_long, y = rome_center_lat), 
             color = "blue", size = 2, shape = 18) +
  annotate("text", x = rome_center_long, y = rome_center_lat + 0.02, 
           label = "Rome Center", color = "blue") +
  #Add a boundary
  annotate("rect", xmin = 12.35, xmax = 12.65, ymin = 41.8, ymax = 42.05, color = 'black', fill=NA)

#remove outliers and invalid points
cleaned_data <- subset(stratified_sample, status != "outlier" & status != "invalid")

# Plot cleaned data
ggplot(cleaned_data, aes(x = cleaned_data$Longitude, y = cleaned_data$Latitude, color = status)) +
  geom_point(alpha = 0.5, size=0.2) +
  scale_color_manual(values = c("valid" = "blue", 
                                "outlier" = "red", 
                                "invalid" = "purple")) +
  labs(title = "Cleaned Taxi Data in Rome (Feb-Mar 2014)",
       subtitle = paste("Valid: ", table(cleaned_data$status)['valid'],
                        "| Invalid: ", table(cleaned_data$status)['invalid'],
                        "| Outlier: ", table(cleaned_data$status)['outlier']),
       x = "Longitude", 
       y = "Latitude",
       color = "Point Type") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  # Add a point for Rome's center
  geom_point(aes(x = rome_center_long, y = rome_center_lat), 
             color = "lightgreen", size = 2, shape = 18) +
  annotate("text", x = rome_center_long, y = rome_center_lat + 0.02, 
           label = "Rome Center", color = "blue") +
  #Add a boundary
  annotate("rect", xmin = 12.35, xmax = 12.65, ymin = 41.8, ymax = 42.05, color = 'black', fill=NA)

#(b) Compute the minimum, maximum, and mean location values
min_lat <- min(cleaned_data$Latitude)
max_lat <- max(cleaned_data$Latitude)
mean_lat <- mean(cleaned_data$Latitude)

min_long <- min(cleaned_data$Longitude)
max_long <- max(cleaned_data$Longitude)
mean_long <- mean(cleaned_data$Longitude)

#(c) Obtain the most active, least active, and average active taxi drivers
#convert datetime to R format
cleaned_data$Date.and.Time <- as.POSIXct(cleaned_data$Date.and.Time)

#find Max and Min time value points for each driver
time_summary <- aggregate(Date.and.Time ~ DriveNo, data = cleaned_data, 
                          FUN = function(x) c(Max = max(x), Min = min(x)))
#the function returns a matrix of max and min time value points
str(time_summary)
#we need to convert the matrix to the dataframe format for further retrievals and calculations
time_summary <- data.frame(DriveNo = time_summary$DriveNo,
                           Date.and.Time.Max = time_summary$Date.and.Time[, "Max"],
                           Date.and.Time.Min = time_summary$Date.and.Time[, "Min"])

#convert datetime to R format
time_summary$Date.and.Time.Max <- as.POSIXct(time_summary$Date.and.Time.Max)
time_summary$Date.and.Time.Min <- as.POSIXct(time_summary$Date.and.Time.Min)

#calculate the amount of time driven for each driver
time_summary$Time.Difference <- time_summary$Date.and.Time.Max - time_summary$Date.and.Time.Min

most_active <- max(time_summary$Time.Difference)
least_active <- min(time_summary$Time.Difference)
average_active <- mean(time_summary$Time.Difference)

#(d)
#get data
setwd("/Users/apple/Documents/UOW/INFO911 Data Mining and Knowledge Discovery/assignment1")
student_data <- read.table("Student_Taxi_Mapping.txt", sep=" ", header = FALSE)
colnames(student_data) <- c("SID","Taxi_ID")

#retrieve the taxi code
mycode <- 8111
taxi_code <- student_data[student_data$SID == mycode, ][2]

#i. Plot the location points for taxi=ID
# The data plot for driver 147 is generated from all rows that contain DriveNo = 147 from the full dataset
# because using 100 observations of DriveNo147 from the stratified sample makes the dataset for driver147 too small
#create a dataframe containing data for driver 147

taxi_data <- read.csv('taxi.csv', header = TRUE, sep = ',', dec = '.')
driver_147 <- taxi_data[taxi_data$DriveNo == 147, ]

driver_147$status <- 'valid'
# Identify missing and invalid points
invalid_points147 <- is.na(driver_147$Latitude) | is.na(driver_147$Longitude) |
  driver_147$Latitude < 41.8 | driver_147$Latitude > 42.05 | 
  driver_147$Longitude < 12.35 | driver_147$Longitude > 12.65
driver_147$status[invalid_points147] <- 'invalid'
valid_points147 <- !invalid_points147

# Apply DBSCAN to detect outliers
# Select relevant columns for clustering
coordinates147 <- driver_147[, c("Longitude", "Latitude")]

# Run DBSCAN
dbscan_result147 <- dbscan(coordinates147, eps = 100, minPts = 10)

# Add cluster information to the sampled data
driver_147$cluster <- dbscan_result147$cluster
table(dbscan_result147$cluster)

# Identify outliers (cluster = 0)
outliers147 <- driver_147$cluster == 0
driver_147$status[outliers147] <- 'outlier'
valid_points147_2 <- valid_points147 | !outliers147

# Plot cleaned dataset fo driver 147
cleaned_data147 <- subset(driver_147, status != "outlier" & status != "invalid")

ggplot(cleaned_data147, aes(x = cleaned_data147$Longitude, y = cleaned_data147$Latitude, color = status)) +
  geom_point(alpha = 0.5, size=0.2) +
  scale_color_manual(values = c("valid" = "blue", 
                                "outlier" = "red", 
                                "invalid" = "purple")) +
  labs(title = "Cleaned Taxi Driver 147 Location Points in Rome (Feb-Mar 2014)",
       x = "Longitude", 
       y = "Latitude",
       color = "Point Type") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  # Add a point for Rome's center
  geom_point(aes(x = rome_center_long, y = rome_center_lat), 
             color = "lightgreen", size = 2, shape = 18) +
  annotate("text", x = rome_center_long, y = rome_center_lat + 0.02, 
           label = "Rome Center", color = "blue") +
  #Add a boundary
  annotate("rect", xmin = 12.35, xmax = 12.65, ymin = 41.8, ymax = 42.05, color = 'black', fill=NA)

#ii. Compare the mean, min, and max location value of taxi=ID with the global mean, min, and max.
min_lat147 <- min(cleaned_data147$Latitude)
max_lat147 <- max(cleaned_data147$Latitude)
mean_lat147 <- mean(cleaned_data147$Latitude)

min_long147 <- min(cleaned_data147$Longitude)
max_long147 <- max(cleaned_data147$Longitude)
mean_long147 <- mean(cleaned_data147$Longitude)

#iii. Compare total time driven by taxi=ID with the global mean, min, and max values.
cleaned_data147$Date.and.Time <- as.POSIXct(cleaned_data147$Date.and.Time)
time_driven147 <- max(cleaned_data147$Date.and.Time) - min(cleaned_data147$Date.and.Time)
hours_driven147 <- as.numeric(time_driven147, units = "hours")

#iv. Compute the distance traveled by taxi=ID
R <- 6371000
dlon147 <- max_long147 - min_long147
dlat147 <- max_lat147 - min_lat147
a147 <- (sin(dlat147/2))^2 + cos(min_lat147) * cos(max_lat147) * (sin(dlon147/2))^2
c147 = 2 * atan2(sqrt(a147), sqrt(1-a147))
distance_travelled147 <- R * c147

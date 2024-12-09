
setwd("C:/R Project")

# Check the working directory
getwd()
data <- read.table("Dataset.txt", header=TRUE)


# This function creates a bar plot based on the 'Group' variable in the given dataframe.
# Each bar represents the count of different categories in the 'Group' variable.

barplot_forGroup <- function(group){

  group <- table(data$Group)
  
  # Set the limits for the x and y axes
  x_limits <- c(0.5,length(group)+1)
  y_limits <- c(0, max(group)+1)
  # Initialize the plot
  plot(1, 1, xlim = x_limits, ylim = y_limits, type = "n",
       xlab = "Groups", ylab = "Number Of Group Members",
       main = "Group Breakdown", 
       xaxt = "n",frame=FALSE)
  
  # Create each bar using the 'polygon' function
  for (i in 1:length(group)) {
    polygon(c(i - 0.4, i - 0.4 , i + 0.4, i + 0.4), c(0, group[i], group[i], 0), col = "skyblue")
  }
  
  # Add x-axis labels
  axis(1, at = 1:length(group), labels = names(group), tick=FALSE, line=NA)
}


# Call the function
barplot_forGroup(data)

# This function creates a bar plot based on the 'Gender' variable in the given dataframe
# Each bar represents the count of different genders
barplot_forGender <- function(data){
  
  gender <- table(data$Gender)
  x_center <- seq(1, length(gender), by=1)
  bar_width <- 0.8
  
  # Initialize the plot
  plot(1, 1, xlim = c(0, length(gender) + 1), ylim = c(0, max(gender) + 1), 
       type = "n", xlab = "Gender", ylab = "Number of people", 
       main = "Gender Distribution", xaxt = "n", frame=FALSE)
  
  # Create each bar using the 'polygon' function
  for (i in 1:length(gender)) {
    polygon(c(x_center[i] - bar_width / 2, x_center[i] - bar_width / 2, 
              x_center[i] + bar_width / 2, x_center[i] + bar_width / 2),
            c(0, gender[i], gender[i], 0), col = "blue")
  }
  # Add x-axis labels
  axis(1, at = x_center, labels = names(gender), tick=FALSE, line=NA)
}


# Call the function
barplot_forGender(data)

#Barplotting vertically in the same page together
par(mfrow=c(2,1))
barplot_forGroup(data)
barplot_forGender(data)


# Converting comma-separated values (Var1 to Var8) into numeric format.
data$Var1<-as.numeric(gsub(",", ".",data$Var1))
data$Var2<-as.numeric(gsub(",", ".",data$Var2))
data$Var3<-as.numeric(gsub(",", ".",data$Var3))
data$Var4<-as.numeric(gsub(",", ".",data$Var4))
data$Var5<-as.numeric(gsub(",", ".",data$Var5))
data$Var6<-as.numeric(gsub(",", ".",data$Var6))
data$Var7<-as.numeric(gsub(",", ".",data$Var7))
data$Var8<-as.numeric(gsub(",", ".",data$Var8))

# This function creates a histogram for the given variable in the dataset.
# It calculates the frequency of values within each bin and plots the histogram accordingly.
histogram_function <- function(var){
  # Determine the minimum and maximum values for the variable
  min1 <- round(min(data[[var]], na.rm=TRUE), 0)
  max1 <- round(max(data[[var]], na.rm=TRUE), 0)
  
  breaks <- 10
  
  bins <- cut(data[[var]], breaks=breaks, include.lowest = TRUE, right = FALSE)
  
  bin_counts <- table(bins)
  
  bar_width <- (max1 - min1) / breaks
  
  # Initialize the plot
  plot(1, 1, xlim = c(min1, max1), ylim = c(0, max(bin_counts)), type = "n",
       xlab = "Value", ylab = "Frequency", main = paste("Histogram of", var), frame = FALSE)
  
  # Create each bar using the 'polygon' function
  for (i in 1:length(bin_counts)) {
    polygon(c((min1 + (i-1) * bar_width), (min1 + (i-1) * bar_width), 
              (min1 + i * bar_width), (min1 + i * bar_width)),
            c(0, bin_counts[i], bin_counts[i], 0), col = "gray")
  }
}


# Call the function for each variable
histogram_function("Var1")
histogram_function("Var2")
histogram_function("Var3")
histogram_function("Var4")
histogram_function("Var5")
histogram_function("Var6")
histogram_function("Var7")
histogram_function("Var8")

# Set up the grid of plots
par(mfrow = c(2, 4))

# Plot the histograms in the same page
variable_names <- paste0("Var", 1:8)
for (var in variable_names) {
  histogram_function(var)
}



# This function creates a box plot for the given variable.
# The box plot visualizes the quartiles, median, and outliers of the data.

boxplot_function <- function(var){
  # Remove missing values
  var_data <- na.omit(data[[var]])

  q1 <- quantile(var_data, 0.25)
  median1 <- median(var_data)
  q3 <- quantile(var_data, 0.75)
  lower_whisker <- min(var_data[var_data >= q1 - 1.5 * IQR(var_data)])
  upper_whisker <- max(var_data[var_data <= q3 + 1.5 * IQR(var_data)])

  plot(1, 1, xlim = c(0.5, 1.5), ylim = c(min(var_data), max(var_data)), type = "n",
       xlab = "", ylab = var, main = paste("Boxplot of", var), frame = FALSE)

  polygon(c(0.7, 1.3, 1.3, 0.7), c(q1, q1, q3, q3), col = "red", border = NA)
  segments(1, lower_whisker, 1, q1)
  segments(1, q3, 1, upper_whisker)

  segments(0.7, median1, 1.3, median1, lwd = 2)
}


# Call the function for each variable
boxplot_function("Var1")
boxplot_function("Var2")
boxplot_function("Var3")
boxplot_function("Var4")
boxplot_function("Var5")
boxplot_function("Var6")
boxplot_function("Var7")
boxplot_function("Var8")

#Set up the grid of boxplots
par(mfrow = c(2, 4))

# Display the boxplots together
variable_names <- paste0("Var", 1:8)
for (var in variable_names) {
  boxplot_function(var)
}

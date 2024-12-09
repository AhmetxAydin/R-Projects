setwd("C:/RProject")

basic_statistics <- function(data, cols){
  for (col in cols){
    print(paste("Descriptive Statistics for column: ", col))
    
    # Get the column data, excluding NA values
    column_data <- data[[col]]
    column_data <- column_data[!is.na(column_data)]
    
    # Number of observations
    N <- length(column_data)
    print(paste("Number of Observations: ", N))
    
    # Minimum
    min_value <- column_data[1]
    for(val in column_data){
      if(val < min_value){
        min_value <- val
      }
    }
    print(paste("Minimum: ", min_value))
    
    # Maximum
    max_value <- column_data[1]
    for(val in column_data){
      if(val > max_value){
        max_value <- val
      }
    }
    print(paste("Maximum: ", max_value))
    
    # Range
    range_val <- max_value - min_value
    print(paste("Range: ", range_val))
    
    # Sum
    sum_val <- 0
    for(val in column_data){
      sum_val <- sum_val + val
    }
    print(paste("Sum: ", sum_val))
    
    # Mean
    mean_val <- sum_val / N
    print(paste("Mean: ", mean_val))
    
    # Median
    sorted_data <- sort(column_data)
    if (N %% 2 == 0){
      median_val <- (sorted_data[N/2] + sorted_data[N/2 + 1])/2
    } else {
      median_val <- sorted_data[(N+1)/2]
    }
    print(paste("Median: ", median_val))
    
    # Sum of squares
    sum_squares <- 0
    for(val in column_data){
      sum_squares <- sum_squares + val^2
    }
    print(paste("Sum of Squares: ", sum_squares))
    
    # Variance
    var_val <- 0
    for(val in column_data){
      var_val <- var_val + (val - mean_val)^2
    }
    var_val <- var_val / (N-1)
    print(paste("Variance: ", var_val))
    
    # Standard deviation
    sd_val <- sqrt(var_val)
    print(paste("Standard Deviation: ", sd_val))
  }
}

basic_statistics(data, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8"))


statistics <- function(data, col1, col2){
  # Get the column data, excluding NA values
  x <- data[[col1]]
  y <- data[[col2]]
  common_indices <- !(is.na(x) | is.na(y))
  x <- x[common_indices]
  y <- y[common_indices]
  
  # Number of observations
  N <- length(x)

  sum1 <- 0
  sum2 <- 0
  sum12 <- 0
  sum21 <- 0
  sum22 <- 0
  for(i in 1:N){
    sum1 <- sum1 + x[i]
    sum1 <- sum2 + y[i]
    sum12 <- sum12 + x[i]*y[i]
    sum21 <- sum21 + x[i]^2
    sum22 <- sum22 + y[i]^2
  }
  
  # Mean of x and y
  mean_x <- sum1 / N
  mean_y <- sum2 / N
  
  # Cross-products
  cross_products <- sum12 - N*mean_x*mean_y
  print(paste("Cross-products of", col1, "and", col2, ": ", cross_products))
  
  # Covariance
  covariance <- cross_products / (N-1)
  print(paste("Covariance between", col1, "and", col2, ": ", covariance))
  
  # Standard deviation of x and y
  var_x <- (sum21 - N*mean_x^2) / (N-1)
  var_y <- (sum22 - N*mean_y^2) / (N-1)
  sd_x <- sqrt(var_x)
  sd_y <- sqrt(var_y)
  
  # Correlation
  correlation <- covariance / (sd_x * sd_y)
  print(paste("Correlation between", col1, "and", col2, ": ", correlation))
}

# Call function for all pairs of variables
variables <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")
for(i in 1:(length(variables)-1)){
  for(j in (i+1):length(variables)){
    statistics(data, variables[i], variables[j])
  }
}


# I wrote the statistics_pair_group function to calculate the group-wise statistics of pairs between two columns.
# data: data frame to process
# col1, col2: columns for calculating statistics
# group_col: column to use for grouping 
# gender_col: the column to use to parse by gender 

statistics_group <- function(data, cols, group_col = NULL, gender_col = NULL){
  # A list to hold the results
  results <- list()
  
  if (!is.null(group_col)){
    groups <- unique(data[[group_col]])
    for(group in groups){
      for(col in cols){
        subset_data <- subset(data, data[[group_col]] == group)
        
        # Get the column data, excluding NA values
        column_data <- subset_data[[col]]
        column_data <- column_data[!is.na(column_data)]
        
        stats <- statistics(column_data)
        stats$Column <- col
        stats$Group <- group
        stats$Gender <- NA
        
        # Add the calculated statistics to the result list
        results <- c(results, list(stats))
      }
    }
  }
  
  if (!is.null(gender_col)){
    genders <- unique(data[[gender_col]])
    for(gender in genders){
      for(col in cols){
        subset_data <- subset(data, data[[gender_col]] == gender)
        
        # Get the column data, excluding NA values
        column_data <- subset_data[[col]]
        column_data <- column_data[!is.na(column_data)]
        
        stats <- statistics(column_data)
        stats$Column <- col
        stats$Gender <- gender
        stats$Group <- NA
        
        results <- c(results, list(stats))
      }
    }
  }
  
  if (!is.null(group_col) & !is.null(gender_col)){
    groups <- unique(data[[group_col]])
    genders <- unique(data[[gender_col]])
    
    # Do the operations for each group and gender
    for(group in groups){
      for(gender in genders){
        for(col in cols){
          subset_data <- subset(data, data[[group_col]] == group & data[[gender_col]] == gender)
          
          # Get the column data, excluding NA values
          column_data <- subset_data[[col]]
          column_data <- column_data[!is.na(column_data)]
          
          # Calculate statistics
          stats <- statistics(column_data)
          stats$Column <- col
          stats$Group <- group
          stats$Gender <- gender
          # Add the calculated statistics to the result list
          results <- c(results, list(stats))
        }
      }
    }
  }
  
  # Convert results to a data frame
  results_df <- do.call(rbind, lapply(results, as.data.frame))
  return(results_df)
}

statistics <- function(column_data){
  
  # Number of observations
  N <- length(column_data)
  
  # Minimum
  min_val <- column_data[1]
  for(val in column_data){
    if(val < min_val){
      min_val <- val
    }
  }
  
  # Maximum
  max_val <- column_data[1]
  for(val in column_data){
    if(val > max_val){
      max_val <- val
    }
  }
  
  # Range
  range_val <- max_val - min_val
  
  # Sum
  sum_val <- 0
  for(val in column_data){
    sum_val <- sum_val + val
  }
  
  # Mean
  mean_val <- sum_val / N
  
  # Median
  sorted_data <- sort(column_data)
  if (N %% 2 == 0){
    median_val <- (sorted_data[N/2] + sorted_data[N/2 + 1])/2
  } else {
    median_val <- sorted_data[(N+1)/2]
  }
  
  # Sum of squares
  sum_squares <- 0
  for(val in column_data){
    sum_squares <- sum_squares + val^2
  }
  
  # Variance
  var_val <- 0
  for(val in column_data){
    var_val <- var_val + (val - mean_val)^2
  }
  var_val <- var_val / (N-1)
  
  # Standard deviation
  sd_val <- sqrt(var_val)
  
  stats <- list(
    "Number of Observations" = N,
    "Minimum" = min_val,
    "Maximum" = max_val,
    "Range" = range_val,
    "Sum" = sum_val,
    "Mean" = mean_val,
    "Median" = median_val,
    "Sum of Squares" = sum_squares,
    "Variance" = var_val,
    "Standard Deviation" = sd_val
  )
  return(stats)
}

# Call function for the variables you want
variables <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")
result <- statistics_group(data, variables, "Group", "Gender")
options(max.print = 9999)
print(result)


# I created statistics_pair_group as a function that calculates the statistics of pairs between two columns by group and/or gender.
# data: data frame to process
# col1, col2: columns for calculating statistics
# group_col: column to use for grouping 
# gender_col: the column to use to parse by gender 
statistics_pair_group <- function(data, col1, col2, group_col = NULL, gender_col = NULL){
  results <- list()
  
  if (!is.null(group_col)){
    groups <- unique(data[[group_col]])
    for(group in groups){
      subset_data <- subset(data, data[[group_col]] == group)
      
      stats <- statistics_pair(subset_data, col1, col2)
      stats$Column1 <- col1
      stats$Column2 <- col2
      stats$Group <- group
      stats$Gender <- NA
      
      results <- c(results, list(stats))
    }
  }
  
  if (!is.null(gender_col)){
    genders <- unique(data[[gender_col]])
    for(gender in genders){
      subset_data <- subset(data, data[[gender_col]] == gender)
      
      stats <- statistics_pair(subset_data, col1, col2)
      stats$Column1 <- col1
      stats$Column2 <- col2
      stats$Gender <- gender
      stats$Group <- NA
      
      results <- c(results, list(stats))
    }
  }
  
  if (!is.null(group_col) & !is.null(gender_col)){
    groups <- unique(data[[group_col]])
    genders <- unique(data[[gender_col]])
    
    for(group in groups){
      for(gender in genders){
        subset_data <- subset(data, data[[group_col]] == group & data[[gender_col]] == gender)
        
        stats <- statistics_pair(subset_data, col1, col2)
        stats$Column1 <- col1
        stats$Column2 <- col2
        stats$Group <- group
        stats$Gender <- gender
        
        results <- c(results, list(stats))
      }
    }
  }
  
  results_df <- do.call(rbind, lapply(results, as.data.frame))
  return(results_df)
}

statistics_pair <- function(data, col1, col2){
  # Get the column data, excluding NA values
  x <- data[[col1]]
  y <- data[[col2]]
  common_indices <- !(is.na(x) | is.na(y))
  x <- x[common_indices]
  y <- y[common_indices]
  
  # Number of observations
  N <- length(x)
  
  # Sum of x, y, x*y, x^2 and y^2
  sum1 <- 0
  sum2 <- 0
  sum12 <- 0
  sum21 <- 0
  sum22 <- 0
  for(i in 1:N){
    sum1 <- sum1 + x[i]
    sum2 <- sum2 + y[i]
    sum12 <- sum12 + x[i]*y[i]
    sum21 <- sum21 + x[i]^2
    sum22 <- sum22 + y[i]^2
  }
  
  # Mean of x and y
  mean_x <- sum1 / N
  mean_y <- sum2 / N
  
  # Cross-products
  cross_products <- sum12 - N*mean_x*mean_y
  
  # Covariance
  covariance <- cross_products / (N-1)
  
  # Standard deviation of x and y
  var_x <- (sum21 - N*mean_x^2) / (N-1)
  var_y <- (sum22 - N*mean_y^2) / (N-1)
  sd_x <- sqrt(var_x)
  sd_y <- sqrt(var_y)
  
  # Correlation
  correlation <- covariance / (sd_x * sd_y)
  
  stats <- list(
    "Cross-products" = cross_products,
    "Covariance" = covariance,
    "Correlation" = correlation
  )
  return(stats)
}

# Call function for all pairs of variables
variables <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")
results <- list()
for(i in 1:(length(variables)-1)){
  for(j in (i+1):length(variables)){
    pair_stats <- statistics_pair_group(data, variables[i], variables[j], group_col = "Group", gender_col = "Gender")
    results <- append(results,list(pair_stats))
  }
}
results_df <- do.call(rbind, results)
print(results_df)


# Function to draw a scatterplot matrix for continuous variables in a data frame, handling missing values
draw_scatterplot_matrix <- function(data) {
  variables <- colnames(data)
  num_vars <- length(variables)
  
  # Filter continuous variables
  
  continuous_vars <- variables[!(variables %in% c("IdNo", "Group", "Gender"))]
  num_continuous_vars <- length(continuous_vars)
  # Set the figure layout to create a grid of plots with dimensions equal to the number of continuous variables
  par(mfrow = c(num_continuous_vars, num_continuous_vars), mar = c(2, 2, 1, 1))
  
  # Iterate over each variable combination
  for (i in 1:num_continuous_vars) {
    for (j in 1:num_continuous_vars) {
      x_var <- continuous_vars[i]
      y_var <- continuous_vars[j]
      # Filter rows with complete observations for both variables
      valid_data <- data[complete.cases(data[c(x_var, y_var)]), ]
      
      # Draw scatterplot with labels
      plot(valid_data[[x_var]], valid_data[[y_var]], xlab = x_var, ylab = y_var, main = "")
      text(x = mean(valid_data[[x_var]]), y = mean(valid_data[[y_var]]), labels = c(x_var, y_var), pos = 4)
    }
  }
  
  # Reset the plot layout
  par(mfrow = c(1, 1))
}

draw_scatterplot_matrix(data)


# Function to scale continuous variables in a data frame
scale_continuous_variables <- function(data) {
  # Create a copy of the original data frame
  scaled_data <- data
  
  # Select continuous variables
  continuous_vars <- colnames(data)[colnames(data) %in% c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")]
  par(mar = c(2, 2, 1, 1))  # Adjust the margins for labels
  # Iterate over each continuous variable
  for (var in continuous_vars) {
    # Get the variable data
    variable <- scaled_data[[var]]
    
    # Compute the mean and standard deviation
    var_mean <- custom_mean(variable)
    var_sd <- custom_sd(variable)
    
    # Scale the variable
    scaled_variable <- custom_scale(variable, var_mean, var_sd)
    
    # Replace the original variable with the scaled variable
    scaled_data[[var]] <- scaled_variable
  }
  
  return(scaled_data)
}

# Custom mean function
custom_mean <- function(data) {
  valid_data <- data[!is.na(data)]
  mean_val <- sum(valid_data) / length(valid_data)
  return(mean_val)
}

# Custom standard deviation function
custom_sd <- function(data) {
  valid_data <- data[!is.na(data)]
  mean_val <- custom_mean(data)
  sum_sq_diff <- sum((valid_data - mean_val)^2)
  sd_val <- sqrt(sum_sq_diff / (length(valid_data) - 1))
  return(sd_val)
}

# Custom scaling function
custom_scale <- function(data, mean_val, sd_val) {
  scaled_data <- (data - mean_val) / sd_val
  return(scaled_data)
}
scale_continuous_variables(data)
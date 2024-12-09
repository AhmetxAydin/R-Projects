setwd("C:/RProject")
data <- read.table("Data.txt", header = TRUE)


# We write the linear regression function.
linear_regression <- function(Y, X) {
 
  X <- cbind(1, X)
  
  # Setting the regression model
  coefficients <- solve(t(X) %*% X ) %*% t(X) %*% Y
  
  # Estimated Y values
  Y_hat <- X %*% coefficients
  
  # residuals Values
  residuals <- Y - Y_hat
  
  # Calculating the Total Sum of Squares (TSS)
  TSS <- sum((Y - mean(Y))^2)
  
  # Calculate Regression Model Sum of Squares (RMSS)
  RMSS <- sum((Y_hat - mean(Y))^2)
  
  # Calculating the Sum of Squares of Residuals (RSS)
  RSS <- sum(residuals^2)
  
  # Calculating the R-squared value
  R_square <- RMSS / TSS
  
  # I returned all the results here.
  return(list(
    coefficients = coefficients,
    Y_hat = Y_hat,
    residuals = residuals,
    TSS = TSS,
    RMSS = RMSS,
    RSS = RSS,
    R_square = R_square
  ))
}

# Simple Linear Regression example I calculated for X1 and 
# I gave them all under each other in other values.

result_simple_x1<- linear_regression(data$Y, data$X1)
result_simple_x2 <- linear_regression(data$Y, data$X2)
result_simple_x3 <- linear_regression(data$Y, data$X3)
result_simple_x4 <- linear_regression(data$Y, data$X4)
result_simple_x5 <- linear_regression(data$Y, data$X5)
result_simple_x6 <- linear_regression(data$Y, data$X6)
result_simple_x7 <- linear_regression(data$Y, data$X7)
  

# Printing calculated values
print(result_simple_x1)
print(result_simple_x2)
print(result_simple_x3)
print(result_simple_x4)
print(result_simple_x5)
print(result_simple_x6)
print(result_simple_x7)


# I am writing the multiple linear regression function here 
multiple_linear_regression <- function(Y, X) {
  
  X <- cbind(1, X)
  
  # I set the regression model
  coefficients <- solve(t(X) %*% as.matrix(X)) %*% t(X) %*% Y
  
  # Predicted Y values
  Y_hat <- as.matrix(X) %*% coefficients
  
  # residuals Values
  residuals <- Y - Y_hat
  
  # Calculating the Total Sum of Squares (TSS)
  TSS <- sum((Y - mean(Y))^2)
  
  # Calculate Regression Model Sum of Squares (RMSS)
  RMSS <- sum((Y_hat - mean(Y))^2)
  
  # Calculating the Sum of Squares of Residuals (RSS)
  RSS <- sum(residuals^2)
  
  # Calculating the R-squared value
  R_square <- RMSS / TSS
  
  # As a result, I also returned all values here.
  return(list(
    coefficients = coefficients,
    Y_hat = Y_hat,
    residuals = residuals,
    TSS = TSS,
    RMSS = RMSS,
    RSS = RSS,
    R_square = R_square
  ))
}

# I called the function and did the operations.
result_multiple <- multiple_linear_regression(data$Y, data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")])

# All calculated values are printed here 
print(result_multiple)

# here only the desired values can be printed.
print("Coefficients:")
print(result_multiple$coefficients)
print("Predicted Y values:")
print(result_multiple$Y_hat)
print("Residuals:")
print(result_multiple$residuals)
print("Total Sum of Squares (TSS):")
print(result_multiple$TSS)
print("Regression Model Sum of Squares (RMSS):")
print(result_multiple$RMSS)
print("Residual Sum of Squares (RSS):")
print(result_multiple$RSS)
print("R-square value:")
print(result_multiple$R_square)



# Function to perform model selection based on R-square
perform_model_selection <- function(data) {
  
  # Extracting column names of independent variables 
  independent_vars <- grep("^X", names(data), value = TRUE)
  
  # Define the dependent variable
  dependent_var <- "Y"
  
  # Create an empty dataframe to store results
  result_df <- data.frame(
    Model = character(),
    Num_Variables = numeric(),
    Variables = character(),
    TSS = numeric(),
    RMSS = numeric(),
    RSS = numeric(),
    R_Square = numeric(),
    stringsAsFactors = FALSE
  )
  
 
  for (i in 1:length(independent_vars)) {
    combinations <- combn(independent_vars, i)
    num_combinations <- ncol(combinations)
    for (j in 1:num_combinations) {
      # Formulate the formula for regression
      formula <- as.formula(paste(dependent_var, "~", paste(combinations[,j], collapse = "+")))
      
      # Fit the regression model
      model <- lm(formula, data = data)
      
      # Calculate total sum of squares (TSS)
      TSS <- sum((data[[dependent_var]] - mean(data[[dependent_var]]))^2)
      
      # Calculate regression model sum of squares (RMSS)
      RMSS <- sum((fitted(model) - mean(data[[dependent_var]]))^2)
      
      # Calculate residual sum of squares (RSS)
      RSS <- sum(model$residuals^2)
      
      # Calculate R-square
      R_Square <- summary(model)$r.squared
      
      # Append results to the dataframe
      result_df <- rbind(result_df, data.frame(
        Model = paste0("Model ", j),
        Num_Variables = i,
        Variables = paste(combinations[,j], collapse = ", "),
        TSS = TSS,
        RMSS = RMSS,
        RSS = RSS,
        R_Square = R_Square
      ))
    }
  }
  
  # Return the dataframe sorted by number of variables and then by R-square in descending order
  result_df <- result_df[order(result_df$Num_Variables, -result_df$R_Square),]
  return(result_df)
}

# Perform model selection
result <- perform_model_selection(data)
print(result)
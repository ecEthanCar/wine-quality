wine = read.csv('wine-quality-white-and-red.csv',header=T,na.strings='?',stringsAsFactor=T)
wine = na.omit(wine)

splits = split(wine,wine$type)
red_wine_data <- splits$red
white_wine_data <- splits$white

library(gam)
#library(mgcv)

# GAM for red wine with 10-fold CV error
# Assuming red_wine_data is your dataset and quality is the response variable
set.seed(123) # for reproducibility

# Shuffle the data
red_wine_data <- red_wine_data[sample(nrow(red_wine_data)), ]

# Create 10 equally sized folds
folds <- cut(seq(1, nrow(red_wine_data)), breaks = 10, labels = FALSE)

# Initialize an empty vector to store the MSE for each fold
mse_values <- vector(length = 10)

# Perform 10-fold cross-validation
for(i in 1:10) {
  # Split the data into training and test sets
  test_indexes <- which(folds == i, arr.ind = TRUE)
  test_data <- red_wine_data[test_indexes, ]
  train_data <- red_wine_data[-test_indexes, ]
  
  # Fit the GAM model on the training set
  gam_model <- gam(quality ~ s(fixed.acidity, 5) + s(volatile.acidity, 5) + s(citric.acid, 5) +
                   s(residual.sugar, 5) + s(chlorides, 5) + s(free.sulfur.dioxide, 5) +
                   s(total.sulfur.dioxide, 5) + s(density, 5) + s(pH, 5) +
                   s(sulphates, 5) + s(alcohol, 5),
                   data = train_data)
  
  # Predict on the test set
  predictions <- predict(gam_model, test_data)
  
  # Calculate MSE for this fold and store it
  mse_values[i] <- mean((test_data$quality - predictions) ^ 2)
}

# The cross-validated MSE is the mean of the MSE values from each fold
cv_mse <- mean(mse_values)
cv_mse


# GAM for white wine with 10-fold CV error
# Assuming red_wine_data is your dataset and quality is the response variable
set.seed(123) # for reproducibility

# Shuffle the data
white_wine_data <- white_wine_data[sample(nrow(white_wine_data)), ]

# Create 10 equally sized folds
folds <- cut(seq(1, nrow(white_wine_data)), breaks = 10, labels = FALSE)

# Initialize an empty vector to store the MSE for each fold
mse_values <- vector(length = 10)

# Perform 10-fold cross-validation
for(i in 1:10) {
  # Split the data into training and test sets
  test_indexes <- which(folds == i, arr.ind = TRUE)
  test_data <- white_wine_data[test_indexes, ]
  train_data <- white_wine_data[-test_indexes, ]
  
  # Fit the GAM model on the training set
  gam_model <- gam(quality ~ s(fixed.acidity, 5) + s(volatile.acidity, 5) + s(citric.acid, 5) +
                   s(residual.sugar, 5) + s(chlorides, 5) + s(free.sulfur.dioxide, 5) +
                   s(total.sulfur.dioxide, 5) + s(density, 5) + s(pH, 5) +
                   s(sulphates, 5) + s(alcohol, 5),
                   data = train_data)
  
  # Predict on the test set
  predictions <- predict(gam_model, test_data)
  
  # Calculate MSE for this fold and store it
  mse_values[i] <- mean((test_data$quality - predictions) ^ 2)
}

# The cross-validated MSE is the mean of the MSE values from each fold
w_cv_mse <- mean(mse_values)
w_cv_mse


# Implement a simple regression tree for red wine first
library(tree)

# Assuming red_wine_data is your dataset and quality is the response variable
set.seed(123) # for reproducibility

# Shuffle the data
red_wine_data <- red_wine_data[sample(nrow(red_wine_data)), ]

# Create 10 equally sized folds
folds <- cut(seq(1, nrow(red_wine_data)), breaks = 10, labels = FALSE)

# Initialize an empty vector to store the MSE for each fold
mse_values <- vector(length = 10)

# Perform 10-fold cross-validation
for(i in 1:10) {
  # Split the data into training and test sets
  test_indexes <- which(folds == i, arr.ind = TRUE)
  test_data <- red_wine_data[test_indexes, ]
  train_data <- red_wine_data[-test_indexes, ]

  # Fit a regression tree to the training set
  tree_model <- tree(quality ~ ., data = train_data)

  # Predict on the test set
  tree_pred <- predict(tree_model, newdata = test_data)

  # Calculate the test MSE
  mse_values[i] <- mean((test_data$quality - tree_pred)^2)
}

# The cross-validated MSE is the mean of the MSE values from each fold
tree_cv_mse <- mean(mse_values)
tree_cv_mse


# Implement a simple regression tree for red wine first
library(tree)

# Assuming red_wine_data is your dataset and quality is the response variable
set.seed(123) # for reproducibility

# Shuffle the data
white_wine_data <- white_wine_data[sample(nrow(white_wine_data)), ]

# Create 10 equally sized folds
folds <- cut(seq(1, nrow(white_wine_data)), breaks = 10, labels = FALSE)

# Initialize an empty vector to store the MSE for each fold
mse_values <- vector(length = 10)

# Perform 10-fold cross-validation
for(i in 1:10) {
  # Split the data into training and test sets
  test_indexes <- which(folds == i, arr.ind = TRUE)
  test_data <- white_wine_data[test_indexes, ]
  train_data <- white_wine_data[-test_indexes, ]

  # Fit a regression tree to the training set
  tree_model <- tree(quality ~ ., data = train_data)

  # Predict on the test set
  tree_pred <- predict(tree_model, newdata = test_data)

  # Calculate the test MSE
  mse_values[i] <- mean((test_data$quality - tree_pred)^2)
}

# The cross-validated MSE is the mean of the MSE values from each fold
tree_cv_mse_w <- mean(mse_values)
tree_cv_mse_w







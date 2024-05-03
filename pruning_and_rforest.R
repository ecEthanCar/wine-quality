wine = read.csv('wine-quality-white-and-red.csv',header=T,na.strings='?',stringsAsFactor=T)
wine = na.omit(wine)

#split into reds and whites
white_wine_data <- wine[wine$type == "white", 2:13]
red_wine_data <- wine[wine$type == "red", 2:13]

library(tree)

# pruned tree for white wine(with optimal size reported)
white.test.MSEs.tree <- numeric(5)
white.test.MSEs.size <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  
  #generate train set
  train <- sample(1:nrow(white_wine_data), nrow(white_wine_data) / 2)
  
  # Fit the full tree model
  tree_model <- tree(quality ~ ., white_wine_data,subset = train)

  # Cross-validate the tree to find the optimal complexity
  cv_tree <- cv.tree(tree_model, FUN=prune.tree)

  # Plot the MSE as a function of tree size
  #plot(cv_tree$size, cv_tree$dev, type='b', xlab="Number of Terminal Nodes", ylab="Cross-Validated MSE")

  # Prune the tree to the optimal level of complexity
  optimal_size <- cv_tree$size[which.min(cv_tree$dev)]
  pruned_tree <- prune.tree(tree_model, best = optimal_size)
  
  # Record optimal size for each iteration
  white.test.MSEs.size[i] <- optimal_size

  #test MSE
  yhat.tree.white <- predict(pruned_tree , newdata = white_wine_data[-train , ]) #change first param to be best tree
  white.test <- white_wine_data[-train, "quality"]
  white.test.MSEs.tree[i] <- mean((yhat.tree.white - white.test)^2)
}
white.test.MSEs.size
white.test.MSEs.tree
mean(white.test.MSEs.tree)


# pruned tree for red wine(with optimal size reported)
red.test.MSEs.tree <- numeric(5)
red.test.MSEs.size <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  
  #generate train set
  train <- sample(1:nrow(red_wine_data), nrow(red_wine_data) / 2)
  
  # Fit the full tree model
  tree_model <- tree(quality ~ ., red_wine_data,subset = train)

  # Cross-validate the tree to find the optimal complexity
  cv_tree <- cv.tree(tree_model, FUN=prune.tree)

  # Plot the MSE as a function of tree size
  #plot(cv_tree$size, cv_tree$dev, type='b', xlab="Number of Terminal Nodes", ylab="Cross-Validated MSE")

  # Prune the tree to the optimal level of complexity
  optimal_size <- cv_tree$size[which.min(cv_tree$dev)]
  pruned_tree <- prune.tree(tree_model, best = optimal_size)
  
  # Record optimal size for each iteration
  red.test.MSEs.size[i] <- optimal_size

  #test MSE
  yhat.tree.red <- predict(pruned_tree , newdata = red_wine_data[-train , ]) #change first param to be best tree
  red.test <- red_wine_data[-train, "quality"]
  red.test.MSEs.tree[i] <- mean((yhat.tree.red - red.test)^2)
}
red.test.MSEs.size
red.test.MSEs.tree
mean(red.test.MSEs.tree)

library(randomForest)

# random forest for white wine (using Out-of-Bag Error for optimal m-try)
white.test.MSEs.tree <- numeric(5)
white.test.MSEs.mtry <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  
  #generate train set
  train <- sample(1:nrow(white_wine_data), nrow(white_wine_data) / 2)
  train_set <- white_wine_data[train, ]
  test_set <- white_wine_data[-train, ]

  # Find the optimal m-try 
  opt_mtry <- tuneRF(train_set[, -which(names(train_set) == "quality")],
                     train_set[["quality"]],
                     stepFactor = 1.5,
                     improve = 0.01,
                     ntreeTry = 500,
                     trace = TRUE,
                     plot = TRUE)
  print(opt_mtry)

  # The best m-try value is stored in
  best_mtry_index <- which.min(opt_mtry[,2])  # Find index of the minimum OOB error in the second row
  best_mtry <- opt_mtry[best_mtry_index, 1] 
  white.test.MSEs.mtry[i] <- best_mtry

  # Fit model on training data with the optimal m-try
  rf_model <- randomForest(quality ~ ., data = train_set, mtry = best_mtry, ntree = 500)

  #test MSE
  yhat.tree.white <- predict(rf_model , newdata = white_wine_data[-train , ]) #change first param to be best tree
  white.test <- white_wine_data[-train, "quality"]
  white.test.MSEs.tree[i] <- mean((yhat.tree.white - white.test)^2)
}
white.test.MSEs.mtry
white.test.MSEs.tree
mean(white.test.MSEs.tree)


# random forest for red wine (using Out-of-Bag Error for optimal m-try)
red.test.MSEs.tree <- numeric(5)
red.test.MSEs.mtry <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  
  #generate train set
  train <- sample(1:nrow(red_wine_data), nrow(red_wine_data) / 2)
  train_set <- red_wine_data[train, ]
  test_set <- red_wine_data[-train, ]

  # Find the optimal m-try 
  opt_mtry <- tuneRF(train_set[, -which(names(train_set) == "quality")],
                     train_set[["quality"]],
                     stepFactor = 1.5,
                     improve = 0.01,
                     ntreeTry = 500,
                     trace = TRUE,
                     plot = TRUE)
  print(opt_mtry)

  # The best m-try value is stored in
  best_mtry_index <- which.min(opt_mtry[,2])  # Find index of the minimum OOB error in the second row
  best_mtry <- opt_mtry[best_mtry_index, 1] 
  red.test.MSEs.mtry[i] <- best_mtry

  # Fit model on training data with the optimal m-try
  rf_model <- randomForest(quality ~ ., data = train_set, mtry = best_mtry, ntree = 500)

  #test MSE
  yhat.tree.red <- predict(rf_model , newdata = red_wine_data[-train , ]) #change first param to be best tree
  red.test <- red_wine_data[-train, "quality"]
  red.test.MSEs.tree[i] <- mean((yhat.tree.red - red.test)^2)
}
red.test.MSEs.mtry
red.test.MSEs.tree
mean(red.test.MSEs.tree)




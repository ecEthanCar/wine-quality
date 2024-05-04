getwd()
setwd("~/Classes/stsci4740/finalproject")
wine <- read.csv("wine-quality-white-and-red.csv")

#split into reds and whites
White <- wine[wine$type == "white", 2:13]
Red <- wine[wine$type == "red", 2:13]
length(White)

library(tree)

#regression tree for white
white.test.MSEs.tree <- numeric(5)
white.test.MSEs.size <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  
  #generate train set
  train <- sample(1:nrow(White), nrow(White) / 2)
  
  regtree.white <- tree(quality ~., White, subset = train)
  summary(regtree.white)
  
  #plot(regtree.white)
  #text(regtree.white, pretty = 0)
  
  #prune tree
  cv.white <- cv.tree(regtree.white, FUN=prune.tree)
  #plot(cv.white$size , cv.white$dev, type = "b")
  
  # Prune the tree to the optimal level of complexity
  optimal_size <- cv.white$size[which.min(cv.white$dev)]
  pruned.white <- prune.tree(regtree.white, best = optimal_size)
  
  # Record optimal size for each iteration
  white.test.MSEs.size[i] <- optimal_size
  
  #plot(pruned.white)
  #text(pruned.white , pretty = 0)
  
  #test MSE
  yhat.tree.white <- predict(pruned.white , newdata = White[-train , ]) #change first param to be best tree
  white.test <- White[-train, "quality"]
  #plot(yhat.tree.white , white.test)
  #abline(0, 1)
  white.test.MSEs.tree[i] <- mean((yhat.tree.white - white.test)^2)
}
white.test.MSEs.tree
mean(white.test.MSEs.tree)
white.test.MSEs.size


#regression tree for red
red.test.MSEs.tree <- numeric(5)
red.test.MSEs.size <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  
  #generate train set
  train <- sample(1:nrow(Red), nrow(Red) / 2)
  
  regtree.red <- tree(quality ~., Red, subset = train)
  summary(regtree.red)
  
  #plot(regtree.red)
  #text(regtree.red, pretty = 0)
  
  #prune tree
  cv.red <- cv.tree(regtree.red, FUN=prune.tree)
  #plot(cv.red$size , cv.red$dev, type = "b")
  
  # Prune the tree to the optimal level of complexity
  optimal_size <- cv.red$size[which.min(cv.red$dev)]
  pruned.red <- prune.tree(regtree.red, best = optimal_size)
  
  # Record optimal size for each iteration
  red.test.MSEs.size[i] <- optimal_size
  
  #test MSE
  yhat.tree.red <- predict(pruned.red , newdata = Red[-train , ]) #change first param to be best tree
  red.test <- Red[-train, "quality"]
  #plot(yhat.tree.red , red.test)
  #abline(0, 1)
  red.test.MSEs.tree[i] <- mean((yhat.tree.red - red.test)^2)
}
red.test.MSEs.tree
mean(red.test.MSEs.tree)
red.test.MSEs.size


# Bagging for white
library(randomForest)
white.test.MSEs.bag <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  #generate train set
  train <- sample(1:nrow(White), nrow(White) / 2)
  
  bag.white <- randomForest(quality ~ ., data = White ,
                subset = train, mtry = 11, importance = TRUE) #mtry = number of predictors to consider
  print(bag.white)
  
  #test MSE
  yhat.bag.white <- predict(bag.white , newdata = White[-train , ])
  white.test <- White[-train, "quality"]
  #plot(yhat.bag.white , white.test)
  #abline(0, 1)
  white.test.MSEs.bag[i] <- mean((yhat.bag.white - white.test)^2)
}
white.test.MSEs.bag
mean(white.test.MSEs.bag)

# Bagging for red
library(randomForest)
red.test.MSEs.bag <- numeric(5)
for (i in 1:5) {
  set.seed(i)
  #generate train set
  train <- sample(1:nrow(Red), nrow(Red) / 2)
  
  bag.red <- randomForest(quality ~ ., data = Red ,
                            subset = train, mtry = 11, importance = TRUE)
  print(bag.red)
  
  #test MSE
  yhat.bag.red <- predict(bag.red, newdata = Red[-train , ])
  red.test <- Red[-train, "quality"]
  #plot(yhat.bag.red , red.test)
  #abline(0, 1)
  red.test.MSEs.bag[i] <- mean((yhat.bag.red - red.test)^2)
}
red.test.MSEs.bag
mean(red.test.MSEs.bag)

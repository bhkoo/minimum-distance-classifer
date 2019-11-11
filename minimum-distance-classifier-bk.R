### Minimum distance classifier
### Bonhwang Koo
### https://www.kaggle.com/olhacher/minimum-distance-classifier/data

### Read in data
setwd("/Users/Bonhwang/Documents/Github/minimum-distance-classifier")
train <- read.csv("./input/train.csv")

### rotate
### Rotates matrix by reversing and then transposing
rotate <- function(x) t(apply(x, 2, rev))

### Visualization
set.seed(71)

par(mfrow = c(10, 10), mar = c(0.1, 0.1, 0.1, 0.1))

for (index in sample(which(train$label == 1), 100)) {
  img <- matrix(data = unlist(rev(train[index, -1])), nrow = 28, ncol = 28, byrow = FALSE)
  img <- apply(img, 2, rev)
  image(img, col = heat.colors(12), axes = FALSE)
}

### Split data into test and train sets
set.seed(123)
train_index <- sample(1:nrow(train), 28000)
train_X <- train[train_index, -1]
train_Y <- train[train_index, 1]
test_X <- train[-train_index, -1]
test_Y <- train[-train_index, 1]

table(train_Y)
table(test_Y)

### Reduce size of training sets through k-means clustering
train_old <- train[train_index, ]
train_reduced <- NULL
for (i in 0:9) {
  train_subset <- train_old[train_old$label == i, -1]
  clusters <- kmeans(train_subset, centers= 50, iter.max = 20)
  new_data <- cbind(rep(i, 50), clusters$centers)
  train_reduced <- rbind(train_reduced, new_data)
}
train_X <- train_reduced[, -1]
train_Y <- train_reduced[, 1]

### Visualization
set.seed(71)

par(mfrow = c(10, 10), mar = c(0.1, 0.1, 0.1, 0.1))

for (index in sample(nrow(train_X), 100)) {
  img <- matrix(data = unlist(rev(train_X[index, ])), nrow = 28, ncol = 28, byrow = FALSE)
  img <- apply(img, 2, rev)
  image(img, col = heat.colors(12), axes = FALSE)
}

### Predictions!
distances <- NULL
train_X <- as.matrix(train_X)
test_X <- as.matrix(test_X)
m <- nrow(train_X)
n <- nrow(test_X)

for (j in 1:n) {
  dist <- rowSums((train_X - t(replicate(m, test_X[j, ])))^2)
  distances <- cbind(distances, dist)
}
distances <- t(distances)

rows <- apply(distances, 1, which.min)
predictions <- data.frame(predicted = train_Y[rows], actual = test_Y, cluster = rows)
table(predictions$predicted, predictions$actual)

print(paste0("Accuracy: ", sum(predictions$predicted == predictions$actual)/nrow(predictions) * 100, "%"))

### Visualization TK
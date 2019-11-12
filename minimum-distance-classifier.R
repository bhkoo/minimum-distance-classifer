
### Load data
setwd("/Users/Bonhwang/Documents/Github/minimum-distance-classifier")
train <- read.csv("./input/train.csv")

### Visualization
set.seed(71)
data <- sample(as.integer(row.names(train[train$label == 1, ])), 100)
par(mfrow=c(10,10), mar=c(0.1, 0.1, 0.1, 0.1))

for (k in data) {
  row <- NULL
  for (n in 2:785) {
    row[n-1] <- train[k, n]
  }
  matrix1 <- matrix(row, 28, 28, byrow = FALSE)
  matrix2 <- matrix(rep(0, 784), 28, 28)
  
  for (i in 1:28) {
    for (j in 1:28) {
      matrix2[i, 28 - j + 1] <- matrix1[i, j]
    }
  }
  if (k == 26611) {
      image(matrix2, axes = FALSE, col = heat.colors(12))
  } else {
    image(matrix2, axes = FALSE, col = topo.colors(12))
  }
}

rm(i, n, j, k, row, matrix1, matrix2)

### Split data into train and test sets
set.seed(25)
intest <- sample(1:42000, 10000)
X_train <- train[-intest, -1]
Y_train <- train[-intest, 1]
X_test <- train[intest, -1]
Y_test <- train[intest, 1]

table(Y_train)
table(Y_test)


### Decrease size of train set
raw_train_set <- train[-intest, ]
final_train_set <- NULL

for(i in 0:9) {
  digit <- raw_train_set[raw_train_set$label == i, -1]
  set.seed(222)
  cluster <- kmeans(x = digit, centers = 50, iter.max = 20)
  new_data <- cbind(rep(i, 50), cluster$centers)
  final_train_set <- rbind(final_train_set, new_data)
}
final_train_set <- as.data.frame(final_train_set)

X_train <- final_train_set[, -1]
Y_train <- final_train_set[, 1]
table(Y_train)

rm(digit, cluster, new_data, i, raw_train_set, final_train_set)

data <- sample(1:500, 100)
par(mfrow=c(10,10), mar = c(0.1, 0.1, 0.1, 0.1))
for (k in data) {
  row <- NULL
  for (n in 1:784) {
    row[n] <- X_train[k, n]
  }
  
  matrix1 <- matrix(row, 28, 28, byrow = FALSE)
  matrix2 <- matrix(rep(0, 784), 28, 28)
  
  for (i in 1:28) {
    for (j in 1:28) {
      matrix2[i, 28 - j + 1] <- matrix1[i, j]
    }
  }
  image(matrix2, axes = FALSE, col = topo.colors(12))
}

rm(i, n, j, k, row, matrix1, matrix2)

### Predictions
# Calculate distances
distances <- NULL
X_train <- as.matrix(X_train)
X_test <- as.matrix(X_test)
m <- dim(X_train)[1]
n <- dim(X_test)[1]

for (j in 1:n) {
  dist <- rowSums((X_train - t(replicate(m, X_test[j,])))^2)
  distances <- cbind(distances, dist)
}
distances <- t(distances)

# Predicting
rows <- NULL
for (i in 1:n) {
  rows <- c(rows, which.min(distances[i, ])[[1]])
}

predictions <- data.frame(predicted = Y_train[rows], actual = Y_test, cluster = rows)
rm(dist, j, m, n, i, rows)

# Model Valuation
accuracy <- function(pred, actual) {
  conf_matrix <- table(predictions = pred, actual = actual)
  sum <- 0
  for(j in 1:10) {
    for (i in 1:10) {
      if (j == i) {
        sum <- sum + conf_matrix[i, j]
      }
    }
  }
  return(list(conf_matrix, sum/length(pred)))
}
accuracy(predictions$predicted, predictions$actual)

### Visualization
par(mfcol = c(14, 14), mar = c(0.1, 0.1, 0.1, 0.1))

data <- 101:198
for (k in data) {
  row <- NULL
  for (n in 1:784) {
    row[n] <- X_test[k, n]
  }
  
  matrix1 <- matrix(row, 28, 28, byrow = FALSE)
  matrix2 <- matrix(rep(0, 784), 28, 28)
  
  for (i in 1:28) {
    for (j in 1:28) {
      matrix2[i, 28 - j + 1] <- matrix1[i, j]
    }
  }
  
  image(matrix2, axes = FALSE, col = topo.colors(12))
}

data <- predictions$cluster[101:198]
for (k in data) {
  row <- NULL
  for (n in 1:784) {
    row[n] <- X_train[k, n]
  }
  
  matrix1 <- matrix(row, 28, 28, byrow = FALSE)
  matrix2 <- matrix(rep(0, 784), 28, 28)
  
  for (i in 1:28) {
    for (j in 1:28) {
      matrix2[i, 28 - j + 1] <- matrix1[i, j]
    }
  }
  
  image(matrix2, axes = FALSE, col = heat.colors(12))
}

rm(i, n, j, k, row, matrix1, matrix2)

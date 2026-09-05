# Step 1: :Load libraries
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!require("EBImage", quietly = TRUE)) {
  BiocManager::install("EBImage", ask = FALSE)
}
library(EBImage)

if (!require("keras3", quietly = TRUE)) {
  install.packages("keras3")
}
library(keras3)


install_keras(backend = "tensorflow")

setwd("C:/Users/admin/Desktop/SEM 7/R language/Lab/Lab problem statement 4/images")
pics <- c(
  "p1.jpg", "p2.jpg", "p3.jpg", "p4.jpg", "p5.jpg", "p6.jpg",
  "c1.jpg", "c2.jpg", "c3.jpg", "c4.jpg", "c5.jpg", "c6.jpg"
)

mypic <- list()

for (i in 1:12) {
  mypic[[i]] <- readImage(pics[i])
}

for (i in 1:12) {
  mypic[[i]] <- resize(mypic[[i]], 28, 28)
}

for (i in 1:12) {
  mypic[[i]] <- array_reshape(mypic[[i]], c(28, 28, 3))
}

x_train <- NULL
for (i in c(1:5, 7:11)) {
  x_train <- rbind(x_train, as.vector(mypic[[i]]))
}

x_test <- rbind(as.vector(mypic[[6]]), as.vector(mypic[[12]]))

y_train <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
y_test <- c(0, 1)

train_labels <- to_categorical(y_train)
test_labels <- to_categorical(y_test)

model <- keras_model_sequential(
  layers = list(
    keras_input(shape = c(2352)),
    layer_dense(units = 256, activation = "relu"),
    layer_dense(units = 128, activation = "relu"),
    layer_dense(units = 2, activation = "softmax")
  )
)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, train_labels,
  epochs = 30,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 0
)

png("history_plot.png")
plot(history)
dev.off()

print("Model Evaluation:")
print(model %>% evaluate(x_train, train_labels, verbose = 0))

prob <- model %>% predict(x_train, verbose = 0)
pred <- max.col(prob) - 1
print("Confusion Matrix:")
print(table(Predicted = pred, Actual = y_train))
print("Probabilities, Predictions, and Actual:")
print(cbind(prob, Predicted = pred, Actual = y_train))

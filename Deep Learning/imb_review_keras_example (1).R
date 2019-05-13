#install.packages("keras")
library(keras)
install_keras()

#hy comment ======
#anaconda install ... path dir .. should be chosen
#PyCharm install 

#if below error occurs, when executing below 2 lines,

# error in multi_assign(substitute(x), value, parent.frame()) :

#fixing it with below
#library(tensor_flow)
#install_tensorflow(version="nightly")
#=== end of hy comment

imdb <- dataset_imdb(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb

str(train_data[[1]])
train_labels[[1]]

vectorize_sequences <- function(sequences, dimension = 10000) {
  # Create an all-zero matrix of shape (len(sequences), dimension)
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    # Sets specific indices of results[i] to 1s
    results[i, sequences[[i]]] <- 1
  results
}

x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)

y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

val_indices <- 1:10000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid") #output layer

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

str(history)
plot(history) #training은 계속 올라가는데 validation set은 오히려 떨어짐 => overfitting 

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)
results

model %>% predict(x_test[1:10,])

# overfitting issue

original_model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")
original_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

smaller_model <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 4, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

smaller_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

original_hist <- original_model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_test, y_test)
)

smaller_model_hist <- smaller_model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_test, y_test)
)

library(ggplot2)
library(tidyr)
plot_training_losses <- function(losses) {
  loss_names <- names(losses)
  losses <- as.data.frame(losses)
  losses$epoch <- seq_len(nrow(losses))
  losses %>% 
    gather(model, loss, loss_names[[1]], loss_names[[2]]) %>% 
    ggplot(aes(x = epoch, y = loss, colour = model)) +
    geom_point()
}

plot_training_losses(losses = list(
  original_model = original_hist$metrics$val_loss,
  smaller_model = smaller_model_hist$metrics$val_loss
))


bigger_model <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

bigger_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c('acc')
)

bigger_model_hist <- bigger_model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_test, y_test)
)

plot_training_losses(losses = list(
  original_model = original_hist$metrics$val_loss,
  bigger_model = bigger_model_hist$metrics$val_loss
))

plot_training_losses(losses = list(
  original_model = original_hist$metrics$loss,
  bigger_model = bigger_model_hist$metrics$loss
))


l2_model <- keras_model_sequential() %>% 
  layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001), #L2 regularization #overfitting해결
              activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001),
              activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

l2_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

l2_model_hist <- l2_model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_test, y_test)
)


plot_training_losses(losses = list(
  original_model = original_hist$metrics$val_loss,
  l2_model = l2_model_hist$metrics$val_loss
))


dpt_model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dropout(rate = 0.5) %>% #rate 0.5 결과는 날리겠다 #random으로!
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>%   #첫번째, 두번째 layer에 대해서 비율을 달리할 수 있다 
  layer_dense(units = 1, activation = "sigmoid")
dpt_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

dpt_model_hist <- dpt_model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_test, y_test)
)

plot_training_losses(losses = list(
    original_model = original_hist$metrics$val_loss,
    dpt_model = dpt_model_hist$metrics$val_loss
))

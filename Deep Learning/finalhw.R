#0. 데이터 로드
load("C:\\Users\\Kim Yuum\\Desktop\\19-1 통계\\머신러닝\\homeworkdata.RData")

#1-1) 전체 데이터에 대해 SVM model 적용
library(e1071)
model.svm = svm(cat~., wordmatrix, cost=1, gamma =1) #cost=1 gamma=1로 파라미터 조정 
model.svm  
svm_pred = predict(model.svm, wordmatrix[,-1])  #적합된 모델로 댓글 분류 결과 예측
result = table(wordmatrix$cat, svm_pred)
acc = (result[1,1]+result[2,2])/sum(result)
acc  #0.98

#1-2) K-fold cross validation 
k = 5
prediction = data.frame()
testsetCopy = data.frame()
wordmatrix$id <- sample(1:k, nrow(wordmatrix), replace = TRUE)
list = 1:k

for(i in 1:k){
  #run svm model
  trainingset <- subset(wordmatrix, id %in% list[-i])
  testset <- subset(wordmatrix, id %in% c(i))

  mymodel = svm(cat~., wordmatrix, cost=1, gamma =1)
  
  #remove response column 1, cat
  temp = as.data.frame(predict(mymodel, wordmatrix[,-1]))
  
  # append this iteration's predictions to the end of the prediction data frame
  prediction = rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the cat Column
  testsetCopy = rbind(testsetCopy, as.data.frame(wordmatrix[,1]))
}

result = cbind(prediction, testsetCopy[, 1])
names(result) = c("Predicted", "Actual")
result
result$Predicted = as.numeric(levels(result$Predicted))[result$Predicted]
result$Actual = as.numeric(levels(result$Actual))[result$Actual]
result$Difference = abs(result$Actual-result$Predicted)
1-mean(result$Difference)

#2-1)
library("caret")
library("tidyr")
library("keras")

index = createDataPartition(y = wordmatrix$cat, p = 0.7, list =FALSE)
train = wordmatrix[index,]
test = wordmatrix[-index,]

x_train = train[,-1]
y_train = train[,1]

x_test = test[,-1]
y_test = test[,1]

y_train=to_categorical(y_train, num_classes = 2)
y_test=to_categorical(y_test, num_classes = 2)

x_train = data.matrix(x_train, rownames.force = NA)
x_test = data.matrix(x_test, rownames.force = NA)


model = keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(7589)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 2, activation = "sigmoid") #output layer

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results = model %>% evaluate(x_train, y_train) #얘를 97%이상 해야돼! #97.7%
results

#2-2)
test_results = model %>% evaluate(x_test, y_test) 
test_results

#2-3: overfitting issue해결하기
dpt_model = keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(7589)) %>% 
  layer_dropout(rate = 0.5) %>% #rate 0.5 결과는 날리겠다 #random으로!
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>%   #첫번째, 두번째 layer에 대해서 비율을 달리할 수 있다 
  layer_dense(units = 2, activation = "sigmoid")

dpt_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

dpt_model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
ov.test_results = dpt_model %>% evaluate(x_test, y_test) 
ov.test_results

#3
library(data.table)
library(text2vec)
library(tm)
vocab <- create_vocabulary(itoken(we_data), ngram = c(1,1))
vocab <- prune_vocabulary(vocab, term_count_min = 5)

iter <- itoken(we_data)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(iter, vectorizer, skip_grams_window = 5) #window=5로 term matrix


glove = GlobalVectors$new(word_vectors_size = 50, vocabulary=vocab, x_max = 10) 
wv_main = glove$fit_transform(tcm, n_iter=20)

wv_context = glove$components
word_vectors = wv_main + t(wv_context)

similar = word_vectors["사업", , drop = FALSE] - 
  word_vectors["대통령", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = similar, method = "cosine", norm = "l2") #위 단어와 유사성이 가장 높은 단어 도출 
head(sort(cos_sim[,1], decreasing = TRUE), 10)  

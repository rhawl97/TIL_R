set.seed(1234)
#install.packages('e1071')
library(e1071)
library(rpart)
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1]
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

index <- 1:nrow(wbcd)
test_data_index <- sample(index, trunc(length(index)/3))
test_data <-wbcd[test_data_index,]
train_data <- wbcd[-test_data_index,]

svm.model <- svm(diagnosis ~., data=train_data, cost=100, gamma =1)  #cost gamma 지정 
svm_pred_train <- predict(svm.model, train_data[,-1])
table(train_data$diagnosis, svm_pred_train)

svm_pred_test <- predict(svm.model, test_data[,-1])
table(test_data$diagnosis, svm_pred_test)  #악성인데 아니라고 나온 에러 71 

linear.tune = tune.svm(diagnosis~., data=train_data, kernel="linear", cost=c(0.001, 0.01, 0.1, 1, 5, 10,100))  #cost키워가며 tuning 
summary(linear.tune)
best.linear = linear.tune$best.model

# svm ?? ????. best.linear vs svm.model ( radial )
best.linear
svm.model

tune.test = predict(best.linear, newdata=train_data)
table(tune.test, train_data$diagnosis)  #tuning 후 best model로 다시 분류 -> 오분류 중 위험한 오분류가 2로 확 줄어듬 + 성능 좋아짐 

poly.tune = tune.svm(diagnosis~., data=train_data, kernel="polynomial", degree=c(3,4,5), coef0=c(0.1, 0.5, 1,2,3,4)) #3,4,5차원 
summary(poly.tune)

best.poly = poly.tune$best.model
poly.test = predict(best.poly, newdata=train_data)
table(poly.test, train_data$diagnosis)

rbf.tune = tune.svm(diagnosis~., data=train_data, kernel="radial",  coef0=c(0.1, 0.5, 1,2,3,4))
summary(rbf.tune)
best.rbf = rbf.tune$best.model
rbf.test = predict(best.rbf, newdata=train_data)
table(rbf.test, train_data$diagnosis)

library(caret)
confusionMatrix(rbf.test, train_data$diagnosis, positive = "Benign")

#CV FOR LINEAR EXAMPLE

rfeCNTL = rfeControl(functions=lrFuncs, method = "cv", number=10)  #어떤 feature를 선택할 것인가 #10fold cv 
svm.features = rfe(train_data[,2:31], train_data[,1],sizes = c(8,7, 6, 5, 4), #sizes: 변수가 8,7,6,5,4인 경우 다 해봐!
                   rfeControl = rfeCNTL, method = "svmLinear")
svm.features #accuracy, Kappa 확인 -> 6개만 써도 비슷한 성능을 보여줌 
svm.features$optVariables #6개 변수가 뭔지 

#
#rfeCNTL = rfeControl(functions=caretFuncs, method = "cv", number=10) #radial #5~6분 소요 
#svm.features = rfe(train_data[,2:31], train_data[,1],sizes = c(8,7, 6, 5, 4),
#                   rfeControl = rfeCNTL, method = "svmRadial")

cost.weights <- c(0.001,0.01,0.1,1,10)
gamma.weights <- c(0.01,0.25,0.5,1)

tuning.results <- tune(svm, diagnosis~., data=train_data, kernel="radial",
                       ranges=list(cost=cost.weights, gamma=gamma.weights))
print(tuning.results)  #24개의 case cv 후 best parameter 도출 -> 과적합 나올 수 있기 때문에 봐야함 -> 이것은 starting point다!여기서부터 잡아나가야 함 
plot(tuning.results, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i") #gamma와 cost에 따라 perfomance 어떤지 plot으로 보여줌 

svm.model.best = tuning.results$best.model 
svm.predictions.best <- predict(svm.model.best, train_data[,2:31])
confusionMatrix(data=svm.predictions.best,reference = train_data[,1])




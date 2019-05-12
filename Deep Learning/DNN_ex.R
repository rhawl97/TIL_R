library("neuralnet")
set.seed(2019)

attribute <- as.data.frame(sample(seq(-2,2, length=50), 50, replace=F), ncol=1) #랜덤 숫자 50개 뽑아냄
response <- attribute^2
test_data <- cbind(attribute, response)
colnames(test_data) <- c("attribute", "response")
head(test_data)  #시뮬레이션을 위해 데이터를 임의적으로 만들어냄
plot(test_data)  #이차함수 형태

fit <- neuralnet(response ~ attribute, data=test_data, hidden=c(3,3),threshold=0.01) #fitting
pred.fit <- compute(fit,test_data) #prediction
plot(fit) #neural net plot #iteration: 5031 error: MSE 

error_calculate = (abs((response - pred.fit$net.result)^2)/2) #sum(error_calcuate) = error

fit.test.data <- as.matrix(sample(seq(-2,2, length=10),10, replace=F), ncol=1)
fit <- neuralnet(response ~ attribute, data=test_data, hidden=c(3,3),threshold=0.01) #fitting

pred <- compute(fit, fit.test.data)
result <- cbind(fit.test.data, pred$net.result, fit.test.data^2)
colnames(result) <- c("Attribute","Prediction","Actual")
round(result,4)
plot(result)

plot(fit)

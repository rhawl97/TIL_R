install.packages('randomForest')
library(randomForest)

# ntree : number of trees. The default is set at 500
# mtry = fixed number of predictors  #노드가 분기할 때마다 추출할 변수의 숫자를 몇개로 할건지 
#           to be selected randomly at every node
# importance : with respect to every category in the response #변수의 중요성과 관련된 수치를 보여줘라
#          one gets information on how important the predcitors in classification
# proximity : claculates proximity scores between any two rows.  

data(iris)
MB <- randomForest(Species ~., data=iris, importance = T, proximity = T) #error rate, confusion matrix 다 만들어줌 #각각의 단계마다 변수 2개씩 선택했음
print(MB)
names(MB) 
round(importance(MB), 2) #보통 관심있어하는 부분 #setosa를 분류할 때는 Petal.Width라는 변수가 제일 중요함 
#MeanDecreaseAccuracy: 변수가 없음으로써 예측의 정확성이 떨어지는 정도 (전체에서 중요성)  #MeanDecreaseGini: 불순도를 얼마나 낮추느냐(각 단계에서 중요성)

MB1 <- data.frame(Sepal.Length = 6.1, Sepal.Width = 3.9, Petal.Length = 1.5, Petal.Width = 0.5)
predictMB1 <- predict(MB, newdata=MB1, type="class")
predictMB1

varImpPlot(MB)


library(MASS)
Boston$chas = factor(Boston$chas)
Boston$rad = factor(Boston$rad)
summary(Boston)

rf.Boston <- randomForest(medv ~., data=Boston, ntree=100, mtry=5, importance=T, na.action=na.omit)
summary(rf.boston) 


Boston$medv.hat = predict(rf.Boston, newdata=Boston)
mean((Boston$medv - Boston$medv.hat)^2)
plot(Boston$medv, Boston$medv.hat, xlab="Observed", ylab="Fitted")  
for_abline <- lm(Boston$medv ~ Boston$medv.hat, data=Boston) #예측치가 선과 붙어서 그려짐 decision tree는 안 그랬는데! 

abline(for_abline, col="blue")


german = read.table("germandata.txt", header = T)
german$numbcredits = factor(german$numcredits)
german$residence = factor(german$residence)
german$residpeople = factor(german$residpeople)

i = sample(1:nrow(german), round(nrow(german)*0.7))
german.train = german[i,]
german.test = german[-i,]
rf.german <- randomForest(y ~. , data = german.train, ntree=100, mtry=5, importance=T, na.action=na.omit)
summary(rf.german)
importance(rf.german, type=1)  #check가 중요한 변수로 떠오름 --> bagging했을 때와 유사한 결과
#변수 너무 많을 때 logistic regression하기 전 변수 선택할 때 쓰임 #130개 변수 -> 32개 변수: 결과가 똑같더라

pred.rf.german <- predict(rf.german, newdata=german.test)
tab=table(german.test$y, pred.rf.german, dnn=c("Actual", "Predicted"))
print(tab)

error_rate = 1-sum(diag(tab)/sum(tab))
error_rate  #24% 성능이 크게 상승하지는 않음 but 오분류 중 중요했던 부분의 수가 줄어들었음 -> error의 내용 자체는 향상되었음 

plot(rf.german)
plot(margin(rf.german))


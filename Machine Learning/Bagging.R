german = read.table("c:\\Users\\lenadad\\Desktop\\germandata.txt", header = T)
german$numbcredits = factor(german$numcredits)
german$residence = factor(german$residence)
german$residpeople = factor(german$residpeople)

#install.packages('adabag')
library(rpart)
library(adabag)

# DT basic

my.dtcontrol <- rpart.control(xval=10, cp=0, minsplit=5) #cross validation 10번 cp=0 끝까지 가봐라 cp는 bagging의 한 요소 - 일단 나무를 크게 만들어야 해!
fit.german <- rpart(y~., data=german, method="class", control=my.dtcontrol)
printcp(fit.german)

fit.prun.german <- prune(fit.german, cp=0.012) #minimum cp 
pred.german <- predict(fit.prun.german, newdata=german, type="class") #y는 good or bad 변수! 
tab = table(german$y, pred.german, dnn = c("Real", "Predicted"))
print(tab)
error_rate_dt = 1 - sum(diag(tab)/sum(tab)) #y는 good or bad 변수이므로 error rate 이렇게 구해
error_rate_dt

# bagging

my.bagcontrol <- rpart.control(xval=0, cp=0, minsplit=5, maxdept=10) #depth 10으로 제한 
bag.german <- bagging(y~., data=german, mfinal=50, control=my.bagcontrol) #bagging 명령어 -> 똑같이 예측해
summary(bag.german)

print(bag.german$trees) #50개 보여줌 하지마!
print(bag.german$importance)
importanceplot(bag.german) #변수의 중요성을 수치로 나타낸다 #중요성 판단 

pred.bag.german <- predict.bagging(bag.german, newdata=german)
head(pred.bag.german$prob,10) #good에 들어갈 확률 0.04 bad 96%
print(pred.bag.german$confusion)
error_rate = 1-sum(diag(pred.bag.german$confusion)/sum(pred.bag.german$confusion)) #error 줄어듬 
error_rate#but 신용이 bad인데 good이라고 할 때 더 문제..! 39명이나..! #우리의 리스크는 39/40 #여기를 집중적으로 줄여야해!
#같은 에러 4%가 아니다 --> 어느 방향으로 에러를 줄일지가 중요..! #high risk를 줄이는 것!
evol.german = errorevol(bag.german, newdata=german)
plot.errorevol(evol.german) #bootstrapping 몇 번 할때마다 error가 얼마나 줄어드는지 
#bagging같은 경우 bootstrapping 몇 번 할 것인지가 중요! -> 과한 computing 막기 위함 #configuration 어떻게 하느냐가 중요 #왜 나타나는지에 대한 설명 

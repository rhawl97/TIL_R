set.seed(1234)
library(rpart)
library(MASS)

Boston$chas = factor(Boston$chas) #data specific command
Boston$rad = factor(Boston$rad)
summary(Boston)

#cp=0: 최소화할 때까지 끝까지 가봐라 
#minsplit: 최소의 마지막 분기를 할 때 분기 안에 남아있는 케이스가 전체 데이터의 5%까지만 남겨라
#5%이하 케이스로 떨어질 때는 분기 하지 말아라
my.control <- rpart.control(xval=10, cp=0, minsplit=nrow(Boston)*0.05) #cptable이나 cross-validation등 default setting 미리 지정 
#method부터 추가함 #미리 정해놓은 my.control지정
#1과 0이 아니기 때문에 anova 설정
fit.Boston = rpart(medv ~., data=Boston, method="anova", control=my.control) 

print(fit.Boston)
printcp(fit.Boston) #가지치기를 어디서 멈출 것인지 확인 #30번까지 일어남! #xerror+xstd가 최소일 때??
plotcp(fit.Boston)  #너무 촘촘해서 어디가 가장 낮은지 알 수 없지만 가장 선이 낮은 위치에서 가지치기를 끊는다!

attach(fit.Boston)
df <- as.data.frame(cptable)
df$sum = df$xerror + df$xstd
min(df$sum)
which.min(df$sum) #19번째 cp값 확인 -- 그 밑으로는 가지치기 하지마! -> 18번째까지만 가지치기를 하겠다!
detach(fit.Boston)

fit.prun.Boston <- prune(fit.Boston, cp=0.00368582) #가지치기 cp=0.003어쩌구에서 끊자! #post complexity #적어도 18번째 가지치기까지만 하겠다!
print(fit.prun.Boston)
plot(fit.prun.Boston, uniform=T, compress=T, margin=0.1) #managable한 size로!
text(fit.prun.Boston, use.n = T, col ="blue")
summary(fit.prun.Boston) #효율적인 분류기를 사용하기 위해! #과적합을 막아내고 적절한 수준에서 가지치기를 멈추기 위해 

#분류가 잘 됐는지 확인해보자
Boston$medv.hat = predict(fit.prun.Boston, newdata=Boston, type="vector") #가지치기에 의해서 나온 값들 
mean((Boston$medv - Boston$medv.hat)^2) #root mean square #실제값과 예상값 오차 확인 
plot(Boston$medv, Boston$medv.hat, xlab="Observed", ylab="Fitted") #원래 데이터, 예측 value plot
for_abline <- lm(Boston$medv ~ Boston$medv.hat, data=Boston)   #regression line 그리기 
abline(for_abline, col="blue")

##train test 나누고 예측해보자
set.seed(1234)
i = sample(1:nrow(Boston), round(nrow(Boston)*0.7)) #7:3으로 분류 
Boston.train = Boston[i,]
Boston.test = Boston[-i,] #sampling된 데이터 나머지 

fit.Boston2 = rpart(medv~., data=Boston.train, method="anova", control=my.control) #train
printcp(fit.Boston2)
df2 <- as.data.frame(fit.Boston2$cptable)
df2$sum <- df2$xerror + df2$xstd
min(df2$sum)
which.min(df2$sum)

fit.prun.Boston2 <- prune(fit.Boston2, cp=0.00305747)
print(fit.prun.Boston2)

medv.hat.test = predict(fit.prun.Boston2, newdata=Boston.test,type="vector") #test
mean((Boston.test$medv - medv.hat.test)^2)

head(Boston)

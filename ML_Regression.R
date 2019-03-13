## 0311 Regression ##
#install.packages("GGally")
library('GGally') #한꺼번에 모든 변수를 그래프화해서 볼 수 있음
library(car)
data(mtcars)

ggpairs(mtcars) 
pairs(mtcars) #모든 변수들의 분포와 상관관계 
scatterplotMatrix(~mpg+disp+drat+hp+wt, data=mtcars) 
scatterplotMatrix(~mpg+disp+drat+hp+wt|cyl, data=mtcars)  #cyl에 따라 달라짐

firstmodel <- lm(mpg ~ disp+hp+wt, data=mtcars) 
summary(firstmodel)
plot(firstmodel)

par(mfrow=c(2,2))
plot(firstmodel)
par(mfrow=c(1,1))
outlierTest(firstmodel) #2 이상이면 outlier로 취급 
vif(firstmodel) #다중공선성 문제가 없음을 알 수 있음 


ggplot(mtcars, aes(x=disp, y=mpg)) + geom_point()
coef(firstmodel)
install.packages('coefplot')
library(coefplot)
coefplot(firstmodel)

library(MASS) #통계적으로 유의미함을 보다 엄격하게 판정
secondmodel<- rlm(mpg ~ disp+hp+wt, data=mtcars, psi=psi.huber)
summary(firstmodel)
summary(secondmodel)
write.csv(as.data.frame(summary(firstmodel)$coef), file="regression1.csv")

#상호작용이 있는가! #interaction effect 
thirdmodel <- lm(mpg ~ disp + hp * wt, data=mtcars)  
summary(thirdmodel)
outlierTest(firstmodel)
plot(thirdmodel)

install.packages('effects')
library(effects)
plot(effect("hp:wt", thirdmodel,, list(wt=c(2.2,3.2,4.2))), multiline=TRUE)


forward_model = step(lm(mpg ~ 1, mtcars), scope=list(lower=~1, upper=~disp+hp+wt+hp*wt),direction="forward") 
#AIC가 가장 낮은 것이 좋은 모델
#wt를 넣었을 때 가장 낮으므로 wt 먼저 넣고 그 다음 넣어감!
#wt+hp
#wt+hp+wt*hp (interaction 추가)



#install.packages('xlsx')
library(xlsx)
drug.data = read.xlsx("C:/Users/Kim Yuum/Desktop/19-1 통계/머신러닝/week2/4강_0315/c1data/drug.xlsx",1)
attach(drug.data)

plot(age, purchase, pch=19)

g_age = age
g_age [age >= 20 & age <=29] = 1  #각 연령별 약을 사먹은 비율 계산 
g_age [age >= 30 & age <=34] = 2
g_age [age >= 35 & age <=39] = 3
g_age [age >= 40 & age <=44] = 4
g_age [age >= 45 & age <=49] = 5
g_age [age >= 50 & age <=54] = 6
g_age [age >= 55 & age <=59] = 7
g_age [age >= 60 & age <=64] = 8

purchase.table = table(g_age, purchase) #연령이 올라갈수록 약을 사먹을 확률이 높아지는 그래프 
purchase.table

percent.table = prop.table(purchase.table,1)
percent.table

perc.1 = percent.table[,2]
g_age.1 = rownames(percent.table)
g_age.1 = as.numeric(g_age.1)
plot(g_age.1, perc.1, pch=19)
lines(perc.1)

logit1 <- glm(purchase ~ g_age, family = binomial, data=drug.data) #로지스틱 결과 #binomial분포를 따르는 
summary(logit1) #연령이 한 등급 올라갈 때 약을 사먹을 확률이 약을 사먹지 않을 확률에 비해 ( )배 이다 라고 해석!

drug.predict = predict(logit1, newdata=drug.data, type="response") #개개인이 약을 사먹을 확률 #logit1모델에 기반 
pred = ifelse(drug.predict < 0.5, "no", "yes") #0.5이하는 약을 사먹지 않은 사람으로 예측 
pred = factor(pred)

confusion.matrix = table(drug.data$purchase, pred) #실제 값과 예측값 비교 
confusion.matrix  #실제와 예측 모두 약을 산 사람 32명 
error_rate = 1 - (sum(diag(confusion.matrix))/sum(confusion.matrix)) #예측값과 실제값이 다른 #diag: 맞은 값들의 합
error_rate

#install.packages('ROCR')
library(ROCR)

perf_level <- performance(prediction(drug.predict, drug.data$purchase), 'tpr', 'fpr') #true/false positive percent
plot(perf_level) #ROC Curve #0.3에 대해 0.7정도의 값을 가짐 #?
abline(a=0, b=1)

auc = performance(prediction(drug.predict, drug.data$purchase), measure = "auc")
auc = auc@y.values[[1]] #?
auc

detach(drug.data)

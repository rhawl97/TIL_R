####Q1####
df = read.csv("C:\\Users\\Kim Yuum\\Downloads\\NPO2019.csv",header = T)
head(df)
set.seed(1234)
df.ab = cbind(df[3:20])
head(df.ab)

library(NbClust)
nc = NbClust(df.ab, min.nc=2, max.nc=15, method="kmeans") #2개~15개 군집 #3개가 가장 많이 voting
table(nc$Best.n[1,])  #그래프에서 어디서 꺾어지는지 눈대중으로 보는 것을 수치화 
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), xlab="# of Clusters", ylab="# of Criteria") #table내용을 barplot으로

df.km = kmeans(df.ab, 2) #kmeans
df.km
df.km$size
df.km$cluster

df$cluster = as.factor(df.km$cluster)
library(cluster)
clusplot(df, df$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)



###Q2###
##(1) 결측치 대체 
colSums(is.na(df)) #컬럼별 결측치 확인 

df.nona = na.omit(df)  #결측치 제거한 df
head(df.nona)

# recommend 결측치: 로지스틱 회귀를 통해 추정값으로 대체 #
df.ab.nona = cbind(df.nona[3:20],df.nona["recommend"]) #a b 설문과 recommend (결측치 x)
head(df.ab.nona)
df.ab.rec = cbind(df.ab, df["recommend"]) #a b 설문과 recommend(결측치 포함)

logit1 = step(glm(recommend ~  ., family = binomial, data=df.ab.nona)) #AIC 값을 이용하여 단계적 회귀 수행
library("car")
vif(logit1) #10이상 값이 없으므로 다중공산성 존재 X

rec.predict = predict(logit1, newdata=df.ab.rec[is.na(df.ab.rec$recommend),], type="response")  #결측치였던 recommend 변수 예측
pred = ifelse(rec.predict < 0.5, "no", "yes") #0.5이하는 NO로 예측 
df[is.na(df$recommend),]$recommend = factor(pred) #결측치 대체

sum(is.na(df$future_recommend))

# future_recommend 결측치: recommend변수와 강한 선형관계는 가지지 않으므로 평균값 대체
cor(df$recommend, df$future_recommend,use="complete.obs")  #0.4784789
df$future_recommend = ifelse(is.na(df$future_recommend), mean(df$future_recommend, na.rm = T), df$future_recommend) 

# lookfor 결측치: 평균값 대체 
df$lookfor = ifelse(is.na(df$lookfor), mean(df$lookfor, na.rm = T), df$lookfor)
sum(is.na(df$lookfor))

# future_lookfor 결측치: lookfor과 강한 상관성 보이므로 회귀를 통해 예측 
cor(df$lookfor, df$future_lookfor, use="complete.obs") #0.8097908
df.ab.lf = cbind(df.ab, df[c("future_lookfor","lookfor")]) #a b설문과 future_lookfor, lookfor(결측치 포함)
df.ab.lf_nona = cbind(df.nona[3:20], df.nona[c("future_lookfor","lookfor")]) #a b설문과 future_lookfor, lookfor(결측치 x)

logit2 = step(lm(future_lookfor ~  ., data=df.ab.lf_nona)) #단계적 회귀를 통해 변수선택
pred = predict(logit2, df.ab.lf)                           
df$future_lookfor = ifelse(is.na(df$future_lookfor), pred, df$lookfor) #회귀 추정치로 결측값 대체 
sum(is.na(df$future_lookfor))

# trust 결측치: 평균값 대체 
df$trust = ifelse(is.na(df$trust), mean(df$trust, na.rm = T), df$trust)
sum(is.na(df$trust))

# experience 결측치: 평균값 대체 
df$experience = ifelse(is.na(df$experience), mean(df$experience, na.rm = T), df$experience)
sum(is.na(df$experience))

# knowyear 결측치: experience 와의 계산을 통해 대체
df$knowyear = ifelse(is.na(df$knowyear),mean(df$knowyear, na.rm = T), df$knowyear)
sum(is.na(df$knowyear))

# firstknow 결측치: knowyear과의 계산을 통해 대체 
df$firstknow = ifelse(is.na(df$firstknow), 2019-df$knowyear , df$firstknow)
sum(is.na(df$firstknow))

# times 결측치: 평균값 대체
df$times = ifelse(is.na(df$times), mean(df$times, na.rm = T), df$times)
colSums(is.na(df))

##(2) 랜덤포레스트를 통해 중요한 변수 파악 
library(randomForest)
df2 = df[-c(1,3:20)]
head(df2)

i = sample(1:nrow(df2), round(nrow(df2)*0.7))
df2.train = df2[i,]
df2.test = df2[-i,]
set.seed(1234)
rfmodel = randomForest(cluster ~ ., data=df2.train, importance=TRUE, ntree=500, mtry = 2, do.trace=100)

pred.rf = predict(rfmodel, newdata=df2.test)
tab=table(df2.test$cluster, pred.rf, dnn=c("Actual", "Predicted"))
print(tab)

error_rate = 1-sum(diag(tab)/sum(tab))
error_rate #오분류 0.2

importance(rfmodel, type=1) 
  # => future_recommend > trust> future_lookfor > lookfor 순으로 중요한 지표 

##(3) Decisition Tree를 통해 
set.seed(1234)
library(rpart)

fit.df2 = rpart(cluster ~., data=df2, method="anova") 

print(fit.df2)
printcp(fit.df2) #가지치기를 어디서 멈출 것인지 확인 #30번까지 일어남! #xerror+xstd가 최소일 때??
plotcp(fit.Boston)

fit.prun.df2 = prune(fit.df2, cp=fit.df2$cptable[which.min(fit.df2$cptable[,"xerror"]),"CP"])  #에러율이 가장 낮을 때의 tree size
print(fit.prun.df2)
plot(fit.prun.df2, uniform=T, compress=T, margin=0.1) #managable한 size로!
text(fit.prun.df2, use.n = T, col ="blue")
summary(fit.prun.df2)

##(3)중요 변수를 통해 응답자들의 성격 특정하기 
df$fu_rec = ifelse(df$future_recommend > 8, 1, 0)
table(df$fu_rec,df$cluster)

df$fu_lf = ifelse(df$future_lookfor> 7,1,0)
table(df$fu_lf,df$cluster)

df$tru = ifelse(df$trust > 8,1,0)
table(df$tru, df$cluster)

df$lf = ifelse(df$lookfor >7, 1, 0)
table(df$lf, df$cluster)
##로지스틱 오즈비 --> 1번으로 분류하게 하는 변수 찾기 
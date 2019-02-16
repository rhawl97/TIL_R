#데이터를 주면 모든 정보와 그림을 보여주는 function 

#### pima: package ####
install.packages("faraway")
library(faraway)

#### pima: data ####
data(pima)
pima

#### data description ####
?pima
help(pima)

#### data description ####
dim(pima) #9개의 variables, 768 rows
summary(pima)  #pregnant, diastolic 등 이상한 데이터(0) 존재 

class(pima)
class(pima[,1]) #변수 하나하나가 보여주는 class
class(pima[,2])
apply(pima,1,class)  #row단위 1옵션
sapply(pima, class)   

#### missing values ####
summary(pima)   #이상한 데이터 missing value일거다!

sort(pima$diastolic)
pima$diastolic[pima$diastolic == 0] = NA #말도 안되는 데이터 age<0 age=0 NA로 바꾸기 데이터 주고 시험!!!
pima$glucose[pima$glucose == 0] = NA
pima$triceps[pima$triceps == 0] = NA
pima$insulin[pima$insulin == 0] = NA
pima$bmi[pima$bmi == 0] =NA

summary(pima)

### qualitative data: categorical ####
pima$test = factor(pima$test) 
summary(pima$test) #원하는 것만 나옴

levels(pima$test) = c("negative", "positive")
summary(pima$test)  #0과 1 대체하기

summary(pima)

##### Graphical Summaries: categorical single variable #####
plot(pima$test)

hist(pima$test)

barplot(table(pima$test))  #table == summary
barplot(prop.table(table(pima$test))) #probability

pie(table(pima$test), labels = pima$test, 
    main="Pie Chart of Test") #옵션 알기


# 3D Exploded Pie Chart
install.packages("plotrix")
library(plotrix)
pie(table(pima$test), labels = levels(pima$test), main="Pie Chart of Test")
pie3D(table(pima$test), 
      labels = levels(pima$test), explode=0.2, #떨어지는 거리 explode
      main="Pie Chart of Test", labelcex=0.9, radius =1.3, #radius 반지름 (파이크기)
      theta=0.9,start=1.5) 


##### Graphical Summaries: single variable #####
hist(pima$diastolic)
?hist #border 경계선
boxplot(pima$diastolic)

#그림 동시에 보기
par(mfrow = c(1,2), mar = c(5,5,5,5)) #1행 2열 margin==양옆공백
hist(pima$diastolic)
boxplot(pima$diastolic)
#줄기잎그림
stem(pima$diastolic, scale = 1)
stem(pima$diastolic, scale = 2)

#### Graphical Summaries: two variables ####
plot(pima$diastolic, pima$diabetes)
plot(pima$test, pima$bmi) #bmi가 조금 더 높은 경향이 있다

plot(pima$diastolic, pima$diabetes, 
     xlab = "Diastolic", #title은 main=으로 넣기
     ylab = "Diabetes", pch = 20, col = "red") #point style 
plot(pima$test, pima$bmi, ylab = "BMI", main = "Boxplot for the Test")

par(mfrow = c(1,1))
plot(pima) #dataframe 전부 그래프 보여줌

head(pima)#처음 6개의 row만 보여줌

#### Individual summary functions ####
mean(pima$diastolic)
mean(pima$diastolic, na.rm = T) #NA remove
median(pima$diastolic, na.rm=T) #mean과 비슷함 == 대칭적이다
quantile(pima$diastolic, prob = c(0.15, 0.25, 0.35), na.rm=T)

range(pima$diastolic, na.rm=T) #min max 값
quantile(pima$diastolic, na.rm=T)
IQR(pima$diastolic, na.rm = T)
var(pima$diastolic, na.rm=T)
sd(pima$diastolic, na.rm=T)
options(digits=3) #자릿수 조정

#### iris #####
data(iris)
iris
dim(iris)
head(iris)
#species는 factor처리 #숫자가아닌애들은 NA처리
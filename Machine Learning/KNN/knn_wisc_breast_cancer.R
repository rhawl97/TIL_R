wbcd <- read.csv("C:/Users/Kim Yuum/Desktop/19-1 통계/머신러닝/week5/knn_0405/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1]  #ID 제거 꼭 제거!

table(wbcd$diagnosis) #악성 종양 음성 종양
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), #데이터 factor화
                         labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 2)
summary(wbcd)

# create normalization function
normalize <- function(x) { #range에 기반한 normalization
  return ((x - min(x)) / (max(x) - min(x)))
}
# test normalization function 
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize)) 
head(wbcd_n)

wbcd_train <- wbcd_n[1:469, ]  #train test split
wbcd_test <- wbcd_n[470:569, ] 

wbcd_train_diagnosis <- wbcd[1:469, 1]  #첫번째 컬럼 악성음성 분류된 열
wbcd_test_diagnosis <- wbcd[470:569, 1]

#install.packages('class')
library(class)

# k = 21 ==> sqrt(469)  #kmeans에서 기본적으로 루트 다룸 

#reference data labeling: cl(469개) / testdata가 input! --> input이 들어가면 어떻게 예측이 되겠느냐가 wbcd_test_pred
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,   
                      cl = wbcd_train_diagnosis, k = 21)

library(gmodels)
CrossTable(x = wbcd_test_diagnosis, y = wbcd_test_pred,  #참값과 매칭시켜봄 
           prop.chisq = FALSE)

# use the scale() function to z-score standardize 
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_diagnosis, k = 21)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_diagnosis, k=1)
CrossTable(x = wbcd_test_diagnosis, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_diagnosis, k=5)
CrossTable(x = wbcd_test_diagnosis, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_diagnosis, k=11)
CrossTable(x = wbcd_test_diagnosis, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_diagnosis, k=15)
CrossTable(x = wbcd_test_diagnosis, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_diagnosis, k=21)
CrossTable(x = wbcd_test_diagnosis, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_diagnosis, k=27)
CrossTable(x = wbcd_test_diagnosis, y = wbcd_test_pred, prop.chisq=FALSE)

#k값 다르게 다 해보기!
#install.packages('pROC')
#library('pROC)
library(caret)

grid1 = expand.grid(.k=seq(2,27, by=1))  #k가 2부터 27까지 
control = trainControl(method="cv") #cross validation
set.seed(1234)

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_diagnosis <- wbcd[1:469, 1]
wbcd_test_diagnosis <- wbcd[470:569, 1]

wbcd2 = cbind(wbcd_train_diagnosis, wbcd_train)

#response variable, traindata, cv, tuneGrid : tuneGrid에 따라 쭉 보여줘라 
knn.train = train(wbcd_train_diagnosis~., data=wbcd2, method="knn", trControl=control, tuneGrid=grid1)
knn.train  #accuracy 높아지다가 떨어지는 point 존재 

knn.test = knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_diagnosis, k=13)

table(knn.test, wbcd_test_diagnosis)

#calculate Kappa
prob.agree = (61+36)/100 # accuracy #정확하게 맞춘 정도 
prob.chance = ((61+3)/100)*((61+0)/100)  #우연히 맞을 확률 -?
kappa = (prob.agree - prob.chance)/(1 - prob.chance) #실제로 맞춘 정도 - 우연히 맞을 정도 
kappa #우연이 일어나지 않을 확률 -?


#knn이 잘 적용되는 경우인지 판단하기 위해 nearest와 far points 평균 거리 차이 비교 
#install.packages('FNN')
library(FNN)

knn.dist(wbcd_train, k=17, algorithm=c("kd_tree")) #각각의 17개 데이터에 대해서 거리 계산한 결과 
neighbormean <- mean(knn.dist(wbcd_train, k=17, algorithm=c("kd_tree")))
neighbormean

allmean <- mean(knn.dist(wbcd_train, k=468, algorithm=c("kd_tree")))  #전체 데이터에 대한 거리 계산 
allmean  #2배 이상 차이가 나므로 괜찮다! 


#gaussian mixture model
library(mclust)
mc = Mclust(iris[,1:4], G=3) #집단 3개로 나누겠다 
summary(mc, parameters= T)
plot.Mclust(mc)
mc$classification

predict(mc, data=)
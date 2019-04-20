
#install.packages(??e1071??)

library(e1071)
library(caret)

data("Titanic")
Titanic_df=as.data.frame(Titanic) #원래 Titanic은 카테고리 별로 되어있음 
Tset=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq)
T_dataset=Titanic_df[Tset,]
T_dataset$Freq=NULL

NB = naiveBayes(Survived ~., data=T_dataset)
NB #살아남은+1st class or 1st class+남자인 경우 등등 확률 도출

NBpred =predict(NB,T_dataset)
confusionMatrix(NBpred,T_dataset$Survived, positive = "Yes") #accuracy 78%


data(iris)
irisnb <- naiveBayes(Species ~ ., data = iris)
irisnb

irispred <- predict(irisnb, iris[,-5])
confusionMatrix(irispred, iris[,5])

iris_laplace = naiveBayes(Species ~ ., data = iris, laplace=1)  #라플라스 스무딩 적용한 경우 #위 결과와 같음(확률 0 되는 변수 없기 때문)
irislpred <- predict(iris_laplace, iris[,-5])
confusionMatrix(irislpred, iris[,5])



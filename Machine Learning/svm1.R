#install.packages('e1071')
library(e1071)
data(iris)

iris.subset = subset(iris, select=c("Sepal.Length", "Sepal.Width", "Species"),  #subset data
                                         Species %in% c("setosa","virginica"))
plot(x=iris.subset$Sepal.Length, y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)

svm.model = svm(Species~., data=iris.subset, kernel='linear', cost=1, scale=F) #cost=1  #support vector 12개 
ls(svm.model)  #cost와 gamma가 선의 형태를 결정함 
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)  #support vector가 되는 지점 #
points(svm.model$SV, col="blue", cex=3)  #point의 데이터 수치가 직접 나옴 

w = t(svm.model$coefs) %*% svm.model$SV  #??
b = -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red", lty=5)   #오류 1을 허용한 채 margin을 최대화한 상태 
w[1,2]
w[1,1]

plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width,
     col=iris.subset$Species, pch=19)

svm.model = svm(Species ~ ., data=iris.subset, type='C-classification',   
                  kernel='linear', cost=10000, scale=FALSE)  #penalizing을 허용하지 않은 hard line  #support vector 3개
points(svm.model$SV, col="blue", cex=3)

w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red", lty=5)

#전체 데이터  
#subset svm.modle할 대는 kernel, cost등이 필요했으나 전체 데이터는 그렇지 않음 -> 어떻게 specify된거지?
#알아서 최적을 적합시킴! 
model.iris = svm(Species~., iris, scale=T)  

plot(model.iris, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))  #slice는 변수 2개((Sepal.Width, Sepal.Length)를 고정한 채 plot하겠다는 옵션 

model.iris


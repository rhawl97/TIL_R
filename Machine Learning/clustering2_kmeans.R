#install.packages('rattle')
data(wine, package="rattle")
head(wine)

df <- scale(wine[-1]) #Type제거 #표준화로 단위 맞추기 
head(df)

#install.packages('NbClust')
library(NbClust) #몇 개의 클러스터가 좋을지 판단하기 위한 패키지 
set.seed(1234)

nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans") #2개~15개 군집 #3개가 가장 많이 voting
table(nc$Best.n[1,])  #그래프에서 어디서 꺾어지는지 눈대중으로 보는 것을 수치화 
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), xlab="# of Clusters", ylab="# of Criteria") #table내용을 barplot으로

fit.wine <- kmeans(df, 3, nstart = 25) #kmeans
fit.wine$size
fit.wine$centers


plot(df, col=fit.wine$cluster)
points(fit.wine$center, col=1:3, pch=4, cex=1.5)
table(wine$Type, fit.wine$cluster)

install.packages('fpc')
install.packages('cluster')
library(fpc)
library(cluster)

pamk.test <- pamk(df)
pamk.test$nc
table(pamk.test$pamobject$clustering, wine$Type)

par(mfrow=c(1,2))
plot(pamk.test$pamobject)
par(mfrow=c(1,1))


pam.fit <- pam(df, 3) #pam
table(pam.fit$clustering, wine$Type)

par(mfrow=c(1,2))
plot(pam.fit)
par(mfrow=c(1,1))

install.packages('factoextra')
library("factoextra")  #군집이 어떻게 이루어졌는지 visualization

fviz_cluster(pam.fit)
fviz_cluster(pamk.test$pamobject)


clara.cluster <- clara(df, 3, samples = 50, pamLike = TRUE)

print(clara.cluster)
clara.cluster$medoids
fviz_cluster(clara.cluster, stand = FALSE, geom = "point", pointsize = 1)

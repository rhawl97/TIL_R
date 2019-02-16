install.packages("jpeg")
install.packages("RCurl")

library(jpeg)
library(RCurl)

rm(list = ls())
gc()
setwd("C:\\Users\\stat18\\Desktop")
source("./mykmeans.R")  #만들어 놓은 함수 불러올 수 있음

url = "https://raw.githubusercontent.com/mages/diesunddas/master/Blog/LloydsBuilding.jpg" 
readImage = readJPEG(getURLContent(url, binary=TRUE)) 
head(readImage)  #컴퓨터는 색을 숫자로 받아들임 RGB 3차원
dm = dim(readImage) 
rgbImage = data.frame( 
  x=rep(1:dm[2], each=dm[1]), 
  y=rep(dm[1]:1, dm[2]), #y축은 위에서 아래로 찍어야 함
  r.value=as.vector(readImage[,,1]), 
  g.value=as.vector(readImage[,,2]), 
  b.value=as.vector(readImage[,,3])) 

head(rgbImage) 
plot(y ~ x, data=rgbImage, main="Lloyd’s building",   #rgb 픽셀을 찍음
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".") 


kColors = 5  #색을 5개 그룹으로 구분하고자 함
system.time((kMeans = mykmeans(data = rgbImage[, c("r.value", "g.value", "b.value")], #초기값을 잘못 부여해서 나오는 error 처리할 수 있기!
                               centers = kColors, iter.max = 20)))

system.time((kMeans2 = kmeans(rgbImage[, c("r.value", "g.value", "b.value")], #내장함수
                              centers = kColors, iter.max = 20)))

clusterColour = rgb(kMeans$centers[kMeans$cluster, ]) #색으로 변환
kMeans$centers
plot(y ~ x, data=rgbImage, main="Lloyd’s building", #5개의 색으로만 사진을 찍음
     col = clusterColour, asp = 1, pch = ".", 
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 5 colours") 

clusterColour2 = rgb(kMeans2$centers[kMeans2$cluster, ]) 

plot(y ~ x, data=rgbImage, main="Lloyd’s building", 
     col = clusterColour2, asp = 1, pch = ".", 
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 5 colours") 


url = " https://2.bp.blogspot.com/-d47GF28COio/VBI3EMlHenI/AAAAAAAAB5U/DoHJ5n6jYwE/s1600/ColorfulBird.jpg" 

readImage = readJPEG(getURLContent(url, binary=TRUE)) 
dm = dim(readImage) 
rgbImage = data.frame( 
  x=rep(1:dm[2], each=dm[1]), 
  y=rep(dm[1]:1, dm[2]), 
  r.value=as.vector(readImage[,,1]), 
  g.value=as.vector(readImage[,,2]), 
  b.value=as.vector(readImage[,,3])) 

plot(y ~ x, data=rgbImage, main="Bird", 
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".") 

kColors = 5 
kMeans = kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                centers = kColors) 
clusterColour = rgb(kMeans$centers[kMeans$cluster, ]) 

plot(y ~ x, data=rgbImage, main="Bird", 
     col = clusterColour, asp = 1, pch = ".", 
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of K colors") 
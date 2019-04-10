data(faithful)
library(mclust)

faithfulMclust <- Mclust(faithful)
summary(faithfulMclust)
summary(faithfulMclust, parameters = T)
plot(faithfulMclust)
#plot(faithfulMclust, col = "grey")

faithfulBIC <- mclustBIC(faithful) #BIC value만 보기 
faithfulBIC
faithfulSummary <- summary(faithfulBIC, data=faithful)
faithfulSummary

plot(faithfulBIC, g = 1:7, ylim = c(-2500,-2300), legendArgs = list(x = "bottomright", ncol=5))

ICL <- mclustICL(faithful) #top3가 모두 2개의 분포가 적당하다고 말하고 있음 
ICL
plot(ICL)


LRT <- mclustBootstrapLRT(faithful, modelName = "VVV") #bootstrapping 
LRT

LRT <- mclustBootstrapLRT(faithful, modelName = "VVE")
LRT

LRT <- mclustBootstrapLRT(faithful, modelName = "VEE")
LRT


mod <- Mclust(faithful, G = 2, modleNmae = "VVV")
summary(mod, parameters = T)
plot(mod, what = "classification", main = F)
plot(mod)

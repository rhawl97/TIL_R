library(arules)
library(arulesViz)
data(Groceries)
Groceries
inspect(Groceries[1:6])
itemFrequencyPlot(Groceries,topN=20,type="absolute")  #어떤 품목이 가장 많이 팔렸는지 
itemFrequencyPlot(Groceries,topN=20,type="relative")
itemFrequency(Groceries[,1:3])
itemFrequencyPlot(Groceries[,1:3])
itemFrequencyPlot(Groceries, support = 0.1)  #support에 따라 어떻게 달라지는지 

image(Groceries[1:10])
image(Groceries[1:100])

groceriesrules <- apriori(Groceries, parameter = list(support=0.006, confidence =0.25, minlen =2)) 
groceriesrules #463개 rule존재 
summary(groceriesrules)
inspect(sort(groceriesrules, by = "lift")[1:5]) #lift 높은 5개 

berryrules <- subset(groceriesrules, items %in% "berries")
inspect(berryrules)  #berry를 살 경우에 다음에 무엇을 살지 / 전체 거래 중에 몇개인지 보여줌 

write(groceriesrules, file="groceryrules.csv", sep=",", quote=T, row.names=F)
berryrules_df <- as(berryrules, "data.frame")

groceriesrules2 <- apriori(Groceries, parameter = list(support=0.001,
                                                       confidence =0.9, maxlen = 4))
groceriesrules2
summary(groceriesrules2)
inspect(sort(groceriesrules2, by = "lift")[1:5])

groceriesrules2 <- sort(groceriesrules2, by="lift", decreasing=T)

table <- crossTable(Groceries)
table[1:3, 1:3]  #품목별로 같이 산 개수
table["bottled beer", "bottled beer"]
table["canned beer", "canned beer"]
table[c("bottled beer", "canned beer"), c("bottled beer", "canned beer")]

beer.rules <- apriori(data=Groceries, parameter=list(support=0.0015, #오른쪽에 bottled beer가 들어가는
                                                     confidence=0.3), appearance =list(default="lhs", rhs="bottled beer")) #right hand side: bottled  beer
beer.rules
inspect(beer.rules)
plot(beer.rules, method="graph", measure="lift",shading="confidence")

retail <- read_excel("OnlineRetail.xlsx")  #transaction형태로 바꿔줘야 asso rule 적용 가능
retail <- retail[complete.cases(retail), ] #비어있는 항목 빼기 
retail <- retail %>% mutate(Description = as.factor(Description))  #각각의 항목을 범주형으로
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))  ##데이터에 대한 전처리 (시간/문자 등)
glimpse(retail)
head(retail)
retail$Time <- as.factor(retail$Time)
itemList <- ddply(retail,c("InvoiceNo","Date"), 
                  function(df1)paste(df1$Description, collapse = ","))

itemList$InvoiceNo <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)  #format만 바꿔주면 asso rule적용할 수 있는 데이터로 변형 가능 
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',') #format 변경 
tr
summary(tr)
install.packages('arules')
library(arules)
data(Adult)
Adult

rules <- apriori(Adult)
inspect(head(rules))

adult.rules <- apriori(Adult, parameter = list(support=0.1, confidence=0.6), 
                      appearance = list(rhs=c('income=small', 'income=large'),
                                        default = 'lhs'),
                      control=list(verbose=F))

adult.rules.sorted <- sort(adult.rules, by='lift')
inspect(head(adult.rules.sorted))

install.packages('arulesViz')
library(arulesViz)

plot(adult.rules.sorted, method = 'scatterplot')
plot(adult.rules.sorted, method='graph', control = list(type='items', alpah=0.5))

ruleExplorer(adult.rules.sorted)

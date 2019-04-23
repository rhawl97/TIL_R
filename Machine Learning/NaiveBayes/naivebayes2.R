#install.packages(c('rsample','h2o'))

library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o) 

attrition <- attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  )

set.seed(123)
split <- initial_split(attrition, prop = .7, strata = "Attrition")
train <- training(split)
test  <- testing(split)

table(train$Attrition) %>% prop.table()
table(test$Attrition) %>% prop.table()

train %>% 
  select(Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")


features <- setdiff(names(train), "Attrition")
x <- train[, features]
y <- train$Attrition

train_control <- trainControl(
  method = "cv", 
  number = 10
)

NB = naiveBayes(Attrition ~. , data=train)
NB

NBpred =predict(NB,train)
confusionMatrix(NBpred,train$Attrition, positive = "Yes")

# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)
confusionMatrix(nb.m1)

search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)  #Kernel을 넓히면 오류를 허용할 범위를 키우겠다 
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

plot(nb.m2)
pred <- predict(nb.m2, newdata = test)
confusionMatrix(pred, test$Attrition)
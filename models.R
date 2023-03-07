

dt <- read.csv("data.csv")

apply(dt, 2, function(x) sum(is.na(x)))

dt$operating_system[is.na(dt$operating_system)] <- "Other"

dt$country <- as.integer(factor(dt$country))
dt$language <- as.integer(factor(dt$language))

#### Logistic Regression #########
logreg <- glm(churned~., data = dt[,-1], family = "binomial")

summary(logreg)

library(caret)

preds <- ifelse(logreg$fitted.values > 0.5, 1, 0)

confusionMatrix(data = as.factor(preds), reference = as.factor(dt$churned))

##### Decision Tree ########
library(rpart)
library(rpart.plot)

dtree <- rpart(churned~., data = dt[,-1], method = "class")

rpart.plot(dtree)

preds <- predict(dtree, dt[,-c(1,20)], type = 'class')

confusionMatrix(data = as.factor(preds), reference = as.factor(dt$churned))


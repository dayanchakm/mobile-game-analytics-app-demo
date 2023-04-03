library(dplyr)

dt <- read.csv("data/data.csv")
dt$vp <- ifelse(dt$user_pseudo_id %in% users_in_app_purchase$user_pseudo_id, 1, 0)

apply(dt, 2, function(x) sum(is.na(x)))

# dt$operating_system[is.na(dt$operating_system)] <- "Other"

dt$country <- as.integer(factor(dt$country))
dt$language <- as.integer(factor(dt$language))

dt$operating_system <- NULL

dt_train <- dt[dt$user_first_engagement < 1536019199,]
dt_test <- dt[dt$user_first_engagement > 1536019199 & dt$user_first_engagement < 1537228799,]


target_train <- dt_train[, c("churn7", "churn14", "vp")]
dt_train[, c("churn7", "churn14", "vp")] <- NULL
target_test <- dt_test[, c("churn7", "churn14", "vp")]
dt_test[, c("churn7", "churn14", "vp")] <- NULL

retain7 <- dt_train[dt_train$churn7 == 0,]
retain7 <- sample_n(retain7, size = 200)

dt_vp <- rbind(dt[dt$vp == 1,], retain7)


#### Logistic Regression #########
logreg <- glm(vp~., data = dt_vp[,-c(1,16,17)], family = "binomial")

summary(logreg)

library(caret)

logreg_preds <- ifelse(logreg$fitted.values > 0.5, 1, 0)

confusionMatrix(data = as.factor(logreg_preds), reference = as.factor(dt_vp$vp))

save(logreg_preds, file = "preds/logreg_preds.RData")
save(logreg, file = "models/logreg.Rdata")

##### Decision Tree ########
library(rpart)
library(rpart.plot)

dt_vp$user_first_engagement <- NULL
dt_vp$julianday <- NULL
dt_vp$month <- NULL

dtree <- rpart(vp~., data = dt_vp[,-c(1,16,17)], method = "class")


rpart.plot(dtree)


dtree$variable.importance

dtree_preds <- predict(dtree, dt_vp, type = 'class')
confusionMatrix(data = as.factor(dtree_preds), reference = as.factor(dt_vp$vp))

save(dtree_preds, file = "preds/dtree_preds.RData")
save(dtree, file = "models/dtree.Rdata")


###### XGBoost ##########
library(caret)
library(xgboost)
trctrl <- trainControl(method = "cv", number = 5)
xgb <- train(as.factor(vp)~., data = dt_vp[,-c(1,16,17)], method = "xgbTree", trControl = trctrl, verbosity = 0)

xgb_preds <- predict(xgb, dt_vp)
confusionMatrix(data = xgb_preds, reference = as.factor(dt_vp$vp))

xgb_preds <- predict(xgb, dt_train[,-c(1,19,20)])
confusionMatrix(data = xgb_preds, reference = as.factor(dt_train$churn7))

xgb_table <- xgb.importance(model = xgb$finalModel)
xgb.ggplot.importance(xgb_table)

save(xgb_preds, file = "preds/xgb_preds.RData")
save(xgb, file = "models/xgb.Rdata")

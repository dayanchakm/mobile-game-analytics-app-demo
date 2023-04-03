library(dplyr)

dt <- read.csv("data/data.csv")
users_in_app_purchase <- read.csv("data/users_in_app_purchase.csv")
dt$vp <- ifelse(dt$user_pseudo_id %in% users_in_app_purchase$user_pseudo_id, 1, 0)

dt$country <- as.integer(factor(dt$country))
dt$language <- as.integer(factor(dt$language))
dt$operating_system <- NULL

dt_train <- dt[dt$user_first_engagement < 1536019199,]

retain7 <- dt_train[dt_train$churn7 == 0,]
retain7 <- sample_n(retain7, size = 200)
dt_vp <- rbind(dt[dt$vp == 1,], retain7)
dt_vp$user_first_engagement <- NULL
dt_vp$julianday <- NULL
dt_vp$month <- NULL
write.csv(x = dt_vp, file = "app/data/data_vp.csv")

#### Logistic Regression #########
logreg <- glm(vp~., data = dt_vp[,-c(1,16,17)], family = "binomial")

summary(logreg)

library(caret)

logreg_preds <- ifelse(logreg$fitted.values > 0.5, 1, 0)
confusionMatrix(data = as.factor(logreg_preds), reference = as.factor(dt_vp$vp))

# save(logreg_preds, file = "preds/vp_logreg_preds.RData")
# save(logreg, file = "models/vp_logreg.Rdata")

##### Decision Tree ########
library(rpart)
library(rpart.plot)

dtree_vp <- rpart(vp~., data = dt_vp[,-c(1,16,17)], method = "class", model = TRUE)
rpart.plot(dtree_vp, type = 3,fallen.leaves = FALSE, branch.lty = 7,box.palette = "BuGn",clip.right.labs = FALSE, branch = .3, under = TRUE,shadow.col = "gray", main = "Model Decision Tree\n(VP)\n")

dtree_preds <- predict(dtree, dt_vp, type = 'class')
confusionMatrix(data = as.factor(dtree_preds), reference = as.factor(dt_vp$vp))

# save(dtree_preds, file = "preds/vp_dtree_preds.RData")
save(dtree_vp, file = "models/dtree_vp.Rdata")


###### XGBoost ##########
library(caret)
library(xgboost)
trctrl <- trainControl(method = "cv", number = 5)
xgb_vp <- train(as.factor(vp)~., data = dt_vp[,-c(1,16,17)], method = "xgbTree", trControl = trctrl, verbosity = 0)

xgb_preds_vp <- predict(xgb_vp, dt_vp)
confusionMatrix(data = xgb_preds_vp, reference = as.factor(dt_vp$vp))

xgb_table <- xgb.importance(model = xgb_vp$finalModel)
xgb.ggplot.importance(xgb_table)

save(xgb_preds_vp, file = "preds/vp_xgb_preds.RData")
save(xgb_vp, file = "models/vp_xgb.Rdata")


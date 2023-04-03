dt <- read.csv("data/data.csv")

dt$country <- as.integer(factor(dt$country))
dt$language <- as.integer(factor(dt$language))
dt$operating_system <- NULL

dt_train <- dt[dt$user_first_engagement < 1536019199,]
dt_test <- dt[dt$user_first_engagement > 1536019199 & dt$user_first_engagement < 1537228799,]
dt_train$user_first_engagement <- NULL
write.csv(x = dt_test, file = "app/data/test_data_churn7.csv")

#### Logistic Regression #########
logreg_churn7 <- glm(churn7~., data = dt_train[,-c(1,19)], family = "binomial")
logreg_churn14 <- glm(churn14~., data = dt_train[,-c(1,18)], family = "binomial")

library(caret)

logreg_preds_churn7 <- predict(logreg_churn7, newdata = dt_test, type = "response")
logreg_preds_churn7 <- ifelse(logreg_preds_churn7 > 0.5, 1, 0)
logreg_preds_churn14 <- predict(logreg_churn14, newdata = dt_test, type = "response")
logreg_preds_churn14 <- ifelse(logreg_preds_churn14 > 0.5, 1, 0)

confusionMatrix(data = as.factor(logreg_preds_churn7), reference = as.factor(dt_test$churn7))
confusionMatrix(data = as.factor(logreg_preds_churn14), reference = as.factor(dt_test$churn14))

save(logreg_preds_churn7, file = "preds/churn7_logreg_preds.RData")
save(logreg_churn7, file = "models/churn7_logreg.Rdata")
save(logreg_preds_churn14, file = "preds/churn14_logreg_preds.RData")
save(logreg_churn14, file = "models/churn14_logreg.Rdata")

# Create confusion matrix
cm <- confusionMatrix(data = as.factor(logreg_preds_churn7), reference = as.factor(dt_test$churn7))

library(knitr)

# Get confusion matrix metrics
metrics <- data.frame(
  Accuracy = cm$overall['Accuracy'],
  Kappa = cm$overall['Kappa'],
  Sensitivity = cm$byClass['Sensitivity'],
  Specificity = cm$byClass['Specificity']
)

# Print table of metrics
kable(metrics)



##### Decision Tree ########
library(rpart)
library(rpart.plot)

dtree_churn7 <- rpart(churn7~., data = dt_train[,-c(1,19)], method = "class", model = TRUE)
rpart.plot(dtree_churn7, type = 3,fallen.leaves = FALSE, branch.lty = 3,box.palette = "BuGn",clip.right.labs = FALSE, branch = .3, under = TRUE,shadow.col = "gray", main = "Model Decision Tree\n(Churn 7)\n")

dtree_churn14 <- rpart(churn14~., data = dt_train[,-c(1,18)], method = "class")
rpart.plot(dtree_churn14,type = 3,fallen.leaves = FALSE, branch.lty = 3,box.palette = "BuGn",clip.right.labs = FALSE, branch = .3, under = TRUE,shadow.col = "gray", main = "Model Decision Tree\n(Churn 14)\n")

dtree_preds_churn7 <- predict(dtree_churn7, dt_test, type = 'class')
confusionMatrix(data = as.factor(dtree_preds_churn7), reference = as.factor(dt_test$churn7))
dtree_preds_churn14 <- predict(dtree_churn14, dt_test, type = 'class')
confusionMatrix(data = as.factor(dtree_preds_churn14), reference = as.factor(dt_test$churn14))

save(dtree_preds_churn7, file = "preds/churn7_dtree_preds.RData")
save(dtree_churn7, file = "models/churn7_dtree.Rdata")
save(dtree_preds_churn14, file = "preds/churn14_dtree_preds.RData")
save(dtree_churn14, file = "models/churn14_dtree.Rdata")


###### XGBoost ##########
library(caret)
library(xgboost)
trctrl <- trainControl(method = "cv", number = 5)
xgb_churn7 <- train(as.factor(churn7)~., data = dt_train[,-c(1,19)], method = "xgbTree", trControl = trctrl, verbosity = 0)
xgb_churn14 <- train(as.factor(churn14)~., data = dt_train[,-c(1,18)], method = "xgbTree", trControl = trctrl, verbosity = 0)

xgb_preds_churn7 <- predict(xgb_churn7, dt_test)
confusionMatrix(data = xgb_preds_churn7, reference = as.factor(dt_test$churn7))
xgb_preds_churn14 <- predict(xgb_churn14, dt_test)
confusionMatrix(data = xgb_preds_churn14, reference = as.factor(dt_test$churn14))

xgb_table <- xgb.importance(model = xgb_churn7$finalModel)
xgb.ggplot.importance(xgb_table)
xgb_table <- xgb.importance(model = xgb_churn14$finalModel)
xgb.ggplot.importance(xgb_table)

save(xgb_preds_churn7, file = "preds/churn7_xgb_preds.RData")
save(xgb_churn7, file = "models/churn7_xgb.Rdata")
save(xgb_preds_churn14, file = "preds/churn14_xgb_preds.RData")
save(xgb_churn14, file = "models/churn14_xgb.Rdata")

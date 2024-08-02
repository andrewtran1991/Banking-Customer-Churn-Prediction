# Tune random forest by using random search
# mtry: Number of variables randomly sampled as candidates at each split.
# ntree: Number of trees to grow.
## Create a control object. 10-fold cross-validation and 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
rf_random <- train(rim_status_closed~., data=train, method="rf", metric='Accuracy', tunegrid=tunegrid, trControl=control)
print(rf_random)
plot(rf_random)

## Make the predictions
rf.pred.prob <- predict(rf_random, test, type = 'prob')
rf.pred <- as.factor(ifelse(rf.pred.prob[,2] > 0.5, 1, 0))
caret::confusionMatrix(rf.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, rf.pred.prob[,2])
roc.curve(test$rim_status_closed, rf.pred.prob[,2])
rf2.roc <- roc(test$rim_status_closed, rf.pred.prob[,2])

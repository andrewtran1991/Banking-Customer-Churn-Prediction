# Objective: Build other ML models

set.seed(1234)

# Logistic Regression
logit.mod_data_balanced_under <- glm(rim_status_closed ~., family = binomial(link = 'logit'), data = train)
logit.pred.prob <- predict(logit.mod_data_balanced_under, test, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
caret::confusionMatrix(logit.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, logit.pred.prob)
roc.curve(test$rim_status_closed, logit.pred.prob)
logit.roc <- roc(test$rim_status_closed, logit.pred.prob)


# Decision Tree
tree <- rpart(rim_status_closed ~. , data = train, method = "class", control = rpart.control((cp = 0.05)))
tree.pred.prob <- predict(tree, test, type = 'prob')
tree.pred <- as.factor(ifelse(tree.pred.prob[,2] > 0.5, 1, 0))
caret::confusionMatrix(tree.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, tree.pred.prob[,2])
roc.curve(test$rim_status_closed, tree.pred.prob[,2])
tree.roc <- roc(test$rim_status_closed, tree.pred.prob[,2])

# Random Forest
rf <- randomForest(rim_status_closed ~ ., data=train, proximity=FALSE,importance = FALSE)
rf.pred.prob <- predict(rf, test, type = 'prob')
rf.pred <- as.factor(ifelse(rf.pred.prob[,2] > 0.5, 1, 0))
caret::confusionMatrix(rf.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, rf.pred.prob[,2])
roc.curve(test$rim_status_closed, rf.pred.prob[,2])
rf.roc <- roc(test$rim_status_closed, rf.pred.prob[,2])




# Plot the AUC

par(pty="s")
plot(logit.roc, main = "ROC Curve for Bank Churn Prediction Approaches",  xlab="False Positive Rate", ylab="True Positive Rate",
     print.auc = F, col = "blue", grid = TRUE, legacy.axes = TRUE)
plot(tree.roc, print.auc = F, col = "green", print.auc.y = .4, add = TRUE)
plot(rf.roc, print.auc = F, col = "red", print.auc.y = .6, add = TRUE)
legend(0.55, 0.3, legend=c("LG (AUC=0.964)", "DT (AUC=0.995)", "RF (AUC=1.000"),
       col=c("green", "blue", "red"), lty=1:2, cex=0.6)




# From the evaluation table in the excel file, random forest is the best model.
# Save the model for UI
saveRDS(rf, "rf_model.rds")

# Predict a new data example
cols_factor <- c("rim_status_closed", "type")
train[cols_factor] <- lapply(train[cols_factor], as.factor) 

test_input <- read_csv("data/processed/mbr_all_final.csv")
test_input <- sample_n(test_input, 50)
write.csv(test_input, "test_input.csv", row.names=FALSE)

drop_cols = c("CRE", "HE", "orig_amt", "online_duration", "missing_payment", "total_ct", "avail_bal",
              "mortgage_rating", "email_rating", "rim_status_closed", "member_id", "rim_closed_dt")
test_input2 <- test_input %>% dplyr::select(-all_of(drop_cols))
test_input2 <- rbind(train[1, -1] , test_input2)
test_input2 <- test_input2[-1,]

pred_prob <- predict(rf, newdata = test_input2, type = 'prob')
round(pred_prob, 3)[,2]


# References:
# https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4
# https://www.datacamp.com/community/tutorials/decision-trees-R
# https://www.r-bloggers.com/2021/04/random-forest-in-r/
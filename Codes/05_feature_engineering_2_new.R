# Objective: Fix the imbalanced classes

set.seed(1234)

# Check the classes distribution of the original train dataset
table(train$rim_status_closed)
prop.table(table(train$rim_status_closed))

# Run the logistic model on the imbalanced train dataset
logit.mod <- glm(rim_status_closed ~., family = binomial(link = 'logit'), data = train)
logit.pred.prob <- predict(logit.mod, test, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))

caret::confusionMatrix(logit.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, logit.pred.prob)
roc.curve(test$rim_status_closed, logit.pred.prob)

# Since the data is unbalanced, we apply several sampling techniques to improve models' performance
major_class_n <- train %>% group_by(rim_status_closed) %>% count()
minor_class_n <- major_class_n$n[2]
major_class_n <- major_class_n$n[1]
#over sampling
data_balanced_over <- ovun.sample(rim_status_closed ~ ., data = train, method = "over",N = major_class_n*2)$data
table(data_balanced_over$rim_status_closed)

#under sampling
data_balanced_under <- ovun.sample(rim_status_closed ~ ., data = train, method = "under",N = minor_class_n*2)$data
table(data_balanced_under$rim_status_closed)

#both sampling
data_balanced_both <- ovun.sample(rim_status_closed ~ ., data = train, method = "both", p=0.5, N=nrow(train), seed = 1)$data
table(data_balanced_both$rim_status_closed)

#ROSE sampling
data.rose <- ROSE(rim_status_closed ~ ., data = train, seed = 1)$data
table(data.rose$rim_status_closed)

# Build 4 Logistic Models for 4 dataset above
logit.mod_data_balanced_over <- glm(rim_status_closed ~., family = binomial(link = 'logit'), data = data_balanced_over)
logit.mod_data_balanced_under <- glm(rim_status_closed ~., family = binomial(link = 'logit'), data = data_balanced_under)
logit.mod_data_balanced_both <- glm(rim_status_closed ~., family = binomial(link = 'logit'), data = data_balanced_both)
logit.mod_data.rose <- glm(rim_status_closed ~., family = binomial(link = 'logit'), data = data.rose)


# Evaluate the models
#Over sampling
summary(logit.mod_data_balanced_over)
logit.pred.prob <- predict(logit.mod_data_balanced_over, test, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
caret::confusionMatrix(logit.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, logit.pred.prob)
roc.curve(test$rim_status_closed, logit.pred.prob)

#Under sampling
summary(logit.mod_data_balanced_under)
logit.pred.prob <- predict(logit.mod_data_balanced_under, test, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
caret::confusionMatrix(logit.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, logit.pred.prob)
roc.curve(test$rim_status_closed, logit.pred.prob)

#Both sampling
summary(logit.mod_data_balanced_both)
logit.pred.prob <- predict(logit.mod_data_balanced_both, test, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
caret::confusionMatrix(logit.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, logit.pred.prob)
roc.curve(test$rim_status_closed, logit.pred.prob)

#ROSE sampling
summary(logit.mod_data.rose)
logit.pred.prob <- predict(logit.mod_data.rose, test, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
caret::confusionMatrix(logit.pred, test$rim_status_closed, positive = "1")
accuracy.meas(test$rim_status_closed, logit.pred.prob)
roc.curve(test$rim_status_closed, logit.pred.prob)

# Because the goal is to identify the churn customer, under-sampling dataset was chosen for further model building.
train_original <- copy(train)
train <- data_balanced_under

write.csv(train_original, "train_original.csv", row.names=FALSE)
write.csv(train, "train.csv", row.names=FALSE)
write.csv(test, "test.csv", row.names=FALSE)
 

# References:
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
# https://www.r-bloggers.com/2017/04/dealing-with-unbalanced-data-in-machine-learning/

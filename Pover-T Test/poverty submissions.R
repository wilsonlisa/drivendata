library(randomForest)
setwd("~/Documents/Learning R!/Driven Data/Pover-T Tests/")

# Country A: predictions
set.seed(18)
model_a <- randomForest(as.factor(poor.x)~., data=af_train, importance=TRUE, ntree=50)
varImpPlot(model_a)
pred_af1 <- predict(model_a, newdata=af_test, type="prob")
summary(pred_af1)
colnames(pred_af1) <- c("rich", "poor")
pred_af1.df <- data.frame(pred_af1)

sub_af1 <- data.frame(af_test$id, af_test$country, pred_af1.df$poor)
colnames(sub_af1) <- c("id", "country", "poor")

# Country B: predictions
# think about factor engineering (pumps, DataCamp) for many levels
bf_train$PxgyaWYq <- NULL
bf_train$OhKTEjVy <- NULL
bf_train$HQPtUibh <- NULL
bf_train$HZEsXrsF <- NULL
bf_train$lCKzGQow <- NULL
bf_train$BqXRUQgi <- NULL
bf_test$PxgyaWYq <- NULL
bf_test$OhKTEjVy <- NULL
bf_test$HQPtUibh <- NULL
bf_test$HZEsXrsF <- NULL
bf_test$lCKzGQow <- NULL
bf_test$BqXRUQgi <- NULL
set.seed(20)
model_b <- randomForest(as.factor(poor.x)~., data=bf_train, ntree=50)
pred_bf1 <- predict(model_b, newdata=bf_test, type="prob")
summary(pred_bf1)
colnames(pred_bf1) <- c("rich", "poor")
pred_bf1.df <- data.frame(pred_bf1)

sub_bf1 <- data.frame(bf_test$id, bf_test$country, pred_bf1.df$poor)
colnames(sub_bf1) <- c("id", "country", "poor")

# Country C: predictions
# think about factor engineering (pumps, DataCamp) for many levels
cf_train$FRcdTUFo <- NULL
cf_train$HNRJQbcm <- NULL
cf_test$FRcdTUFo <- NULL
cf_test$HNRJQbcm <- NULL
set.seed(22)
model_c <- randomForest(as.factor(poor.x)~., data=cf_train, ntree=50)
pred_cf1 <- predict(model_c, newdata=cf_test, type="prob")
summary(pred_cf1)
colnames(pred_cf1) <- c("rich", "poor")
pred_cf1.df <- data.frame(pred_cf1)

sub_cf1 <- data.frame(cf_test$id, cf_test$country, pred_cf1.df$poor)
colnames(sub_cf1) <- c("id", "country", "poor")

# combine country results
subf1 <- rbind(sub_af1, sub_bf1, sub_cf1)
write.csv(subf1, file="povertyf1.csv", row.names=F)
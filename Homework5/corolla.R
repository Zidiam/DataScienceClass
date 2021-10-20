#Question 1
corolla = read.csv('ToyotaCorolla.csv')
View(corolla)
hist(corolla$Price)
plot(corolla$Age, corolla$Price)
boxplot(corolla$Price ~ corolla$FuelType)

set.seed(12345)
corolla = corolla[sample(1:dim(corolla)[1]),]
corolla_train = corolla[1:1077,]
corolla_test = corolla[1078:1436,]

#Question2
lm_corolla = lm(Price ~ Age, data=corolla_train)
plot(corolla_train$Age, corolla_train$Price)
abline(lm_corolla)
summary(lm_corolla)
#Hand Calculation for 12month
y = -170.21*12 + 20280.20
y
#Predicting
predict(lm_corolla, data.frame(Age=c(12,24,120)), interval = "prediction")
#Hand calculatio and 12 month predict are close but not exact 
#since hand calculation doesn't include past two decimal places
corolla_test_preds = predict(lm_corolla, corolla_test)
rmse = sqrt(mean((corolla_test_preds - corolla_test$Price)^2))
rmse

#Question3
corolla_train$Age_sqrt = sqrt(corolla_train$Age)
corolla_test$Age_sqrt = sqrt(corolla_test$Age)
View(corolla_train)

lm_corolla = lm(Price ~ Age_sqrt, data=corolla_train)
plot(corolla_train$Age_sqrt, corolla_train$Price)
abline(lm_corolla)
summary(lm_corolla)
predict(lm_corolla, data.frame(Age_sqrt=c(sqrt(12))), interval = "prediction")
corolla_test_preds = predict(lm_corolla, corolla_test)
rmse = sqrt(mean((corolla_test_preds - corolla_test$Price)^2))
rmse

#Question4
lm_corolla = lm(Price ~ KM, data=corolla_train)
plot(corolla_train$KM, corolla_train$Price)
abline(lm_corolla)
summary(lm_corolla)
y = -.05584*50000 + 14577
y
predict(lm_corolla, data.frame(KM=c(100000)))
predict(lm_corolla, data.frame(KM=c(100000)), interval = "prediction")
corolla_test_preds = predict(lm_corolla, corolla_test)
rmse = sqrt(mean((corolla_test_preds - corolla_test$Price)^2))
rmse

#Question5
corolla_train$KM_sqrt = sqrt(corolla_train$KM)
corolla_test$KM_sqrt = sqrt(corolla_test$KM)
View(corolla_train)

lm_corolla = lm(Price ~ KM_sqrt, data=corolla_train)
plot(corolla_train$KM_sqrt, corolla_train$Price)
abline(lm_corolla)
summary(lm_corolla)
y = -31.13*100 + 18565.41
y
predict(lm_corolla, data.frame(KM_sqrt=c(100)), interval = "prediction")
corolla_test_preds = predict(lm_corolla, corolla_test)
rmse = sqrt(mean((corolla_test_preds - corolla_test$Price)^2))
rmse

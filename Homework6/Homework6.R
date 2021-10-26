corolla = read.csv('ToyotaCorolla.csv')
View(corolla)
set.seed(12345)
corolla = corolla[sample(1:dim(corolla)[1]),]
corolla$Age_sqrt = sqrt(corolla$Age)
corolla_train = corolla[1:718,]
corolla_val = corolla[719:1077,]
corolla_test = corolla[1078:1436,]
#Question 1
lm_corolla = lm(Price ~ Age, data=corolla_train)
plot(corolla_train$Age, corolla_train$Price)
abline(lm_corolla)
summary(lm_corolla)
corolla_test_preds = predict(lm_corolla, corolla_test)
rmse = sqrt(mean((corolla_test_preds - corolla_test$Price)^2))
rmse
#Question 2
lm_corolla2 = lm(Price ~ ., data=corolla_train)
plot(corolla_train$Age, corolla_train$Price)
summary(lm_corolla2)

lm_corolla2 = lm(Price ~ ., data=corolla_val)
corolla_test_preds = predict(lm_corolla2, corolla_test)
rmse = sqrt(mean((corolla_test_preds - corolla_test$Price)^2))
rmse

#Question 3
#First Predictor
lm_corolla3 = lm(Price ~ . -Age, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -KM, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -CC, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -HP, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Weight, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -FuelType, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age_sqrt, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

#Second Predictor with Age 1286.468
lm_corolla3 = lm(Price ~ . -Age -KM, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -CC, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -HP, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Weight, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -FuelType, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Age_sqrt, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

#Third Predictor with Age Automatic 1284.538
lm_corolla3 = lm(Price ~ . -Age -KM -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -CC -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -HP -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Weight -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Doors -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -FuelType -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -MetColor -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Age_sqrt -Automatic, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

#Third Predictor with Age Automatic MetColor 1282.641
lm_corolla3 = lm(Price ~ . -Age -KM -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -CC -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -HP -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Weight -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Doors -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -FuelType -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Age_sqrt -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

#Fourth Predictor with Age Automatic MetColor Doors 1281.702
lm_corolla3 = lm(Price ~ . -Age -KM -Automatic -MetColor -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -CC -Automatic -MetColor -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -HP -Automatic -MetColor -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Weight -Automatic -MetColor -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -FuelType -Automatic -MetColor -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

lm_corolla3 = lm(Price ~ . -Age -Age_sqrt -Automatic -MetColor -Doors, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_val)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_val$Price)^2))
rmse3

#We dont include the last model test since it did not decrease RMSE
lm_corolla3 = lm(Price ~ . -Age -Doors -Automatic -MetColor, data=corolla_train)
lm_val_preds3 = predict(lm_corolla3, corolla_test)
rmse3 = sqrt(mean((lm_val_preds3 - corolla_test$Price)^2))
rmse3
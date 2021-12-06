boston = read.csv('boston.csv')
set.seed(123)
boston = boston[sample(1:dim(boston)[1]),]
boston_train = boston[0:(nrow(boston)*.6),]
boston_test = boston[(nrow(boston)*.6 + 1): nrow(boston),]
#Question 1
lm_boston= lm(medv ~ lstat, data=boston_train)
predict(lm_boston, data.frame(lstat=c(15)), interval = "prediction")

#Question 2
plot(boston_train$lstat, boston_train$medv)
abline(lm_boston)
summary(lm_boston)

#Question 3
boston_test_preds = predict(lm_boston, boston_test)
rmse = sqrt(mean((boston_test_preds - boston_test$medv)^2))
rmse

#Question 4
lm_boston2 = lm(medv ~ ., data=boston_train)
boston_test_preds = predict(lm_boston2, boston_test)
rmse = sqrt(mean((boston_test_preds - boston_test$medv)^2))
rmse

#Question 5
#First Predictor
#Best
lm_boston3 = lm(medv ~ . -crim, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

lm_boston3 = lm(medv ~ . -indus, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

lm_boston3 = lm(medv ~ . -nox, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

lm_boston3 = lm(medv ~ . -rm, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

lm_boston3 = lm(medv ~ . -age, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

lm_boston3 = lm(medv ~ . -dis, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

lm_boston3 = lm(medv ~ . -ptratio, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

lm_boston3 = lm(medv ~ . -lstat, data=boston_train)
lm_val_preds3 = predict(lm_boston3, boston_test)
rmse3 = sqrt(mean((lm_val_preds3 - boston_test$medv)^2))
rmse3

#Question 6
grades = read.csv('grades_example.csv')
set.seed(11)
grades_scale = as.data.frame(scale(grades))
grades_clusters = kmeans(grades_scale, 3)
grades_clusters

#Question 7
plot(grades_scale[c(2, 4)], col=grades_clusters$cluster)
#1 == Black, 2 == Red, 3 == Green

#Question 8
grades_clusters$centers
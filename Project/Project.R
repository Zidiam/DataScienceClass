#Data
fortune = read.csv('Fortune_1000.csv')
fortune = fortune[,c(-1, -5, -6, -9, -14)]
fortune$Profitable = as.factor(fortune$Profitable)

#Analysis
plot(fortune$Rank, fortune$Revenue, col=fortune$Profitable, 
     main="Rank Compared to Revenue",
     xlab="Rank",
     ylab="Revenue")
hist(fortune$Rank_change,
     main="Frequency in Rank Change",
     xlab="Rank Change",
     xlim=c(-150,150),
     col="darkmagenta")

#Setting up data
set.seed(12345)
fortune = fortune[sample(1:dim(fortune)[1]),]
fortune_train = fortune[1:650,]
fortune_test = fortune[651:841,]

#Classification
##C50
#install.packages('C50')
library(C50)
fortune_tree = C5.0(fortune_train[,-8], fortune_train$Profitable)
fortune_tree_pred = predict(fortune_tree, fortune_test)
tab = table(fortune_tree_pred, fortune_test$Profitable)
#Confusion Matrix
tab
#Sum of all values in confusion Matrix
sum(tab)
#Accuracy
sum(diag(tab))/sum(tab) * 100
plot(fortune_tree)

##Random Forest
#install.packages('randomForest')
library(randomForest)
fortune_tree = randomForest(fortune_train[,-8], fortune_train$Profitable)
fortune_tree_pred = predict(fortune_tree, fortune_test)
tab = table(fortune_tree_pred, fortune_test$Profitable)
#Confusion Matrix
tab
#Sum of all values in confusion Matrix
sum(tab)
#Accuracy
sum(diag(tab))/sum(tab)
plot(fortune_tree)

#Simple Regression
lm_fortune = lm(Revenue ~ Num_of_employees, data=fortune_train)
plot(fortune_train$Revenue, fortune_train$Num_of_employees, ylim=c(0,100000), xlim=c(2000,10000),
     main="Revenue Compared to Number of Employees",
     xlab="Revenue",
     ylab="Number of Employees")
abline(lm_fortune)
summary(lm_fortune)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

#Multiple Regression
fortune_train = fortune[1:400,]
fortune_test = fortune[401:650,]
fortune_val = fortune[651:841,]

lm_fortune = lm(Revenue ~ ., data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ ., data=fortune_val)
fortune_val_preds = predict(lm_fortune, fortune_val)
rmse = sqrt(mean((fortune_val_preds - fortune_val$Revenue)^2))
rmse

##Removing Columns
lm_fortune = lm(Revenue ~ .-Rank, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Rank_change, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Num_of_employees, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Sector, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_founder, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Market.Cap, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Share.Price, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Average.Volume, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

##Second
lm_fortune = lm(Revenue ~ .-Ceo_woman -Rank, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Rank_change, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Num_of_employees, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Sector, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Market.Cap, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Share.Price, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Average.Volume, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

##Third
lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder -Rank, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder -Rank_change, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder -Num_of_employees, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder -Sector, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder -Market.Cap, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder -Share.Price, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder -Average.Volume, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

##The best model:
lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder, data=fortune_train)
fortune_test_preds = predict(lm_fortune, fortune_test)
rmse = sqrt(mean((fortune_test_preds - fortune_test$Revenue)^2))
rmse

##Validating the best model:
lm_fortune = lm(Revenue ~ .-Ceo_woman -Ceo_founder, data=fortune_val)
fortune_val_preds = predict(lm_fortune, fortune_val)
rmse = sqrt(mean((fortune_val_preds - fortune_val$Revenue)^2))
rmse

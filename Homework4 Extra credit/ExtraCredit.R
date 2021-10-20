install.packages('C50') #Install Once
install.packages("randomForest") #Install Once
library(C50)
library(randomForest)

#Start Code:
spam = read.csv('spam.csv')
spam$result = as.factor(spam$result)
set.seed(12345)
spam = spam[sample(1:dim(spam)[1]),]
View(spam)
size = lengths(spam[1])
spam_train = spam[1:(size*.9),]
spam_test = spam[(size*.9+1):size,]

#C50 Tree Prediction Code:
spam_tree = C5.0(spam_train[,-1], spam_train[,1])
spam_tree_pred = predict(spam_tree, spam_test)
tab = table(spam_tree_pred, spam_test$result)
tab
sum(diag(tab)/sum(tab))*100

#randomForest Tree Prediction Code:
spam_tree = randomForest(spam_train[,-1], spam_train[,1])
spam_tree_pred = predict(spam_tree, spam_test)
tab = table(spam_tree_pred, spam_test$result)
tab
sum(diag(tab)/sum(tab))*100


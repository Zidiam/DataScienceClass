glass = read.csv('glass.csv')
glass$Type = as.factor(glass$Type)
levels(glass$Type)
levels(glass$Type) = c('bwf', 'bwnf', 'cont', 'head', 'table', 'veh')
set.seed(12345)
glass = glass[sample(1:dim(glass)[1]),]

View(glass)
mean(glass[,2])
mean(glass[,3])
mean(glass[,4])
mean(glass[,5])
mean(glass[,6])
mean(glass[,7])
mean(glass[,8])
mean(glass[,9])
mean(glass[,10])
boxplot(glass$Na ~ glass$Type)
boxplot(glass$Si ~ glass$Type)
boxplot(glass$Fe ~ glass$Type)

View(glass)

glass_train = glass[1:139,]
glass_test = glass[140:214,]
#install.packages('C50')
library(C50)
glass_tree = C5.0(glass_train[,-1], glass_train[,1])
glass_tree_pred = predict(glass_tree, glass_test)
tab = table(glass_tree_pred, glass_test$Type)
#Confusion Matrix
tab
#Sum of all values in confusion Matrix
sum(tab)
#Accuracy
sum(diag(tab))/sum(tab)

#install.packages('randomForest')
library(randomForest)
glass = read.csv('glass.csv')
glass$Type = as.factor(glass$Type)
levels(glass$Type)
levels(glass$Type) = c('bwf', 'bwnf', 'cont', 'head', 'table', 'veh')
set.seed(12345)
glass = glass[sample(1:dim(glass)[1]),]

glass_train = glass[1:139,]
glass_test = glass[140:214,]
glass_tree = randomForest(glass_train[,-1], glass_train[,1])
glass_tree_pred = predict(glass_tree, glass_test)
tab = table(glass_tree_pred, glass_test$Type)
#Confusion Matrix
tab
#Sum of all values in confusion Matrix
sum(tab)
#Accuracy
sum(diag(tab))/sum(tab)

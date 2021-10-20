#Default Values
seeds = read.csv('seeds.csv')
seeds$type = as.factor(seeds$type)

boxplot(seeds$asymmetry ~ seeds$type)
plot(seeds$kernel_length, seeds$kernel_width, col=seeds$type)

set.seed(12)
seeds = seeds[sample(1:dim(seeds)[1]),]
View(seeds)
seed_train = seeds[1:140,]
seed_test = seeds[141:length(seeds$type),]

#C50 Tree 
install.packages('C50') #Run Once
library('C50')
seed_tree = C5.0(seed_train[,-1], seed_train[,1])
plot(seed_tree)
seed_pred = predict(seed_tree, seed_test)
tab = table(seed_pred, seed_test$type)
tab #Confusion Matrix
sum(diag(tab))/sum(tab) * 100 #Percent Accuracy

#Random Forest Tree
install.packages('randomForest') #Run Once
library('randomForest')
seed_tree = randomForest(seed_train[,-1], seed_train[,1])
seed_pred = predict(seed_tree, seed_test)
tab = table(seed_pred, seed_test$type)
tab #Confusion Matrix
sum(diag(tab))/sum(tab) * 100 #Percent Accuracy
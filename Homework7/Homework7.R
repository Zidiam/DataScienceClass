boston = read.csv("boston.csv")
mean(boston$lstat)
sd(boston$lstat)
boston_norm = as.data.frame(scale(boston))
set.seed(12345)
boston_norm = boston_norm[sample(1:dim(boston_norm)[1]),]
View(boston_norm)
boston_clusters = kmeans(boston_norm, 5)
boston_clusters$size
plot(boston_norm, col=boston_clusters$cluster)
plot(boston_norm[c(3, 4, 8)], col=boston_clusters$cluster)
boston_clusters$centers
#1 = Black, 2 = Red, 3 = Green, 4 = Cyan, 5 = Blue

within_ss = rep(0,15)
for (k in 1:15) {
  boston_clusters = kmeans(boston_norm,k)
  within_ss[k] = sum(boston_clusters$withinss)
}
plot(1:15, within_ss, xlab="k", ylab="within-cluster ss")
install.packages('Matrix')
install.packages('arules')
library(Matrix)
library(arules)

#Question 1
amazon = read.transactions("amazon_words.csv", sep = ",")
summary(amazon)

#Question 2
amazon_rules = apriori(amazon, parameter = list(support = 0.005, confidence = 0.5, minlen = 2))
summary(amazon_rules)

#Question 3
inspect(sort(amazon_rules, by = "lift")[1:20])

#Quesrion 4
amazon_rules = apriori(amazon, parameter = list(support = 0.01, confidence = 0.5, minlen = 2))
summary(amazon_rules)
inspect(sort(amazon_rules, by = "lift")[1:20])

#Question 5
amazon_rules = apriori(amazon, parameter = list(support = 0.005, confidence = 0.5, minlen = 2))
not_rules = subset(amazon_rules, items %in% "not")
inspect(sort(not_rules, by = "lift")[1:30])

#Question 6
amazon_rules = apriori(amazon, parameter = list(support = 0.005, confidence = 0.5, minlen = 2))
not_rules = subset(amazon_rules, items %in% "recommend")
inspect(sort(not_rules, by = "lift")[1:20])
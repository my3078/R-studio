install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
item <- read.csv("dataset_group.csv")
names(item) <- c("date", "ID", "product")
str(item)
#트랙잭션 형태로 변환
item_list <- split(item$product, item$ID)
item_list
trans <- as(item_list, "transactions")
#연관규칙 만들기
rules <- apriori(trans)
summary(rules)
inspect(head(rules))
plot(rules, method="grouped")
#유용한 연관규칙 확인
sum.rules <- apriori(trans, parameter=list(support=0.04, confidence=1.0))
summary(sum.rules)
inspect(sum.rules)

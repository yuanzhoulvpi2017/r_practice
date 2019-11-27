data1 <- read.table("ceshi1.txt", header = TRUE, stringsAsFactors = TRUE)

library(arules)
test <- data1[!duplicated(data1[,c(1, 2)]),]
Sales_trans<- as(split(test$CateName,test$SheetID),"transactions")


Sales_apriori_model <- apriori(Sales_trans, 
                               parameter=list(support=0.2,confidence=0.1))

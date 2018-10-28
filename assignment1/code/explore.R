
library(clValid)
library(e1071)
library(car)
library(corrplot)


temp <- cbind(qs.data$Q1,qs.data$Q2,qs.data$Q9,qs.data$Q17,qs.data$Q28,qs.data$Q29,qs.data$Q31,qs.data$Q36,qs.data$Q52,qs.data$Q53,qs.data$Q56,qs.data$Q68)
colnames(temp) <- c("Work Exp","Work Rate", "Reading", "Salary", "Height","Age", "Countries", "Chocolate", "Happiness", "Weight", "Sleep", "Coffee")

summary(temp)
for(i in 1:12) {
  boxplot(temp[,i], main = colnames(temp)[i])
}

clv <- clValid(t(temp), 2:6, metric = "correlation", validation = c("internal", "stability"))
summary(clv)

corrplot(cor(temp), order = "hclust", addrect = 2, bg="#888888", method="number", tl.pos = "d")
corrplot(cor(temp), order = "hclust", addrect = 5, bg="#888888", method="number", tl.pos = "d")

scatterplotMatrix(temp)


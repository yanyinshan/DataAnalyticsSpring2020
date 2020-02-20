rm(list = ls())
library(rpart)
library(rpart.plot)
library(ggplot2)
library(C50)


data2 <- data(msleep)
str(msleep)

mSleepDF1 <- msleep[,c(3,6,10,11)]
sleepModel_1 <- rpart(sleep_total ~ ., data = mSleepDF1, method = 'anova')

sleepModel_1

rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)

# ----- iris -----
install.packages("C50")
require(C50)
head(iris)
set.seed(9850)
grn <-runif(nrow(iris))
irisrand <-iris[order(grn),]

classificationmodel1 <- C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1

prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1

# ----- iris -----
install.packages('e1071')
library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 


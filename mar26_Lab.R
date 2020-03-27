rm(list = ls())


wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")

# variable names
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")

heatmap(cor(wine_data),Rowv = NA, Colv = NA)

cultivar_classes <- factor(wine_data$Cvs)

#PCA 

wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)

#rpart
require(rpart)
wine_rpart <- rpart(Cvs ~ ., data = wine_data)

summary(wine_rpart)

plot(wine_rpart)
text(wine_rpart)

# tree
require(party)
wine_tree <-ctree(Cvs ~ ., data=wine_data)
plot(wine_tree)



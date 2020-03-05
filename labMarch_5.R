set.seed(808)

par(mar = rep(0.2,4))
data_m <- matrix(rnorm(400),nrow = 40)
image(1:10, 1:40,t(data_m)[,nrow(data_m):1])

heatmap(data_m)

set.seed(678910)
for(i in 1:40){
        coin_Flip <- rbinom(1,size = 1, prob = 0.5)
        if(coin_Flip){
                data_m[i,] <- data_m[i,] + rep(c(0,3),each = 5)
        }
}

image(1:10, 1:40,t(data_m)[,nrow(data_m):1])
par(mar = rep(0.2,4))
image(1:10, 1:40,t(data_m)[,nrow(data_m):1])
par(mar = rep(0.2,4))
heatmap(data_m)

hh <- hclust(dist(data_m))
data_m_Ordered <- data_m[hh$order,]
par(mfrow = c(1,3))

image(t(data_m_Ordered),40:1,,xlab = "The Row Mean", ylab = "Raw",pch = 19)

plot(rowMeans(data_m),40:1,,xlab = "The Row Mean", ylab = "Raw",pch = 19)



#lab knn1

nyt1 <- read.csv("nyt1.csv")
nnyt1<-dim(nyt1)[1]

sampling.rate=0.9
num.test.set.labels=nnyt1*(1.-sampling.rate)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]
classif<-knn(train,test,cg,k=5)
classif
attributes(.Last.value)



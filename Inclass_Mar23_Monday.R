# ----- US arrests -----
data("USArrests")
states=row.names(USArrests) 

states
names(USArrests)

apply(USArrests,2,mean)
apply(USArrests,2,var)

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)
pr.out$x

biplot(pr.out, scale=0)

pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)


# ----- iris -----
data("iris")
iris

irisdata <- iris[,1:4]
p_c <- princomp(irisdata, cor = TRUE, score = TRUE)
summary(p_c)


plot(p_c)
biplot(p_c)


# ----- Boston dataset -----

library("MASS")
data('Boston')
Boston

pca_out <- prcomp(Boston,scale. = T)
pca_out
biplot(pca_out, scale = 0)

plot(pca_out)
boston_pc <- pca_out$x
summary(boston_pc)



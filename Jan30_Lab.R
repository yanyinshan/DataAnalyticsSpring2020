rm(list = ls())

# -----inclass-----

setwd("C:/Users/Yinshan/Desktop/Study/RPI/2020 SPRING/ITWS 6600 Data Analytics/Week2")

GPW3 <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
# ----_Excercise 1-----
EPI_data <- read.csv('2010EPI_data.csv',header = TRUE,skip = 1)                

EPI <- EPI_data$EPI
plot(ecdf(EPI),do.points = F, verticals = T)
qqnorm(EPI)
qqline(EPI)

x <- seq(30,95,1)
qqplot(qt(ppoints(250),df = 5),x,xlab = "Q-Q plot for tdsn")
qqline(x)

plot(ecdf(EPI),do.points = T, verticals = T)
plot(ecdf(EPI),do.points = F, verticals = T)



# Other variable to practice plot functions

DALY <- na.omit(EPI_data$DALY)

par(pty="s")
plot(ecdf(DALY),do.points = T,verticals = T)
plot(ecdf(DALY),do.points = F,verticals = T)
qqnorm(DALY)

Water <- na.omit(EPI_data$WATER_H)
plot(ecdf(Water),do.points = T, verticals = T)
plot(ecdf(Water),do.points = F, verticals = T)
qqnorm(Water)

qqplot(EPI,DALY)

boxplot(EPI,DALY,Water)
EnvHealth <- na.omit(EPI_data$ENVHEALTH)
Ecosystem <- na.omit(EPI_data$ECOSYSTEM)
AIR_H <- na.omit(EPI_data$AIR_H)
AIR_EWATER_E <- na.omit(EPI_data$AIR_E)
Biodiversity <- na.omit(EPI_data$BIODIVERSITY)

boxplot(EPI,DALY,Water,EnvHealth,Ecosystem,AIR_H,AIR_EWATER_E,Biodiversity)


multi <- read.csv('multivariate.csv')
attach(multi)
mm <-lm(Homeowners~Immigrant)
mm

# -----inclass-----
setwd("C:/Users/Yinshan/Desktop/Study/RPI/2020 SPRING/ITWS 6600 Data Analytics/Week2")
days <- c('Mon','Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) 
snowed <- c(T,T,F,F,T,T,F)

RPI_Weather_Week <- data.frame(days,temp,snowed)

RPI_Weather_Week
head(RPI_Weather_Week)

str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp

subset(RPI_Weather_Week,subset=snowed==TRUE)

sorted.snowed<- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow

empty.DataFrame <- data.frame()
v1 <- 1:10
v1
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df

write.csv(df,file = 'saved_df1.csv')
# -----get data in-----
rm(list = ls())
GPW3 <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")

# -----Excercise 1-----
EPI_data <- read.csv('2010EPI_data.csv',header = TRUE,skip = 1)                

EPI <- EPI_data$EPI
summary(EPI)
tf <- is.na(EPI) 
E <- EPI[!tf]
fivenum(E)

jpeg('EPI.jpg')
hist(EPI,seq(30,95,1),probability = TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
dev.off()


DALY <- EPI_data$DALY
summary(DALY)

jpeg('BOX_EPI_DALY.jpg')
boxplot(EPI,DALY)
dev.off()

jpeg('CDF_EPI_DALY.jpg')
qqplot(EPI,DALY)
dev.off()

#CDF
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

#QQ
par(pty="s") 
qqnorm(EPI)

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
# -----Excercise 2-----

EPI_landlock <- EPI_data$Landlock
ETF <- is.na(EPI_landlock)
EPI_Landlock <- EPI[!ETF]
hist(EPI_Landlock,seq(30., 95., 1.0), prob=TRUE)

lines(density(EPI_Landlock,na.rm=TRUE,bw=1.)) 
rug(EPI_Landlock)


plot(ecdf(EPI_Landlock), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(EPI_Landlock)

NoSurWater <- EPI_data$No_surface_water
STF <- is.na(NoSurWater)     
EPINosurwater <- EPI[!STF]

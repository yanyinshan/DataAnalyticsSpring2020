rm(list = ls())
library(ggplot2)
data(diamonds)
head(diamonds)


ggplot(
        data = diamonds,
        mapping = aes(color = cut_number(carat, 5), x = price)
) +
        geom_freqpoly() +
        labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
        geom_boxplot() +
        coord_flip() +
        xlab("Price")

ggplot(diamonds, aes(x = cut_number(carat, 5), y =
                             price, colour = cut)) +
        geom_boxplot()


ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()

ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)

ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept=12000)

#lab1
data(cars)
cars.lo<-loess(dist~speed, cars)
predict(cars.lo, data.frame(speed=seq(5,30,1)),se=T)
#To allow extrapolation
cars.lo2<-loess(dist~speed, cars, control=loess.control(surface="direct"))
predict(cars.lo2, data.frame(speed=seq(5,30,1)),se=T)
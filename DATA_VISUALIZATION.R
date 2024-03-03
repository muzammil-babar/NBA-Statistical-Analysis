#DATA VISUALIZATION

stats1 <- FINALSTATS
#NAMING

role <- stats1$`Position`
points <- stats1$`Career Points`
assists <- stats1$`Career Assists`
rebound <- stats1$`Career Rebounds`
height<- stats1$`Height (cm)`
weight<- stats1$`Weight (kg)`
games_played <- stats1$`Games Played`

roles = as.numeric(as.factor(role))
roles

#HISTOGRAM
install.packages("ggplot2")
library(ggplot2)

x11()
par(mfrow=c(3,3))
#hist(roles,xlim = range(1,5))

hist(points,probability = TRUE)
abline(v=mean(points), col='red')
abline(v=median(points), col='green')
curva_x <- seq(min(points), max(points), length = 1000)
curva_y <- dnorm(curva_x, mean=mean(points), sd = sd(points))
lines(curva_x, curva_y, col = "blue")

hist(assists,probability = TRUE)
abline(v=mean(assists), col='red')
abline(v=median(assists), col='green')
curva_x <- seq(min(assists), max(assists), length = 1000)
curva_y <- dnorm(curva_x, mean=mean(assists), sd = sd(assists))
lines(curva_x, curva_y, col = "blue")

hist(rebound,probability = TRUE)
abline(v=mean(rebound), col='red')
abline(v=median(rebound), col='green')
curva_x <- seq(min(rebound), max(rebound), length = 1000)
curva_y <- dnorm(curva_x, mean=mean(rebound), sd = sd(rebound))
lines(curva_x, curva_y, col = "blue")

hist(height,probability = TRUE)
abline(v=mean(height), col='red')
abline(v=median(height), col='green')
curva_x <- seq(min(height), max(height), length = 1000)
curva_y <- dnorm(curva_x, mean=mean(height), sd = sd(height))
lines(curva_x, curva_y, col = "blue")

hist(weight,probability = TRUE)
abline(v=mean(weight), col='red')
abline(v=median(weight), col='green')
curva_x <- seq(min(weight), max(weight), length = 1000)
curva_y <- dnorm(curva_x, mean=mean(weight), sd = sd(weight))
lines(curva_x, curva_y, col = "blue")

hist(games_played,probability = TRUE)
abline(v=mean(games_played), col='red')
abline(v=median(games_played), col='green')
curva_x <- seq(min(games_played), max(games_played), length = 1000)
curva_y <- dnorm(curva_x, mean=mean(games_played), sd = sd(games_played))
lines(curva_x, curva_y, col = "blue")

hist(career_span,probability = TRUE)
abline(v=mean(career_span), col='red')
abline(v=median(career_span), col='green')
curva_x <- seq(min(career_span), max(career_span), length = 1000)
curva_y <- dnorm(curva_x, mean=mean(career_span), sd = sd(career_span))
lines(curva_x, curva_y, col = "blue")



#BOXPLOT 
install.packages("ggplot2")
library(ggplot2)


x11()
par(mfrow=c(3,1))
boxplot(points ~ role)
boxplot(rebound ~ role)
boxplot(assists ~ role)
x11()
par(mfrow=c(2,1))
boxplot(weight ~ role)
boxplot(height~ role)



mean(stats$`Career Points`)
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)


par(mfrow=c(3,3)
hist(stats1$`Career Points`)
hist(stats1$`Height (cm)`)
    

install.packages("cluster")
library(cluster)
kmeans(x, centers, iter.max = 10, nstart = 1)


x11()
plot(stats1$`Height (cm)`, stats1$`Weight (kg)`)
formal<- stats1[,-c(1,2,3,4,5,6,7,10)]
clust<- kmeans(formal, centers = 5, nstart=2)
plot(formal, col=clust$cluster, asp = 1, pch=16)

PAR <- stats1[,-c(1,2,3,4,5,6,10,11,12,13,14,15,16)]

install.packages(rgl)
library(rgl)

value <- stats1[,-c(1,2,3,4,5,6,10,11,12,13,14,15,16)]
rom<- kmeans(value, centers = 3, nstart=2)

X11()
plot(value, col=rom$cluster, asp = 1, pch=16)

x11()
plot(value, col = rom$cluster,asp = 1,pch=16, size=5)

x11()
plot3d(value, col = rom$cluster,asp = 1,pch=16, size=5)

#boxplot
install.packages("ggplot2")
library(ggplot2)

x11()
par(mfrow=c(3,1))
boxplot(stats1$`Career Rebounds`~ stats1$`Position 1`)
boxplot(stats1$`Career Assists`~ stats1$`Position 1`)
boxplot(stats1$`Career Points`~ stats1$`Position 1`)

#career span for different eras
x11()
plot(stats1$`Career Start`,stats1$`Career End`)



#optimal
metric = dist(PAR)


hc.complete = hclust(metric, method="average")  # Linkage completo
hc.average = hclust(metric, method="average")    # Linkage average
hc.single = hclust(metric, method="single")      # Single Linkage

x11()
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

clusters_complete = cutree(hc.complete, 4)
clusters_average = cutree(hc.average, 4)
clusters_single = cutree(hc.single, 4)
clusters_complete


#
within.SS <- matrix(NA, 1000, 10)
for(rep in 1:1000)
{
  for(k in 1:10)
  {
    clustK <- kmeans(PAR, k)
    within.SS[rep,k] <- clustK$tot.withinss
  }
}
X11()
boxplot(within.SS/dim(PAR)[1], main='Within SS')
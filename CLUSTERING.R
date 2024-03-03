#CLUSTER
install.packages(rgl)
library(rgl)
install.packages("cluster")
library(cluster)

#########

# Load the readxl package
library(readxl)

# Read in the Excel file FINALSTATS.xlsx
FINALSTATS <- read_excel("C:/Users/Muzammil Babar/Downloads/FINALSTATS.xlsx")

# Load the rgl package
library(rgl)

# Install the cluster package
install.packages("cluster")

# Load the cluster package
library(cluster)

  
######

role <- stats1$`Position`
points <- stats1$`Career Points`
assists <- stats1$`Career Assists`
rebound <- stats1$`Career Rebounds`
height<- stats1$`Height (cm)`
weight<- stats1$`Weight (kg)`
games_played <- stats1$`Games Played`  

######

PAR <- FINALSTATS[,-c(1,2,3,4,5,9,10,11,12,13,14)]
PAR$Position = as.numeric(as.factor(role))



# Create a new variable in the PAR data frame and assign it the values of the `role` column
PAR$Position <- as.numeric(as.factor(FINALSTATS$Position))  

#OPTIMAL NUMBER OF CLUSTERS
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



#OPTIMAL NUMBER OF CLUSTERS
x11()
tot.withinss = NULL
for(i in 1:10) 
  tot.withinss =c(tot.withinss,kmeans(PAR,i,nstart=20)$tot.withinss) 
plot(1:10,tot.withinss,type='b', xlab='K') 



#K - MEANS POINTS ASSISTS REBOUND
cluster<- kmeans(PAR, centers = 3, nstart=2)

#X11()
#plot(PAR, col=rom$cluster, asp = 1, pch=16)

#x11()
#plot(PAR, col = rom$cluster,asp = 1,pch=16, size=5)

x11()
plot3d(PAR, col = cluster$cluster,asp = 1,pch=16, size=5)



#hierarchical CLUSTERING 
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


#OPTIMAL NUMBER OF CLUSTERS
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



#K-MEANS ON THE CAREER SPAN

#career span for different eras
CAREER <- FINALSTATS[,-c(1,2,5,6,7,8,9,10,11,12,13,14)]

cluster1<- kmeans(CAREER, centers = 2, nstart=2)
x11()
plot(CAREER, col = cluster1$cluster,asp = 1,pch=16)

#OPTIMAL NUMBER OF CLUSTERS
x11()
tot.withinss = NULL
for(i in 1:10) 
  tot.withinss =c(tot.withinss,kmeans(CAREER,i,nstart=20)$tot.withinss) 
plot(1:10,tot.withinss,type='b', xlab='K') 




role <- FINALSTATS$`Position`
roles = as.numeric(as.factor(role))


PCstats <- FINALSTATS[,-c(1,2,3,4,5,11,12,13,14)]
X <- PCstats

# Data rescaling
rescale <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
X.res <- as.data.frame(lapply(X, rescale))

# Data standardization
standardize <- function(x) {
  return ((x - mean(x)) / sd(x)) }
X.stand <- as.data.frame(lapply(X, standardize))

PCs = princomp(X.stand) 
summary(PCs) 
PCs$loadings

x11()
par(mar = c(1,4,0,2), mfrow = c(3,3))
for(i in 1:7) 
  barplot(PCs$loadings[,i], ylim = c(-1, 1))

X11()
biplot(PCs)


################################################################################
################################################################################
################################################################################



mydata <- read.table('cleaneddata.txt', header=T, sep=',')

# Select social features
X <- PAR

### You might need to preprocess your data
# Data rescaling
rescale <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
X.rescaled <- as.data.frame(lapply(X, rescale))

# Data standardization
standardize <- function(x) {
  return ((x - mean(x)) / sd(x)) }
X.standardized <- as.data.frame(lapply(X, standardize))

# Auxiliary packages
library(rgl)
library(resample)

### Frechet Mean and Variance

# Compute the vectors of means (i.e., Euclidean Frechet Mean)
colMeans(X)

# Compute the vectors of variance
colVars(X)

# Compute the total variance  (i.e., Euclidean Frechet Variance)
sum(colVars(X))

### PCA of a social features

# Compute the vectors of means
M <- colMeans(X)
# Compute the matrix of covariances
S <- cov(X)

# Plot social features
open3d()
points3d(X, asp=1, size=4)
axes3d()
plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.25, add = TRUE)

# PC0: dimension of the approximating space = 0

open3d()
points3d(X, asp=1, size=4)
axes3d()

points3d(t(M), col='red', size=6)

for(i in 1:dim(PAR)[1])
  lines3d(rbind(X[i,], M))

plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.25, add = TRUE)


# PC1: dimension of the approximating space = 1

open3d()
points3d(X, asp=1, size=4)
axes3d()

PC <- princomp(X)
PC1 <- NULL
for(i in 1:dim(PAR)[1])
  PC1 <- rbind(PC1, PC$loadings[,1]*PC$scores[i,1] + M)
points3d(PC1, col='red', size=6)

for(i in 1:dim(PAR)[1])
  lines3d(rbind(X[i,], PC1[i,]))

for(i in 1:dim(PAR)[1])
  lines3d(rbind(M, PC1[i,]), col='red')

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]), col='red') 
plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.25, add = TRUE)

# PC12: dimension of the approximating space = 2

open3d()
points3d(X, asp=1, size=4)
axes3d()

PC <- princomp(X)
PC12 <- NULL
for(i in 1:dim(PAR)[1])
  PC12 <- rbind(PC12, PC$loadings[,1]*PC$scores[i,1] + PC$loadings[,2]*PC$scores[i,2] + M)
points3d(PC12, col='red', size=6)

for(i in 1:dim(PAR)[1])
  lines3d(rbind(X[i,], PC12[i,]))

for(i in 1:dim(PAR)[1])
  lines3d(rbind(M, PC12[i,]), col='red')

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]), col='red') 
lines3d(rbind(M + 2*PC$sdev[2] * PC$loadings[,2], M - 2*PC$sdev[2] * PC$loadings[,2]), col='red') 
plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.25, add = TRUE)

# PC123: : dimension of the approximating space = 3

open3d()
points3d(X, asp=1, size=4)
axes3d()

PC <- princomp(X)
PC123 <- NULL
for(i in 1:dim(PAR)[1])
  PC123 <- rbind(PC123, PC$loadings[,1]*PC$scores[i,1] + PC$loadings[,2]*PC$scores[i,2] + PC$loadings[,3]*PC$scores[i,3] + M)
points3d(PC123, col='red', size=6)

for(i in 1:dim(PAR)[1])
  lines3d(rbind(X[i,], PC123[i,]))

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]), col='red') 
lines3d(rbind(M + 2*PC$sdev[2] * PC$loadings[,2], M - 2*PC$sdev[2] * PC$loadings[,2]), col='red') 
lines3d(rbind(M + 2*PC$sdev[3] * PC$loadings[,3], M - 2*PC$sdev[3] * PC$loadings[,3]), col='red') 
plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.25, add = TRUE)









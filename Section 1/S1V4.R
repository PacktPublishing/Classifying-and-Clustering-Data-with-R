# Density-based clustering

# Iris data
data("iris")
str(iris)
new <- iris[,-5]

# Installing packages
library(fpc)
library(dbscan)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)

# Obtaining optimal eps value
kNNdistplot(new, k=3)
abline(h = 0.45, lty=2)

# Density-based clustering with fpc & dbscan 
set.seed(123)
f <- fpc::dbscan(new, eps=0.45, MinPts = 2)
d <- dbscan::dbscan(new, 0.45, 2)

# Cluster visualization
fviz_cluster(d, new, geom = "point")

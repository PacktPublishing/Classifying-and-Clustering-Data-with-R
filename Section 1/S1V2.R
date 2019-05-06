# Cluster Analysis

# Data preparation
str(iris)
set.seed(1234)
new <- sample(1:dim(iris)[1], 15)
data <- iris[new,]

library(psych)
pairs.panels(data[,-5],
             gap=0,
             bg=c("red", "yellow", "blue")[data$Species],
             pch=21)
summary(data)

# Data normalization & distance matrix
z <- data[,-5]
m <- apply(z, 2, mean)
s <- apply(z, 2, sd)
z <- scale(z, center=m, scale=s)
z <- data.frame(z)

distance <- dist(z)
print(distance, digits=2)

# Dendrogram - complete linkage
hc <- hclust(distance)
plot(hc, labels=data$Species, main="Dendrogram")

# Dendrogram - average linkage
ha <- hclust(distance, method="average")
plot(ha, labels=data$Species, main="Dendrogram", hang=-1)

# Characterizing clusters
member <- cutree(hc, 3)
aggregate(z, list(member),mean)

# Silhouette plot
library(cluster)
plot(silhouette(cutree(hc,3), distance))

# Iris data
data(iris)
str(iris)

# Scatter plots and correlations
library(psych)
pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)

# Data partition
set.seed(222)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.7, 0.3))
training <- iris[ind == 1,]
testing <- iris[ind==2,]

# Linear discriminant analysis
library(MASS)
linear <- lda(Species~., training)
linear
attributes(linear)
p <- predict(linear, training)

# Stacked histograms
ldahist(data = p$x[,1], g = training$Species)
ldahist(data = p$x[,2], g = training$Species)

# Bi-plot
library(devtools)
# install_github("fawda123/ggord")
library(ggord)
ggord(linear, training$Species, ylim = c(-10,10))

# Partition plots
library(klaR)
partimat(Species~., data=training, method = "lda")

# Confusion matrix and accuracy for training data
p1 <- predict(linear, training)$class
tab1 <- table(Predicted = p1, Actual = training$Species)
tab1
sum(diag(tab1))/sum(tab1)

# Confusion matrix and accuracy for testing data
p2 <- predict(linear, testing)$class
tab2 <- table(Predicted = p2, Actual = testing$Species)
tab2
sum(diag(tab2))/sum(tab2)

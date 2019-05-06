# Time series data
data("AirPassengers")
AP <- AirPassengers
str(AP)
head(AP)
ts(AP, frequency = 12, start=c(1949,1))
attributes(AP)
plot(AP)

# Log transform
AP <- log(AP)
plot(AP)

# Decomposition of additive time series
decomp <- decompose(AP)
decomp$figure
plot(decomp$figure,
     type = 'b',
     xlab = 'Month',
     ylab = 'Seasonality Index',
     col = 'blue',
     las = 2)
plot(decomp)

# ARIMA - Autoregressive Integrated Moving Average
library(forecast)
model <- auto.arima(AP)
attributes(model)

# ACF and PACF plots
acf(model$residuals, main = 'Correlogram')
pacf(model$residuals, main = 'Partial Correlogram' )

# Ljung-Box test
Box.test(model$residuals, lag=20, type = 'Ljung-Box')

# Residual plot
hist(model$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of Residuals',
     freq = FALSE)
lines(density(model$residuals))

# Forecast
f <- forecast(model, 48)
library(ggplot2)
autoplot(f)
accuracy(f)

## Time series clustering

# Data 
# http://kdd.ics.uci.edu/databases/synthetic_control/synthetic_control.html
data <- read.table(file.choose(), header = F, sep = "")
str(data)
plot(data[,60], type = 'l')
j <- c(5, 105, 205, 305, 405, 505)
sample <- t(data[j,])
plot.ts(sample,
        main = "Time-series Plot",
        col = 'blue',
        type = 'b')

# Data preparation
n <- 10
s <- sample(1:100, n)
i <- c(s,100+s, 200+s, 300+s, 400+s, 500+s)
d <- data[i,]
str(d)

pattern <- c(rep('Normal', n),
             rep('Cyclic', n),
             rep('Increasing trend', n),
             rep('Decreasing trend', n),
             rep('Upward shift', n),
             rep('Downward shift', n))

# Calculate distances
library(dtw)
distance <- dist(d, method = "DTW")

# Hierarchical clustering
hc <- hclust(distance, method = 'average')
plot(hc,
     labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
rect.hclust(hc, k=4)

# Time series classification

# Data preparation
pattern100 <- c(rep('Normal', 100),
                rep('Cyclic', 100),
                rep('Increasing trend', 100),
                rep('Decreasing trend', 100),
                rep('Upward shift', 100),
                rep('Downward shift', 100))
newdata <- data.frame(data, pattern100)
str(newdata)

# Classification with decision tree
library(party)
tree <- ctree(pattern100~., newdata)

# Classification performance
tab <- table(Predicted = predict(tree, newdata), Actual = newdata$pattern100)
sum(diag(tab))/sum(tab)

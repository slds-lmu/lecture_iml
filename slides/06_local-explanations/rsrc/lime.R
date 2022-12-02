
library(mlr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(readr)
library(randomForest)
library(earth)
library(MASS)
# library(lime)
library(foreach)
# library(doRNG)
library(doParallel)
library(grDevices)
library(proxy)

# make_split <- function(data, share, seed = 100) {
#   set.seed(seed)
#   split <- sample(1:nrow(data), floor(share * nrow(data)))
#   return(list(train = data[split, ], test = data[-split, ]))
# 
# }


set.seed(10)
x <- seq(0, 10, length.out = 200)
y <- 2 * (sin(0.04* x ^ 2) + 0.1 * x) + 2
data <- data.frame(x = x, y = y)

fig0 <- ggplot(data, aes(x = x, y = y)) + 
  # geom_smooth(se = FALSE, col = "black") + 
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(y ~ x))),
    alpha = 0.6,fill = 'lightgray') + 
  geom_ribbon(aes(ymin = predict(loess(y ~ x)), ymax = 8),
    alpha = 0.9,fill = 'darkgray') + 
  theme_bw() + ylim(c(0, 8)) +
  ylab("x2") + xlab("x1") 
fig0
ggsave("../figure/lime1.pdf", plot = fig0, width = 4, height = 3)

x1train <-  runif(200, 0, 10)
x2train <-  runif(200, 0, 8)
data_train <- data.frame(x1 = x1train, x2 = x2train)

# add point to explain
fig0 <- fig0 + geom_point(aes(x=2.8, y=3.5), colour="yellow", cex = 3)
fig0 
ggsave("../figure/lime2.pdf", plot = fig0, width = 4, height = 3)

# add sampled points
fig0 <- fig0 + geom_point(aes(x = x1train, y = x2train), col = "blue")
fig0
ggsave("../figure/lime3.pdf", plot = fig0, width = 4, height = 3)

xorig <- c(2.8, 3.5)
euclidean <- function(x) sqrt(sum((xorig - x)^2))
kernel <- function(x, kernel.width = 0.1) {
    d = dist(rbind(x, xorig), method = "euclidean")
    sqrt(exp(-(d^2) / (kernel.width^2)))
  }

size <- apply(data_train, 1, kernel, kernel.width = 1)
# range01 <- function(x){0.5*(x-min(x))/(max(x)-min(x))}
# size <- range01(size)

# add weights
fig0 <- fig0 + geom_point(aes(x = x1train, y = x2train, size = size), col = "blue") + 
  theme(legend.title = element_blank(), legend.position = "none")
fig0
ggsave("../figure/lime4.pdf", plot = fig0, width = 4, height = 3)

# add linear model 
fig0 <- fig0 + geom_abline(intercept = 1.85, slope = 0.58, color="red", 
    linetype="dashed", size=1.51) +
  geom_point(aes(x = x1train, y = x2train), col = "blue") +
  theme(legend.title = element_blank(), legend.position = "none")  
fig0 <- fig0 + geom_point(aes(x=2.8, y=3.5), colour="yellow", cex = 3)
fig0
ggsave("../figure/lime5.pdf", plot = fig0, width = 4, height = 3)


#----------- SAMPLING STRATEGIES ----------------------
fig0 <- ggplot(data, aes(x = x, y = y)) + 
  # geom_smooth(se = FALSE, col = "black") + 
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(y ~ x))),
    alpha = 0.6,fill = 'lightgray') + 
  geom_ribbon(aes(ymin = predict(loess(y ~ x)), ymax = 7),
    alpha = 0.9,fill = 'darkgray') + 
  theme_bw() + ylim(c(0, 7.4)) +
  ylab("x2") + xlab("x1") 
fig0

x1train <-  seq(0.5, 9.5, length.out = 14)
x2train <-  seq(0.3, 8, length.out = 15)
data_train <- data.frame(expand.grid(x1train, x2train))

# GRID
fig1 <- fig0 + geom_point(aes(x = data_train$Var1[1:200], 
  y = data_train$Var2[1:200]), col = "blue") + 
  ylim(c(1, 7.5)) +
  ylim(c(0, 7)) +
  geom_point(aes(x=2.8, y=3.5), colour="yellow", cex = 3)
fig1
ggsave("../figure/lime3a.pdf", plot = fig1, width = 4, height = 3)




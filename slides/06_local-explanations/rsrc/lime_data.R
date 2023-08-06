
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

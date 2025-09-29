
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(gower)
theme_set(theme_bw())


set.seed(123)
load("../../../data/bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)
bike.x = bike[names(bike) != 'cnt']

#####################################################

set.seed(123)
n_features_lime = 3
mod = train("regr.randomForest", task)
pred = Predictor$new(mod, data = bike.x, class = "above")
lim = LocalModel$new(pred, x.interest = bike.x[1,], k = n_features_lime)
a = plot(lim)
a

##################################################

set.seed(123)
lim = LocalModel$new(pred, x.interest = bike.x[100,], k = 3)
b = plot(lim)

grid.arrange(a, b, nrow = 2, ncol = 1)

#################################################


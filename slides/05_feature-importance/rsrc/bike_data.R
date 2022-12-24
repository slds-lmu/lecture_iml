library("iml")
library("randomForest")
library("mlr")

# setwd('~/university/phd/2021/teaching/lecture_iml/slides/feature-importance/rsrc')

set.seed(123)
load("../../../data/bike.RData")
bike = na.omit(bike)
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)

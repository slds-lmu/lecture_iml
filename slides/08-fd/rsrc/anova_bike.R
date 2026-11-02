# PREREQ -----------------------------------------------------------------------

library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(iml)

# DATA -------------------------------------------------------------------------

set.seed(123)
load("../../../data/bike.RData")
tsk = TaskRegr$new(id = "bike", backend = bike, target = "cnt")
mod = lrn("regr.ranger")$train(tsk)

pred.bike = Predictor$new(mod, data = bike)
bike.x = bike[names(bike) != 'cnt']
mean.pred = round(mean(mod$predict(tsk)$response), 3)

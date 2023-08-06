# PREREQ -----------------------------------------------------------------------
library(knitr)
library(mlr3verse)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggplot2)
library(patchwork)
library(ggrastr)
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------

set.seed(123)
load("../../../data/bike.RData")
tsk = TaskRegr$new(id = "bike", backend = bike, target = "cnt")
mod = lrn("regr.ranger")$train(tsk)

pred.bike = Predictor$new(mod, data = bike[,names(bike) != 'cnt'])
mean.pred = round(mean(mod$predict(tsk)$response), 3)

#################################################

instance.index = 200
mean.prediction = mean(pred.bike$predict(bike)[[1]])
instance.prediction = pred.bike$predict(bike[instance.index,])[[1]]
shap = Shapley$new(pred.bike, x.interest = bike[instance.index, names(bike) != 'cnt'], sample.size = 10000)
strongest.feature = shap$results[shap$results$phi == max(shap$results$phi),]
p = plot(shap)

ggsave("../figure/shapley-bike.pdf", p, width = 9, height = 4)

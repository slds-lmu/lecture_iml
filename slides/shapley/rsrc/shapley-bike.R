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
load("data/bike.RData")
tsk = TaskRegr$new(id = "bike", backend = bike, target = "cnt")
mod = lrn("regr.ranger")$train(tsk)

pred.bike = Predictor$new(mod, data = bike)
mean.pred = round(mean(mod$predict(tsk)$response), 3)

#################################################

instance.index = 200
mean.prediction = mean(pred.bike$predict(bike)[[1]])
instance.prediction = pred.bike$predict(bike[instance.index,])[[1]]
shap = Shapley$new(pred.bike, x.interest = bike[instance.index,], sample.size = 300)
strongest.feature = shap$results[shap$results$phi == max(shap$results$phi),]
p = plot(shap)

ggsave("slides/shapley/figure/shapley-bike.pdf", p, width = 8, height = 5)

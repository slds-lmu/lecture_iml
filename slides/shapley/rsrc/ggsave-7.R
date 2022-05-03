
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)


set.seed(123)
load("data/bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)
bike.x = bike[names(bike) != 'cnt']

#################################################

instance.index = 200
mean.prediction = mean(predictor$predict(bike)[[1]])
instance.prediction = predictor$predict(bike[instance.index,])[[1]]
shap = Shapley$new(predictor, x.interest = bike.x[instance.index,], sample.size = 300)
strongest.feature = shap$results[shap$results$phi == max(shap$results$phi),]
plot(shap)

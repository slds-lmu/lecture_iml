library("iml")
library("randomForest")
library("mlr")

set.seed(123)
load("bike.RData")
bike = na.omit(bike)
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)

imp <- FeatureImp$new(predictor, loss = "mae")
library("ggplot2")
plot(imp)
imp$results
ggsave('../figure_man/bike_pfi.pdf', width=4, height=4)

pdp <- FeatureEffects$new(predictor, method='pdp+ice')
pdp$plot()
ggsave('../figure_man/bike_pdp+ice.pdf', width=7, height=4)

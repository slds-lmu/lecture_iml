library(ggplot2)
library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggpubr)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("helpers.R")

set.seed(123)
load("bike.RData")
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
bike.x = bike[names(bike) != 'cnt']

pred.sub = Predictor$new(mod, data = bike, y = bike$cnt)

pdp = FeatureEffect$new(pred.sub, "windspeed", method = "pdp+ice")
p = pdp$plot() + scale_x_continuous('Humidity') + scale_y_continuous('Predicted bike rentals')

pdp = FeatureEffect$new(pred.sub, "windspeed", method = "pdp+ice", center.at = min(bike$windspeed))
pmin = pdp$plot() + scale_x_continuous('Humidity') + scale_y_continuous('Predicted bike rentals')

p + ggtitle("ICE plot") +
  pmin + ggtitle("centered ICE plot")

ggsave("../figure_man/cICE.pdf", pmin, width = 5.5, height = 3)

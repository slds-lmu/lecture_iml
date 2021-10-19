library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(ggplot2)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("slides/feature-effects/rsrc/helpers.R")

load("data/bike.RData")

set.seed(123)
task = makeRegrTask(data = bike, target = "cnt")
mod = train("regr.randomForest", task)
predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)
bike.x = bike[names(bike) != 'cnt']

pred.bike = Predictor$new(mod, data = bike)

H.global = Interaction$new(pred.bike)
p1 = H.global$plot() + ggtitle("Overall interactions")

H.twoway = Interaction$new(pred.bike, feature = "temp")
p2 = H.twoway$plot() + scale_x_continuous("2-way interaction strength") +
  ggtitle("2-way interactions with 'temp'")

library(patchwork)
p = p1 + p2

ggsave("slides/feature-effects/figure_man/h-statistic.pdf", p, width = 8, height = 3)

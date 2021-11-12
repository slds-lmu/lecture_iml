# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(mlr3)
library(mlr3extralearners)
library(iml)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

set.seed(123)
load("data/bike.RData")
task = TaskRegr$new(id = "bike_task", backend = bike, target = "cnt")
learner = lrn("regr.randomForest")
learner$train(task)
pred.bike = Predictor$new(learner, data = bike)

pdp = FeatureEffect$new(pred.bike, "hum", method = "pdp", grid.size = 50)
ale = FeatureEffect$new(pred.bike, "hum", method = "ale", grid.size = 50)

# PLOT -------------------------------------------------------------------------

pdp_plot <- pdp$plot() + scale_y_continuous('Univariate PD on humidity')
ale_plot <- ale$plot() + scale_y_continuous('First order ALE of humidity')
ale1d = gridExtra::grid.arrange(pdp_plot, ale_plot, nrow = 1, ncol = 2)

ggsave("slides/feature-effects/figure/ale1d.pdf", ale1d, width = 8, height = 4)

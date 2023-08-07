# PREREQ -----------------------------------------------------------------------

library(mlr3)
library(iml)
library(ggplot2)
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------

set.seed(10)
n = 700
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
eps = rnorm(n, 0, 1)
y = 0.2*x1 - 8*x2 + ifelse(x3 >= 0, 16*x2, 0) + eps
dat = data.frame(x1, x2, x3, y)

tsk = TaskRegr$new(id = "synthdta", backend = dat, target = "y")
lrn = lrn("regr.gbm", interaction.depth = 6)
lrn$train(tsk)

pred = Predictor$new(lrn, data = dat[-which(names(dat) == "y")], y = dat$y)
pdp = FeatureEffect$new(pred, "x2", "pdp+ice")

# PLOT -------------------------------------------------------------------------

p1 = pdp$plot()

ggsave("../figure/pdp_pitfall.pdf", p1)

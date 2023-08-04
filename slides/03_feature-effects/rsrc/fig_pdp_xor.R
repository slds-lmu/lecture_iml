# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(mlr3)
<<<<<<< HEAD
library(mlr3learners)
library(mlr3extralearners)
library(gbm)
library(iml)
library(ggplot2)
=======
source("anova_bike.R")
>>>>>>> 695105b2b72f4ffa5ce2cd9d55cdf2d154fab5b5

# DATA -------------------------------------------------------------------------

set.seed(10)
n = 700
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
eps = rnorm(n, 0, 1)
y = 0.2*x2 - 8*x1 + ifelse(x3 >= 0, 16*x1, 0) + eps
dat = data.frame(x1, x2, x3, y)

tsk = TaskRegr$new(id = "dAt", backend = dat, target = "y")
lrn = lrn("regr.gbm", interaction.depth = 6)
mod = lrn$train(tsk)
pred = Predictor$new(mod, data = dat[-which(names(dat) == "y")], y = dat$y)

pdp = FeatureEffect$new(pred, "x1", method = "pdp+ice")

# PLOT -------------------------------------------------------------------------

p1 = pdp$plot()
ggxor = p1 + scale_y_continuous(name = expression(hat(f)[S])) +
  scale_x_continuous(name = expression(x[1]))

ggsave("../figure/pdp_xor.pdf", ggxor, width = 6, height = 4)
